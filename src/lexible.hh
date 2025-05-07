#pragma once

#include <algorithm>
#include <concepts>
#include <format>
#include <functional>
#include <iostream>
#include <map>
#include <mutex>
#include <optional>
#include <regex>
#include <set>
#include <span>
#include <sstream>
#include <stdexcept>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>

namespace lexible {

template<typename T>
concept is_enum = requires { requires std::is_scoped_enum_v<T>; };

template<typename T>
concept is_token = requires(T t) {
  requires is_enum<decltype(t.type)>;
  { t.what } -> std::convertible_to<std::string_view>;

  { t.col } -> std::convertible_to<std::size_t>;
  { t.row } -> std::convertible_to<std::size_t>;
};

template<typename T>
concept is_lexer = requires(T t) {
  requires is_token<typename T::token>;
  { t.next() } -> std::same_as<std::optional<typename T::token>>;
};

template<auto const& REGEX,
         auto TAG,
         size_t AFFINITY,
         bool CASE_INSENSITIVE = false>
  requires std::convertible_to<std::string_view,
                               std::remove_reference_t<decltype(REGEX)>>
class morpheme
{
  using Enum = decltype(TAG);

public:
  static constexpr std::string_view regex_contents = REGEX;
  static constexpr Enum tag = TAG;

  // higher affinity means that the tag
  // will apply higher than an other tag
  static constexpr int binding_affinity = AFFINITY;
  static constexpr bool case_insensitive = CASE_INSENSITIVE;
};

template<typename Enum, typename SKIP_MORPHEME, typename... MORPHEMES>
  requires std::is_scoped_enum_v<Enum>
class lexer
{
public:
  struct token
  {
    Enum type;
    std::string_view what;
    std::size_t col, row;
  };

  lexer(std::string_view src)
    : m_src(src)
  {
    static std::once_flag init;
    std::call_once(init, sort_regex_orders);
  };

  std::optional<token> next()
  {
  restart:
    if (m_off == m_src.size())
      return std::nullopt;

    std::cmatch out;
    auto substr = get_substr();

    Enum ce{};

    for (auto const& [match, e] : m_regexOrders) {
      ce = e;
      if (std::regex_search(substr.begin(), substr.end(), out, match))
        break;
      out = {};
    }

    if (out.empty())
      throw std::runtime_error(
        std::format("unknown input in lexer @ pos {}", m_off));

    m_off += out.length();

    std::string_view out_sv((char const*)&*out.begin(),
                            (char const*)&*out.end());

    // save these for the _start_ position of the token
    auto const saved_row = m_rowCtr;
    auto const saved_col = m_colCtr;

    // have to put this here so the skip token
    // also counts towards advancing the row & col
    if (out_sv.contains('\n')) {
      auto const last_of = out_sv.find_last_of('\n');

      m_rowCtr += std::count(out_sv.begin(), out_sv.end(), '\n');
      m_colCtr = out_sv.size() - last_of;
    } else {
      m_colCtr += out.size();
    }

    if (ce == SKIP_MORPHEME::tag)
      goto restart;

    token t;
    t.type = ce;
    t.what = substr.substr(0, out.length());
    t.col = saved_col;
    t.row = saved_row;

    return t;
  }

  std::string_view get_substr() const
  {
    return { m_src.begin() + m_off, m_src.end() };
  }

  std::string_view m_src;
  size_t m_off = 0;

  size_t m_rowCtr = 0;
  size_t m_colCtr = 0;

private:
  // initializes regexes from
  using regex_pair = std::pair<std::regex, Enum>;
  static inline std::vector<regex_pair> m_regexOrders;

  static int sort_regex_orders()
  {
    auto const get_regex = []<typename T>() -> std::regex {
      return [](std::string const& x) {
        return std::regex(
          x.begin(),
          x.end(),
          (T::case_insensitive
             ? std::regex_constants::ECMAScript | std::regex_constants::icase
             : std::regex_constants::ECMAScript));
      }((std::stringstream() << "^(" << T::regex_contents << ")").str());
    };

    using sort_type = std::tuple<std::regex, Enum, int>;
    std::vector<sort_type> m_unsorted;

    m_unsorted.push_back(
      std::make_tuple(get_regex.template operator()<SKIP_MORPHEME>(),
                      SKIP_MORPHEME::tag,
                      SKIP_MORPHEME::binding_affinity));

    (m_unsorted.push_back(
       std::make_tuple(get_regex.template operator()<MORPHEMES>(),
                       MORPHEMES::tag,
                       MORPHEMES::binding_affinity)),
     ...);

    auto out = m_unsorted.size();

    std::sort(m_unsorted.begin(),
              m_unsorted.end(),
              [](sort_type const& lhs, sort_type const& rhs) {
                return std::get<2>(lhs) > std::get<2>(rhs);
              });

    // make sure we move the regex instead of copying
    for (auto const& [r, e, m] : m_unsorted)
      m_regexOrders.push_back(std::pair{ r, e });

    return out;
  }
};

template<typename T>
  requires is_token<T>
class NonowningQueue
{
public:
  NonowningQueue(std::span<T const> toks)
    : m_toks(toks)
  {
  }

  std::optional<T> next()
  {
    if (m_idx >= m_toks.size())
      return std::nullopt;
    return m_toks[m_idx++];
  }

  bool empty() const { return m_idx >= m_toks.size(); }

  size_t size() const { return m_toks.size() - m_idx; }

private:
  std::span<T const> m_toks;
  size_t m_idx = 0;
};

template<typename Ret, typename... Params>
struct FunctionTypes
{
  using ret_type = Ret;
};

template<typename Ret, typename Class, typename... Params>
struct FunctionTypes<Ret (Class::*)(Params...) const>
{
  using ret_type = Ret;
};

template<typename Ret, typename Class, typename... Params>
struct FunctionTypes<Ret (Class::*)(Params...)>
{
  using ret_type = Ret;
};

template<typename Ret, typename... Params>
struct FunctionTypes<Ret (*)(Params...)>
{};

template<typename T>
concept Parser = requires { true; };

// use this for when a parser shouldn't return anything,
// other than the fact that it successfully parsed or not
struct empty_t
{};

template<typename Self, typename STATE>
concept has_err_fn = requires(Self s) { s.err(std::declval<STATE&>(), {}); };

template<typename LEXER, typename STATE>
  requires is_lexer<LEXER>
class ParsingContext
{
  using token_t = typename LEXER::token;
  using token_queue_t = NonowningQueue<token_t>;

  static inline thread_local size_t parsing_depth;

  struct err_buckets
  {
    std::map<std::size_t, std::vector<std::string>> m_buckets;

    void push_error(std::size_t depth, std::string_view what)
    {
      if (not m_buckets.contains(depth))
        m_buckets.insert_or_assign(depth, std::vector<std::string>{});

      m_buckets[depth].push_back(std::string(what));
    }

    auto const& get_deepest_bucket() const
    {
      if (m_buckets.size() == 0)
        throw std::runtime_error(
          "attempting to get an error bucket when there are no buckets!");
      return (--m_buckets.end())->second;
    }
  };

  // parsing_depth, bucket of errors
  // report all errors at said depth
  // maybe just the last.
  static inline thread_local err_buckets parsing_errors;

public:
  template<int>
  struct placeholder_t
  {
    constexpr placeholder_t() = default;
  };

  template<typename START>
  class Engine
  {
  public:
    Engine(std::string_view const str)
    {
      LEXER lex(str);

      for (;;) {
        auto const tok = lex.next();
        if (not tok)
          break;
        m_toks.push_back(*tok);
      }
    }

    Engine(const Engine&) = delete;
    Engine(Engine&&) = delete;
    Engine& operator=(const Engine&) = delete;
    Engine& operator=(Engine&&) = delete;

    auto parse(STATE& s) &&
    {
      token_queue_t t(std::span{ m_toks });
      auto&& out = START().template run<START>(s, t);

      if (not t.empty()) {
        auto const& errs = parsing_errors.get_deepest_bucket();

        if (errs.size() == 0)
          throw std::runtime_error(
            "especially weird error, parsing error bucket was created but no "
            "errors were ever pushed?");

        // TODO: make parsing type
        throw std::runtime_error(errs.back());
      }

      return out;

      // NOTE: from reading how another person implemented
      // a backtracking parser, it seems that one
      // should expect the end of the file (i.e. no tokens)
      // after a parse.
      // if there is still tokens,
      // that means the file failed to parse
      // and one should return the _DEEPEST_ syntax error.
      // i.e., keep a ThreadLocal index of the stackframes &
      // a list of syntax errors.
      // when the EOF fails to parse, return the syntax error
      // that corresponds with the highest index of stackframe
    }

    auto parse() &&
    {
      STATE s;
      return std::move(*this).parse(s);
    }

  private:
    std::vector<token_t> m_toks;
  };

  struct Parser
  {
    constexpr Parser() { ParsingContext::parsing_depth += 1; }
    constexpr ~Parser() { ParsingContext::parsing_depth -= 1; }
  };

  // ON_TRUE should be a lambda that takes:
  //   - STATE& s
  //   - std::string_view what
  // and returns any value,
  //   if no value is desired then use empty_t
  template<auto const EXPECTED_TYPE>
  struct MorphemeParser : private Parser
  {
    template<typename Self>
    auto run(STATE& state, token_queue_t& toks) -> std::optional<
      typename FunctionTypes<decltype(&Self::operator())>::ret_type>
    {
      auto const tok_opt = toks.next();

      if (not tok_opt)
        return std::nullopt;

      is_token auto const tok = *tok_opt;
      if (tok.type != EXPECTED_TYPE) {
        parsing_errors.push_error(parsing_depth, "failed to parse in morpheme");
        return std::nullopt;
      }

      return static_cast<Self&>(*this)(state, tok.what);
    }

    std::string_view operator()(STATE&, std::string_view s) const { return s; }
  };

  // ON_TRUE should be a lambda that takes:
  //   - STATE& s
  //   - std::tuple<...> whats
  //     each value in "whats" are the return values
  //     of each parser requested. (can be auto)
  // and returns any value,
  //   if no value is desired then use empty_t
  template<typename... EXPECTED>
  struct AndThen : private Parser
  {
    template<typename Self>
    auto run(STATE& state, token_queue_t& toks)
    {
      auto tup =
        std::tuple{ EXPECTED().template run<EXPECTED>(state, toks)... };
      bool failed = false;

      using out = decltype(std::declval<Self>().operator()(state, tup));

      std::apply(
        [&](auto const&... args) {
          (
            [&](auto const& arg) {
              if (not arg.has_value())
                failed = true;
              return;
            }(args),
            ...);
        },
        tup);

      // TODO: make error reporting better by
      // allowing optional calls to error reporting functions
      // inside the derived parsing structure
      if (failed == true) {
        parsing_errors.push_error(parsing_depth, "failed to parse in andthen");

        return std::optional<out>{};
      } else {
        auto unwrapped_tup =
          std::apply([](auto&&... args) { return std::tuple(*args...); }, tup);

        return (std::optional<out>)static_cast<Self&>(*this)(state,
                                                             unwrapped_tup);
      }
    }
  };

  // NOTE: maybe make it so that attaching ::err()
  // to a parser makes it a non-failable parse?
  // that is, if it is used within

  // MAYBE... should be a list of parsing types
  // that are allowed to be parsed in this expression.
  // the derived parsing type of Any must have a series of
  // overloaded definitions for operator(),
  // each one taking a State&, a type T, and a placeholder_t<I> that
  // denotes which index into the MAYBE parsing type list that this operator()
  // will handle when said parsing type is the selected one to succeed.
  // the type T in these operators() must be the return type of the
  // parsing type it is associated with
  template<typename... MAYBE>
  struct Any : private Parser
  {
    using first_type = std::tuple_element_t<0, std::tuple<MAYBE...>>;

    template<typename S, std::size_t I>
    struct out_type_at
    {
      template<typename R, typename O>
      std::tuple<R, STATE&, O, placeholder_t<I>> operator()(
        R (S::* const)(STATE&, O, placeholder_t<I>)) const;

      template<typename R, typename O>
      std::tuple<R, STATE&, O, placeholder_t<I>> operator()(
        R (S::*)(STATE&, O, placeholder_t<I>));
    };

    template<typename Self, std::size_t I>
    using out_type_of_out_type_at =
      typename FunctionTypes<decltype(out_type_at<Self, I>{}(
        &Self::operator()))>::ret_type;

    template<typename Self, std::size_t... Is>
    void assert(auto&, std::index_sequence<Is...>)
    {
      static_assert(
        (std::same_as<
           std::remove_reference_t<
             std::tuple_element_t<0, out_type_of_out_type_at<Self, 0>>>,
           std::remove_reference_t<
             std::tuple_element_t<0, out_type_of_out_type_at<Self, Is>>>> &&
         ...));
    }

    template<typename Self, typename PARSER, int I>
    bool apply_impl(auto& out,
                    STATE& state,
                    token_queue_t token_queue,
                    token_queue_t& fin)
    {
      auto first_match = PARSER().template run<PARSER>(state, token_queue);

      if (first_match.has_value()) {
        out.emplace(
          static_cast<Self&>(*this)(state, *first_match, placeholder_t<I>()));
        fin = token_queue;
      }

      return first_match.has_value();
    }

    template<typename Self, typename... PARSER, std::size_t... Is>
    void apply(auto& out,
               STATE& state,
               token_queue_t& tok_out,
               std::index_sequence<Is...>)
    {
      token_queue_t token_copy(tok_out);
      (... || apply_impl<Self, PARSER, Is>(out, state, token_copy, tok_out));
    }

    template<has_err_fn<STATE> Self>
    void run_err(STATE& s, token_t tok)
    {
      Self().err(s, tok);
    }

    template<typename>
    auto run_err(STATE&, std::optional<token_t>)
    {
    }

    template<typename Self>
    auto run(STATE& state, token_queue_t& toks)
    {
      using index_seq = std::index_sequence_for<MAYBE...>;

      // verify that all return values of the operators()
      // are the same
      assert<Self>(state, index_seq{});

      // then, if they are, get the return type of the operator()
      using out_type_of_out_type_at =
        typename FunctionTypes<decltype(out_type_at<Self, 0>{}(
          &Self::operator()))>::ret_type;

      using out_type = std::remove_reference_t<
        std::tuple_element_t<0, out_type_of_out_type_at>>;

      // need to get result type
      std::optional<out_type> first_match;

      // idk? maybe this gets rid of recursion??
      if (toks.empty())
        return std::optional<out_type>();

      apply<Self, MAYBE...>(first_match, state, toks, index_seq{});

      // verified that at least by now theres
      // one token in the queue, so it's free to unwrap
      if (not first_match.has_value()) {
        parsing_errors.push_error(parsing_depth, "failed to parse in any");
        return decltype(first_match)();
      }

      return first_match;
    }
  };

  // INNER must be the type of another parsing structure
  // AT_LEAST_ONE tells Repeat if it should fail parsing
  //   if it fails to parse a single INNER structure
  // the operator() of the derived structure should take as its
  // second argument a std::span of the return values
  // of Inner
  // by default, a Repeat converts all of the successfully parsed
  // values into a std::vector
  template<typename INNER, bool AT_LEAST_ONE>
  struct Repeat : private Parser
  {
    using child_return_type = typename FunctionTypes<
      decltype(&INNER::template run<INNER>)>::ret_type::value_type;

    template<typename Self>
    auto run(STATE& state, token_queue_t& toks)
    {
      using return_type =
        typename FunctionTypes<decltype(&Self::operator())>::ret_type;

      // take all of the values of the repeat,
      // put them into a vector,
      // then pipe the vector as a span into the repeater child class
      std::vector<child_return_type> pipe;

      for (int i = 0; true; i++) {
        token_queue_t copy = toks;
        auto const into = INNER().template run<INNER>(state, copy);

        if (not into) {
          // return nullopt if at least one is true,
          // and this is the first parse and it failed to match
          if constexpr (AT_LEAST_ONE) {
            if (i == 0) {
              // err'd, push one to the queue
              parsing_errors.push_error(parsing_depth,
                                        "failed to parse in repeat");

              return std::optional<return_type>(std::nullopt);
            }
          }
          // otherwise, return what we got.
          break;
        }

        toks = copy;
        pipe.push_back(*into);
      }

      return std::optional<return_type>(static_cast<Self&>(*this)(
        state, std::span{ pipe.begin(), pipe.end() }));
    }

    auto operator()(STATE&, std::span<child_return_type> in) const
    {
      return std::vector(in.begin(), in.end());
    }
  };
};
};
