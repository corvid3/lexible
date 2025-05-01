#pragma once

#include <algorithm>
#include <concepts>
#include <format>
#include <iostream>
#include <mutex>
#include <optional>
#include <regex>
#include <span>
#include <sstream>
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
};

template<typename T>
concept is_lexer = requires(T t) {
  requires is_token<typename T::token>;
  { t.next() } -> std::same_as<std::optional<typename T::token>>;
};

template<auto const& REGEX, auto TAG, size_t AFFINITY>
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

    if (ce == SKIP_MORPHEME::tag)
      goto restart;

    token t;
    t.type = ce;
    t.what = substr.substr(0, out.length());
    return t;
  }

  std::string_view get_substr() const
  {
    return { m_src.begin() + m_off, m_src.end() };
  }

  std::string_view m_src;
  size_t m_off = 0;

private:
  // initializes regexes from
  using regex_pair = std::pair<std::regex, Enum>;
  static inline std::vector<regex_pair> m_regexOrders;

  static int sort_regex_orders()
  {
    auto const get_regex = []<typename T>() -> std::regex {
      return [](std::string const& x) {
        return std::regex(x.begin(),
                          x.end(),
                          std::regex_constants::ECMAScript |
                            std::regex_constants::multiline);
      }((std::stringstream() << '^' << T::regex_contents).str());
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
{};

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

template<typename T>
concept Parser = requires { true; };

template<typename LEXER, typename STATE>
  requires is_lexer<LEXER>
class ParsingContext
{
  using token_t = typename LEXER::token;
  using token_queue_t = NonowningQueue<token_t>;

public:
  // use this for when a parser shouldn't return anything,
  // other than the fact that it successfully parsed or not
  struct empty_t
  {};

  template<int>
  struct placeholder_t
  {
    constexpr placeholder_t() = default;
  };

  template<typename START>
  class Parser
  {
  public:
    Parser(std::string_view const str)
    {
      LEXER lex(str);

      for (;;) {
        auto const tok = lex.next();
        if (not tok)
          break;
        m_toks.push_back(*tok);
      }
    }

    Parser(const Parser&) = delete;
    Parser(Parser&&) = delete;
    Parser& operator=(const Parser&) = delete;
    Parser& operator=(Parser&&) = delete;

    auto parse(STATE& s) &&
    {
      token_queue_t t(std::span{ m_toks });
      return START().template run<START>(s, t);
    }

    auto parse() &&
    {
      STATE s;
      token_queue_t t(std::span{ m_toks });
      return START().template run<START>(s, t);
    }

  private:
    std::vector<token_t> m_toks;
  };

  // ON_TRUE should be a lambda that takes:
  //   - STATE& s
  //   - std::string_view what
  // and returns any value,
  //   if no value is desired then use empty_t
  template<auto const EXPECTED_TYPE>
  struct MorphemeParser
  {
    template<typename Self>
    auto run(STATE& state, token_queue_t& toks) -> std::optional<
      typename FunctionTypes<decltype(&Self::operator())>::ret_type>
    {
      auto const tok_opt = toks.next();

      if (not tok_opt)
        return std::nullopt;

      is_token auto const tok = *tok_opt;
      if (tok.type != EXPECTED_TYPE)
        return std::nullopt;

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
  struct AndThen
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
      if (failed == true)
        return std::optional<out>(std::nullopt);

      else {
        auto unwrapped_tup =
          std::apply([](auto&&... args) { return std::tuple(*args...); }, tup);

        return (std::optional<out>)static_cast<Self&>(*this)(state,
                                                             unwrapped_tup);
      }
    }
  };

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
  struct Any
  {
    using first_type = std::tuple_element_t<0, std::tuple<MAYBE...>>;

    template<typename Self, int i>
    auto out_type_at(auto& state)
    {
      return decltype(std::declval<Self>().operator()(
        state, {}, placeholder_t<i>()))();
    }

    template<typename Self, std::size_t... Is>
    void assert(auto& state, std::index_sequence<Is...>)
    {
      static_assert((std::same_as<decltype(out_type_at<Self, Is>(state)),
                                  decltype(out_type_at<Self, 0>(state))> &&
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
        out =
          static_cast<Self&>(*this)(state, *first_match, placeholder_t<I>());
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

    template<typename Self>
    auto run(STATE& state, token_queue_t& toks)
    {
      using index_seq = std::index_sequence_for<MAYBE...>;

      // verify that all return values of the
      assert<Self>(state, index_seq{});
      using out_type = decltype(out_type_at<Self, 0>(state));

      // need to get result type
      std::optional<out_type> first_match;

      // idk? maybe this gets rid of recursion??
      if (toks.empty())
        return (decltype(first_match))std::nullopt;

      apply<Self, MAYBE...>(first_match, state, toks, index_seq{});

      return first_match;
    }
  };

  template<typename T>
  struct DenatureOptional;
  template<typename INNER>
  struct DenatureOptional<std::optional<INNER>>
  {
    using type = INNER;
  };

  // INNER must be the type of another parsing structure
  // AT_LEAST_ONE tells Repeat if it should fail parsing
  //   if it fails to parse a single INNER structure
  // the operator() of the derived structure should take as its
  // second argument a std::span of the return values
  // of Inner
  template<typename INNER, bool AT_LEAST_ONE>
  struct Repeat
  {
    template<typename Self>
    auto run(STATE& state, token_queue_t& toks)
    {
      using return_type =
        typename FunctionTypes<decltype(&Self::operator())>::ret_type;

      using child_return_type = typename FunctionTypes<
        decltype(&INNER::template run<INNER>)>::ret_type::value_type;

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
          if constexpr (AT_LEAST_ONE)
            if (i == 0)
              return std::optional<return_type>(std::nullopt);

          // otherwise, return what we got.
          break;
        }

        toks = copy;
        pipe.push_back(*into);
      }

      return std::optional<return_type>(static_cast<Self&>(*this)(
        state, std::span{ pipe.begin(), pipe.end() }));
    }
  };
};
};
