#pragma once

#include <algorithm>
#include <concepts>
#include <expected>
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

  // required EOF token type
  { decltype(t.type)::LEXIBLE_EOF } -> std::same_as<decltype(t.type)>;

  { t.what } -> std::convertible_to<std::string_view>;
  { t.col } -> std::convertible_to<std::size_t>;
  { t.row } -> std::convertible_to<std::size_t>;
};

template<typename T>
concept is_lexer = requires(T t) {
  requires is_token<typename T::token>;
  { t.next() } -> std::same_as<typename T::token>;
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

  // will always return a token, as EOF is a valid token
  token next()
  {
  restart:
    if (m_off == m_src.size()) {
      token t;
      t.type = Enum::LEXIBLE_EOF;
      t.what = { m_src.end(), m_src.end() };
      t.col = m_colCtr;
      t.row = m_rowCtr;
      return t;
    }

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
        std::format("unknown input in lexer @ pos {}, {}:{}: {}",
                    m_off,
                    m_rowCtr + 1,
                    m_colCtr + 1,
                    (int)m_src[m_off]));

    m_off += out.length();

    // save these for the _start_ position of the token
    auto const saved_row = m_rowCtr;
    auto const saved_col = m_colCtr;

    auto const out_sv = out.begin()->str();

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

  std::vector<token> consume_all()
  {
    std::vector<token> toks;

    for (;;) {
      auto const tok = this->next();
      toks.push_back(tok);

      if (tok.type == decltype(tok.type)::LEXIBLE_EOF)
        break;
    }

    return toks;
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

  // will always return EOF if at end of stream
  T next()
  {
    auto&& out = m_toks[m_idx];

    if (m_idx < m_toks.size() - 1)
      m_idx++;

    return out;
  }

  // if we're at EOF, that means the stream is empty
  bool empty() const { return m_idx == m_toks.size() - 1; }

  size_t size() const { return m_toks.size() - m_idx; }

private:
  std::span<T const> m_toks;
  size_t m_idx = 0;
};

template<typename Ret>
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
concept has_err_fn = requires(Self s) {
  { s.err(std::declval<STATE&>()) } -> std::convertible_to<std::string>;
};

template<size_t N>
struct ComptimeStr
{
  constexpr ComptimeStr(char const (&str)[N]) { std::copy(str, str + N, data); }
  operator std::string_view() const
  {
    return std::string_view{ data, data + N };
  }

  char data[N];
};

template<ComptimeStr s>
constexpr auto
operator""_cs()
{
  return s;
}

template<typename token_t, typename STATE>
class ParsingContext
{
  using token_queue_t = NonowningQueue<token_t>;

  static inline thread_local int parsing_depth = 0;

public:
  // TODO: be able to mark errors chains as fatal,
  // and then bubble up all fatal errors
  class Error
  {
    int m_depth;
    std::string m_error;
    token_t m_errTok;
    bool m_fatal = false;

    std::unique_ptr<Error> m_child = nullptr;

  public:
    struct empty_m
    {};

    Error(const Error&) = delete;
    Error(Error&&) = default;
    Error& operator=(const Error&) = delete;
    Error& operator=(Error&&) = default;
    // creates an empty, uninitialized error
    Error(empty_m) {};

    Error(token_t const& tok, std::string_view what)
      : m_depth(parsing_depth)
      , m_error(what)
      , m_errTok(tok) {};

    // formats the error into a human readable string
    std::string what() const
    {
      // increment row & col by 1, because they're zero indexed
      auto const this_str =
        std::format("{}:{} | {}\n", row() + 1, col() + 1, error());

      auto const child_str = m_child ? m_child->what() : std::string();

      return this_str + child_str;
    }

    auto const& error() const { return m_error; }
    auto row() const { return m_errTok.row; }
    auto col() const { return m_errTok.col; }

    bool operator<(Error const& rhs) const { return m_depth < rhs.m_depth; }

    Error& operator<<(Error&& rhs) &
    {
      if (not m_child or *m_child < rhs) {
        // bubble up fatal errors
        m_fatal |= rhs.m_fatal;

        // bubble up the depth
        m_depth = rhs.m_depth;

        m_child.reset(new Error(std::move(rhs)));
      }

      return *this;
    }

    Error&& operator<<(Error&& rhs) &&
    {
      if (not m_child or *m_child < rhs) {
        // bubble up fatal errors
        m_fatal |= rhs.m_fatal;

        // bubble up the depth
        m_depth = rhs.m_depth;

        m_child.reset(new Error(std::move(rhs)));
      }

      return std::move(*this);
    }

    // selects the deepest error,
    // and returns a reference to it
    Error&& operator|(Error&& rhs) &&
    {
      if (*this < rhs)
        return std::move(rhs);
      return std::move(*this);
    }

    Error&& fatal() &&
    {
      this->m_fatal = true;
      return std::move(*this);
    }

    bool is_fatal() const { return this->m_fatal; }

    std::optional<std::reference_wrapper<Error>> get_child() const
    {
      if (m_child)
        return (*m_child);
      else
        return std::nullopt;
    }
  };

  template<typename T>
  using Result = std::expected<T, Error>;

  template<typename Result>
  static auto create_error(auto&&... v)
  {
    return Result(std::unexpected<Error>(Error(std::move(v)...)));
  }

  template<int>
  struct placeholder_t
  {
    constexpr placeholder_t() = default;
  };

  template<typename START>
  class Engine
  {
  public:
    Engine(std::span<token_t const> toks)
      : m_toks(toks)
    {
    }

    Engine(const Engine&) = delete;
    Engine(Engine&&) = delete;
    Engine& operator=(const Engine&) = delete;
    Engine& operator=(Engine&&) = delete;

    auto parse(STATE& s) &&
    {
      using return_type =
        typename FunctionTypes<decltype(&START::template run<START>)>::ret_type;

      token_queue_t t(std::span{ m_toks });
      return_type&& out = START().template run<START>(s, t);

      // if (not t.empty()) {
      //   if (out.has_value()) {
      //     throw std::runtime_error(
      //       "didn't hit EOF by end of parse in lexible, "
      //       "an incomplete grammar was provided. please edit your grammar "
      //       "definition to error on non-eof");
      //   }

      //   // TODO: make parsing err type
      //   throw std::runtime_error(out.error().what());
      // }

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

      // can keep a chain of syntax errors?
      // i.e., keep a mutating tree of syntax errors,
      // and then if failed to parse print
      // the entire chain of syntax errors

      // FIGURED IT OUT!
      // keep a chain of errors, but also return values.
      // an error can happen in a parser, but the
      // actual error won't flag until potentially
      // several depths up in the chain.
      // if it does this, and we're not keeping errors and instead just using an
      // std::expected struct, we'll lose a lot of the parsing data

      // basically,
      // * keep a unidirectional nonbranching graph of errors
      // * never throw away error information, keep a pair
      // * only bubble up "exception"-like once a parser truly fails
      //    a parser fail is when it fails to yield a meaningful value

      // NOTE: implement cut for AndThen
      // basically, forces the parser
      // to bubble up an error if the parse fails after a certain point
    }

    auto parse() &&
    {
      STATE s;
      return std::move(*this).parse(s);
    }

  private:
    std::span<token_t const> m_toks;
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
  template<auto const EXPECTED_TYPE,
           ComptimeStr error =
             "blanket error for MorphemeParser. report this to project "
             "maintainer if this appears in an output">
  struct MorphemeParser : private Parser
  {
    template<typename Self>
    auto run(STATE& state, token_queue_t& toks)
    {
      using result_type =
        Result<typename FunctionTypes<decltype(&Self::operator())>::ret_type>;

      auto const tok = toks.next();

      if (tok.type != EXPECTED_TYPE)
        return create_error<result_type>(tok, error);

      return result_type(static_cast<Self&>(*this)(state, tok.what));
    }

    std::string_view operator()(STATE&, std::string_view s) const { return s; }
  };

  // thrown when AndThen encounters a cut error
  // struct CutException
  // {
  //   CutException(token_t t, std::string_view w)
  //     : tok(t)
  //     , what(w) {};
  //   token_t tok;
  //   std::string what;
  // };

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
    // this is unbelievably bad.
    // this was crafted in the forge of desperation
    // eventually, it will perish and be replaced
    // with something better. for now, behold
    template<typename Self, size_t... Is>
    auto apply(STATE& state, token_queue_t& toks, std::index_sequence<Is...>)
    {
      // make'em optional such that
      // we can defer the construction of the values
      // until the parsers emit
      std::tuple outs{ std::optional<typename FunctionTypes<
        decltype(&EXPECTED::template run<EXPECTED>)>::ret_type>()... };

      // optional<expected>, so gotta unwrap twice
      using unwrapped_out_type = decltype(std::apply(
        [](auto&&... in) { return std::make_tuple(**std::move(in)...); },
        outs));

      // whoa, is functiontypes useless here??
      // i'm really confused on how i'm selecting overloaded functions
      // this is confusing myself...
      using result_type = Result<typename FunctionTypes<decltype(Self()(
        state, std::declval<unwrapped_out_type>()))>::ret_type>;

      auto const begin = token_queue_t(toks).next();
      std::optional<Error> error;

      bool successful = (... && [&]<typename PARSER, size_t I>(PARSER&& p) {
        auto&& res = p.template run<PARSER>(state, toks);

        if (res) {
          std::get<I>(outs) = std::move(res);
          return true;
        }

        if (I >= Self::CUT_AT)
          error = Error(begin, Self::CUT_ERROR).fatal();
        // << std::move(res.error());
        else
          error = std::move(res.error());

        // move the error into this error

        return false;
      }.template operator()<EXPECTED, Is>(EXPECTED()));

      if (not successful) {
        return create_error<result_type>(std::move(error.value()));
      } else {
        auto&& unwrapped_tup = std::apply(
          [](auto&&... args) { return std::make_tuple(**std::move(args)...); },
          std::move(outs));

        return result_type(
          static_cast<Self&>(*this)(state, std::move(unwrapped_tup)));
      }
    }

    template<typename Self>
    auto run(STATE& state, token_queue_t& toks)
    {
      // static_assert(std::same_as<decltype(Self::CUT_AT), size_t>,
      //               "AndThen parser should have a defined CUT size_t
      //               constexpr " "static field, that defines the point at
      //               which the parser " "should throw a fatal error");

      return apply<Self>(state, toks, std::index_sequence_for<EXPECTED...>());
    }
  };

  // NOTE: maybe make it so that attaching ::err()
  // to a parser makes it a non-failable parse?
  // that is, if it is used within

  // hmm, more brainstorming,
  // maybe each Parser returns a std::result,
  // and then user-impls just take or return JUST the Correct Result,
  // then, it'd be possible to give custom error messages
  // for each parser, and the ability to "override" sub-errors
  // which would be useful for Any / AndThen parsers

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

    template<typename Self, typename Result, typename PARSER, int I>
    bool apply_impl(Result& out,
                    STATE& state,
                    token_queue_t token_queue,
                    token_queue_t& fin)
    {
      auto first_match = PARSER().template run<PARSER>(state, token_queue);

      if (not first_match.has_value()) {
        // keep matching for the deepest error
        out = create_error<Result>(std::move(out).error() |
                                   std::move(first_match).error());

        // if the error is fatal, immediately return and break
        // out of the fold loop
        if (out.error().is_fatal())
          return true;

        return false;
      } else {
        // set it to be a true result,
        // and then use short-circuiting boolean operators in apply
        // to skip processing the rest of the parsers
        out = Result(
          static_cast<Self&>(*this)(state, *first_match, placeholder_t<I>()));

        fin = token_queue;

        return true;
      }
    }

    template<typename Self,
             typename Result,
             typename... PARSER,
             std::size_t... Is>
    void apply(auto& result,
               STATE& state,
               token_queue_t& tok_out,
               std::index_sequence<Is...>)
    {
      // this method uses the short-circuiting property
      // of c++ boolean operators,
      // see apply_impl
      token_queue_t token_copy(tok_out);
      (... || apply_impl<Self, Result, PARSER, Is>(
                result, state, token_copy, tok_out));
    }

    template<typename Self, typename Result>
      requires has_err_fn<Self, STATE>
    void run_err(STATE& s, token_t tok, auto& result)
    {
      result = create_error<Result>(tok, Self().err(s));
    }

    template<typename Self, typename Result>
      requires(not has_err_fn<Self, STATE>)
    void run_err(STATE&, token_t, auto&)
    {
      printf("wrong err function ran.\n");
    }

    template<typename Self>
    auto run(STATE& state, token_queue_t& toks)
    {
      // TODO: Any should actually choose the parse that consumes the most
      // tokens!
      using index_seq = std::index_sequence_for<MAYBE...>;

      // verify that all return values of the operators()
      // are the same
      assert<Self>(state, index_seq{});

      using out_type_of_out_type_at =
        typename FunctionTypes<decltype(out_type_at<Self, 0>{}(
          &Self::operator()))>::ret_type;

      using out_type = std::remove_reference_t<
        std::tuple_element_t<0, out_type_of_out_type_at>>;

      using result_type = Result<out_type>;

      auto const first_token = token_queue_t(toks).next();

      // gets rid of some recursion overflow edge cases when @ EOF
      // NOTE: need to implement "deepest depth" errors,
      // i.e. if recursing >50 times just throw a fatal error
      if (toks.empty())
        return result_type(create_error<result_type>(
          Error(first_token, "hit EOF in any parser")));

      // put in some garbage data that immediately gets
      // overwritten
      result_type first_match(
        create_error<result_type>(Error(first_token, "garbage data")));

      apply<Self, result_type, MAYBE...>(first_match, state, toks, index_seq{});

      if (not first_match.has_value()) {
        // return create_error<result_type>(
        //   Error(first_token, "unable to match in Any")
        //   << std::move(first_match.error()));
        return create_error<result_type>(std::move(first_match.error()));

        // run_err<Self, result_type>(state, first_token, first_match);
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
  // will only ever return an error if AT_LEAST_ONE is
  // true, and not a single inner term was parsed.
  template<typename INNER, bool AT_LEAST_ONE>
  struct Repeat : private Parser
  {
    using child_return_type = typename FunctionTypes<
      decltype(&INNER::template run<INNER>)>::ret_type::value_type;

    template<typename Self>
    auto run(STATE& state, token_queue_t& toks)
    {
      using result_type =
        Result<typename FunctionTypes<decltype(&Self::operator())>::ret_type>;

      auto const starting_token = token_queue_t(toks).next();

      // take all of the values of the repeat,
      // put them into a vector,
      // then pipe the vector as a span into the repeater child class
      std::vector<child_return_type> pipe;

      for (int i = 0; true; i++) {
        token_queue_t copy = toks;
        auto into = INNER().template run<INNER>(state, copy);

        if (not into) {
          // return nullopt if at least one is true,
          // and this is the first parse and it failed to match
          if constexpr (AT_LEAST_ONE) {
            if (i == 0) {
              // err'd, push one to the queue
              return create_error<result_type>(
                Error(
                  starting_token,
                  "failed to parse in repeat, expected at least one possible "
                  "parse")
                  .fatal()
                << std::move(into.error()));
            }
          }

          if (into.error().is_fatal())
            return create_error<result_type>(std::move(into).error());

          // otherwise, return what we got.
          break;
        }

        toks = copy;
        pipe.push_back(*into);
      }

      return result_type(static_cast<Self&>(*this)(
        state, std::span{ pipe.begin(), pipe.end() }));
    }

    auto operator()(STATE&, std::span<child_return_type> in) const
    {
      return std::vector(in.begin(), in.end());
    }
  };

  template<typename INNER>
  struct ExpectEOF : private Parser
  {
    template<typename Self>
    auto run(STATE& state, token_queue_t& toks)
    {
      auto out = INNER().template run<INNER>(state, toks);

      using result_type = decltype(out);

      if (not toks.empty())
        if (out.has_value())
          return create_error<result_type>(Error(toks.next(), "expected EOF"));

      return std::move(out);
    }
  };
};
};
