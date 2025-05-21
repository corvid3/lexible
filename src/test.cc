#include "lexible.hh"
#include <cstdio>
#include <expected>
#include <format>
#include <functional>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <utility>

enum class TokenType
{
  // required EOF token type
  LEXIBLE_EOF,

  LeftParanthesis,
  RightParanthesis,

  Plus,
  Minus,
  Asterisk,
  Solidus,

  Identifier,
  Number,

  Skip,
};

#define LEXDEF(name, regex, type, binding)                                     \
  constexpr std::string_view static name##_REGEX_SRC = regex;                  \
  using name = lexible::morpheme<name##_REGEX_SRC, type, binding>;

LEXDEF(whitespace, "\\s+", TokenType::Skip, 1000);

LEXDEF(number, "[0-9]+", TokenType::Number, 0);
LEXDEF(identifier, "[a-zA-Z]+", TokenType::Identifier, 0);
LEXDEF(left_paren, "\\(", TokenType::LeftParanthesis, 0);
LEXDEF(right_paren, "\\)", TokenType::RightParanthesis, 0);
LEXDEF(plus, "\\+", TokenType::Plus, 0);
LEXDEF(minus, "\\-", TokenType::Minus, 0);
LEXDEF(asterisk, "\\*", TokenType::Asterisk, 0);
LEXDEF(solidus, "\\/", TokenType::Solidus, 0);

using lexer = lexible::lexer<TokenType,
                             whitespace,
                             identifier,
                             number,
                             left_paren,
                             right_paren,
                             plus,
                             minus,
                             asterisk,
                             solidus>;

struct State
{};

using pctx = lexible::ParsingContext<lexer, State>;
using empty_t = lexible::empty_t;

struct paranthesis_parser;
struct factor_parser;
struct term_parser;
struct expression_parser;

// struct paranthesis_parser
//   : pctx::AndThen<pctx::MorphemeParser<TokenType::LeftParanthesis>,
//                   expression_parser,
//                   pctx::MorphemeParser<TokenType::RightParanthesis>>
// {
//   empty_t operator()(State&, auto) const { return {}; };
// };

// struct expression_parser
//   : pctx::Any<pctx::MorphemeParser<TokenType::Number>, paranthesis_parser>
// {
//   empty_t operator()(State&, std::string_view s, pctx::placeholder_t<0>)
//   {
//     std::cout << s << std::endl;
//     return {};
//   };

//   empty_t operator()(State&, empty_t, pctx::placeholder_t<1>) { return {}; };
// };

// struct num_parser : pctx::MorphemeParser<TokenType::Number>
// {};

// struct ident_parser : pctx::MorphemeParser<TokenType::Identifier>
// {};

// struct any_parser : pctx::Any<num_parser, ident_parser>
// {
//   std::string_view operator()(State&,
//                               std::string_view s,
//                               pctx::placeholder_t<0>)
//   {
//     return s;
//   };

//   std::string_view operator()(State&,
//                               std::string_view s,
//                               pctx::placeholder_t<1>)
//   {
//     return s;
//   };

//   void err(State&, std::optional<lexer::token>)
//   {
//     throw std::runtime_error("huh");
//   }
// };

// struct repeater : pctx::Repeat<any_parser, false>
// {
//   empty_t operator()(State&, std::span<std::string_view> s) const
//   {
//     for (auto const& i : s)
//       std::cout << std::format("list ident: {}\n", i);

//     return {};
//   };
// };

// class y
// {
//   y() = default;

// public:
//   y(int) {};
// };

// struct y_inner_parser : pctx::Any<pctx::MorphemeParser<TokenType::Asterisk>>
// {
//   y operator()(State&, std::string_view, pctx::placeholder_t<0>)
//   {
//     return y(1);
//   };

//   std::string err(State&) { return "m"; }
// };

// struct y_parser : pctx::Repeat<y_inner_parser, true>
// {
//   empty_t operator()(State&, std::span<y const>) { return {}; };
// };

// struct athen : pctx::AndThen<pctx::MorphemeParser<TokenType::Asterisk>>
// {
//   constexpr static size_t CUT_AT = 0;

//   empty_t operator()(State&, auto&&) const { return {}; }
// };

// using parser = pctx::Engine<pctx::ExpectEOF<athen>>;

//

//

//

//

//

//

using lexer2 = lexible::lexer<TokenType, whitespace, identifier, asterisk>;
using pctx2 = lexible::ParsingContext<lexer2::token, State>;

struct andthen2
  : pctx2::AndThen<
      pctx2::MorphemeParser<TokenType::Identifier, "expected identifier">>
{
  constexpr static size_t CUT_AT = 3;
  constexpr static std::string_view CUT_ERROR = "";

  empty_t operator()(State&, auto&& m) const
  {
    std::cout << typeid(m).name() << std::endl;

    return {};
  }
};

struct andthen
  : pctx2::AndThen<
      pctx2::MorphemeParser<TokenType::Asterisk, "expected asterisk">,
      andthen2>
{
  constexpr static size_t CUT_AT = 3;
  constexpr static std::string_view CUT_ERROR = "";

  empty_t operator()(State&, auto&& m) const
  {
    std::cout << typeid(m).name() << std::endl;

    return {};
  }
};

struct repeat : pctx2::Repeat<andthen, false>
{
  empty_t operator()(State&, std::span<empty_t const>) { return {}; };
};

using parser2 = pctx2::Engine<repeat>;

int
main()
{
  std::string_view input = "*";
  auto const toks = lexer2(input).consume_all();

  auto out = parser2(toks).parse();

  if (out.has_value()) {
    std::cout << std::format("got a successful parse\n");
  } else {
    std::cout << std::format("got an unsuccessful parse: {}\n",
                             out.error().what());
  }
}
