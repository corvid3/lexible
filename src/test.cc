#include "lexible.hh"
#include <format>
#include <functional>
#include <iostream>
#include <optional>
#include <string_view>
#include <utility>

enum class TokenType
{
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

struct paranthesis_parser;
struct factor_parser;
struct term_parser;
struct expression_parser;

struct paranthesis_parser
  : pctx::AndThen<pctx::MorphemeParser<TokenType::LeftParanthesis>,
                  expression_parser,
                  pctx::MorphemeParser<TokenType::RightParanthesis>>
{
  auto operator()(State&, auto) const -> pctx::empty_t { return {}; };
};

struct expression_parser
  : pctx::Any<pctx::MorphemeParser<TokenType::Number>, paranthesis_parser>
{
  auto operator()(State&, std::string_view s, pctx::placeholder_t<0>) const
    -> pctx::empty_t
  {
    std::cout << s << std::endl;
    return {};
  };

  auto operator()(State&, pctx::empty_t, pctx::placeholder_t<1>) const
    -> pctx::empty_t
  {
    return {};
  };
};

struct num_parser : pctx::MorphemeParser<TokenType::Number>
{};

struct ident_parser : pctx::MorphemeParser<TokenType::Identifier>
{};

struct any_parser : pctx::Any<num_parser, ident_parser>
{
  std::string_view operator()(State&,
                              std::string_view s,
                              pctx::placeholder_t<0>) const
  {
    return s;
  };

  std::string_view operator()(State&,
                              std::string_view s,
                              pctx::placeholder_t<1>) const
  {
    return s;
  };
};

struct repeater : pctx::Repeat<any_parser, false>
{
  pctx::empty_t operator()(State&, std::span<std::string_view> s) const
  {
    for (auto const& i : s)
      std::cout << std::format("list ident: {}\n", i);

    return {};
  };
};

using parser = pctx::Parser<repeater>;

int
main()
{
  std::string_view input = "ag ab 5 ad 2 3";
  auto out = parser(input).parse();
  std::cout << (out.has_value() ? "success" : "failed") << std::endl;
}
