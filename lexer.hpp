// This file defines the Cigrid lexical analyser.  It exposes a simple
// interface for turning a source file into a stream of tokens.  The
// lexer is entirely hand written: it reads the entire file into
// memory, maintains a position pointer and a line counter, and
// implements a deterministic finite–state automaton by means of
// explicit conditionals.  The lexer performs maximal munch – it
// always chooses the longest sequence of characters that form a valid
// token.  Whitespace and comments are skipped automatically.

#pragma once

#include <string>
#include <stdexcept>

namespace cigrid {

// Token kinds returned by the lexer.  They correspond to keywords,
// identifiers, literals, operators and punctuation.  The naming
// roughly matches the spelling in the source for ease of
// understanding.  New tokens for the G‑level (unary operators,
// shift operators, array punctuation, etc.) are included here.
enum class TokenType {
    End,        // end of input
    Ident,      // identifier or type name
    Number,     // integer literal (decimal or hexadecimal)
    CharLit,    // character literal
    StringLit,  // string literal
    // keywords
    Kw_int,
    Kw_char,
    Kw_void,
    Kw_if,
    Kw_else,
    Kw_while,
    Kw_return,
    Kw_break,
    Kw_for,
    Kw_struct,
    Kw_new,
    Kw_delete,
    Kw_extern,
    // operators
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Assign,     // =
    Eq,         // ==
    Neq,        // !=
    Lt,         // <
    Gt,         // >
    Le,         // <=
    Ge,         // >=
    And,        // &
    Or,         // |
    AndAnd,     // &&
    OrOr,       // ||
    Not,        // !
    Tilde,      // ~
    ShiftL,     // <<
    ShiftR,     // >>
    PlusPlus,   // ++
    MinusMinus, // --
    Arrow,      // ->
    // punctuation
    LParen,     // (
    RParen,     // )
    LBrace,     // {
    RBrace,     // }
    LBracket,   // [
    RBracket,   // ]
    Comma,      // ,
    Semicolon,  // ;
    Dot,        // .

    Xor,        // ^ bitwise xor
};

// Token value returned by the lexer.  Contains the token type, the
// original lexeme text (useful for identifiers and numbers) and the
// line number where the token occurred.  The line number starts at 1.
struct Token {
    TokenType type;
    std::string text;
    int line;
};

// Exception type thrown by the lexer and parser on errors.  It
// records the error message and the line number where the error
// occurred.  The parser uses this to optionally report only the
// offending line when --line-error is provided.
struct ParseError : public std::exception {
    std::string message;
    int line;
    ParseError(const std::string &msg, int ln) : message(msg), line(ln) {}
    const char *what() const noexcept override { return message.c_str(); }
};

// Lexer reads an input string (the contents of a file) and tokenises
// it.  It provides next() to consume a token and peek() to look at
// the next token without consuming it.  The lexer skips whitespace
// and comments.  Calling next() or peek() will never return tokens
// representing whitespace or comments – those are entirely removed.
class Lexer {
public:
    // Construct a lexer from the contents of a file.  The file is
    // read entirely into memory at construction time.
    explicit Lexer(const std::string &filename);

    // Return the next token and consume it.  If a token has been
    // previously peeked, that token is returned and the peek buffer is
    // cleared.
    Token next();

    // Return the next token without consuming it.  This can be used
    // ahead of time to decide how to parse ambiguous constructs (e.g.
    // assignment vs. expression statement).
    Token peek();

private:
    std::string data;
    size_t pos = 0;
    int line = 1;
    bool hasPeek = false;
    Token peekToken;

    // Helper functions for scanning
    bool eof() const;
    char getChar();            // consume a character and update line number
    char peekChar() const;     // look at the current character
    void skipWhitespaceAndComments();
    std::string readNumber();
    std::string readIdent();
    std::string readCharLiteral();
    std::string readStringLiteral();
};

} // namespace cigrid
