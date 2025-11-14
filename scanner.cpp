#include "scanner.hpp"

#include <fstream>
#include <sstream>
#include <stdexcept>
#include <iostream>

namespace cigrid {

// Construct a scanner by reading an entire file into memory.  If the file
// cannot be opened an exception is thrown.  The scanner maintains a
// position pointer into the string and a current line counter.
Scanner::Scanner(const std::string &filename) {
    std::ifstream in(filename);
    if (!in.is_open()) {
        throw ParseError("cannot open file", 0);
    }
    std::ostringstream ss;
    ss << in.rdbuf();
    data = ss.str();
    pos = 0;
    line = 1;
    hasPeek = false;
}

// Check if we have reached end of the file buffer.
bool Scanner::eof() const { return pos >= data.size(); }

// Peek at the current character without consuming it.  Returns '\0' if at
// end of file.
char Scanner::peekChar() const {
    if (eof()) return '\0';
    return data[pos];
}

// Consume the current character and return it.  Also updates the line
// counter on newline.
char Scanner::getChar() {
    if (eof()) return '\0';
    char c = data[pos++];
    if (c == '\n') line++;
    return c;
}

// Skip whitespace and comments.  Handles spaces, tabs, CR, LF as whitespace.
// Handles single line comments starting with '#', '//' and multi‑line
// comments starting with '/*' and ending with '*/'.  Updates the line
// counter accordingly.
void Scanner::skipWhitespaceAndComments() {
    while (!eof()) {
        char c = peekChar();
        // whitespace
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            getChar();
            continue;
        }
        // single line comment starting with '#'
        if (c == '#') {
            // skip until newline
            while (!eof() && peekChar() != '\n') {
                getChar();
            }
            // newline will be consumed in next iteration
            continue;
        }
        // C++ style single line comment '//'
        if (c == '/' && pos + 1 < data.size() && data[pos + 1] == '/') {
            // consume '//' then skip until newline
            getChar(); // '/'
            getChar(); // '/'
            while (!eof() && peekChar() != '\n') {
                getChar();
            }
            continue;
        }
        // C style multi‑line comment '/* ... */'
        if (c == '/' && pos + 1 < data.size() && data[pos + 1] == '*') {
            // consume '/*'
            getChar();
            getChar();
            // skip until '*/'
            while (!eof()) {
                if (peekChar() == '*' && pos + 1 < data.size() && data[pos + 1] == '/') {
                    getChar(); // '*'
                    getChar(); // '/'
                    break;
                }
                getChar();
            }
            continue;
        }
        // not whitespace or comment -> break
        break;
    }
}

// Helper to convert escape sequences in a character or string literal.  Takes
// the character following the backslash and returns the actual character.
static char decodeEscape(char esc) {
    switch (esc) {
        case 'n': return '\n';
        case 't': return '\t';
        case 'r': return '\r';
        case '\\': return '\\';
        case '\'': return '\'';
        case '"': return '"';
        case '0': return '\0';
        default:
            // Unknown escape, just return the character itself
            return esc;
    }
}

// Read a numeric literal (decimal or hexadecimal).  If it starts with 0x or
// 0X it is parsed as a hex literal.  Otherwise it is decimal.  Returns a
// string containing the lexeme.  The scanner does not convert to a number
// here; the parser will handle numeric conversion.
std::string Scanner::readNumber() {
    std::string num;
    // handle hex prefix 0x or 0X
    if (peekChar() == '0' && pos + 1 < data.size() && (data[pos + 1] == 'x' || data[pos + 1] == 'X')) {
        // read 0x
        num += getChar();
        num += getChar();
        // read hex digits
        while (!eof()) {
            char c = peekChar();
            if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
                num += getChar();
            } else {
                break;
            }
        }
        return num;
    }
    // decimal digits
    while (!eof()) {
        char c = peekChar();
        if (c >= '0' && c <= '9') {
            num += getChar();
        } else {
            break;
        }
    }
    return num;
}

// Read an identifier or keyword.  Identifiers start with a letter or
// underscore and may contain letters, digits or underscores.  Returns the
// lexeme.
std::string Scanner::readIdent() {
    std::string id;
    char c = getChar();
    id += c;
    while (!eof()) {
        char p = peekChar();
        if ((p >= 'a' && p <= 'z') || (p >= 'A' && p <= 'Z') || (p >= '0' && p <= '9') || p == '_') {
            id += getChar();
        } else {
            break;
        }
    }
    return id;
}

// Read a character literal.  Assumes the current character is the opening
// single quote.  Returns the decoded character.  Throws on malformed
// literals.
std::string Scanner::readCharLiteral() {
    // consume opening quote
    getChar();
    if (eof()) {
        throw ParseError("unterminated character literal", line);
    }
    char c = getChar();
    char result;
    if (c == '\\') {
        // escape sequence
        if (eof()) {
            throw ParseError("unterminated character literal", line);
        }
        char esc = getChar();
        result = decodeEscape(esc);
    } else {
        result = c;
    }
    // expect closing quote
    if (eof() || getChar() != '\'') {
        throw ParseError("unterminated character literal", line);
    }
    return std::string(1, result);
}

// Read a string literal.  Assumes the current character is the opening
// double quote.  Returns the decoded string (without the surrounding
// quotes).  Throws on malformed strings.
std::string Scanner::readStringLiteral() {
    // consume opening quote
    getChar();
    std::string out;
    while (!eof()) {
        char c = getChar();
        if (c == '"') {
            // end of string
            return out;
        }
        if (c == '\\') {
            if (eof()) {
                throw ParseError("unterminated string literal", line);
            }
            char esc = getChar();
            out.push_back(decodeEscape(esc));
        } else {
            out.push_back(c);
        }
    }
    throw ParseError("unterminated string literal", line);
}

// Main function to get the next token.  Uses internal peek buffer to
// implement lookahead.  Throws ParseError on invalid characters.
Token Scanner::next() {
    if (hasPeek) {
        hasPeek = false;
        return peekToken;
    }
    skipWhitespaceAndComments();
    if (eof()) {
        return Token(TokenType::End, "", line);
    }
    int tokLine = line;
    char c = peekChar();
    // identifiers and keywords
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_') {
        std::string id = readIdent();
        // keywords
        if (id == "int") return Token(TokenType::Kw_int, id, tokLine);
        if (id == "char") return Token(TokenType::Kw_char, id, tokLine);
        if (id == "void") return Token(TokenType::Kw_void, id, tokLine);
        if (id == "if") return Token(TokenType::Kw_if, id, tokLine);
        if (id == "else") return Token(TokenType::Kw_else, id, tokLine);
        if (id == "while") return Token(TokenType::Kw_while, id, tokLine);
        if (id == "return") return Token(TokenType::Kw_return, id, tokLine);
        if (id == "break") return Token(TokenType::Kw_break, id, tokLine);
        if (id == "for") return Token(TokenType::Kw_for, id, tokLine);
        if (id == "struct") return Token(TokenType::Kw_struct, id, tokLine);
        if (id == "new") return Token(TokenType::Kw_new, id, tokLine);
        if (id == "delete") return Token(TokenType::Kw_delete, id, tokLine);
        if (id == "extern") return Token(TokenType::Kw_extern, id, tokLine);
        return Token(TokenType::Ident, id, tokLine);
    }
    // numbers
    if (c >= '0' && c <= '9') {
        std::string num = readNumber();
        return Token(TokenType::Number, num, tokLine);
    }
    // character literal
    if (c == '\'') {
        std::string ch = readCharLiteral();
        return Token(TokenType::CharLit, ch, tokLine);
    }
    // string literal
    if (c == '"') {
        std::string str = readStringLiteral();
        return Token(TokenType::StringLit, str, tokLine);
    }
    // operators and punctuation with possible multi‑char tokens
    // handle two and three character sequences first
    // '<<' and '>>'
    if (c == '<' && pos + 1 < data.size() && data[pos + 1] == '<') {
        getChar(); getChar();
        return Token(TokenType::ShiftL, "<<", tokLine);
    }
    if (c == '>' && pos + 1 < data.size() && data[pos + 1] == '>') {
        getChar(); getChar();
        return Token(TokenType::ShiftR, ">>", tokLine);
    }
    // '==' and '!='
    if (c == '=' && pos + 1 < data.size() && data[pos + 1] == '=') {
        getChar(); getChar();
        return Token(TokenType::Eq, "==", tokLine);
    }
    if (c == '!' && pos + 1 < data.size() && data[pos + 1] == '=') {
        getChar(); getChar();
        return Token(TokenType::Neq, "!=", tokLine);
    }
    // '<=' and '>='
    if (c == '<' && pos + 1 < data.size() && data[pos + 1] == '=') {
        getChar(); getChar();
        return Token(TokenType::Le, "<=", tokLine);
    }
    if (c == '>' && pos + 1 < data.size() && data[pos + 1] == '=') {
        getChar(); getChar();
        return Token(TokenType::Ge, ">=", tokLine);
    }
    // '&&' and '||'
    if (c == '&' && pos + 1 < data.size() && data[pos + 1] == '&') {
        getChar(); getChar();
        return Token(TokenType::AndAnd, "&&", tokLine);
    }
    if (c == '|' && pos + 1 < data.size() && data[pos + 1] == '|') {
        getChar(); getChar();
        return Token(TokenType::OrOr, "||", tokLine);
    }
    // '++' and '--'
    if (c == '+' && pos + 1 < data.size() && data[pos + 1] == '+') {
        getChar(); getChar();
        return Token(TokenType::PlusPlus, "++", tokLine);
    }
    if (c == '-' && pos + 1 < data.size() && data[pos + 1] == '-') {
        getChar(); getChar();
        return Token(TokenType::MinusMinus, "--", tokLine);
    }
    // '->'
    if (c == '-' && pos + 1 < data.size() && data[pos + 1] == '>') {
        getChar(); getChar();
        return Token(TokenType::Arrow, "->", tokLine);
    }
    // single char tokens
    switch (c) {
        case '+': getChar(); return Token(TokenType::Plus, "+", tokLine);
        case '-': getChar(); return Token(TokenType::Minus, "-", tokLine);
        case '*': getChar(); return Token(TokenType::Star, "*", tokLine);
        case '/': getChar(); return Token(TokenType::Slash, "/", tokLine);
        case '%': getChar(); return Token(TokenType::Percent, "%", tokLine);
        case '~': getChar(); return Token(TokenType::Tilde, "~", tokLine);
        case '!': getChar(); return Token(TokenType::Not, "!", tokLine);
        case '&': getChar(); return Token(TokenType::And, "&", tokLine);
        case '|': getChar(); return Token(TokenType::Or, "|", tokLine);
        case '^': getChar(); return Token(TokenType::Xor, "^", tokLine);
        case '<': getChar(); return Token(TokenType::Lt, "<", tokLine);
        case '>': getChar(); return Token(TokenType::Gt, ">", tokLine);
        case '=': getChar(); return Token(TokenType::Assign, "=", tokLine);
        case '(': getChar(); return Token(TokenType::LParen, "(", tokLine);
        case ')': getChar(); return Token(TokenType::RParen, ")", tokLine);
        case '{': getChar(); return Token(TokenType::LBrace, "{", tokLine);
        case '}': getChar(); return Token(TokenType::RBrace, "}", tokLine);
        case '[': getChar(); return Token(TokenType::LBracket, "[", tokLine);
        case ']': getChar(); return Token(TokenType::RBracket, "]", tokLine);
        case ',': getChar(); return Token(TokenType::Comma, ",", tokLine);
        case ';': getChar(); return Token(TokenType::Semicolon, ";", tokLine);
        case '.': getChar(); return Token(TokenType::Dot, ".", tokLine);
    }
    // unknown character
    throw ParseError(std::string("invalid character: ") + c, line);
}

// Peek at the next token without consuming it.  If there is no cached
// token we call next() and save it.
Token Scanner::peek() {
    if (!hasPeek) {
        peekToken = next();
        hasPeek = true;
    }
    return peekToken;
}

} // namespace cigrid