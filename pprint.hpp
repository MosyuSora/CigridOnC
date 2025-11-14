#pragma once

#include "ast.hpp"
#include <iostream>
#include <string>

namespace cigrid {

// Pretty print an entire program to standard output.  This produces
// output in a format similar to examples.txt.  It recursively prints
// types, expressions and statements with indentation to reflect the
// structure of the AST.  The output is always printed to std::cout.
void pprint(const ASTProgram &prog);

} // namespace cigrid
