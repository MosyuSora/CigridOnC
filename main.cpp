#include "parser.hpp"
#include "pprint.hpp"
#include <iostream>
#include <string>

using namespace std;
using namespace cigrid;

int main(int argc, char **argv) {
    bool pretty = false;
    bool lineError = false;
    string filename;

    // Parse command line arguments.  Flags start with '--' and must
    // appear before the file name.  Unknown flags cause exit code 1.
    for (int i = 1; i < argc; ++i) {
        string arg = argv[i];
        if (arg == "--pretty-print") {
            pretty = true;
        } else if (arg == "--line-error") {
            lineError = true;
        } else if (arg.rfind("--", 0) == 0) {
            cerr << "Unknown flag: " << arg << endl;
            return 1;
        } else {
            if (!filename.empty()) {
                cerr << "Multiple input files specified" << endl;
                return 1;
            }
            filename = arg;
        }
    }

    if (filename.empty()) {
        cerr << "No input file provided" << endl;
        return 1;
    }

    try {
        ASTProgram prog = parse_file(filename);
        if (pretty) {
            pprint(prog);
        }
        return 0;
    } catch (const ParseError &e) {
        if (lineError) {
            // Print only the line number to stderr
            cerr << e.line << endl;
        } else {
            cerr << "Error on line " << e.line << ": " << e.message << endl;
        }
        return 1;
    } catch (const exception &ex) {
        // Unexpected exceptions
        cerr << ex.what() << endl;
        return 1;
    }
}