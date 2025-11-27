#include "asmgen.hpp"
#include "parser.hpp"
#include "pprint.hpp"
#include "ir.hpp"
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>
#include <stdexcept>

using namespace std;
using namespace cigrid;

int main(int argc, char **argv) {
    bool pretty = false;
    bool lineError = false;
    bool emitAsm = false;
    bool compileAsm = false;
    string filename;

    // Parse command line arguments.  Flags start with '--' and must
    // appear before the file name.  Unknown flags cause exit code 1.
    for (int i = 1; i < argc; ++i) {
        string arg = argv[i];
        if (arg == "--pretty-print") {
            pretty = true;
        } else if (arg == "--asm") {
            emitAsm = true;
        } else if (arg == "--compile") {
            compileAsm = true;
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

    if (compileAsm && !emitAsm) {
        cerr << "--compile requires --asm" << endl;
        return 1;
    }

    try {
        ASTProgram prog = parse_file(filename);
        if (emitAsm) {
            IrFunction ir = build_linear_ir(prog);
            AsmProgram asmProg = generate_asm(ir);
            std::string asmText = format_asm(asmProg);
            cout << asmText;

            if (compileAsm) {
                namespace fs = std::filesystem;
                fs::path inputPath(filename);
                std::string stem = inputPath.stem().string();
                std::string asmPath = stem + ".asm";
                std::string objPath = stem + ".o";
                std::string exePath = stem;

                std::ofstream out(asmPath);
                out << asmText;
                out.close();

                std::string nasmCmd = "nasm -felf64 " + asmPath + " -o " + objPath;
                if (std::system(nasmCmd.c_str()) != 0) {
                    throw std::runtime_error("nasm failed");
                }

                std::string gccCmd = "gcc -no-pie " + objPath + " -o " + exePath;
                if (std::system(gccCmd.c_str()) != 0) {
                    throw std::runtime_error("gcc failed");
                }
            }
        } else if (pretty) {
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