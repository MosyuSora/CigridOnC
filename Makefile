CC := g++
CXXFLAGS := -std=c++17 -O2 -Wall -Wextra -pedantic

SRC := main.cpp scanner.cpp parser.cpp pprint.cpp ir.cpp asmgen.cpp
TARGET := cigrid

all: $(TARGET)

$(TARGET): $(SRC)
	$(CC) $(CXXFLAGS) -o $@ $^

clean:
	rm -f $(TARGET) *.o
