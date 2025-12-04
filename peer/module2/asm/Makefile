.PHONY: all clean

all:
	gcc factorial.c main-c.c -o main-c
	gcc -c factorial.c -o factorial.o

	nasm -felf64 main-asm.asm -o main-asm.o
	gcc -no-pie main-asm.o factorial.o -o main-asm

clean:
	rm -f *.o main-c main-asm *~
