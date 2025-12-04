#include <stdio.h>

// There is an unexplainable bug here where assembly of this C code
// tries to access the format char pointer but only
// reads highest 2 bytes of 8 byte address.
// Program works as normal when message is used directly in printf.
const char* format = "My message is '%s'.\n %d! = ";

int fact(int n)
{
    if (n <= 1)
        return 1;
    else
        return n * fact(n - 1);
}

int factorial_message(int n, char* str)
{
    //printf(format, str, n);
    printf("My message is '%s'.\n %d! = ", str, n);
    return fact(n);
}
