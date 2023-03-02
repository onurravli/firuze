#include <stdio.h>

char opLetter[] = {
    '+',
    '-',
    '*',
    '/',
    '%',
    '&',
    '|',
    '^',
    '~',
    '!',
    '<',
    '>',
    '='
};

char* tokenize(char* input) {
    size_t len = 0;
    while (input[len] != '\0') {
        printf("%c", input[len]);
        ++len;
    }
}

int main() {
    
}