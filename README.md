# Self-hosting YACC-like parser generators in Forth and C

## Building
### Forth
```
gforth meta.fs meta-forth.txt meta.fs
```
### C
```
gcc -o meta meta.c && ./meta meta-c.txt meta.c && gcc -o meta meta.c
```
## Usage
Suppose one has a grammar file called `my-grammar.txt`:
- Using the Forth parser generator, run `gforth meta.fs my-grammar.txt
  my-grammar.fs`
- Using the C parser generator, run `./meta my-grammar.txt
  my-grammar.c`

Check the generated file.  Syntax errors will be indicated in the
output file.
## Example compilers included
### FlooP language from Gödel, Escher, Bach
The version of FlooP in this repository is differs from the book in
several ways, some of which are (list is subject to change):
- The last statement in a block does not have a semicolon
- Variables must be declared before use
- Comments are allowed only after procedure declarations

A FlooP → C compiler is given in `floop.txt`.  See `floop-test.txt`
for an example FlooP program.  Here's a simple Floop program that
has the output:
```
Hello, world!
Counting up to 10: 1 2 3 4 5 6 7 8 9 10 
```
```
def count [n]:

 'Count up from 1 to n inclusive.'

begin
  int out;
  out <- 1;
  forever:
  begin
    if out > n, then: break;
    print out;
    out <- out + 1
  end;
  println ''
end

main
begin
  println 'Hello, world!';
  print 'Counting up to 10: ';
  do count[10]
end
```
Generated C code:
```c
#include <stdio.h>
#include <stdlib.h>

int count(int n) {
  int out = 0;
  do {
    out = 1;
    {
      int f = 1;
      while(1 && f) {
        do {
          if (out > n) {
            f = 0;
          } else { 
            printf("%d ",out);
          }
          out = out + 1;
        } while (0);
      }
    }
    puts("");
  } while (0);
}
int main(void) {
  do {
    puts("Hello, world!");
    printf("Counting up to 10: ");
    count(10);
  } while (0);
}
```
### Convert infix to postfix
Input:
```
29 * 19293 - 129 + (992 * 30 - 10) - (15 * (-15 + 34 * (182 + 3 - 4)) + 382) * 3 + (102 + 239 * 314) - 222
```
Output:
```
29 19293 * 129 - 992 30 * 10 - + 15 -15 34 182 3 + 4 - * + * 382 + 3 * - 102 239 314 * + + 222 - 
```
