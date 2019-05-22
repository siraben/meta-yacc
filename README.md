# Self-hosting YACC-like parser generators in Forth and C

To build, run

## Self-build
### Forth
```
gforth meta.fs meta-forth.txt meta.fs
```
### C
```
gcc -o meta meta.c && ./meta meta-c.txt meta.c && gcc -o meta meta.c
```
meta.fs and meta.c remain unchanged (fixpoint).

Note that you should not use meta.fs to generate C code or meta.c to
generate Forth code, as the convention for outputting strings is
different (although some manual tweaking was done to bootstrap meta.fs
from meta.c).

## Example grammar, convert infix to postfix
```
.syntax [ ex3 ex2 ex1 ] ex1

[~ I am a comment! ~]
ex3 =  (.id | .number) < * ' ' > | '(' ex1 ')' ;

ex2 = ex3 $ ('*' ex3 < '* ' > ) ;

ex1 = ex2 $ ( ('+' ex2 < '+ ' > )
            | ('-' ex2 < '- ' > )) ;

.end
```

Save this as arith.txt, then run `gforth meta.fs arith.txt arith.fs`

If arith-test.txt has the contents 
```
29 * 19293 - 129 + (992 * 30 - 10) - (15 * (-15 + 34 * (182 + 3 - 4)) + 382) * 3 + (102 + 239 * 314) - 222
```

After running `gforth arith.fs arith-test.txt out.txt`, the contents of out.txt become
```
29 19293 * 129 - 992 30 * 10 - + 15 -15 34 182 3 + 4 - * + * 382 + 3 * - 102 239 314 * + + 222 - 
```
