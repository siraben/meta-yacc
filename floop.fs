variable meta-arglist'
variable meta-arglistp'
variable meta-assignst'
variable meta-block'
variable meta-floop'
variable meta-breakst'
variable meta-callarglist'
variable meta-callarglistp'
variable meta-callexpr'
variable meta-callst'
variable meta-compare'
variable meta-expr'
variable meta-factor'
variable meta-foreverst'
variable meta-ifst'
variable meta-intst'
variable meta-loopst'
variable meta-pmain'
variable meta-printlnst'
variable meta-printst'
variable meta-proc'
variable meta-continuest'
variable meta-returnst'
variable meta-st'
variable meta-term'

\ input string | string length  | token buffer | token length
variable s       variable slen    0 value t      variable tlen

\ Current location in string
variable p                  0 p !

\ Flags
false value flagged?        false value newlined?

\ Indentation level
variable indent

\ Line counter
variable lines              1 lines !

10  constant \n    9   constant \t    126 constant tilde
39  constant tick  34  constant dtick

: set-flag!   ( -- ) true  to flagged? ;
: unflag!     ( -- ) false to flagged? ;

\ Run a parser, which is a pointer to a word.
: do-parse    ( -- ) @ execute ;

: set-source! ( c-addr u -- ) slen ! s ! ;
: c-array-ref ( a b -- a[b] ) + c@ ;
: isspace ( c -- # )
  dup dup
   bl =
  swap
   \t =
  or swap
   \n =
  or
;

: isdelim   ( c -- # ) dup tick = swap tilde = or ;
: curr-char (   -- c ) s @ p @ c-array-ref ;

: advance   ( -- ) 1 p     +! ;
: inc-lines ( -- ) 1 lines +! ;

: skip-whitespace ( -- )
  begin curr-char isspace while
    curr-char \n = if inc-lines then
    advance
  repeat
;

: ?free ( p|0 -- )
  ?dup-if free if s" failed to free token buffer" exception throw then then
;
: ?free-token ( -- ) t ?free ;

: realloc-token ( -- )
  tlen @ 1+ allocate if ." failed to allocate memory for token" then
  to t
;

: nul-terminate-token    ( -- )    0 t tlen @ + c!       ;
: copy-token-from-string ( sp -- ) s @ + t tlen @ cmove  ;

: write-token       ( sp -- ) nul-terminate-token copy-token-from-string ;
: calc-token-length ( sp -- sp ) p @ over - tlen ! ;

\ Make a token up to char sp.
: make-token ( sp -- ) ?free-token calc-token-length realloc-token write-token ;

\ Emit a character in a string, possibly quoted.
: emit-string-char ( c -- )
  case
    \n       of .\" \\n"  endof
    dtick    of .\" \\\"" endof
    tick     of .\" \\\'" endof
    [char] \ of .\" \\\\" endof
    \ Otherwise, print the character.
    dup emit
  endcase
;

\ Current character from the token buffer.
: tok-char ( -- c ) t c@ ;

: emit-string ( -- )
  tok-char
    tlen @ 1 do
      t i c-array-ref 2dup
      = if 2drop leave then
      emit-string-char
    loop
  dtick emit
;

: emit-token ( -- ) tok-char isdelim if emit-string else t tlen @ type then ;


: print-indent ( -- ) indent @ spaces ;
: mtype ( c-addr u -- ) newlined? if print-indent then type 0 to newlined? ;
: emit-newline 1 to newlined? cr ;

: read-literal ( c-addr u -- )
  { length }
  p @ 0 { l e i  }
  skip-whitespace

  length 0 do
        curr-char 0<>
        l i c-array-ref 0<>
      and
      curr-char  l i c-array-ref   =
    and
    if
      advance
      i 1+ to i
    else
      leave
    then
  loop

  i length = if
    set-flag!
    e make-token
  else
    e p ! unflag!
  then
;

: isupper ( c -- # ) [char] A [char] Z 1+ within ;
: islower ( c -- # ) [char] a [char] z 1+ within ;
: isalpha ( c -- # ) dup isupper swap islower or ;

: isdigit ( c -- # ) [char] 0 [char] 9 1+ within ;
: isalnum ( c -- # ) dup isalpha swap isdigit or ;

: advance-while-alnum ( -- ) begin curr-char isalnum while advance repeat ;
: advance-while-digit ( -- ) begin curr-char isdigit while advance repeat ;

: read-id ( -- )
  skip-whitespace
  p @
  curr-char isalpha if
    advance set-flag!
  else
    unflag! drop exit
  then

  advance-while-alnum make-token
;


: read-number ( -- )
  skip-whitespace
  p @
  
  \ Possibly with a leading dash.
  curr-char [char] - = if advance then

  curr-char isdigit if
    advance set-flag!
    advance-while-digit make-token
  else
    drop unflag! exit
  then
;

\ Advance the pointer until the next occurence of c.
: advance-while-<> ( c -- )
  begin dup curr-char <> while
    curr-char \n = if inc-lines then
    advance
  repeat
;

: read-string ( -- )
  skip-whitespace
  p @
  
  curr-char isdelim if
    curr-char advance advance-while-<>
    
    curr-char = if
      advance set-flag! make-token
    else
      \ If we hit the end of the file, backtrack.
      curr-char 0= if p ! then
    then
  else
    drop unflag!
  then
;

: maybe-error
  flagged? invert if ( -- )
    ." Error in line " lines ? ." at token '" t tlen @ type ." ' "
    ." at character " p @ . cr
    s" Parse error" exception throw
  then
;

\ File ID in       | File ID out
0 value fd-in        0 value fd-out

: open-input  ( addr u -- ) r/o open-file   throw to fd-in  ;
: open-output ( addr u -- ) w/o create-file throw to fd-out ;

\ Size of each read.
1000 1000 * constant blk-size

\ Current size of the file buffer | Pointer to the file buffer.
0 value curr-buf-size               0 value file-buffer

blk-size allocate throw to file-buffer

\ Read a file, zero-delimited.
: do-read-file ( -- )
  file-buffer blk-size fd-in read-file
  if s" failed to read file" exception throw then
  dup to curr-buf-size
  file-buffer + 0 swap !
;

: close-input  ( -- ) fd-in  close-file throw ;
: close-output ( -- ) fd-out close-file throw ;

: set-file-as-input file-buffer curr-buf-size set-source! ;
: print-file        file-buffer curr-buf-size type ;


: check-args
  argc @ 3 <> if s" usage: meta <input> <output>" exception throw then ;

: process-input-arg ( -- )
  next-arg 2dup ." Input file: " type cr open-input
  do-read-file set-file-as-input close-input
;

: process-output-arg ( -- )
  next-arg 2dup ." Output file: " type cr open-output ;

: process-args ( -- ) process-input-arg process-output-arg ;
: start-msg    ( -- ) cr ." meta-yacc has started." cr ;
: assert-clean-stack  ( -- )
  depth if
    s" stack not empty on exit" exception throw
  else
    cr ." Parsed without errors." cr
  then
;

: run-meta-program find-name name>int fd-out outfile-execute ;
: main
  start-msg check-args process-args
  s" meta-floop" run-meta-program
  close-output assert-clean-stack bye
;
meta-arglistp'
: meta-arglistp
  1 0 do
    s\" ," read-literal
    flagged? if
      read-id
      maybe-error
      s\" , int " mtype
      emit-token
    then
  loop
; latestxt swap ! 
meta-arglist'
: meta-arglist
  1 0 do
    s\" (" read-literal
    flagged? if
      s\" (int " mtype
      read-id
      maybe-error
      emit-token
      0 0 do
        1 0 do
          meta-arglistp' do-parse
          flagged? if
          then
        loop
      flagged? invert if leave then loop
      set-flag!
      maybe-error
      s\" )" read-literal
      maybe-error
      s\" :" read-literal
      maybe-error
      s\" )" mtype
    then
  loop
; latestxt swap ! 
meta-intst'
: meta-intst
  1 0 do
    s\" int" read-literal
    flagged? if
      read-id
      maybe-error
      s\" int " mtype
      emit-token
      s\"  = 0;" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-assignst'
: meta-assignst
  1 0 do
    read-id
    flagged? if
      s\" " mtype
      emit-token
      s\"  = " mtype
      s\" <-" read-literal
      maybe-error
      meta-expr' do-parse
      maybe-error
      s\" ;" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-printst'
: meta-printst
  1 0 do
    s\" print" read-literal
    flagged? if
      1 0 do
        read-id
        flagged? if
          s\" printf(\"%d \"," mtype
          emit-token
          s\" );" mtype
          emit-newline
        then
        flagged? if leave then
        read-string
        flagged? if
          s\" printf(\"" mtype
          emit-token
          s\" );" mtype
          emit-newline
        then
      loop
      maybe-error
    then
  loop
; latestxt swap ! 
meta-printlnst'
: meta-printlnst
  1 0 do
    s\" println" read-literal
    flagged? if
      1 0 do
        read-id
        flagged? if
          s\" printf(\"%d\\n\"," mtype
          emit-token
          s\" );" mtype
          emit-newline
        then
        flagged? if leave then
        read-string
        flagged? if
          s\" puts(\"" mtype
          emit-token
          s\" );" mtype
          emit-newline
        then
      loop
      maybe-error
    then
  loop
; latestxt swap ! 
meta-ifst'
: meta-ifst
  1 0 do
    s\" if" read-literal
    flagged? if
      s\" " mtype
      s\" if (" mtype
      meta-expr' do-parse
      maybe-error
      s\" ) {" mtype
      emit-newline
      2 indent +!
      maybe-error
      s\" ," read-literal
      maybe-error
      s\" then" read-literal
      maybe-error
      s\" :" read-literal
      maybe-error
      meta-st' do-parse
      maybe-error
      s\" ;" read-literal
      maybe-error
      -2 indent +!
      maybe-error
      s\" } else { " mtype
      emit-newline
      2 indent +!
      maybe-error
      meta-st' do-parse
      maybe-error
      -2 indent +!
      maybe-error
      s\" }" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-continuest'
: meta-continuest
  1 0 do
    s\" continue" read-literal
    flagged? if
      s\" continue;" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-breakst'
: meta-breakst
  1 0 do
    s\" break" read-literal
    flagged? if
      s\" f = 0;" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-loopst'
: meta-loopst
  1 0 do
    s\" loop" read-literal
    flagged? if
      s\" at" read-literal
      maybe-error
      s\" most" read-literal
      maybe-error
      s\" " mtype
      s\" for(int i = 0, f = 1; i < " mtype
      meta-expr' do-parse
      maybe-error
      s\"  && f; i++) {" mtype
      emit-newline
      2 indent +!
      maybe-error
      s\" times" read-literal
      maybe-error
      s\" :" read-literal
      maybe-error
      meta-block' do-parse
      maybe-error
      -2 indent +!
      maybe-error
      s\" }" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-foreverst'
: meta-foreverst
  1 0 do
    s\" forever" read-literal
    flagged? if
      s\" :" read-literal
      maybe-error
      s\" {" mtype
      emit-newline
      2 indent +!
      maybe-error
      s\" int f = 1;" mtype
      emit-newline
      s\" " mtype
      s\" while(1 && f)" mtype
      s\"  {" mtype
      emit-newline
      2 indent +!
      maybe-error
      meta-block' do-parse
      maybe-error
      -2 indent +!
      maybe-error
      s\" }" mtype
      emit-newline
      -2 indent +!
      maybe-error
      s\" }" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-st'
: meta-st
  1 0 do
    meta-printlnst' do-parse
    flagged? if
    then
    flagged? if leave then
    meta-printst' do-parse
    flagged? if
    then
    flagged? if leave then
    meta-ifst' do-parse
    flagged? if
    then
    flagged? if leave then
    meta-loopst' do-parse
    flagged? if
    then
    flagged? if leave then
    meta-foreverst' do-parse
    flagged? if
    then
    flagged? if leave then
    meta-continuest' do-parse
    flagged? if
    then
    flagged? if leave then
    meta-breakst' do-parse
    flagged? if
    then
    flagged? if leave then
    meta-block' do-parse
    flagged? if
    then
    flagged? if leave then
    meta-returnst' do-parse
    flagged? if
    then
    flagged? if leave then
    meta-callst' do-parse
    flagged? if
    then
    flagged? if leave then
    meta-assignst' do-parse
    flagged? if
    then
  loop
; latestxt swap ! 
meta-block'
: meta-block
  1 0 do
    s\" begin" read-literal
    flagged? if
      0 0 do
        1 0 do
          meta-intst' do-parse
          flagged? if
            s\" ;" read-literal
            maybe-error
          then
        loop
      flagged? invert if leave then loop
      set-flag!
      maybe-error
      s\" do {" mtype
      emit-newline
      2 indent +!
      maybe-error
      meta-st' do-parse
      maybe-error
      0 0 do
        1 0 do
          s\" ;" read-literal
          flagged? if
            meta-st' do-parse
            maybe-error
          then
        loop
      flagged? invert if leave then loop
      set-flag!
      maybe-error
      -2 indent +!
      maybe-error
      s\" } while (0);" mtype
      emit-newline
      s\" end" read-literal
      maybe-error
    then
  loop
; latestxt swap ! 
meta-proc'
: meta-proc
  1 0 do
    s\" def" read-literal
    flagged? if
      read-id
      maybe-error
      s\" int " mtype
      emit-token
      meta-arglist' do-parse
      maybe-error
      s\"  {" mtype
      emit-newline
      2 indent +!
      maybe-error
      0 0 do
        1 0 do
          read-string
          flagged? if
          then
        loop
      flagged? invert if leave then loop
      set-flag!
      maybe-error
      meta-block' do-parse
      maybe-error
      -2 indent +!
      maybe-error
      s\" }" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-pmain'
: meta-pmain
  1 0 do
    s\" main" read-literal
    flagged? if
      s\" int main(void) {" mtype
      emit-newline
      2 indent +!
      maybe-error
      meta-block' do-parse
      maybe-error
      -2 indent +!
      maybe-error
      s\" }" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-expr'
: meta-expr
  1 0 do
    meta-factor' do-parse
    flagged? if
      0 0 do
        1 0 do
          1 0 do
            s\" +" read-literal
            flagged? if
              s\"  + " mtype
              meta-factor' do-parse
              maybe-error
            then
          loop
          flagged? if
          then
          flagged? if leave then
          1 0 do
            s\" -" read-literal
            flagged? if
              s\"  - " mtype
              meta-factor' do-parse
              maybe-error
            then
          loop
          flagged? if
          then
        loop
      flagged? invert if leave then loop
      set-flag!
      maybe-error
    then
  loop
; latestxt swap ! 
meta-term'
: meta-term
  1 0 do
    meta-callexpr' do-parse
    flagged? if
    then
    flagged? if leave then
    1 0 do
      read-id
      flagged? if
      then
      flagged? if leave then
      read-number
      flagged? if
      then
    loop
    flagged? if
      emit-token
    then
    flagged? if leave then
    s\" (" read-literal
    flagged? if
      s\" (" mtype
      meta-expr' do-parse
      maybe-error
      s\" )" read-literal
      maybe-error
      s\" )" mtype
    then
  loop
; latestxt swap ! 
meta-compare'
: meta-compare
  1 0 do
    meta-term' do-parse
    flagged? if
      0 0 do
        1 0 do
          1 0 do
            s\" <" read-literal
            flagged? if
              s\"  < " mtype
              meta-term' do-parse
              maybe-error
            then
          loop
          flagged? if
          then
          flagged? if leave then
          1 0 do
            s\" >" read-literal
            flagged? if
              s\"  > " mtype
              meta-term' do-parse
              maybe-error
            then
          loop
          flagged? if
          then
          flagged? if leave then
          1 0 do
            s\" =" read-literal
            flagged? if
              s\"  == " mtype
              meta-term' do-parse
              maybe-error
            then
          loop
          flagged? if
          then
          flagged? if leave then
          1 0 do
            s\" >=" read-literal
            flagged? if
              s\"  >=" mtype
              meta-term' do-parse
              maybe-error
            then
          loop
          flagged? if
          then
          flagged? if leave then
          1 0 do
            s\" <=" read-literal
            flagged? if
              s\"  <=" mtype
              meta-term' do-parse
              maybe-error
            then
          loop
          flagged? if
          then
        loop
      flagged? invert if leave then loop
      set-flag!
      maybe-error
    then
  loop
; latestxt swap ! 
meta-factor'
: meta-factor
  1 0 do
    meta-compare' do-parse
    flagged? if
      0 0 do
        1 0 do
          s\" *" read-literal
          flagged? if
            s\"  * " mtype
            meta-compare' do-parse
            maybe-error
          then
        loop
      flagged? invert if leave then loop
      set-flag!
      maybe-error
    then
  loop
; latestxt swap ! 
meta-callarglistp'
: meta-callarglistp
  1 0 do
    s\" ," read-literal
    flagged? if
      s\" , " mtype
      meta-expr' do-parse
      maybe-error
    then
  loop
; latestxt swap ! 
meta-callarglist'
: meta-callarglist
  1 0 do
    s\" (" read-literal
    flagged? if
      s\" (" mtype
      meta-expr' do-parse
      maybe-error
      0 0 do
        1 0 do
          meta-callarglistp' do-parse
          flagged? if
          then
        loop
      flagged? invert if leave then loop
      set-flag!
      maybe-error
      s\" )" read-literal
      maybe-error
      s\" )" mtype
    then
  loop
; latestxt swap ! 
meta-callst'
: meta-callst
  1 0 do
    s\" do" read-literal
    flagged? if
      read-id
      maybe-error
      s\" " mtype
      emit-token
      meta-callarglist' do-parse
      maybe-error
      s\" ;" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-returnst'
: meta-returnst
  1 0 do
    s\" return" read-literal
    flagged? if
      s\" " mtype
      s\" return " mtype
      meta-expr' do-parse
      maybe-error
      s\" ;" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-callexpr'
: meta-callexpr
  1 0 do
    s\" do" read-literal
    flagged? if
      read-id
      maybe-error
      s\" " mtype
      emit-token
      meta-callarglist' do-parse
      maybe-error
    then
  loop
; latestxt swap ! 
meta-floop'
: meta-floop
  1 0 do
    s\" #include <stdio.h>\n#include <stdlib.h>\n" mtype
    emit-newline
    true if
      0 0 do
        1 0 do
          meta-proc' do-parse
          flagged? if
          then
          flagged? if leave then
          read-string
          flagged? if
          then
        loop
      flagged? invert if leave then loop
      set-flag!
      maybe-error
      meta-pmain' do-parse
      maybe-error
    then
  loop
; latestxt swap ! 
main
