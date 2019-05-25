variable meta-program'
variable meta-exp1'
variable meta-exp3'
variable meta-arg'
variable meta-output'
variable meta-exp2'
variable meta-exp1'
variable meta-comment'
variable meta-stat'
variable meta-support'
variable meta-declist'
variable meta-main'

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
: isspace ( c -- # ) bl over = over \t = or swap \n = or ;

: isdelim   ( c -- # ) dup tick = swap tilde = or ;
: curr-char (   -- c ) s @ p @ c-array-ref ;

: advance   ( -- ) 1 p     +! ;
: inc-lines ( -- ) 1 lines +! ;

: skip-whitespace ( -- )
  begin curr-char isspace while
    curr-char \n = negate lines +! advance
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
    curr-char advance
    advance-while-<>
    
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
    ." Error in line " lines ? ." at token '" t tlen @ type ." '"
    ." character " p @ .
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
  s" meta-program" run-meta-program
  close-output assert-clean-stack bye
;
meta-arg'
: meta-arg
  1 0 do
    s\" *" read-literal
    flagged? if
      s\" emit-token" mtype
      emit-newline
    then
    flagged? if leave then
    read-string
    flagged? if
      s\" s\\\" " mtype
      emit-token
      s\"  mtype" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-output'
: meta-output
  1 0 do
    1 0 do
      s\" {" read-literal
      flagged? if
        0 0 do
          meta-arg' do-parse
        flagged? invert if leave then loop
        set-flag!
        maybe-error
        s\" }" read-literal
        maybe-error
        s\" emit-newline" mtype
        emit-newline
      then
      flagged? if leave then
      s\" <" read-literal
      flagged? if
        0 0 do
          meta-arg' do-parse
        flagged? invert if leave then loop
        set-flag!
        maybe-error
        s\" >" read-literal
        maybe-error
      then
    loop
    flagged? if
    then
  loop
; latestxt swap ! 
meta-exp3'
: meta-exp3
  1 0 do
    read-id
    flagged? if
      s\" meta-" mtype
      emit-token
      s\" \' do-parse" mtype
      emit-newline
    then
    flagged? if leave then
    read-string
    flagged? if
      s\" s\\\" " mtype
      emit-token
      s\"  read-literal" mtype
      emit-newline
    then
    flagged? if leave then
    s\" .id" read-literal
    flagged? if
      s\" read-id" mtype
      emit-newline
    then
    flagged? if leave then
    s\" .number" read-literal
    flagged? if
      s\" read-number" mtype
      emit-newline
    then
    flagged? if leave then
    s\" .string" read-literal
    flagged? if
      s\" read-string" mtype
      emit-newline
    then
    flagged? if leave then
    s\" .lm+" read-literal
    flagged? if
      s\" 2 indent +!" mtype
      emit-newline
    then
    flagged? if leave then
    s\" .lm-" read-literal
    flagged? if
      s\" -2 indent +!" mtype
      emit-newline
    then
    flagged? if leave then
    s\" (" read-literal
    flagged? if
      meta-exp1' do-parse
      maybe-error
      s\" )" read-literal
      maybe-error
    then
    flagged? if leave then
    s\" .e" read-literal
    flagged? if
      s\" set-flag!" mtype
      emit-newline
    then
    flagged? if leave then
    s\" $" read-literal
    flagged? if
      s\" 0 0 do" mtype
      emit-newline
      2 indent +!
      maybe-error
      meta-exp3' do-parse
      maybe-error
      -2 indent +!
      maybe-error
      s\" flagged? invert if leave then loop" mtype
      emit-newline
      s\" set-flag!" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-exp2'
: meta-exp2
  1 0 do
    1 0 do
      meta-exp3' do-parse
      flagged? if
        s\" flagged? if" mtype
        emit-newline
      then
      flagged? if leave then
      meta-output' do-parse
      flagged? if
        s\" true if" mtype
        emit-newline
      then
    loop
    flagged? if
      2 indent +!
      maybe-error
      0 0 do
        1 0 do
          meta-exp3' do-parse
          flagged? if
            s\" maybe-error" mtype
            emit-newline
          then
          flagged? if leave then
          meta-output' do-parse
          flagged? if
          then
        loop
      flagged? invert if leave then loop
      set-flag!
      maybe-error
      -2 indent +!
      maybe-error
      s\" then" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-exp1'
: meta-exp1
  1 0 do
    s\" 1 0 do" mtype
    emit-newline
    true if
      2 indent +!
      maybe-error
      meta-exp2' do-parse
      maybe-error
      0 0 do
        1 0 do
          s\" |" read-literal
          flagged? if
            s\" flagged? if leave then" mtype
            emit-newline
            meta-exp2' do-parse
            maybe-error
          then
        loop
      flagged? invert if leave then loop
      set-flag!
      maybe-error
      -2 indent +!
      maybe-error
      s\" loop" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-comment'
: meta-comment
  1 0 do
    s\" [" read-literal
    flagged? if
      read-string
      maybe-error
      s\" ]" read-literal
      maybe-error
    then
  loop
; latestxt swap ! 
meta-stat'
: meta-stat
  1 0 do
    read-id
    flagged? if
      s\" meta-" mtype
      emit-token
      s\" \'" mtype
      emit-newline
      s\" : meta-" mtype
      emit-token
      emit-newline
      2 indent +!
      maybe-error
      s\" =" read-literal
      maybe-error
      meta-exp1' do-parse
      maybe-error
      s\" ;" read-literal
      maybe-error
      -2 indent +!
      maybe-error
      s\" ; latestxt swap ! " mtype
      emit-newline
    then
    flagged? if leave then
    meta-comment' do-parse
    flagged? if
    then
  loop
; latestxt swap ! 
meta-support'
: meta-support
  1 0 do
    s\" \n\\ input string | string length  | token buffer | token length\nvariable s       variable slen    0 value t      variable tlen\n\n\\ Current location in string\nvariable p                  0 p !\n\n\\ Flags\nfalse value flagged?        false value newlined?\n\n\\ Indentation level\nvariable indent\n\n\\ Line counter\nvariable lines              1 lines !\n\n10  constant \\n    9   constant \\t    126 constant tilde\n39  constant tick  34  constant dtick\n\n: set-flag!   ( -- ) true  to flagged? ;\n: unflag!     ( -- ) false to flagged? ;\n\n\\ Run a parser, which is a pointer to a word.\n: do-parse    ( -- ) @ execute ;\n\n: set-source! ( c-addr u -- ) slen ! s ! ;\n: c-array-ref ( a b -- a[b] ) + c@ ;\n: isspace ( c -- # )\n  dup dup\n   bl =\n  swap\n   \\t =\n  or swap\n   \\n =\n  or\n;\n\n: isdelim   ( c -- # ) dup tick = swap tilde = or ;\n: curr-char (   -- c ) s @ p @ c-array-ref ;\n\n: advance   ( -- ) 1 p     +! ;\n: inc-lines ( -- ) 1 lines +! ;\n\n: skip-whitespace ( -- )\n  begin curr-char isspace while\n    curr-char \\n = if inc-lines then\n    advance\n  repeat\n;\n\n: ?free ( p|0 -- )\n  ?dup-if free if s\" failed to free token buffer\" exception throw then then\n;\n: ?free-token ( -- ) t ?free ;\n\n: realloc-token ( -- )\n  tlen @ 1+ allocate if .\" failed to allocate memory for token\" then\n  to t\n;\n\n: nul-terminate-token    ( -- )    0 t tlen @ + c!       ;\n: copy-token-from-string ( sp -- ) s @ + t tlen @ cmove  ;\n\n: write-token       ( sp -- ) nul-terminate-token copy-token-from-string ;\n: calc-token-length ( sp -- sp ) p @ over - tlen ! ;\n\n\\ Make a token up to char sp.\n: make-token ( sp -- ) ?free-token calc-token-length realloc-token write-token ;\n\n\\ Emit a character in a string, possibly quoted.\n: emit-string-char ( c -- )\n  case\n    \\n       of .\\\" \\\\n\"  endof\n    dtick    of .\\\" \\\\\\\"\" endof\n    tick     of .\\\" \\\\\\\'\" endof\n    [char] \\ of .\\\" \\\\\\\\\" endof\n    \\ Otherwise, print the character.\n    dup emit\n  endcase\n;\n\n\\ Current character from the token buffer.\n: tok-char ( -- c ) t c@ ;\n\n: emit-string ( -- )\n  tok-char\n    tlen @ 1 do\n      t i c-array-ref 2dup\n      = if 2drop leave then\n      emit-string-char\n    loop\n  dtick emit\n;\n\n: emit-token ( -- ) tok-char isdelim if emit-string else t tlen @ type then ;\n\n\n: print-indent ( -- ) indent @ spaces ;\n: mtype ( c-addr u -- ) newlined? if print-indent then type 0 to newlined? ;\n: emit-newline 1 to newlined? cr ;\n\n: read-literal ( c-addr u -- )\n  { length }\n  p @ 0 { l e i  }\n  skip-whitespace\n\n  length 0 do\n        curr-char 0<>\n        l i c-array-ref 0<>\n      and\n      curr-char  l i c-array-ref   =\n    and\n    if\n      advance\n      i 1+ to i\n    else\n      leave\n    then\n  loop\n\n  i length = if\n    set-flag!\n    e make-token\n  else\n    e p ! unflag!\n  then\n;\n\n: isupper ( c -- # ) [char] A [char] Z 1+ within ;\n: islower ( c -- # ) [char] a [char] z 1+ within ;\n: isalpha ( c -- # ) dup isupper swap islower or ;\n\n: isdigit ( c -- # ) [char] 0 [char] 9 1+ within ;\n: isalnum ( c -- # ) dup isalpha swap isdigit or ;\n\n: advance-while-alnum ( -- ) begin curr-char isalnum while advance repeat ;\n: advance-while-digit ( -- ) begin curr-char isdigit while advance repeat ;\n\n: read-id ( -- )\n  skip-whitespace\n  p @\n  curr-char isalpha if\n    advance set-flag!\n  else\n    unflag! drop exit\n  then\n\n  advance-while-alnum make-token\n;\n\n\n: read-number ( -- )\n  skip-whitespace\n  p @\n  \n  \\ Possibly with a leading dash.\n  curr-char [char] - = if advance then\n\n  curr-char isdigit if\n    advance set-flag!\n    advance-while-digit make-token\n  else\n    drop unflag! exit\n  then\n;\n\n\\ Advance the pointer until the next occurence of c.\n: advance-while-<> ( c -- )\n  begin dup curr-char <> while\n    curr-char \\n = if inc-lines then\n    advance\n  repeat\n;\n\n: read-string ( -- )\n  skip-whitespace\n  p @\n  \n  curr-char isdelim if\n    curr-char advance advance-while-<>\n    \n    curr-char = if\n      advance set-flag! make-token\n    else\n      \\ If we hit the end of the file, backtrack.\n      curr-char 0= if p ! then\n    then\n  else\n    drop unflag!\n  then\n;\n\n: maybe-error\n  flagged? invert if ( -- )\n    .\" Error in line \" lines ? .\" at token \'\" t tlen @ type .\" \'\"\n    .\" character \" p @ .\n    s\" Parse error\" exception throw\n  then\n;\n\n\\ File ID in       | File ID out\n0 value fd-in        0 value fd-out\n\n: open-input  ( addr u -- ) r/o open-file   throw to fd-in  ;\n: open-output ( addr u -- ) w/o create-file throw to fd-out ;\n\n\\ Size of each read.\n1000 1000 * constant blk-size\n\n\\ Current size of the file buffer | Pointer to the file buffer.\n0 value curr-buf-size               0 value file-buffer\n\nblk-size allocate throw to file-buffer\n\n\\ Read a file, zero-delimited.\n: do-read-file ( -- )\n  file-buffer blk-size fd-in read-file\n  if s\" failed to read file\" exception throw then\n  dup to curr-buf-size\n  file-buffer + 0 swap !\n;\n\n: close-input  ( -- ) fd-in  close-file throw ;\n: close-output ( -- ) fd-out close-file throw ;\n\n: set-file-as-input file-buffer curr-buf-size set-source! ;\n: print-file        file-buffer curr-buf-size type ;\n" mtype
    emit-newline
    true if
    then
  loop
; latestxt swap ! 
meta-declist'
: meta-declist
  1 0 do
    s\" [" read-literal
    flagged? if
      0 0 do
        1 0 do
          read-id
          flagged? if
            s\" variable meta-" mtype
            emit-token
            s\" \'" mtype
            emit-newline
          then
        loop
      flagged? invert if leave then loop
      set-flag!
      maybe-error
      s\" ]" read-literal
      maybe-error
    then
  loop
; latestxt swap ! 
meta-main'
: meta-main
  1 0 do
    read-id
    flagged? if
      s\" \n: check-args\n  argc @ 3 <> if s\" usage: meta <input> <output>\" exception throw then ;\n\n: process-input-arg ( -- )\n  next-arg 2dup .\" Input file: \" type cr open-input\n  do-read-file set-file-as-input close-input\n;\n\n: process-output-arg ( -- )\n  next-arg 2dup .\" Output file: \" type cr open-output ;\n\n: process-args ( -- ) process-input-arg process-output-arg ;\n: start-msg    ( -- ) cr .\" meta-yacc has started.\" cr ;\n: assert-clean-stack  ( -- )\n  depth if\n    s\" stack not empty on exit\" exception throw\n  else\n    cr .\" Parsed without errors.\" cr\n  then\n;\n\n: run-meta-program find-name name>int fd-out outfile-execute ;\n: main\n  start-msg check-args process-args\n  s\" meta-" mtype
      emit-token
      s\" \" run-meta-program\n  close-output assert-clean-stack bye\n;" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-program'
: meta-program
  1 0 do
    s\" .syntax" read-literal
    flagged? if
      meta-declist' do-parse
      maybe-error
      meta-support' do-parse
      maybe-error
      meta-main' do-parse
      maybe-error
      0 0 do
        meta-stat' do-parse
      flagged? invert if leave then loop
      set-flag!
      maybe-error
      s\" .end" read-literal
      maybe-error
      s\" main" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
main
