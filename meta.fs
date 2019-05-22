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

\ Output name, input string, token
variable out-name
variable s
variable slen
0 value t
variable tlen
\ Output
variable o

\ Labels
variable a-counter
variable b-counter
\ Label locks
true value a-unlocked?
true value b-unlocked?

\ Patching for forward declarations
0 value patchlen
0 value patchaddr

\ Current location in string, flag, indentation level, newline flag
variable p
0 p !
false value flagged?
false value newlined?
variable indent


\ Line counter
variable lines
1 lines !

10 constant \n

: set-source! ( c-addr u -- ) slen ! s ! ;
: c-array-ref ( a b -- a[b] ) + c@ ;
: isspace ( c -- # )
  dup dup
   32 = \ space
  swap
   9  = \ tab
  or swap
   10 = \ newline
  or
  ;

: curr-char s @ p @ c-array-ref ;
: next-char 1 p +! ;
: skip-whitespace
  begin
    curr-char isspace
  while
    curr-char
    \n = if 1 lines +! then
    next-char
  repeat
;

: ?free ?dup if free then ;

\ Make a token up to char sp.
: make-token { sp -- }

  t ?free
  p @ sp - { length } \ length = p - sp
  length 1+ allocate if ." failed to allocate memory in make-token" then
  to t
  \ Store NUL
  0 t length +  c!
  s @ sp + t length cmove
  \ Store token length
  length tlen !
;

126 constant tilde
39  constant tick
34  constant dtick
: emit-token
    t c@ tick  =
    t c@ tilde =
  or
    if
      t c@ { d }
      tlen @ 1 do
        t i c-array-ref dup
        d = if drop leave then
        case
          \n       of .\" \\n"  endof
          dtick    of .\" \\\"" endof
          tick     of .\" \\\'" endof
          [char] \ of .\" \\\\" endof
        \ Otherwise, print the character.
        dup emit
        endcase
      loop
      dtick  emit
    else
      t tlen @ type
    then
;


: print-indent indent @ spaces ;

: mtype ( c-addr u --)
  newlined? if print-indent then
  type
  0 to newlined?
;

\ Print a label number
: pp 0 <<# # # #> type space #>> ;

: emit-label-a
  a-unlocked? if 1 else 0 then a-counter +!
  [char] a emit
  a-counter @ pp
  space
;

: emit-label-b
  b-unlocked? if 1 else 0 then b-counter +!
  [char] b emit
  b-counter @ pp
  space
;

: unlock-labels
  true to a-unlocked?
  true to b-unlocked?
;

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
      next-char
      i 1+ to i
    else
      leave
    then
  loop

  i length = if
    true to flagged?
    e
    make-token
  else
    e p !
    false to flagged?
  then
;

: isalpha
  dup
    [char] A [char] Z 1+ within swap
    [char] a [char] z 1+ within
  or
;

: isdigit
  [char] 0 [char] 9 1+ within
;

: isalnum
  dup
    isalpha swap
    isdigit
  or
;

: read-id
  skip-whitespace
  p @ { e }
  curr-char isalpha if
    next-char
    true to flagged?
  else
    false to flagged?
    exit
  then

  begin
    curr-char isalnum
  while
    next-char
  repeat

  e make-token
;

: read-number
  skip-whitespace
  p @ { e }
  curr-char [char] - = if next-char then

  curr-char isdigit if
    next-char
    true to flagged?
  else
    false to flagged?
    exit
  then

  begin
    curr-char isdigit
  while
    next-char
  repeat

  e make-token
;

: read-string
  skip-whitespace
  p @ { e }
  0 { delim }

    curr-char tick  =
    curr-char tilde =
  or
  if
    curr-char to delim
    next-char

    begin
      curr-char delim <>
    while
      curr-char \n =
      if
        1 lines +!
      then
      next-char
    repeat
    curr-char delim = if
      next-char
      true to flagged?
      e make-token
      exit
    else
      curr-char 0= if
        e p !
        false to flagged?
        exit
      then
    else
      false to flagged?
      exit
    then
  then
;

: maybe-error
  flagged? invert if
    ." Error in line " lines ? ." at token '" t tlen @ type ." '"
    ." character " p @ .
    s" Parse error" exception throw
  then
;

0 value fd-in
0 value fd-out
: open-input ( addr u -- )
  r/o open-file throw to fd-in
;
: open-output ( addr u -- )
  w/o create-file throw to fd-out
;

\ Size of each read.
1000 1000 * constant blk-size

\ Current size of the file buffer.
0 value curr-buf-size

\ Pointer to the file buffer.
0 value file-buffer

blk-size allocate throw to file-buffer

\ Read a file, zero-delimited.
: do-read-file
  file-buffer blk-size fd-in read-file { bytes status }
  bytes to curr-buf-size
  0 file-buffer bytes + !
;

: close-input ( -- ) fd-in close-file throw ;
: close-output ( -- ) fd-out close-file throw ;


: set-file-as-input file-buffer curr-buf-size set-source! ;
: print-file file-buffer curr-buf-size type ;
: main
  argc @ 3 <> if
    s" usage: meta <input> <output>" exception throw
  then
  next-arg 2dup type cr open-input
  do-read-file set-file-as-input
  close-input

  next-arg 2dup type cr open-output

  s" meta-program" find-name name>int fd-out outfile-execute
  close-output
  bye

;
meta-arg'
: meta-arg
  1 0 do
    s\" *1" read-literal
    flagged? if
      s\" emit-label-a" mtype
      emit-newline
      s\" 0 to a-unlocked?" mtype
      emit-newline
    then
    flagged? if leave then
    s\" *2" read-literal
    flagged? if
      s\" emit-label-b" mtype
      emit-newline
      s\" 0 to b-unlocked?" mtype
      emit-newline
    then
    flagged? if leave then
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
    s\" {" read-literal
    flagged? if
      0 0 do
        meta-arg' @ execute
      flagged? invert if leave then loop
      true to flagged?
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
        meta-arg' @ execute
      flagged? invert if leave then loop
      true to flagged?
      maybe-error
      s\" >" read-literal
      maybe-error
      s\" unlock-labels" mtype
      emit-newline
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
      s\" \' @ execute" mtype
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
      meta-exp1' @ execute
      maybe-error
      s\" )" read-literal
      maybe-error
    then
    flagged? if leave then
    s\" .e" read-literal
    flagged? if
      s\" true to flagged?" mtype
      emit-newline
    then
    flagged? if leave then
    s\" $" read-literal
    flagged? if
      s\" 0 0 do" mtype
      emit-newline
      2 indent +!
      maybe-error
      meta-exp3' @ execute
      maybe-error
      -2 indent +!
      maybe-error
      s\" flagged? invert if leave then loop" mtype
      emit-newline
      s\" true to flagged?" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-exp2'
: meta-exp2
  1 0 do
    1 0 do
      meta-exp3' @ execute
      flagged? if
        s\" flagged? if" mtype
        emit-newline
      then
      flagged? if leave then
      meta-output' @ execute
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
          meta-exp3' @ execute
          flagged? if
            s\" maybe-error" mtype
            emit-newline
          then
          flagged? if leave then
          meta-output' @ execute
          flagged? if
          then
        loop
      flagged? invert if leave then loop
      true to flagged?
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
      meta-exp2' @ execute
      maybe-error
      0 0 do
        1 0 do
          s\" |" read-literal
          flagged? if
            s\" flagged? if leave then" mtype
            emit-newline
            meta-exp2' @ execute
            maybe-error
          then
        loop
      flagged? invert if leave then loop
      true to flagged?
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
      meta-exp1' @ execute
      maybe-error
      s\" ;" read-literal
      maybe-error
      -2 indent +!
      maybe-error
      s\" ; latestxt swap ! " mtype
      emit-newline
    then
    flagged? if leave then
    meta-comment' @ execute
    flagged? if
    then
  loop
; latestxt swap ! 
meta-support'
: meta-support
  1 0 do
    s\" \n\\ Output name, input string, token\nvariable out-name\nvariable s\nvariable slen\n0 value t\nvariable tlen\n\\ Output\nvariable o\n\n\\ Labels\nvariable a-counter\nvariable b-counter\n\\ Label locks\ntrue value a-unlocked?\ntrue value b-unlocked?\n\n\\ Patching for forward declarations\n0 value patchlen\n0 value patchaddr\n\n\\ Current location in string, flag, indentation level, newline flag\nvariable p\n0 p !\nfalse value flagged?\nfalse value newlined?\nvariable indent\n\n\n\\ Line counter\nvariable lines\n1 lines !\n\n10 constant \\n\n\n: set-source! ( c-addr u -- ) slen ! s ! ;\n: c-array-ref ( a b -- a[b] ) + c@ ;\n: isspace ( c -- # )\n  dup dup\n   32 = \\ space\n  swap\n   9  = \\ tab\n  or swap\n   10 = \\ newline\n  or\n  ;\n\n: curr-char s @ p @ c-array-ref ;\n: next-char 1 p +! ;\n: skip-whitespace\n  begin\n    curr-char isspace\n  while\n    curr-char\n    \\n = if 1 lines +! then\n    next-char\n  repeat\n;\n\n: ?free ?dup if free then ;\n\n\\ Make a token up to char sp.\n: make-token { sp -- }\n\n  t ?free\n  p @ sp - { length } \\ length = p - sp\n  length 1+ allocate if .\" failed to allocate memory in make-token\" then\n  to t\n  \\ Store NUL\n  0 t length +  c!\n  s @ sp + t length cmove\n  \\ Store token length\n  length tlen !\n;\n\n126 constant tilde\n39  constant tick\n34  constant dtick\n: emit-token\n    t c@ tick  =\n    t c@ tilde =\n  or\n    if\n      t c@ { d }\n      tlen @ 1 do\n        t i c-array-ref dup\n        d = if drop leave then\n        case\n          \\n       of .\\\" \\\\n\"  endof\n          dtick    of .\\\" \\\\\\\"\" endof\n          tick     of .\\\" \\\\\\\'\" endof\n          [char] \\ of .\\\" \\\\\\\\\" endof\n        \\ Otherwise, print the character.\n        dup emit\n        endcase\n      loop\n      dtick  emit\n    else\n      t tlen @ type\n    then\n;\n\n\n: print-indent indent @ spaces ;\n\n: mtype ( c-addr u --)\n  newlined? if print-indent then\n  type\n  0 to newlined?\n;\n\n\\ Print a label number\n: pp 0 <<# # # #> type space #>> ;\n\n: emit-label-a\n  a-unlocked? if 1 else 0 then a-counter +!\n  [char] a emit\n  a-counter @ pp\n  space\n;\n\n: emit-label-b\n  b-unlocked? if 1 else 0 then b-counter +!\n  [char] b emit\n  b-counter @ pp\n  space\n;\n\n: unlock-labels\n  true to a-unlocked?\n  true to b-unlocked?\n;\n\n: emit-newline 1 to newlined? cr ;\n\n: read-literal ( c-addr u -- )\n  { length }\n  p @ 0 { l e i  }\n  skip-whitespace\n\n  length 0 do\n        curr-char 0<>\n        l i c-array-ref 0<>\n      and\n      curr-char  l i c-array-ref   =\n    and\n    if\n      next-char\n      i 1+ to i\n    else\n      leave\n    then\n  loop\n\n  i length = if\n    true to flagged?\n    e\n    make-token\n  else\n    e p !\n    false to flagged?\n  then\n;\n\n: isalpha\n  dup\n    [char] A [char] Z 1+ within swap\n    [char] a [char] z 1+ within\n  or\n;\n\n: isdigit\n  [char] 0 [char] 9 1+ within\n;\n\n: isalnum\n  dup\n    isalpha swap\n    isdigit\n  or\n;\n\n: read-id\n  skip-whitespace\n  p @ { e }\n  curr-char isalpha if\n    next-char\n    true to flagged?\n  else\n    false to flagged?\n    exit\n  then\n\n  begin\n    curr-char isalnum\n  while\n    next-char\n  repeat\n\n  e make-token\n;\n\n: read-number\n  skip-whitespace\n  p @ { e }\n  curr-char [char] - = if next-char then\n\n  curr-char isdigit if\n    next-char\n    true to flagged?\n  else\n    false to flagged?\n    exit\n  then\n\n  begin\n    curr-char isdigit\n  while\n    next-char\n  repeat\n\n  e make-token\n;\n\n: read-string\n  skip-whitespace\n  p @ { e }\n  0 { delim }\n\n    curr-char tick  =\n    curr-char tilde =\n  or\n  if\n    curr-char to delim\n    next-char\n\n    begin\n      curr-char delim <>\n    while\n      curr-char \\n =\n      if\n        1 lines +!\n      then\n      next-char\n    repeat\n    curr-char delim = if\n      next-char\n      true to flagged?\n      e make-token\n      exit\n    else\n      curr-char 0= if\n        e p !\n        false to flagged?\n        exit\n      then\n    else\n      false to flagged?\n      exit\n    then\n  then\n;\n\n: maybe-error\n  flagged? invert if\n    .\" Error in line \" lines ? .\" at token \'\" t tlen @ type .\" \'\"\n    .\" character \" p @ .\n    s\" Parse error\" exception throw\n  then\n;\n\n0 value fd-in\n0 value fd-out\n: open-input ( addr u -- )\n  r/o open-file throw to fd-in\n;\n: open-output ( addr u -- )\n  w/o create-file throw to fd-out\n;\n\n\\ Size of each read.\n1000 1000 * constant blk-size\n\n\\ Current size of the file buffer.\n0 value curr-buf-size\n\n\\ Pointer to the file buffer.\n0 value file-buffer\n\nblk-size allocate throw to file-buffer\n\n\\ Read a file, zero-delimited.\n: do-read-file\n  file-buffer blk-size fd-in read-file { bytes status }\n  bytes to curr-buf-size\n  0 file-buffer bytes + !\n;\n\n: close-input ( -- ) fd-in close-file throw ;\n: close-output ( -- ) fd-out close-file throw ;\n\n\n: set-file-as-input file-buffer curr-buf-size set-source! ;\n: print-file file-buffer curr-buf-size type ;" mtype
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
      true to flagged?
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
      s\" : main\n  argc @ 3 <> if\n    s\" usage: meta <input> <output>\" exception throw\n  then\n  next-arg 2dup type cr open-input\n  do-read-file set-file-as-input\n  close-input\n\n  next-arg 2dup type cr open-output\n\n  s\" meta-" mtype
      emit-token
      s\" \" find-name name>int fd-out outfile-execute\n  close-output\n  bye\n\n;" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
meta-program'
: meta-program
  1 0 do
    s\" .syntax" read-literal
    flagged? if
      meta-declist' @ execute
      maybe-error
      meta-support' @ execute
      maybe-error
      meta-main' @ execute
      maybe-error
      0 0 do
        meta-stat' @ execute
      flagged? invert if leave then loop
      true to flagged?
      maybe-error
      s\" .end" read-literal
      maybe-error
      s\" main" mtype
      emit-newline
    then
  loop
; latestxt swap ! 
main
