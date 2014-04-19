USE: accessors
USE: assocs
USE: hashtables
USE: io
USE: kernel
USE: math
USE: math.parser
USE: namespaces
USE: prettyprint
USE: sequences
USE: splitting
USE: vectors
IN: lisp

TUPLE: lobj tag data ;
C: <lobj> lobj
TUPLE: cell car cdr ;
C: <cell> cell
TUPLE: expr args body env ;
C: <expr> expr

SYMBOL: kNil
"nil" "nil" <lobj> kNil set-global

: safe-car ( lobj -- lobj )
  dup tag>> "cons" = [
    data>> car>>
  ] [
    drop kNil get-global
  ] if ;

: safe-cdr ( lobj -- lobj )
  dup tag>> "cons" = [
    data>> cdr>>
  ] [
    drop kNil get-global
  ] if ;

: make-error ( str -- lobj )
  "error" swap <lobj> ;

SYMBOL: sym-table
256 <hashtable> sym-table set-global
: make-sym ( str -- lobj )
  dup "nil" = [
    drop kNil get-global
  ] [
    dup sym-table get-global at* [
      nip
    ] [
      drop dup "sym" swap <lobj> swap
      dupd
      sym-table get-global set-at
    ] if
  ] if ;

: make-num ( n -- lobj )
  "num" swap <lobj> ;

: make-cons ( car cdr -- lobj )
  <cell> "cons" swap <lobj> ;

: make-expr ( args body env -- lobj )
  <expr> "expr" swap <lobj> ;

: nreverse ( lobj -- lobj )
  kNil get-global swap
  [ dup tag>> "cons" = ] [  ! ret lst
    dup data>> cdr>> -rot  ! tmp ret lst
    dup data>> swapd cdr<<  ! tmp lst
    swap  ! lst tmp
  ] while drop ;

: space? ( ch -- ? )
  V{ 0x09 0x0a 0x0d 0x20 } member? ;  ! Tab, Linefeed, Return, Space

: delimiter? ( ch -- ? )
  dup V{ 0x27 0x28 0x29 } member? swap space? or ; ! Quote, Lpar, Rpar

: first-index ( seq quot: ( ch -- ? ) -- n )
 { } map-as
 dup [ not ] all? [
   length
 ] [
   0
   [ 2dup swap nth not ] [
     1 +
   ] while nip
 ] if ; inline

: skip-spaces ( str -- str )
  dup [ space? not ] first-index swap dup length swap subseq ;

: make-num-or-sym ( str -- lobj )
  dup string>number dup [
    make-num nip
  ] [
    drop make-sym
  ] if ;

: read-atom ( str -- lobj str )
  dup [ delimiter? ] first-index
  0 swap rot 3dup  ! 0 i str 0 i str
  subseq make-num-or-sym  ! 0 i str lobj
  rot rot  ! 0 lobj i str
  dup length  ! 0 lobj i str n
  rot swap rot subseq  ! 0 lobj str
  rot drop ;

DEFER: read-list

: read ( str -- lobj str )
  skip-spaces
  dup length 0 = [
    drop "empty input" make-error ""
  ] [
    dup 0 swap nth 0x29 = [  ! Rpar
      drop "invalid syntax" make-error ""
    ] [
      dup 0 swap nth 0x28 = [  ! Lpar
        dup length 1 swap rot subseq read-list
      ] [
        dup 0 swap nth 0x27 = [  ! Quote
          dup length 1 swap rot subseq read  !  obj next
          swap kNil get-global make-cons "quote" make-sym swap make-cons swap
        ] [
          read-atom
        ] if
      ] if
    ] if
  ] if ;

: read-list-internal ( lobj str -- lobj str ? )
  skip-spaces
  dup length 0 = [
    2drop "unfinished parenthesis" make-error "" f
  ] [
    dup 0 swap nth 0x29 = [  ! Rpar
      dup length 1 swap rot subseq swap nreverse swap f
    ] [
      read swap  ! ret next obj
      dup tag>> "error" = [
        -rot 2drop "" f
      ] [
        rot make-cons swap t
      ] if
    ] if
  ] if ;

: read-list ( str -- lobj str )
  kNil get-global swap
  t
  [ ] [  ! quot lobj str
    read-list-internal
  ] while ;

DEFER: print-list

: print-obj ( lobj -- str )
  dup tag>>  ! lobj tag
  dup "sym" = swap dup "nil" = rot or [
    drop data>>
  ] [
    dup "error" = [
      drop data>> "<error: " swap ">" 3append
    ] [
      dup "num" = [
        drop data>> number>string
      ] [
        dup "cons" = [
          drop print-list
        ] [
          nip "<" swap ">" 3append
        ] if
      ] if
    ] if
  ] if ;

: print-list ( lobj -- str )
  dup data>> car>> print-obj
  "(" swap append
  swap data>> cdr>>
  [ dup tag>> "cons" = ] [  ! ret obj
    dup data>> car>> print-obj
    rot " "  ! obj str ret " "
    rot 3append swap
    data>> cdr>>
  ] while
  dup tag>> "nil" = [
    drop ")" append
  ] [
    print-obj " . " swap 3append ")" append
  ] if ;

"> " write flush
[ readln dup ] [
  read drop print-obj print
  "> " write flush
] while drop
