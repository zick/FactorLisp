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

: make-subr ( n -- lobj )
  "subr" swap <lobj> ;

: make-expr ( args body env -- lobj )
  <expr> "expr" swap <lobj> ;

: nreverse ( lobj -- lobj )
  kNil get-global swap
  [ dup tag>> "cons" = ] [  ! ret lst
    dup data>> cdr>> -rot  ! tmp ret lst
    dup data>> swapd cdr<<  ! tmp lst
    swap  ! lst tmp
  ] while drop ;

: pairlis ( lst1 lst2 -- lobj )
  kNil get-global -rot  ! ret lst1 lst2
  [ dup tag>> "cons" = rot dup tag>> "cons" = rot and swapd ] [  ! ret lst1 lst2
    dup data>> car>> rot dup data>> car>> rot make-cons swapd
    [ rot ] dip  ! lst1 lst2 ret cons
    swap make-cons -rot  ! ret lst1 lst2
    data>> cdr>> swap data>> cdr>> swap
  ] while 2drop nreverse ;

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

: find-internal ( sym alist -- lobj ? )
  dup tag>> "cons" = [
    dup data>> car>> data>> car>> ! sym alist key
    rot = [
      data>> car>> f
    ] [
      data>> cdr>> t
    ] if
  ] [
    2drop kNil get-global f
  ] if ;

: find-var ( sym env -- lobj )
  [ dup tag>> "cons" = ] [  ! sym env
    dup data>> car>> swapd  ! env sym alist
    [ dupd find-internal ] [  ! env sym lobj
    ] while
    dup tag>> "cons" = [
      2nip kNil get-global  ! lobj nil
    ] [
      drop swap data>> cdr>>  ! sym env
    ] if
  ] while
  drop dup tag>> "cons" = [
  ] [
    drop kNil get-global
  ] if ;

SYMBOL: g-env
kNil get-global dup make-cons g-env set-global

: add-to-env ( sym val env -- )
  -rot make-cons swap  ! pair env
  data>> dup car>> rot swap make-cons  ! data pair
  swap car<< ;

DEFER: apply
DEFER: evlis

: eval ( lobj env -- lobj )
  swap dup tag>>  ! env lobj tag
  dup "nil" = swap dup "num" = rot or swap dup "error" = rot or [
    drop nip
  ] [
    "sym" = [  ! env lobj
      swap dupd find-var  ! lobj bind
      dup kNil get-global eq? [
        drop data>> " has no value" append make-error
      ] [
        nip data>> cdr>>
      ] if
    ] [
      data>> dup cdr>> swap car>>  ! env args op
      dup "quote" make-sym eq? [
        drop nip safe-car
      ] [
        dup "if" make-sym eq? [
          drop dupd dup safe-cdr swap safe-car  ! env env CDR(args) CAR(args)
          rot eval kNil get-global eq? [  ! env CDR(args)
            safe-cdr safe-car
          ] [
            safe-car
          ] if
          swap eval
        ] [
          dup "lambda" make-sym eq? [
            drop dup safe-car -rot  ! CAR(args) env args
            safe-cdr swap make-expr
          ] [
            dup "defun" make-sym eq? [
              3drop "defun" make-error
            ] [
              dup "setq" make-sym eq? [
                3drop "setq" make-error
              ] [
                rot dup -rot  ! args env op env
                eval  ! args env fn
                -rot dup -rot  ! fn env args env
                evlis  ! fn env args
                swap apply
              ] if
            ] if
          ] if
        ] if
      ] if
    ] if
  ] if ;

: evlis ( lst env -- lobj )
  swap kNil get-global -rot  ! ret env lst
  [ dup tag>> "cons" = ] [
    dupd dup data>> car>>  ! ret env env lst CAR(lst)
    rot eval  ! ret env lst lobj
    dup tag>> "error" = [
      2nip nip kNil get-global dup  ! lobj nil nil
    ] [
      [ rot ] dip  ! env lst ret lobj
      swap make-cons -rot  ! ret env lst
      data>> cdr>>
    ] if
  ] while 2drop
  dup tag>> "error" = [
  ] [
    nreverse
  ] if ;

: progn ( body env -- lobj )
  swap kNil get-global -rot  ! ret env body
  [ dup tag>> "cons" = ] [
    rot drop  ! env body
    dupd data>> dup cdr>> swap car>>  ! env env CDR(body) CAR(body)
    rot eval -rot  ! lobj env CDR(body)
  ] while 2drop ;

: subr-car ( args -- lobj )
  safe-car safe-car ;

: subr-cdr ( args -- lobj )
  safe-car safe-cdr ;

: subr-cons ( args -- lobj )
  dup safe-car swap safe-cdr safe-car make-cons ;

: subr-call ( args n -- lobj )
  dup 0 = [
    drop subr-car
  ] [
    dup 1 = [
      drop subr-cdr
    ] [
      dup 2 = [
        drop subr-cons
      ] [
        2drop "subr-noimpl" make-error
      ] if
    ] if
  ] if ;

: apply ( fn args env -- lobj )
  -rot dup tag>> "error" = [  ! env fn args
    2nip
  ] [
    swap dup tag>> "error" = [  ! env args fn
      2nip
    ] [
      dup tag>> "subr" = [
        data>> subr-call nip
      ] [
        dup tag>> "expr" = [
          rot drop  ! args fn
          data>> dup args>>  ! args fnd args
          rot pairlis  ! fnd binds
          swap dup env>>  ! binds fnd env
          rot swap make-cons  ! fnd env
          swap body>> swap progn
        ] [
          3drop "noimpl" make-error
        ] if
      ] if
    ] if
  ] if ;

"car" make-sym 0 make-subr g-env get-global add-to-env
"cdr" make-sym 1 make-subr g-env get-global add-to-env
"cons" make-sym 2 make-subr g-env get-global add-to-env
"t" make-sym "t" make-sym g-env get-global add-to-env

"> " write flush
[ readln dup ] [
  read drop g-env get-global eval print-obj print
  "> " write flush
] while drop
