USE: io
USE: kernel

IN: lisp

"> " write flush
[ readln dup ] [
  print
  "> " write flush
] while drop
