# CL-ELEMENTAL

## Common Lisp computer science lab.

Clone this repo to `~/.quicklisp/local-projects' then do

```common-lisp 
(ql:register-local-projects)
(ql:quickload :cl-elemental)
```

### Integer groups

Do ```(in-package :integer-groups)``` and then use it interactively.
Create Z/nZ additive groups and (Z/nZ)* multiplicative groups.

```common-lisp
INTEGER-GROUPS> (defparameter ag (make-agroup 10))
INTEGER-GROUPS> (defparameter mg (make-mgroup 18))
INTEGER-GROUPS> ag
#<AG 10 MOD+10 0 (0 1 2 3 4 5 6 7 8 9)>
INTEGER-GROUPS> mg
#<MG 6 MOD*18 1 (1 5 7 11 13 17)>
INTEGER-GROUPS> 
```
print their Cayley tables:

```common-lisp
INTEGER-GROUPS> (print-cayley-table ag :width 3)
  0  1  2  3  4  5  6  7  8  9
  1  2  3  4  5  6  7  8  9  0
  2  3  4  5  6  7  8  9  0  1
  3  4  5  6  7  8  9  0  1  2
  4  5  6  7  8  9  0  1  2  3
  5  6  7  8  9  0  1  2  3  4
  6  7  8  9  0  1  2  3  4  5
  7  8  9  0  1  2  3  4  5  6
  8  9  0  1  2  3  4  5  6  7
  9  0  1  2  3  4  5  6  7  8
NIL
INTEGER-GROUPS> (print-cayley-table mg :width 3)
  1  5  7 11 13 17
  5  7 17  1 11 13
  7 17 13  5  1 11
 11  1  5 13 17  7
 13 11  1 17  7  5
 17 13 11  7  5  1
INTEGER-GROUPS> 
```
Find subgroups of length k: 

```common-lisp
INTEGER-GROUPS> (find-subgroups ag 5)
(#<ZG 5 MOD+10 0 (0 2 4 6 8)>)
INTEGER-GROUPS> (find-subgroups mg 3)
(#<ZG 3 MOD*18 1 (1 7 13)>)
INTEGER-GROUPS> 
```


