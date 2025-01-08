# CL-ELEMENTAL

## Common Lisp computer science lab.

Clone this repo to `~/.quicklisp/local-projects' then do

```common-lisp 
(ql:register-local-projects)
(ql:quickload :cl-elemental)
```

### Integer groups

Do ```(in-package :integer-groups)``` and then use it.
You can create Z/nZ additive groups and (Z/nZ)* multiplicative groups,
print their Cayley tables and find subgroups.

```common-lisp
INTEGER-GROUPS> (defparameter ag (make-agroup 10))
AG
INTEGER-GROUPS> (defparameter mg (make-mgroup 18))
MG
INTEGER-GROUPS> ag
#<AG 10 MOD+10 0 (0 1 2 3 4 5 6 7 8 9)>
INTEGER-GROUPS> mg
#<MG 6 MOD*18 1 (1 5 7 11 13 17)>
INTEGER-GROUPS> 
```






