# TLS-Scheme-Interpreter

1) The Goal os this project is to understand how the Scheme interpreter works on a deeper level by learning how primitive works and designing and adding new primitives to the interpreter.
  Examples of primitive:
  primitive: my-add, my-division

  Before we add primitives we assume that TLS works as advertised.
  we then design a primitive that would be used for the interpreter.

2) We then change the representation of bindings in the system to explicit pairs of the form (name value) without affecting the working of the TLS interpreter. Specifically our changed subsystem must also satisfy the specifications of the interpreter.

  previously when a call like '((lambda (x y z) (* x y z)) 1 2 3) results in a entry ((x y z)(1 2 3)) where x is name1, 1 is value1
  y is name2, 2 is value2 and z is name3, 3 is value3
  what we want is to make some changes so that the result of entry is of the form ((x 1) (y 2) (z 3)) instea of the form ((x y z)(1 2 3))

3) We then design a simple syntax checker that imitates and works just like the original scheme interpreter checker.

  if the expression is a valid tls input (expression e), then return #t. other wise throws error as expression is not a valid e.

4) We then Add let to TLS, and prove that the resulting interpreter (for the language TLS-let, that is, TLS with let) is correct and works like the let provided by R5RS scheme interpreter.




