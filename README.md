# It's Alive!
A Clojure dataflow library for building application models that run by themselves. It's alive!(tm) is starting out as a literal translation of my Common Lisp Cells library, where you will find the closest thing I have done to documentation: https://github.com/kennytilton/cells/wiki

Once I have this Clojure version working I will do a proper tutorial demonstrating all the many features.

[Now I have the Clojure version working.

``` lisp
(defun ufb-queue-ensure (opcode)
  (or (ufb-queue opcode)
    (cdr (car (push (cons opcode (make-fifo-queue))
                *unfinished-business*)))))
```
A second exercise will be incorporating (welcome!) complaints from the peanut gallery about my non-idiomatic Clojure (I will be writing Common Lisp in Clojure at first).

Then we have some fun: parallel state propagation. I think we can do pretty well on data integrity commuting the pulse ref and perhaps even dependency state between cells. Just need a good use case -- perhaps a physics engine?

More soon.
