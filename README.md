# It's Alive!
Welcome to *It's Alive!*, a model-building paragdigm of computer programming in which we declaratively specify models which then run by themselves. Two good examples have been:

* a Web site reshaped dynamically by the working model when stimulated with AJAX inputs from the client; and
* a virtual RoboCup team player model stimulated by sensory input provided by the game server over UDP.

For a much longer introduction to *IA!*, see [my Cells Manifesto write-up](http://smuglispweeny.blogspot.com/2008/02/cells-manifesto.html) detailing *IA!'s* progenitor, my Common Lisp [Cells project](https://github.com/kennytilton/cells).

Me, I like examples.

[to be continued]

# Pardon Our Appearance During Doc Authoring

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
