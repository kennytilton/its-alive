# It's Alive!
Welcome to *It's Alive!*, a library offering a model building paragdigm for Clojure and Common Lisp computer programming. It takes a while to grasp that one can program this way, but Cells has been applied successfully to several real-world applications, including enterprise Web applications (check out all-Cells-all-the-time [Tilton's Algebra](http://tiltonsalgebra.com)), desktop applications, and distributed computing. 

In the modelling paradigm we declaratively specify models which run by themselves
* acting on the world via APIs
* when first instantiated and then stimulated by input piped into the model by a straightforward supervisor polling, eg., an event loop, socket input, AJAX requests, or database notification. 

Two good examples have been:

* a Web application reshaped dynamically to accomodate the user's activity (AJAX in and HTML, JSON, or JS out); and
* a virtual RoboCup team player (i) stimulated by sensory input from the game server over a UDP socket (ii) sending back run, turn, and kick commands.

For a much longer introduction to *IA!*, see [my Cells Manifesto write-up](http://smuglispweeny.blogspot.com/2008/02/cells-manifesto.html) detailing *IA!'s* progenitor, my Common Lisp [Cells project](https://github.com/kennytilton/cells).

Me, I like examples. Let's take a quick look at an interesting-enough example during which I am afraid much will seem like magic (unless of course you are familiar with the seeming dozens of libraries doing the same thing -- espicially [Hoplon/Javelin](https://github.com/hoplon/javelin)).

I myself find magic examples unhelpful, but I will try to make this quick and once we turn technical things will get dense in a hurry. Let us give you the big picture view first.

### hello, world
If you want to play at home, code that follows will be found in
```
its-alive/test/tiltontec/its-alive/01_hellow_world.clj
```
If you are new to Clojure I recommend [Brave Clojure](http://www.braveclojure.com/). It covers everything from the tooling to getting started with the Emacs editor.

``` clojure

(deftest hw-01
  (let [v ;;"visitor"
        {:name "World"
         :action (make-cell :value "knocks"
                            :input? true)}]

    (println (c-get (:name v))
             (c-get (:action v)))

    (is (=  (c-get (:name v)) "World"))
    (is (=  (c-get (:action v)) "knocks"))))
```
Not much to see there in return for all that *make-cell* and _c-get_ effort, eh? Just setting the baseline. Now let's take our first win:
```clojure
(deftest hw-02
  (let [obs-action (atom nil)
        v ;;"visitor"
        {:name "World"
         :action (c-in "knocks"
                       :obs ;; short for observer
                       (fn [slot me new old c]
                         (reset! obs-action new)
                         (println :observing slot me new old)))}]
    (println (c-get (:name v))) ;; no Cell, no observer: println to the rescue
    (is (=  (c-get (:name v)) "World"))
    (is (=  (c-get (:action v)) "knocks"))
    (is (= "knocks" @obs-action))))
```
We have:
* abbreviated *make-cell/:input? true* to *c-in*; and
* created an observer to both echo the state change details to the console and stash the new value in an atom (yes, just a Stupid Pet Trick).
Still not much to shout about, except perhaps this: the model has sprung to life seemingly on its own (but in fact by the act of reading -- the *c-get* calls).

When we get serious about model-building we will be even more pro-active about bringing our models to life smoothly. For now, let's see about life after birth:
``` clojure

(deftest hw-03
  (let [action (atom nil)
        obs-action (fn [slot me new old c]
                     (reset! action new)
                     (println :observing slot me new old))
        v {:name "World"
           :action (c-in nil :obs obs-action)}]

    (println :v-name (c-get (:name v))) ;; no Cell/observer, so println
    (is (=  (c-get (:name v)) "World"))
    (is (nil? (c-get (:action v))))
    (is (nil? @action))

    (c-reset! (:action v) "knock-knock")
    (is (= "knock-knock" @action))
    (is (= (c-get (:action v)) "knock-knock"))))
```
Here we:
* define the observer (a bit) separately from the cell; and
* start with a nil *action* we then bash in place.
Note that we can now inspect that *action* atom immediately after *c-reset!* and before any c-gets, meaning the propagation from visitor action to our test atom action happened by itself as part of the c-reset! handling. ie, It happened eagerly.

And now another baby step, in which we introduce formulaic or "ruled" cells:
``` clojure
(deftest hw-04
  (let [r-action (c-in nil
                       :slot :r-action
                       :obs gobs)
        r-loc (make-c-formula
               :slot :r-loc
               :obs gobs
               :rule (fn [c]
                       (case (c-get r-action)
                         :leave :away
                         :return :at-home
                         :missing)))]

    (is (= :missing (c-get r-loc)))

    (c-reset! r-action :leave)
    (is (= :away (c-get r-loc)))))
```
We actually simplify here and just work with standalone cells to minimize the noise (and because we are not really ready yet for true model-building). What we do see is:
* we are using the :slot (short for slot-name) key to give our cells names; and
* our first *make-c-formula* that will keep our resident's location in synch with their actions.


[to be continued]


Props to Mr. Doob and his [code editor](http://mrdoob.com/projects/code-editor/), from which
the inspiration to this, and some handy implementation hints, came.

### Stuff used to make this:

 * [markdown-it](https://github.com/markdown-it/markdown-it) for Markdown parsing
 * [CodeMirror](http://codemirror.net/) for the awesome syntax-highlighted editor
 * [highlight.js](http://softwaremaniacs.org/soft/highlight/en/) for syntax highlighting in output code blocks
 * [js-deflate](https://github.com/dankogai/js-deflate) for gzipping of data to make it fit in URLs
