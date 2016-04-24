# It's Alive!
Welcome to *It's Alive!*, a library offering a model-building paragdigm for Clojure and Common Lisp computer programming, a paradigm that has been applied successfully to several real-world applications, including enterprise Web applications, desktop applications, and distributed computing. 

In the modelling paradigm we declaratively specify models which run by themselves (i) acting on the world via APIs when (ii) first instantiated and then stimulated by input piped into the model by a straightforward supervisor polling eg. an event loop, socket input, AJAX requests, or database notification. 

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
Not much to see there in return for all that `make-cell` and `c-get` effort, except introducing those two functions. Your repl should show:
```
World knocks
```
Now let's chalk up our first *IA!* win:
```clojure
(deftest hw-02
  (let [obs-action (atom nil)
        v ;;"visitor"
        {:name "World"
         :action (c-in "knocks"
                       :slot :v-action
                       :obs ;; short for observer
                       (fn [slot me new old c]
                         (reset! obs-action new)
                         (println :observing slot new old)))}]
    (is (=  (c-get (:name v)) "World"))
    (is (=  (c-get (:action v)) "knocks"))
    (is (= "knocks" @obs-action)))
```
The observer gives us our model a bit of life independent of the code we invoke explicitly, in this case a simple logging to the console of the call to the observer:
```
:observing :v-action knocks knocks
```
The astute observer will spot a problem: both old and new value are the same. Let's ignore for now that artifact of standalone cells (which I suspect are of little value to model building because attributes tend to be attributes of something larger (a model)).

Anyway, here we have:
* abbreviated `make-cell/:input?` true to `c-in`;
* created an observer to both echo the state change details to the console and stash the new value in an atom (yes, just a Stupid Pet Trick);
* *very* importantly, the model has sprung to life seemingly on its own (but in fact by the act of our reading the slot values -- the `c-get` calls). When we get serious about model-building we will be even more pro-active about bringing our models smoothly to life (and easing them off this mortal coil); and finally
* something new to those familiar with Cells in Common Lisp: a Cell can exist on its own.

Now let's see about life after birth, introducing *change*:
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

    (c-reset! (:action v) "knock-knock") ;; <== change!
    (is (= "knock-knock" @action))
    (is (= (c-get (:action v)) "knock-knock"))))
```
The console:
```
:observing :v-action nil nil nil
:observing :v-action nil knock-knock nil
```
Here we:
* define the observer (a bit) separately from the cell; and
* start with a nil *action* we then bash in place.
Note that we can now inspect that *action* atom immediately after *c-reset!* and before any c-gets, meaning the propagation from visitor action to our test atom action happened by itself as part of the c-reset! handling.

ie, It happened eagerly...It's Alive!

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
* our first `make-c-formula` that will keep our resident's location in synch with their actions.

Now let's put it all together:

``` clojure
(deftest hello-world
  (println :--go------------------)
  (let [obs-action (fn [slot me new old c]
                     (println slot new old))
        v {:name "World"
           :action (c-in nil :slot :v-action
                         :obs obs-action)}
        r-action (c-in nil)
        r-loc (c?+ [:obs (fn-obs (when new (trx :honey-im new)))]
                   (case (c-get r-action)
                     :leave :away
                     :return :home
                     :missing))
        r-response (c?+ [:obs (fn-obs (trx :r-resp new))]
                        (when (= :home (c-get r-loc))
                          (when-let [act (md-get :action v)]
                            (case act
                              :knock-knock "hello, world"))))]
    (is (nil? (c-get r-response)))
    (c-reset! (:action v) :knock-knock)
    (c-reset! r-action :return)
    (is (= :home (c-get r-loc)))))
```
...and voila!
```
:--go------------------
:honey-im: :missing
:r-resp: 
:v-action :knock-knock nil
:honey-im: :home
:r-resp: hello, world
```
The astute observer will spot a serious problem: the resident responded to a knock that happened when they were not home. The sequence went like this:
* the visitor `:action` was set to `:knock-knock`;
* no `r-response` was computed because our resident was not `:home`;
* the `r-action` was set to `return`;
* their  `r-loc` got recomputed as `:home`;
* the `r-response` rule got kicked off;
* the `r-response` rule looked at the visitor `:action` and there was the `:knock-knock`.

We need a way to express events, things that happen at a point in time and then are over with. And it is not that they go away. They happen more than exist, so there is no need to stop existing. They are fleeting, evanescent, ephemeral. Let's add that specification to all but the only long-lived state in this example, the resident location:
``` clojure
(deftest hello-world
  (println :--go------------------)
  (let [obs-action (fn [slot me new old c]
                     (println slot new old))
        v {:name "World"
           :action (c-in nil
                         :slot :v-action
                         :ephemeral? true
                         :obs obs-action)}
        r-action (c-in nil)
        r-loc (c?+ [:obs (fn-obs (when new (trx :honey-im new)))]
                   (case (c-get r-action)
                     :leave :away
                     :return :home
                     :missing))
        r-response (c?+ [:obs (fn-obs (trx :r-response new))
                         :ephemeral? true]
                        (when (= :home (c-get r-loc))
                          (when-let [act (md-get :action v)]
                            (case act
                              :knock-knock "hello, world"))))]
    (is (nil? (c-get r-response)))
    (c-reset! (:action v) :knock-knock)
    (c-reset! r-action :return)
    (is (= :home (c-get r-loc)))
    (c-reset! (:action v) :knock-knock)))
```
Success:
```
:honey-im: :missing
:r-response: 
:v-action :knock-knock nil
:honey-im: :home
:v-action :knock-knock nil
:r-response: hello, world
```
And one last astutism: above we set the visitor action to the same value :knock-knock consecutively. Normally with Cells we do not bother propagating values that do not change the world; if I change my speed from 60mph to 60mph, no one needs to know. But again events are different: two events with the same value are not the same event, and *IA!* propagates accordingly.
