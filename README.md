# It's Alive!
Welcome to *It's Alive!*, a model-building paradigm atop Clojure or Common Lisp that has been applied successfully to several real-world applications, including enterprise Web applications, desktop applications, and distributed computing. 

In the modelling paradigm we declaratively specify models which run by themselves acting on the world via APIs when first instantiated and then stimulated by input piped into the model by a straightforward supervisor polling an event loop, a socket, AJAX requests, database notifications, or whatever. 

Two good examples:
* a Web application reshaped dynamically to accomodate the user's activity (AJAX in and HTML, JSON, or JS out); and
* a virtual RoboCup team player (i) stimulated by sensory input from the game server over a UDP socket (ii) sending back run, turn, and kick commands.

For a much longer introduction to *IA!*, see [my Cells Manifesto write-up](http://smuglispweeny.blogspot.com/2008/02/cells-manifesto.html) detailing *IA!'s* progenitor, my Common Lisp [Cells project](https://github.com/kennytilton/cells).

Me, I like examples. Let's take a quick look at an interesting-enough example during which I am afraid much will seem like magic (unless of course you are familiar with the seeming dozens of libraries doing the same thing -- espicially [Hoplon/Javelin](https://github.com/hoplon/javelin)).

I myself find magic examples unhelpful, but I will try to make this quick and once we turn technical things will get dense in a hurry. Let us give you the big picture view first.

### hello, world
If you want to play at home, code that follows will be found in
```
its-alive/test/tiltontec/its-alive/01_hello_world.clj
```
[I'll get this up on Clojars when I get to version 0.1.0.]

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
The observer gives our model its first bit of life independent of the code we invoke explicitly, in this case a simple logging to the console:
```
:observing :v-action knocks knocks
```
The astute observer will spot a problem: both old and new value are the same. Let's ignore for now that artifact of standalone cells (which I suspect are of little value to model building because attributes tend to be attributes of something -- I digress).

Experienced Lisp Cells users will notice a new twist: an individual Cell can specifcy its own, if you will, anonymous observer.

Anyway, in this example we have:
* abbreviated `make-cell/:input? true` to `c-in`;
* created an observer to both echo the state change details to the console and stash the new value in an atom (yes, just a Stupid Pet Trick);
* *very* importantly, the model has sprung to life seemingly on its own (but in fact by the act of our reading the slot values -- the `c-get` calls). (More below on this coming to life business.); and finally
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
(defn gobs [slot me new old c]
  (println :gobs> slot new old))
  
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
    (c-awaken r-loc)
    (is (= :missing (:value @r-loc)))
    (println :---about-to-leave------------------)
    (c-reset! r-action :leave)
    (println :---left------------------)
    (is (= :away (c-get r-loc)))))
```
We actually simplify here and just work with standalone cells to minimize the noise (and because we are not really ready yet for true model-building). What we do see is:
* we are using the :slot (short for slot-name) key to give our cells names;
* our first `make-c-formula` that will keep our resident's location in synch with their actions; and
* `c-awaken` to force a nascent cell into full participation in the model, something we need to emphasize (next).

### You must remember this...
When we get serious about modelling, our model-creation API will gracefully bring models into existence, including seeing to it that all their slots/cells are brought into the game of life with integrity, ie, consistent with the state of the model at the pulse at which they are created. That means computing the initial value of all formula cells and observing all cells and even constants provided for a slot where an observer has been specified on the slot name (something we have not looked at yet).

Until then, or as long as we want to have a standalone cell, that cell will not be brought to life as just described until we pass it to `c-awaken` or `c-get`. If you start playing with this example adding new cells and they seem inoperative, there is a good chance you neglected to awaken the cell one way or the other.
#### Do as I say, not as I do
In the remaining code I am letting the testing assertions take care of awakening cells via `c-get`. I have also spent more than a few minutes tracking down bugs that were no more than failures to awaken. Do not let this happen to you (and when we get to model-building this will all be behind us).

By the way, yes, we could bake `c-awaken` into `c-in`, but with the formulaic variants be careful not to `c-awaken` a cell that uses a cell not yet defined (possible with sufficient legerdemain).
### Back to "hello, world"
Now let's put it all together:

``` clojure
(deftest hw-5
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
The astute observer will spot a serious problem: the resident responded to a knock that happened when they were way. How could that have happened? The sequence went like this:
* the visitor `:action` was set to `:knock-knock`;
* no `r-response` was computed because our resident `r-loc` was not `:home`;
* the `r-action` was set to `return`;
* their  `r-loc` got recomputed as `:home`;
* the `r-response` rule, seeing `r-loc` had changed, got kicked off;
* the `r-response` rule looked at the visitor `:action` and the value `:knock-knock` was still sitting there.

##### Ephemerality
We need a way to express events -- things that happen at a point in time and then are over with. As a corrollary, they need not go away in any fashion visible to the model; they do not exist so much as happen, so there is no need to stop existing. They are fleeting, evanescent, *ephemeral*. 

Let us get a good understanding of this. Cells is a magic bullet with its spreadsheet-like automatic ability to recalculate everything in the right order, but the spreadsheet metaphor fails us when we turn to handling events. Hence the `ephemeral?` option.

Let's add that specification to all but the only non-ephemeral state in this example, the resident location:
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
And one last astutism: above we set the visitor action to the same value :knock-knock twice consecutively. Normally with Cells we do not bother propagating values that do not change the world; if I change my speed from 60mph to 60mph, no one needs to know. But again events are different: two events with the same value are not the same event, and *IA!* propagates accordingly.
#### How alarming
Let us now get just the slightest glimpse of a real world application built with *IA!*:
``` clojure
(deftest hello-world-2
  (println :--go------------------)
  (let [obs-action (fn [slot me new old c]
                     (when new (trx visitor-did new)))
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
        r-response (c?+ [:obs (fn-obs (when new
                                        (trx :r-response new)))
                         :ephemeral? true
                         ]
                        (when (= :home (c-get r-loc))
                              (when-let [act (c-get (:action v))]
                                (case act
                                  :knock-knock "hello, world"))))
        alarm (c?+ [:obs (fn-obs
                          (trx :telling-alarm-api new))]
                   (if (= :home (c-get r-loc)) :off :on))
        alarm-do (c?+ [:obs (fn-obs
                            (case new
                              :call-police (trx :auto-dialing-911)
                              nil))]
                     (when (= :on (c-get alarm))
                       (when-let [action (c-get (:action v))]
                         (case action
                           :smashing-window :call-police
                           nil))))]
    (c-awaken [alarm-do r-response r-loc (:action v)])
    (is (= :missing (:value @r-loc)))
    (c-reset! (:action v) :knock-knock)
    (c-reset! (:action v) :smashing-window)
    (c-reset! r-action :return)
    (is (= :home (c-get r-loc))) 
    (c-reset! (:action v) :knock-knock)))
```
That is all a bit silly because we left out the bit where the alarm has a microphone to detect sounds or the window panes are wired to interrupt a circuit when broken and who says I do not want the alarm to be on when I am home and how did... but hopefully you get the idea:
```
 :honey-im: :missing
 :telling-alarm-api: :on
 visitor-did: :knock-knock
 visitor-did: :smashing-window
 :auto-dialing-911: 
 :honey-im: :home
 :telling-alarm-api: :off
 visitor-did: :knock-knock
 :r-response: hello, world
 ```
Next up (in a few days, after I finish a neat coding interview exercise): modelling.

#### Hello, Model!
So far it has been fun playing with standalone Cells, which is a new feature for the Clojure version. Hoplon/Javelin did that and it turned out to be easy so why not? But the world is made of entities with one or more attributes, so without making a big deal about OOP per se let us take a look at the same example using aggregates of Cells, or *models*. The following is an annotated version of the last test in model_test.clj:

``` clojure
(deftest hello-model
  (let [uni (md/make
             ::fm/family
             :kids (c? (the-kids
```
`make` returns a `ref` holding a map of all the key-values supplied, with the caveat that if as here the argument list has an odd count the first gets installed as the :type in the `meta`. We could just do `:type ::fm/family` with the same effect. `make` does quite a bit as advertised above, including bring all the cells to life so we need not bother any longer with `c-awaken`. It also shunts the cells off into the `meta` of the ref so the map just magically holds the values supplied or calculated for each key.

But, hey, I did not mean this to be a complete reference, so let's skim a little faster.

`kids` is a super big deal. *IA!* modelling is about a tree (a DAG, in fact) of model aggregates of cells variously but not cyclically dependent on each other. (Cyclic is caught at run time.) Crucially, the kids of any given family are computed based on other data and can vary over time, and *IA!* takes care of ushering models onto and off the stage gracefully.

In this case the list of kids is unconditional, but computing the list in a cell rule means the owning model `me` already exists and can be passed in as parent to `make`. In general, one of the incidental wins of *IA!* is that ruled cells allow us to initialize models with values to be computed with awareness of surrounding state. I digress.

`the-kids` handles a bit of boilerplate, binding `*par*` to the parent so we need not specify `:par me` on each call to make. It also flattens the input (which gets wrapped in `(list ...)` so we can code up a nested list of models if that is more convenient knowing `the-kids` will do the flattening.

``` clojure
                        (md/make
                         :name :visitor
                         :moniker "World"
                         :action (c-in nil 
                                       :ephemeral? true
                                       :obs (fn [slot me new old c]
                                              (when new (trx visitor-did new)))))
                        (md/make
                         :name :resident
                         :action (c-in nil :ephemeral? true)
                         :location (c?+ [:obs (fn-obs (when new (trx :honey-im new)))]
                                        (case (md-get me :action)
                                          :leave :away
                                          :return :home
                                          :missing))
```
`md-get` locates the associated cell (if there is one) and `c-get`s the current data-consistent value.
```
                         :response (c?+ [:obs (fn-obs (when new
                                                        (trx :r-response new)))
                                         :ephemeral? true]
                                        (when (= :home (md-get me :location))
                                          (when-let [act (mdv! :visitor :action)]
```
`mdv!` could be written as `(md-get (fm! :visitor) :action)`, where `fm!` is a `::fm/family` utility for searching the family tree for a given family member. What is going on here is that we need a handy way to reference a cell of some other model we know is out there. 

In this case we are doing it by `:name`, but more often these days I find myself searching by type, so soon enough we will extend `fm!` to accept a type (defined as a member of the `cell_types/ia_types` type hierarchy) and support search by `isa?`. A good use case for search by type is a selectable item being clicked and informing its selection manager that it is now the selection, finding that manager by searching up the GUI widget family tree for the first container of type, say, `selection-mgr`.

```
                                            (case act
                                              :knock-knock "hello, world")))))
                        (md/make
                         :name :alarm
                         :on-off (c?+ [:obs (fn-obs
                                             (trx :telling-alarm-api new))]
                                      (if (= :home (mdv! :resident :location)) :off :on))
                         :activity (c?+ [:obs (fn-obs
                                               (case new
                                                 :call-police (trx :auto-dialing-911)
                                                 nil))]
                                        (when (= :on (md-get me :on-off))
                                          (when-let [action (mdv! :visitor :action)]
                                            (case action
                                              :smashing-window :call-police
                                              nil))))))))]
    (let [viz (fm! :visitor uni)
          rez (fm! :resident uni)]
      (is (not (nil? viz)))
      (is (not (nil? rez)))
      (is (not (nil? (md-cell rez :action))))
      (is (= :missing (mdv! :resident :location uni)))
      (md-reset! viz :action :knock-knock)
      (md-reset! viz :action :smashing-window)
      (is (not (nil? (md-cell rez :action))))
      (md-reset! rez :action :return)
      (is (= :home (mdv! :resident :location uni)))
      (md-reset! viz :action :knock-knock))))
```

Results as before:
```
:honey-im: :missing
 :telling-alarm-api: :on
 visitor-did: :knock-knock
 visitor-did: :smashing-window
 :auto-dialing-911: 
 :honey-im: :home
 :telling-alarm-api: :off
 visitor-did: :knock-knock
 :r-response: hello, world
 ```
#### Next Up?
Not sure. I am tempted to try a port to clojurescript if it turns out to be as straightforward as I think. Then it is time to apply *IA!* to Web application development, starting with *qooxdoo* backend, and then one with *ReactJS*.
