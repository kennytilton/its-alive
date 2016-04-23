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

[to be continued]
