(ns tiltontec.its-alive.utility
  (:require   [clojure.string :as $]))

(set! *print-level* 10) ;; lose this if we lose recursive data structures

(defmacro prog1 [& body]
  `(let [result# ~(first body)]
     ~@(rest body)
     result#))

(defmacro b-when [var form & body]
  `(when-let [~var ~form]
     ~@body))

(defn cl-find [sought coll]
  (some #{sought} coll))

(defmacro unless [form & body]
  `(when-not ~form
     ~@body))

(defn type-of [x] (type x))

(defn set-ify [x]
  (cond
   (nil? x) #{}
   (sequential? x) (set x)
   :else #{x}))

;; --- refs with maps conveniences -------------------

(defmacro def-rmap-slots [reader-prefix & slots]
  `(do
     ~@(map (fn [slot#]
              `(defn ~(symbol (str (or reader-prefix "")
                                   slot#))
                 [~'ref]
                 (~(keyword slot#) @~'ref))) slots)))

(defmacro rmap-setf [[slot ref] new-value-form]
  `(let [new-value# ~new-value-form]
     (alter ~ref assoc ~slot new-value#)
     new-value#))

;; --- error handling -----------------

(ns-unmap *ns* 'c-model)

(defn error
  ([msg] (throw (Exception. msg)))
  ([m1 & mr] (error
              (if (fn? m1)
                   (apply m1 mr)
                   ($/join " " (cons m1 mr))))))

(do
  (defmulti err (fn [a1 & args] (fn? a1)))

  (defmethod err true [fn & mas]
    (err (apply fn mas)))

  (defmethod err :default [& bits]
    (throw (Exception. ($/join " " (cons "jz/err>" bits))))))

(defn any-ref? [x]
  (instance? clojure.lang.Ref x))

(def ^:dynamic *trx?* true)

#_
(alter-var-root #'*trx?* not)

(def ^:dynamic *trc-ensure* nil)
(def ^:dynamic *trx-path-id* nil)
(def ^:dynamic *trxdepth* 0)
(def last-trc (atom 0)) ;; s/b universal time

(defn call-trc$ [s bits]
  (str s ": " ($/join ", " bits)))

;; (call-trc$ nil (list :himom-shouldnot-appear 1 2 3))
;; (call-trc$ "cool" (list :himom-shouldnot-appear 1 2 3))

(defn call-trc [s & os]
  ;; (break) ;; uncomment to escape loop
  (when *trx?*
    (when s
      (let [path (apply str (repeat *trxdepth* "."))]
        (println path (call-trc$ s os))))))

(defmacro trx [label & vals]
  `(call-trc ~(when (not (nil? label))
                (str label))
             ~@vals))

(defmacro wtrx [[lo hi & trxargs] & body]
  `(binding [*trxdepth* (inc *trxdepth*)]
     (cond
      (<= ~lo *trxdepth* ~hi)
      (trx ~@trxargs)

      (> *trxdepth* ~hi)
      (throw (Exception. (str
                          (format "wtrx exceeded max(%d): " ~hi)
                          (call-trc$ '~(first trxargs)
                                     (list ~@(rest trxargs)))))))
     ~@body))

#_
(binding [*trxdepth* 5]
  (wtrx (0 100 "cool" 1 2 3)
        (println :body)))

(defn wtrx-test [n]
  (wtrx (0 10 "test" n)
        (when (> n 0)
          (wtrx-test (dec n)))))

;; --- deftest support ---------------------
;; These next two are lame because they just
;; look at slots (ignoring models). Use only
;; in tests looking at one model or at least
;; slot names do not duplicate.
;;

(defn slot-users [me slot]
  (set (map :slotq
            (map deref
                 (:callers @(slot @me) #{})))))

(defn slot-useds [me slot]
  (set (map :slot
            (map deref
                 (:useds @(slot @me) #{})))))

;;; --- FIFO Queue -----------------------------

(defn make-fifo-queue []
  (ref []))

(defn fifo-data [q] @q)
(defn fifo-clear [q]
  (alter q empty))
(defn fifo-empty? [q]
  (empty? @q))
(defn fifo-peek [q]
  (first @q))

(defn fifo-add [q new]
  (alter q conj new))

(defn fifo-pop [q]
  (when-not (fifo-empty? q)
    (prog1
     (first @q)
     (alter q subvec 1))))

;;; --- learning curve exercises
;;

(comment
  (loop [[slot v & r]  '(:a 0 :b 1 :c 9)
         acc (transient {})]
    (if (nil? slot)
      (persistent! acc)
      (recur r (assoc! acc
                       slot
                       (cond 
                        (typep v :jz)
                        (merge {:slot slot} v)
                        :else v)))))

  (into (hash-map)
      (map (fn [[k v]] (vector k (inc v)))
           (partition 2 '(:a 0 :b 1 :c 9))))

  (reduce (fn [m [k v]]
          (assoc m k (inc v)))
        (hash-map)
        (partition 2 '(:a 0 :b 1 :c 9))))

:utility-ok
