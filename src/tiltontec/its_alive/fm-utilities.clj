(comment

    Cells -- Automatic Dataflow Managememnt



)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   '(;; Family member creation
     make-part
     mk-part
     mk-part-spec
     upper
     u^
     kontainer
     container-typed
     
     ;; Family member finding
     fm-descendant-typed
     fm-ascendant-typed
     fm-kid-named
     fm-descendant-named
     fm-ascendant-named
     fm-ascendant-some
     fm-ascendant-if
     fm-descendant-if
     fm-descendant-common
     fm-collect-if
     fm-collect-some
     fm-value-dictionary
     fm-max
     fm-traverse
     fm-traverse-bf
     fm-ordered-p
     sub-nodes
     fm-ps-parent
     with-like-fm-parts
     do-like-fm-parts
     true-that
     fm-do-up
     fm-gather
     fm-find-all
     fm-find-next
     fm-find-next-within
     fm-find-prior
     fm-find-prior-within 
     fm-find-last-if
     fm-prior-sib
     fm-next-sib-if
     fm-next-sib
     ^fm-next-sib
     fm-find-if

     ;; Family ordering
     fm-kid-add
     fm-kid-insert-last
     fm-kid-insert-first
     fm-kid-insert
     fm-kid-remove
     fm-quiesce-all
     fm-kid-replace

     ;; Family high-order ops
     fm-min-kid
     fm-max-kid
     fm-other
     fmv
     fm-otherx
     fm-other-v
     fm-otherv?
     fm-other?
     fm-other!
     fm^ fm^v
     fm?
     fm!
     fm!v
     fm-other?!
     fm-collect
     fm-map
     fm-mapc
     fm-pos
     fm-count-named
     fm-count
     fm-top
     fm-first-above
     fm-nearest-if
     fm-includes
     fm-ancestor-p
     fm-kid-containing
     fm-ascendant-p
     fm-find-one
     fm-find-kid
     fm-kid-typed
     
     ;; Other family stuff
     make-name
     name-root
     name-subscript
     kid-no
     n^
     ;; Debug flags
     *fmdbg*
     
     )))

(def ^:dynamic *fmdbg* nil)

(defn make-part (partname part-class & initargs)
  ;;(trc "make-part > name class" partname partclass)
  (when part-class ;;a little programmer friendliness
    (apply #'make-instance part-class (append initargs (list :md-name partname)))))

(defmacro mk-part (md-name (md-class) & initargs)
  `(make-part ',md-name ',md-class ~@initargs
     :fm-parent (progn (assert self () "mk-part null self for parent") self)))

(defmethod make-part-spec ((part-class symbol))
  (make-part part-class part-class))

(defmethod make-part-spec ((part model))
  part)


(defmacro upper (self &optional (type t))
  `(container-typed ,self ',type))

(defmacro u^ (type)
  `(upper self ,type))

(defmacro u^v (type)
  `(value (upper self ,type)))

(defmacro n^ (type)
  `(nearest self ,type))

(defmacro n^v (type)
  `(value (nearest self ,type)))

(export! u^v n^v)

(defmethod kontainer (self) (fm-parent self))

(defmethod container-typed ((self model-object) type)
   (let ((parent (kontainer self))) ;; fm- or ps-parent
      (cond
       ((null parent) nil)
       ((typep parent type) parent)
       (t (container-typed parent type)))))

(defn fm-descendant-typed (self type)
  (when self
    (or (find-if (fn (k) (typep k type)) (kids self))
        (some (fn (k)
                  (fm-descendant-typed k type)) (kids self)))))

(defn fm-kid-named (self name)
  (cl/find name (^kids) :key 'md-name))

(defn fm-descendant-named (parent name &key (must-find t))
   (fm-find-one parent name :must-find must-find :global-search nil))

(defn fm-ascendant-named (parent name)
   (when parent
      (or (when (eql (md-name parent) name)
             parent)
          (fm-ascendant-named (fm-parent parent) name))))

(defn fm-ascendant-typed (parent name)
   (when parent
      (or (when (typep parent name)
             parent)
          (fm-ascendant-typed (fm-parent parent) name))))

(defn fm-ascendant-some (parent some-function)
   (when (and parent some-function)
     (or (some-function parent)
         (fm-ascendant-some (fm-parent parent) some-function))))

(defn fm-ascendant-if (self test)
  (when (and self test)
    (or (when (test self)
           self)
      (fm-ascendant-if .parent test))))

(defn fm-descendant-if (self test)
  (when (and self test)
    (or (when (test self)
          self)
      (loop for k in (^kids)
          thereis (fm-descendant-if k test)))))

(defn fm-ascendant-common (d1 d2)
  (fm-ascendant-some d1 (fn (node)
                            (when (fm-includes node d2)
                              node))))

(defn fm-collect-if (tree test &optional skip-top dependently)
  (let (collection)
    (fm-traverse tree (fn (node)
                        (unless (and skip-top (eq node tree))
                          (when (test node)
                            (push node collection))))
      :with-dependency dependently)
    (nreverse collection)))

(defn fm-collect-some (tree test &optional skip-top dependently)
  (let (collection)
    (fm-traverse tree (fn (node)
                        (unless (and skip-top (eq node tree))
                          (bwhen (s (test node))
                            (push s collection))))
      :with-dependency dependently)
    (nreverse collection)))

(defn fm-value-dictionary (tree value-fn &optional include-top)
  (let (collection)
    (fm-traverse tree
      (fn (node)
        (when (or include-top (not (eq node tree)))
          (bwhen (v (value-fn node))
            (push (cons (md-name node) v) collection)))))
    (nreverse collection)))

(defn fm-max (tree key)
  (let (max)
    (fm-traverse tree (fn (node)
                        (if max
                            (setf max (max max (key node)))
                          (setf max (key node)))))
    max))


(defn fm-traverse (family applied-fn &key skip-node skip-tree global-search opaque with-dependency)
   ;;(when *fmdbg* (trc "fm-traverse" family skipTree skipNode global-search))

   (when family
     (labels ((tv-family (fm)
                (etypecase fm
                  (cons (loop for md in fm do (tv-family md)))
                  (model-object
                   (unless (eql fm skip-tree)
                     (let ((outcome (and (not (eql skip-node fm)) ;; skipnode new 990310 kt
                                      (applied-fn fm))))
                       (unless (and outcome opaque)
                         (dolist (kid (kids fm))
                           (tv-family kid))
                         ;(tv-family (mdValue fm))
                         )))))))
       (flet ((tv ()
                (tv-family family)
                (when global-search
                  (fm-traverse (fm-parent family) applied-fn 
                    :global-search t
                    :skip-tree family
                    :skip-node skip-node
                    :with-dependency t)))) ;; t actually just defaults to outermost call
         (if with-dependency
             (tv)
             (without-c-dependency (tv))))))
  (values))

(defn fm-traverse-bf (family applied-fn &optional (cq (make-fifo-queue)))
  (when family
    (flet ((process-node (fm)
               (applied-fn fm)
               (when (kids fm)
                 (fifo-add cq (kids fm)))))
      (process-node family)
      (loop for x = (fifo-pop cq)
            while x
            do (mapcar #'process-node x)))))

#+test-bf
(progn
  (defmd bftree (family)
    (depth 0 :cell nil)
    (id (c? (klin self)))
    :kids (c? (when (< (depth self) 4)
                (loop repeat (1+ (depth self))
                    collecting (make-kid 'bftree :depth (1+ (depth self)))))))
  
  (defn klin (self)
    (when self
      (if .parent
          (cons (kid-no self) (klin .parent))
        (list 0))))
  
  (defn test-bf ()
    (let ((self (make-instance 'bftree)))
      (fm-traverse-bf self
        (fn (node)
          (print (id node)))))))

(defn fm-ordered-p (n1 n2 &aux (top (fm-ascendant-common n1 n2)))
  (assert top)
  (fm-traverse top (fn (n)
                     (cond
                      ((eq n n1)(return-from fm-ordered-p t))
                      ((eq n n2)(return-from fm-ordered-p nil))))))

(defn kids-ordered (k1 k2)
  (assert (eq (fm-parent k1)(fm-parent k2)))
  (if (cl/find k2 (member k1 (kids (fm-parent k1))))
      (list k1 k2)
    (list k2 k1)))

(export! kids-ordered)

(defmethod sub-nodes (other)
  (declare (ignore other)))

(defmethod sub-nodes ((self family))
  (kids self))

(defmethod fm-ps-parent ((self model-object))
  (fm-parent self))

(defmacro with-like-fm-parts ((parts-var (self like-class)) & body)
   `(let (,parts-var)
       (fm-traverse ,self (fn (node)
                              ;;(trc "with like sees node" node (type-of node) ',likeclass)
                              (when (typep node ',like-class)
                                 (push node ,parts-var)))
         :skip-node ,self
         :opaque t)
       (setf ,parts-var (nreverse ,parts-var))
       (progn ~@body)))

(defmacro do-like-fm-parts ((part-var (self like-class) &optional return-var) & body)
   `(progn
     (fm-traverse ,self (fn (,part-var)
                            (when (typep ,part-var ',like-class)
                               ~@body))
       :skip-node ,self
       :opaque t)
       ,return-var)
   )

;;
;; family member finding
;;


(comment
 (defn fm-member-named (kidname kids)
  (member kidname kids :key #'md-name))
 )

(defn true-that (that) (declare (ignore that)) t)
;;
;; eventually fm-find-all needs a better name (as does fm-collect) and they
;; should be modified to go through 'gather', which should be the real fm-find-all
;;

(export! fm-heritage)

(defn fm-heritage (self)
  (loop for p = self then (fm-parent p)
        while p
        collect (list p (md-name p) (type-of p) (cz::md-state p))))

(defn fm-do-up (self &optional (fn 'identity))
  (when self
    (fn self)
    (if .parent (fm-do-up .parent fn) self))
  (values))

(defn fm-gather (family &key (test #'true-that))
     (packed-flat!
      (cons (when (test family) family)
        (mapcar (fn (fm)
                    (fm-gather fm :test test))
          (kids family)))))

(defn fm-find-all (family md-name &key (must-find t) (global-search t))
     (let ((matches (catch 'fm-find-all
                             (with-dynamic-fn
                              (traveller (family)
                               (with-dynamic-fn
                                (filter (kid) (eql md-name (md-name kid)))
                                (let ((matches (remove-if-not filter (kids family))))
                                   (when matches
                                        (throw 'fm-find-all matches)))))
                              (fm-traverse family traveller :global-search global-search)))))
        (when (and must-find (null matches))
          (c-stop :fm-find-all-must-find-failed)
          (describe family)
          (loop for h in (fm-heritage family)
                do (trcx heritage-ayway h))
          (fm-traverse family (fn (node)
                                (trc "known node" (md-name node))) :global-search global-search)
          (brk "fm-find-all > +stop+ping...did not find ~a ~a ~a" family md-name global-search)
          ;; (error 'fm-not-found (list md-name family global-search))
          )
        matches))

(defn fm-find-next (fm test-fn)
  (fm-find-next-within fm test-fn))

(defn fm-find-next-within (fm test-fn &optional upperbound &aux (fm-parent (unless (eql upperbound fm)
                                                                              (fm-parent fm))))
   (let ((sibs (and fm-parent (rest (member fm (kids fm-parent))))))
      (or (dolist (s sibs)
             (let ((winner (fm-find-if s test-fn)))
                (when winner (return winner))))
          (if fm-parent
             (fm-find-next-within fm-parent test-fn upperbound)
             (fm-find-if fm test-fn)))))

(defn fm-find-prior (fm test-fn)
  (fm-find-prior-within fm test-fn))

(defn fm-find-prior-within (fm test-fn &optional upperbound &aux (fm-parent (unless (eql upperbound fm)
                                                                              (fm-parent fm))))
  (let ((sibs (and fm-parent (kids fm-parent))))
    (or (loop with next-ok
            for s on sibs
            for last-ok = nil then (or next-ok last-ok)
            when (eql fm (first s)) do (loop-finish)
              finally (return last-ok)
            do (setf next-ok (fm-find-last-if (car s) test-fn)))
      (if fm-parent
          (fm-find-prior-within fm-parent test-fn upperbound)
        (fm-find-last-if fm test-fn)))))
  
  (defn fm-find-last-if (family test-fn)
    (let ((last))
      (or (and (kids family)
            (dolist (k (kids family) last)
              (setf last (or (fm-find-last-if k test-fn) last))))
        (when (test-fn family)
          family))))
(defn fm-psib (self test-fn)
  (some test-fn (cdr (member self (kids (fm-parent self))))))

(defn fm-prior-sib (self &optional (test-fn #'true-that))
  "Find nearest preceding sibling passing TEST-FN"
  (chk self 'psib)
  (let ((kids (kids (fm-parent self))))
    (find-if test-fn kids :end (position self kids) :from-end t)))

(defn fm-next-sib-if (self test-fn)
     (some test-fn (cdr (member self (kids (fm-parent self))))))

(defn fm-next-sib (self)
     (car (cdr (member self (kids (fm-parent self))))))

(defmacro ^fm-next-sib (&optional (self 'self))
     (let ((s (gensym)))
        `(let ((,s ,self))
             (car (cdr (member ,s (kids (fm-parent ,s))))))))

(defn find-prior (self sibs &key (test #'true-that))
  (c-assert (member self sibs) () "find-prior of ~a does not find it in sibs arg ~a" self sibs)
  (unless (eql self (car sibs))
    (labels
        ((fpsib (rsibs &aux (psib (car rsibs)))
                (c-assert rsibs () "find-prior > fpsib > self ~s not found to prior off" self)
                (if (eql self (cadr rsibs))
                   (when (test psib) psib)
                   (or (fpsib (cdr rsibs))
                       (when (test psib) psib)))))
      (fpsib sibs))))

(defn fm-find-if (family test-fn &key skip-top-p) ;; 99-03 kt why is thsi depth-first?
  (c-assert test-fn)
  (when family
    (or (dolist (b (sub-nodes family))
          (let ((match (fm-find-if b test-fn)))
             (when match (return match))))
        (when (and (not skip-top-p)
                   (test-fn family))
          family))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  family ordering
;;;;
(defn fm-kid-add (fm-parent kid &optional before)
     (c-assert (or (null (fm-parent kid)) (eql fm-parent (fm-parent kid))))
   (c-assert (typep fm-parent 'family))
     (setf (fm-parent kid) fm-parent)
     (fm-kid-insert kid before))

(defn fm-kid-insert-last (goal &aux (fm-parent (fm-parent goal)))
     (setf (kids fm-parent) (nconc (kids fm-parent) (list goal))))

(defn fm-kid-insert-first (goal &aux (fm-parent (fm-parent goal)))
     (setf (kids fm-parent) (cons goal (kids fm-parent))))

(defn fm-kid-insert (kid &optional before &aux (da-kids (kids (fm-parent kid))))
  (c-assert (or (null before) (eql (fm-parent kid) (fm-parent before))))
  (setf (kids (fm-parent kid))
          (if before
             (if (eql before (car da-kids))
                (cons kid da-kids)
                (let ((cell (member before da-kids)))
                   (rplaca cell kid)
                   (rplacd cell (cons before (cdr cell)))
                   (cons (car da-kids) (rest da-kids))))
             (if da-kids
                (progn
                  (rplacd (last da-kids) (cons kid nil))
                  (cons (car da-kids) (rest da-kids)))
                (cons kid da-kids)))))

(defn fm-kid-remove (kid &key (quiesce t) &aux (parent (fm-parent kid)))
  (when quiesce
    (fm-quiesce-all kid))
  (when parent
    (setf (kids parent) (remove kid (kids parent)))
    ;; (setf (fm-parent kid) nil) gratuitous housekeeping caused ensuing focus output
    ;; image-invalidate to fail since no access to containing window via fm-parent chain
    ))

(defn fm-quiesce-all (md)
  (md-quiesce md)
  (dolist (kid (kids md))
    (fm-quiesce-all kid)))

(defn fm-kid-replace (old-kid new-kid &aux (fm-parent (fm-parent old-kid)))
     (c-assert (member old-kid (kids fm-parent)) ()
        "~&oldkid ~s not amongst kids of its fm-parent ~s"
        old-kid fm-parent)
     (when fm-parent ;; silly test given above assert--which is right?
        (c-assert (typep fm-parent 'family))
          (setf (fm-parent new-kid) fm-parent)
          (setf (kids fm-parent) (substitute new-kid old-kid (kids fm-parent)))
          new-kid))

;----------------------------------------------------------
;;
;; h i g h  -  o r d e r   f a m i l y   o p s
;;
;; currently not in use...someday?
;;


(defn fm-min-kid (self slot-name)
  (or (loop for k in (^kids)
            minimizing (slot-name k))
    0))
(defn fm-max-kid (self slot-name)
  (or (loop for k in (^kids)
            maximizing (slot-name k))
    0))

(defmacro fm-other (md-name &key (starting 'self) skip-tree (test '#'true-that))
  `(fm-find-one ,starting ,(if (consp md-name)
                               `(list ',(car md-name) ,(cadr md-name))
                             `',md-name)
                :must-find t
                :skip-tree ,skip-tree
                :global-search t
                :test ,test))

(defmacro fmv (name)
  `(value (fm-other ,name)))

(defmacro fm-otherx (md-name &key (starting 'self) skip-tree)
   (if (eql starting 'self)
      `(or (fm-find-one ,starting ,(if (consp md-name)
                                      `(list ',(car md-name) ,(cadr md-name))
                                      `',md-name)
             :must-find t
             :skip-tree ,skip-tree
             :global-search t))
      `(fm-find-one ,starting ,(if (consp md-name)
                                  `(list ',(car md-name) ,(cadr md-name))
                                  `',md-name)
         :must-find t
         :skip-tree ,skip-tree
         :global-search t)))

(defn fm-other-v (md-name starting &optional (global-search t))
  (fm-find-one starting md-name
    :must-find nil
    :global-search global-search))

(defmacro fm-otherv? (md-name &optional (starting 'self) (global-search t))
  `(fm-other-v ,md-name ,starting ,global-search))

(defmacro fm-other? (md-name &optional (starting 'self) (global-search t))
    `(fm-find-one ,starting ,(if (consp md-name)
                                               `(list ',(car md-name) ,(cadr md-name))
                                               `',md-name)
          :must-find nil
          :global-search ,global-search))

(defn fm-other! (starting md-name &optional (global-search t))
  (fm-find-one starting md-name
    :must-find t
    :global-search global-search))

(defmacro fm^ (md-name &key (skip-tree 'self) (must-find t))
  `(without-c-dependency
    (fm-find-one (fm-parent self) ,md-name
      :skip-tree ,skip-tree
      :must-find ,must-find
      :global-search t)))

(defmacro fm^v (id)
  `(value (fm^ ,id)))

(defmacro fm^^ (scope-md-name md-name)
  (let ((scope (gensym)))
    `(let ((,scope (fm-ascendant-named self ,scope-md-name)))
       (assert ,scope () "fm^^ unable to locate scope named ~a starting at ~a" ,scope-md-name self)
       (without-c-dependency
           (fm-find-one ,scope ,md-name
             :skip-tree self
             :must-find t
             :global-search nil)))))

(defmacro fm^^v (scope-md-name md-name)
  `(value (fm^^ ,scope-md-name ,md-name)))

(export! fm^^ fm^^v)

(defmacro fm? (md-name &optional (starting 'self) (global-search t))
    `(fm-find-one ,starting ,(if (consp md-name)
                                               `(list ',(car md-name) ,(cadr md-name))
                                               `',md-name)
          :must-find nil
          :global-search ,global-search))

(defmacro fm! (md-name &optional (starting 'self))
    `(without-c-dependency 
      (fm-find-one ,starting ,(if (consp md-name)
                                  `(list ',(car md-name) ,(cadr md-name))
                                `',md-name)
        :must-find t
        :global-search nil)))

(defmacro fm!v (id)
  `(value (fm! ,id)))

(defmacro fm-other?! (md-name &optional (starting 'self))
   `(fm-find-one ,starting ,(if (consp md-name)
                                         `(list ',(car md-name) ,(cadr md-name))
                                  `',md-name)
     :must-find nil
     :global-search nil))

(defmacro fm-collect (md-name &key (must-find t))
   `(fm-find-all self ',md-name :must-find ,must-find)) ;deliberate capture

(defmacro fm-map (fn md-name)
         `(mapcar ,fn (fm-find-all self ',md-name))) ;deliberate capture

(defmacro fm-mapc (fn md-name)
   `(mapc ,fn (fm-find-all self ',md-name))) ;deliberate capture

(defn fm-pos (goal &aux (fm-parent (fm-parent goal)))
   (when fm-parent
           (or (position goal (kids fm-parent))
                               (length (kids fm-parent))))) ;; ?!!

(defmacro fm-count-named (family md-name &key (global-search t))
    `(length (fm-find-all ,family ,md-name
                 :must-find nil
                 :global-search ,global-search)))

(defn fm-count (fm &key (test 'identity) &aux (ct 0))
  (fm-traverse fm (fn (md)
                    (trx counting md)
                    (when (test md)
                      (incf ct))))
  ct)

;---------------------------------------------------------------
(defn fm-top (fm &optional (test #'true-that) &aux (fm-parent (fm-parent fm)))
    (cond ((null fm-parent) fm)
                ((not (test fm-parent)) fm)
                (t (fm-top fm-parent test))))

(defn fm-first-above (fm &key (test #'true-that) &aux (fm-parent (fm-parent fm)))
    (cond ((null fm-parent) nil)
              ((test fm-parent) fm-parent)
              (t (fm-first-above fm-parent :test test))))

(defn fm-nearest-if (test fm)
  (when fm
    (if (test fm)
       fm
       (fm-nearest-if test (fm-parent fm)))))

(defn fm-ancestry-do (fm fn &optional (start t))
  (when start (trcx fm-ancestry-do-sees-fm fm))
  (when fm
    (fn fm)
    (fm-ancestry-do (fm-parent fm) fn nil)))

(defn fm-includes (fm sought)
  (fm-ancestor-p fm sought))

(defn fm-ancestor-p (fm sought)
   (c-assert fm)
   (when sought
      (or (eql fm sought)
          (fm-includes fm (fm-parent sought)))))

(defn fm-kid-containing (fm-parent descendant)
   (with-dynamic-fn (finder (node) (not (eql fm-parent node)))
     (fm-top descendant finder)))

;;; above looks confused, let's try again

(defn fm-ascendant-p (older younger)
  (cond
   ((null (fm-parent younger)) nil)
   ((eq older (fm-parent younger)) t)
   (t (fm-ascendant-p older (fm-parent younger)))))

(defn make-name (root &optional subscript)
   (if subscript (list root subscript) root))

(defn name-root (md-name)
   (if (atom md-name) md-name (car md-name)))

(defn name-subscript (md-name)
   (when (consp md-name) (cadr md-name)))

(defn fm-find-one (family md-name &key (must-find t)
                     (global-search t) skip-tree (test #'true-that)
                     &aux diag)
  (count-it :fm-find-one)
  (flet ((matcher (fm)
           (when diag
             (trc
              "fm-find-one matcher sees name" (md-name fm) :ofthing (type-of fm) :seeking md-name global-search))
           (when (and (eql (name-root md-name)(md-name fm))
                   (or (null (name-subscript md-name))
                     (eql (name-subscript md-name) (fm-pos fm)))
                   (progn
                     (when diag
                       (trc "fm-find-one testing" fm))
                     (test fm)))
             (throw 'fm-find-one fm))))
    
    (trc nil "fm-find-one> entry " md-name family)    
    (let ((match (catch 'fm-find-one
                   (fm-traverse family #'matcher
                     :skip-tree skip-tree
                     :global-search global-search))))
      (when (and must-find (null match))
        (trc "fm-find-one > erroring fm-not-found, in family: " family :seeking md-name :global? global-search)
        #+shhhh
        (progn
          (describe family)
        
          (loop for h in (fm-heritage family)
              do (trcx heritage-anyway h))
          (setq diag t must-find nil)
          (fm-traverse family #'matcher
            :skip-tree skip-tree
            :global-search global-search))
        (c-break "fm-find-one > +stop+ping...did not find ~a ~a ~a" family md-name global-search)
        )
      match)))

(defn fm-find-kid (self name)
   (cl/find name (kids self) :key #'md-name))

(defn fm-kid-typed (self type)
   (c-assert self)
  (cl/find type (kids self) :key #'type-of))

(defn kid-no (self)
  (unless (typep self 'model-object)
    (brk "not a model object ~a" self))
  (when (and self (fm-parent self))
    (unless (member self (kids (fm-parent self)))
      (c-break "kid-no self ~a not member of kids ~a of parent ~a"
        self (kids .pa) .pa))
    (position self (kids (fm-parent self)))))
