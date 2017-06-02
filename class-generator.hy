(defmacro make-vars [data]
  (setv res '())
  (for [element data]
    (setv varname (HySymbol (+ "var" (str element))))
    (setv res (cons `(setv ~varname 0) res)))
  `(do ~@res))


(defn collect-factions [unit-classes]
  (setv res (set))
  (for [uc unit-classes]
    (for [fac (. uc faction)]
      (.add res fac)))
  (list res))

(defn filter-unit-classes [unit-classes faction]
  (list-comp uc [uc unit-classes] (.check_faction uc faction)))

(import re [string [capwords]])

(defn fix-faction-string [fname]
  (re.sub "[^0-9a-zA-Z]+" "_" (.lower fname)))

(defn fix-faction-name [fname]
  (capwords (.lower fname)))

(defmacro skip [&body]
  '(do))

(import [hy [HyString HyDict HyInteger]])

(defn make-faction-detaches [faction metadata unit-types]
  (let [meta-base (get metadata "Base")
        meta-pattern (get metadata "Sections")
        class-cand []
        class-def '()
        class-grouping (dict)]
    (for [(, sec-name sec-flag) (.iteritems meta-pattern)]
      ;; if section flag is set but no unit types with the section are found, break and return nothing
      (print "checking" sec-name)
      (if-not (or (not sec-flag) (any (genexpr (in sec-name (. ut roles)) [ut unit-types])))
              (break)
              ;; save unit types for section 
              (do
               (print "match for section" sec-name)
               (setv sec-grouping (list-comp ut [ut unit-types]
                                             (in sec-name (. ut roles))))
               (print (len sec-grouping) "types found for section" sec-name)
               (when sec-grouping
                 (assoc class-grouping sec-name sec-grouping))))
      ;; in case we finished the cycle
      (else
       (do
        (def
          class-name (.format "{}_{}" (. meta-base __name__) (fix-faction-string faction))
          army-id (.format "{}_{}" (. meta-base army_id) (fix-faction-string faction))
          army-name (.format "{} ({})" (fix-faction-name faction) (. meta-base army_name)))
         (print "Class name is" class-name)
         (print "Army id is" army-id)
         (print "Army name is" army-name)
         (setv class-cand [(HySymbol class-name)])
         (setv class-def [`(defclass ~(HySymbol class-name) [~(HySymbol (. meta-base __name__))]
                            [army_name ~(HyString army-name)
                             faction ~(HyString faction)
                             army_id ~(HyString army-id)]
                             (defn --init-- [self]
                               (.--init-- (super) ~(HyDict (interleave (genexpr (HyString k) [k class-grouping])
                                                                       (cycle [(HyInteger 1)]))))
                               ~@(map (fn [key]
                                        `(.add-classes (. self ~(HySymbol key))
                                                       ~(HyList (genexpr (HySymbol (. ut __name__))
                                                                         [ut (get class-grouping key)]))))
                                      class-grouping)))]))))
    (, class-def class-cand)))

(defn make-detach-classes [unit-classes detach-metadata]
  (let [factions (collect-factions unit-classes)
        detach-defs '()
        detach-class-list []]
    (for [faction factions]
      (print "For faction" faction)
      (with-gensyms [defs classes]
        (for [single-meta detach-metadata]
           (setv (, defs classes) (make-faction-detaches faction single-meta (filter-unit-classes unit-classes faction)))
          (.extend detach-defs defs)
          (.extend detach-class-list classes))))
    `(do
      ~@detach-defs
      (setv detachments  ~(HyList detach-class-list)))))

;; prepare env
(do
 (import os)
 (os.chdir "d:/hq-git")
 (import [builder.games.wh40k8ed.adepta_sororitas [unit_types :as sister_types]])
 (import [builder.games.wh40k8ed.necrons [unit_types :as necron_types]])
 (import [builder.games.wh40k8ed.rosters [detach_metadata]]))
