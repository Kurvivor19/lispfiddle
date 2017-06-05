(defmacro make-vars [data]
  (setv res '())
  (for [element data]
    (setv varname (HySymbol (+ "var" (str element))))
    (setv res (cons `(setv ~varname 0) res)))
  `(do ~@res))

(defn collect-factions [unit-classes]
  (setv res (set))
  (for [uc unit-classes]
    ;; debug print
    (if-not (. uc faction) (print (. uc __name__) "has no faction"))
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
        class-grouping (dict)
        army-factions None]
    (print "For detachment" (. meta-base __name__))
    (for [(, sec-name sec-flag) (.iteritems meta-pattern)]
      ;; if section flag is set but no unit types with the section are found, break and return nothing
      ; (print "checking" sec-name)
      (if-not (or (not sec-flag) (any (genexpr (in sec-name (. ut roles)) [ut unit-types])))
              (break)
              ;; save unit types for section 
              (do
               ; (print "match for section" sec-name)
               (setv sec-grouping (list-comp ut [ut unit-types]
                                             (in sec-name (. ut roles))))
               ; (print (len sec-grouping) "types found for section" sec-name)
               (when sec-grouping
                 (assoc class-grouping sec-name sec-grouping)
                 (when sec-flag
                   (print "Army factins at point" army-factions)
                   (if army-factions
                     (setv army-factions (.intersection army-factions
                                                        (set (apply chain (genexpr (+ (. ut.faction)
                                                                                      (.calc-faction ut))
                                                                                   [ut sec-grouping])))))
                     (setv army-factions (set (apply chain (genexpr (+ (. ut.faction)
                                                                       (.calc-faction ut))
                                                                    [ut sec-grouping])))))))))
      ;; in case we finished the cycle
      (else
       (do
        (def
          class-name (HySymbol (.format "{}_{}" (. meta-base __name__) (fix-faction-string faction)))
          army-id (HyString (.format "{}_{}" (. meta-base army_id) (fix-faction-string faction)))
          army-name (HyString (.format "{} ({})" (fix-faction-name faction) (. meta-base army_name))))
        (unless army-factions
          (setv army-factions [faction]))
        ; (print "Class name: " class-name "Army id: " army-id "Army name: " army-name)
        (setv class-cand [class-name])
        (setv class-def [`(defclass ~class-name [~(HySymbol (. meta-base __name__))]
                            [army_name ~army-name
                             faction ~(HyString faction)
                             army_id ~army-id
                             army-factions [~@(map HyString army-factions)]]
                            (defn --init-- [self]
                              (.--init-- (super) {~@(interleave (map HyString class-grouping)
                                                                (repeat 'True))})
                              ~@(map (fn [key]
                                       `(.add-classes (. self ~(HySymbol key))
                                                      [~@(genexpr (HySymbol (. ut __name__))
                                                                  [ut (get class-grouping key)])]))
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
          (setv (, defs classes) (make-faction-detaches faction single-meta
                                                        (filter-unit-classes unit-classes faction)))
          (.extend detach-defs defs)
          (.extend detach-class-list classes))))
    `(do
      ~@detach-defs
      (setv detachments [~@detach-class-list]))))

(defn make-army-classes [factions detachment-list]
  (let [army-defs '()
        army-class-list []]
    (for [faction factions]
      (print "Army for faction" faction)
      (def
        class-name (HySymbol (.format "{}8ed" (fix-faction-name (fix-faction-string faction))))
        army-name (HyString (.format "Army of {}" (fix-faction-name faction)))
        army-id (HyString (fix-faction-string faction)))
      (.append army-defs
               `(defclass ~class-name [Wh40k8edBase]
                  [army_name ~army-name
                   faction ~(HyString faction)
                   army_id ~army-id]
                  (defn --init-- [self]
                    (.--init-- (super))
                    (for [det [~@(list-comp (HySymbol (. dt __name__))
                                            [dt detachment-list]
                                            (in faction (. dt army-factions)))]]
                      (.build_detach (. self det) det ~(HyString faction)
                                     :group (. det faction))))))
      (.append army-class-list class-name))
    `(do
      ~@army-defs
      (setv armies [~@army-class-list]))))

;; perform code generation
(do
 (import os)
 (os.chdir "d:/hq-git")
 (import builder.games.wh40k8ed.gen_detachments)
 (import [builder.games.wh40k8ed.gen_detachments [all_unit_types]])
 (import [builder.games.wh40k8ed.rosters [detach_metadata]])
 (setv code (make-detach-classes all_unit_types detach-metadata))
 (with [fd (open "builder/games/wh40k8ed/gen_detachments.py" "a")]
       (.write fd (disassemble code True)))
 (reload builder.games.wh40k8ed.gen_detachments)
 (import [builder.games.wh40k8ed.gen_detachments [detachments]])
 (setv code (make-army-classes (collect-factions all_unit_types) detachments))
 (with [fd (open "builder/games/wh40k8ed/gen_detachments.py" "a")]
       (.write fd (disassemble code True))))
