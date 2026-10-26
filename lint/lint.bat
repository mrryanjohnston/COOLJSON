;;; Lints the rules of the codec against the practices a COOL codec follows:
;;;
;;;   1. only one rule is activated at a time while one example is decoded
;;;   2. many examples decode at once, each to the same result as alone
;;;   3. the right hand side of a rule holds no (if ...)
;;;
;;; Prints one line per violation and a summary, then exits with 1 when it
;;; found a violation and 0 when it found none.
(load* ../tests/compline.clp)
(load* ../program.clp)

;;; the count survives the reset before each run
(set-reset-globals FALSE)
(defglobal ?*violations* = 0)
;;; guards against a rule set that never empties its agenda
(defglobal ?*max-steps* = 100000)

(deffunction violation (?message)
	(bind ?*violations* (+ ?*violations* 1))
	(println "LINT " ?message))

;;; the cases in tests/cases: test.json, test2.json, test3.json ... up to the
;;; first number that has no file
(deffunction find-cases ()
	(bind ?cases (create$))
	(bind ?n 1)
	(bind ?file test.json)
	(while (open (sym-cat ../tests/cases/ ?file) probe) do
		(close probe)
		(bind ?cases (create$ ?cases ?file))
		(bind ?n (+ ?n 1))
		(bind ?file (sym-cat test ?n .json)))
	?cases)

(deffunction start-case (?file)
	(open (sym-cat ../tests/cases/ ?file) ?file)
	(assert (json-parse (stream ?file) (id ?file)) (json-ready (id ?file))))

;;; the name of every rule on the agenda, once per activation
(deffunction activations ()
	(bind ?active (create$))
	(foreach ?rule (get-defrule-list)
		(loop-for-count (nth$ 3 (matches ?rule terse))
			(bind ?active (create$ ?active ?rule))))
	?active)

;;; 1. decodes one case a rule at a time and checks the agenda after every
;;; firing.  Returns the number of rules that fired.
(deffunction lint-one-activation (?file)
	(reset)
	(start-case ?file)
	(bind ?fired 0)
	(bind ?offending-steps 0)
	(bind ?active (activations))
	(while (and (> (length$ ?active) 0) (< ?fired ?*max-steps*)) do
		(if (> (length$ ?active) 1) then
			(bind ?offending-steps (+ ?offending-steps 1))
			(if (= ?offending-steps 1) then
				(violation (format nil "%s: %d activations after %d rules fired: %s"
				                   ?file (length$ ?active) ?fired (implode$ ?active)))))
		(run 1)
		(bind ?fired (+ ?fired 1))
		(bind ?active (activations)))
	(close ?file)
	(if (> ?offending-steps 1) then
		(println "     " ?file ": " ?offending-steps " steps had more than one activation"))
	(if (>= ?fired ?*max-steps*) then
		(violation (format nil "%s: the agenda was not empty after %d rules fired" ?file ?fired)))
	?fired)

;;; 2. decodes every case in one run, then checks that each one printed
;;; what it prints alone and that the run fired as many rules as the cases
;;; fired alone put together.
(deffunction lint-many-at-once (?cases ?fired-alone)
	(reset)
	(foreach ?file ?cases (start-case ?file))
	(bind ?fired 0)
	(while (and (> (length$ (activations)) 0) (< ?fired ?*max-steps*)) do
		(run 1)
		(bind ?fired (+ ?fired 1)))
	(if (<> ?fired ?fired-alone) then
		(violation (format nil "%d rules fired decoding the cases together, %d fired decoding them alone"
		                   ?fired ?fired-alone)))
	(foreach ?file ?cases
		(bind ?done (find-all-facts ((?f json-done)) (eq ?f:id ?file)))
		(if (<> (length$ ?done) 1)
		    then (violation (format nil "%s: found %d json-done facts decoding the cases together"
		                            ?file (length$ ?done)))
		    else (open actual actual "w")
		         (send (fact-slot-value (nth$ 1 ?done) element) pprint actual)
		         (close actual)
		         (bind ?differences (compare-files (sym-cat ../tests/expected/ ?file) actual))
		         (if (> (length$ ?differences) 0) then
		             (violation (format nil "%s: decoded differently together with the other cases" ?file))
		             (foreach ?line ?differences (println ?line))))
		(close ?file)))

;;; 3. the text of the rule from => on holds no (if
(deffunction lint-rhs (?rule)
	(bind ?text (ppdefrule ?rule nil))
	(bind ?rhs (sub-string (str-index "=>" ?text) (str-length ?text) ?text))
	(if (or (str-index "(if " ?rhs) (str-index (format nil "(if%n") ?rhs)) then
		(violation (format nil "%s: (if in the right hand side" ?rule))))

(deffunction lint ()
	(bind ?cases (find-cases))
	(bind ?fired-alone 0)
	(foreach ?file ?cases
		(bind ?fired-alone (+ ?fired-alone (lint-one-activation ?file))))
	(lint-many-at-once ?cases ?fired-alone)
	(foreach ?rule (get-defrule-list) (lint-rhs ?rule))
	(println (length$ (get-defrule-list)) " rules, " (length$ ?cases) " cases, "
	         ?*violations* " violations")
	(if (> ?*violations* 0) then (exit 1) else (exit 0)))

(lint)
