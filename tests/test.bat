;;; Runs every case under every conflict resolution strategy CLIPS offers.  A
;;; COOL codec keeps only one rule on the agenda at a time, so it decodes the
;;; same way under each strategy; a case that fails under one strategy and not
;;; another points at a rule that relies on firing order.  All the cases
;;; decode in one run, so a strategy like breadth advances every message a
;;; character at a time in lockstep: a rule whose patterns are not all tied to
;;; one message then matches facts from two messages at once, which decoding
;;; one case at a time cannot show.
;;;
;;; A test*.json case holds one complete value.  The codec makes a JSON-TEXT
;;; for it.  An eof*.json case stops before its value is complete.  The codec
;;; asserts json-eof for it and makes no JSON-TEXT.
;(watch rules)
;(watch facts)
;(watch activations)
;(watch statistics)
(load* compline.clp)
(load* ../program.clp)

(defglobal ?*strategies* = (create$ depth breadth lex mea complexity simplicity random))
;;; the limit of rules that one run fires.  The cases need about a sixth of
;;; it.  A rule set that does not empty its agenda fails and does not hang.
(defglobal ?*max-rules* = 100000)

;;; Compares what the codec decoded for one case with the file of the same
;;; name in expected.  Prints the failure, if any, and returns TRUE when the
;;; case passed.
(deffunction check-case (?strategy ?file)
	(open actual actual "w")
	(bind ?found FALSE)
	(do-for-instance ((?j JSON-TEXT)) (eq ?j:id ?file)
		(bind ?found TRUE)
		(send ?j pprint actual))
	(close actual)
	(if (not ?found) then
		(println "FAIL " ?file " (" ?strategy "): did not find the JSON-TEXT instance")
		(return FALSE))
	(bind ?differences (compare-files (sym-cat expected/ ?file) actual))
	(if (> (length$ ?differences) 0) then
		(println "FAIL " ?file " (" ?strategy ")")
		(foreach ?line ?differences (println ?line))
		(return FALSE))
	TRUE)

;;; Checks one eof case.  The codec must assert one json-eof fact for it,
;;; make no JSON-TEXT and keep its json-stream fact: the end of the stream
;;; changes no other fact.  Prints the failure, if any, and returns TRUE
;;; when the case passed.
(deffunction check-eof-case (?strategy ?file)
	(bind ?ends (length$ (find-all-facts ((?e json-eof)) (eq ?e:id ?file))))
	(if (<> ?ends 1) then
		(println "FAIL " ?file " (" ?strategy "): found " ?ends " json-eof facts, 1 expected")
		(return FALSE))
	(if (any-instancep ((?j JSON-TEXT)) (eq ?j:id ?file)) then
		(println "FAIL " ?file " (" ?strategy "): found a JSON-TEXT instance")
		(return FALSE))
	(if (not (any-factp ((?s json-stream)) (eq ?s:id ?file))) then
		(println "FAIL " ?file " (" ?strategy "): the codec did not keep the json-stream fact")
		(return FALSE))
	TRUE)

;;; Decodes every case at once under one strategy, then checks each.  Returns
;;; the number of checks that failed.
(deffunction run-strategy (?strategy ?files ?eof-files)
	(set-strategy ?strategy)
	(reset)
	(foreach ?file (create$ ?files ?eof-files)
		(open (sym-cat cases/ ?file) ?file)
		(assert (json-parse (stream ?file) (id ?file)) (json-ready (id ?file))))
	(run ?*max-rules*)
	(bind ?failed 0)
	(foreach ?file ?files
		(if (not (check-case ?strategy ?file)) then
			(bind ?failed (+ ?failed 1))))
	(foreach ?file ?eof-files
		(if (not (check-eof-case ?strategy ?file)) then
			(bind ?failed (+ ?failed 1))))
	(foreach ?file (create$ ?files ?eof-files)
		(close ?file))
	?failed)

;;; Runs every strategy, reporting only the failures plus a one line summary.
(deffunction run-tests (?files ?eof-files)
	(bind ?failed 0)
	(foreach ?strategy ?*strategies*
		(bind ?failed (+ ?failed (run-strategy ?strategy ?files ?eof-files))))
	(bind ?cases (+ (length$ ?files) (length$ ?eof-files)))
	(bind ?total (* ?cases (length$ ?*strategies*)))
	(println ?cases " cases x " (length$ ?*strategies*) " strategies = "
	         ?total " tests, " (- ?total ?failed) " passed, " ?failed " failed")
	(if (> ?failed 0) then (exit 1) else (exit 0)))

(run-tests (create$
test.json
test2.json
test3.json
test4.json
test5.json
test6.json
test7.json
test8.json
test9.json
test10.json
test11.json
test12.json
test13.json
test14.json
test15.json
test16.json
test17.json
test18.json
test19.json
test20.json
test21.json
test22.json
test23.json
test24.json
test25.json
test26.json
test27.json
test28.json
) (create$
eof.json
eof2.json
eof3.json
eof4.json
eof5.json
eof6.json
eof7.json
eof8.json
eof9.json
eof10.json
eof11.json
eof12.json
eof13.json
eof14.json
eof15.json
eof16.json
eof17.json
eof18.json
))
