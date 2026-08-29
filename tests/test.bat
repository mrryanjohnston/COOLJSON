;(watch rules)
(load* compline.clp)
(load* ../program.clp)

;;; Runs every case file, reporting only the failures plus a one line summary.
(deffunction run-tests (?files)
	(bind ?failed 0)
	(foreach ?file ?files
		(reset)
		(open actual actual "w")
		(open (sym-cat cases/ ?file) ?file)
		(assert (filename ?file))
		(run)
		(bind ?found FALSE)
		(do-for-fact ((?f element)) TRUE
			(bind ?found TRUE)
			(send ?f:value pprint actual))
		(close actual)
		(close ?file)
		(if (not ?found)
		    then (bind ?failed (+ ?failed 1))
		         (println "FAIL " ?file ": did not find element fact")
		    else (bind ?differences (compare-files (sym-cat expected/ ?file) actual))
		         (if (> (length$ ?differences) 0)
		             then (bind ?failed (+ ?failed 1))
		                  (println "FAIL " ?file)
		                  (foreach ?line ?differences (println ?line)))))
	(println (length$ ?files) " tests, " (- (length$ ?files) ?failed) " passed, "
	         ?failed " failed"))

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
))
(exit)
