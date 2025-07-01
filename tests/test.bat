;(watch rules)
(load compline.clp)
(load ../program.clp)

(foreach ?file (create$
test.json
test2.json
test3.json
test4.json
test5.json
test6.json
test7.json
)
	(reset)
	(open actual actual "w")
	(open (sym-cat cases/ ?file) ?file)
	(assert (filename ?file))
	(run)
	(bind ?found FALSE)
	(do-for-fact ((?f element)) TRUE
		(bind ?found TRUE)
		(send ?f:value pprint actual))
	(if ?found
	    then (println "Comparing " ?file "...")
	         (close actual)
	         (close ?file)
	         (compare-files (sym-cat expected/ ?file) actual t)
	    else (close actual)
	         (println "Failure: did not find element fact for " ?file))
)
(exit)
