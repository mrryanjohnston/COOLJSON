; i cannot stop
; element
; ws value ws
;
; member
; ws string ws ':' element
(defclass JSON (is-a USER))
(defclass JSON-NUMBER (is-a JSON) (slot value))
(defclass JSON-STRING (is-a JSON) (slot value))
(defclass JSON-ARRAY (is-a JSON) (multislot values))
(defclass JSON-OBJECT (is-a JSON) (multislot members))
(defclass JSON-MEMBER (is-a JSON) (slot key) (slot value))
(defclass JSON-FALSE (is-a JSON))
(defclass JSON-NULL (is-a JSON))
(defclass JSON-TRUE (is-a JSON))
(deftemplate string
	(slot value))
(deftemplate number
	(multislot value))
(deftemplate array
	(multislot values))
(deftemplate obj
	(multislot members))
(deftemplate element
	(slot value (default nil)))
(deftemplate member
	(slot key)
	(slot value (default nil)))
(deftemplate false-null-true
	(multislot values))

(defrule start-parsing
	(filename ?name)
	=>
	(assert (element)))

(defrule pop-char
	(filename ?name)
	(not (popped-char ?name ?))
	=>
	(assert (popped-char ?name (get-char ?name))))

(defrule ignore-whitespace
	(filename ?name)
	?p <- (popped-char ?name 9|10|13|32)
	(or
		(element (value nil))
		(obj (members $?members&:(= 0 (length$ ?members))|:(instancep (nth$ (length$ ?members) ?members)))))
	=>
	(retract ?p))

(defrule init-element-for-empty-array
	(filename ?name)
	;(popped-char ?name ~9&~10&~13&~32)
	?a <- (array (values))
	(not (element (value nil)))
	=>
	(modify ?a (values (assert (element)))))

(defrule init-element-for-empty-obj
	(filename ?name)
	(popped-char ?name 34)
	?o <- (obj (members))
	(not (element (value nil)))
	=>
	(modify ?o (members (assert (member (key (assert (element))))))))

(defrule start-array
	(filename ?name)
	?p <- (popped-char ?name 91) ; [
	?e <- (element (value nil))
	(not (member (key ?e)))
	=>
	(retract ?p)
	(modify ?e (value (assert (array)))))

(defrule array-unwrap-element
	(filename ?name)
	?e <- (element (value ?obj&:(instancep ?obj)))
	?a <- (array (values $?values ?e))
	=>
	(retract ?e)
	(modify ?a (values ?values ?obj)))

(defrule member-value-unwrap-element
	(filename ?name)
	?e <- (element (value ?obj&:(instancep ?obj)))
	?m <- (member (value ?e))
	=>
	(retract ?e)
	(modify ?m (value ?obj)))

(defrule array-find-comma-next-element
	(filename ?name)
	?p <- (popped-char ?name 44) ; ,
	?a <- (array (values $?values&:(= 0 (length$ ?values))|:(instancep (nth$ (length$ ?values) ?values))))
	=>
	(retract ?p)
	(modify ?a (values ?values (assert (element)))))

(defrule end-array
	(filename ?name)
	?p <- (popped-char ?name 93) ; ]
	?a <- (array (values $?values ?value&:(instancep ?value)))
	?e <- (element (value ?a))
	=>
	(retract ?p ?a)
	(modify ?e (value (make-instance of JSON-ARRAY (values ?values ?value)))))

(defrule end-empty-array
	(filename ?name)
	?ee <- (element (value nil))
	?p <- (popped-char ?name 93) ; ]
	?a <- (array (values ?ee))
	?e <- (element (value ?a))
	=>
	(retract ?ee ?p ?a)
	(modify ?e (value (make-instance of JSON-ARRAY))))

(defrule start-object
	(filename ?name)
	?p <- (popped-char ?name 123) ; {
	?e <- (element (value nil))
	(not (member (key ?e)))
	=>
	(retract ?p)
	(modify ?e (value (assert (obj)))))

(defrule end-member-key
	(filename ?name)
	?s <- (object (is-a JSON-STRING))
	?e <- (element (value =(instance-name ?s)))
	?m <- (member (key ?e))
	=>
	(retract ?e)
	(modify ?m (key (instance-name ?s))))

(defrule find-colon-member
	(filename ?name)
	?p <- (popped-char ?name 58) ; :
	?m <- (member (key ?key&:(instancep ?key)) (value nil))
	=>
	(retract ?p)
	(modify ?m (value (assert (element)))))

(defrule find-comma-next-member
	(filename ?name)
	?p <- (popped-char ?name 44) ; ,
	?o <- (obj (members $?members&:(= 0 (length$ ?members))|:(instancep (nth$ (length$ ?members) ?members))))
	=>
	(retract ?p)
	(modify ?o (members ?members (assert (member (key (assert (element))))))))

(defrule end-object
	(filename ?name)
	?p <- (popped-char ?name 125) ; }
	?o <- (obj (members $?members&:(= 0 (length$ ?members))|:(instancep (nth$ (length$ ?members) ?members))))
	?e <- (element (value ?o))
	=>
	(retract ?p ?o)
	(modify ?e (value (make-instance of JSON-OBJECT (members ?members)))))

(defrule start-string
	(filename ?name)
	?p <- (popped-char ?name 34) ; "
	?e <- (element (value nil))
	=>
	(retract ?p)
	(modify ?e (value (assert (string (value ""))))))

(defrule continue-string
	(filename ?name)
	?p <- (popped-char ?name ?char&~34&~92) ; " \
	?s <- (string (value ?str))
	(not (escape))
	=>
	(retract ?p)
	(modify ?s (value (format nil "%s%c" ?str ?char))))

(defrule end-string
	(filename ?name)
	?p <- (popped-char ?name 34) ; "
	?s <- (string (value ?str))
	?e <- (element (value ?s))
	(not (escape))
	=>
	(retract ?p ?s)
	(modify ?e (value (make-instance of JSON-STRING (value ?str)))))

(defrule begin-string-escape
	(filename ?name)
	?p <- (popped-char ?name 92) ; \
	?s <- (string (value ?str))
	(not (escape))
	=>
	(retract ?p)
	(modify ?s (value (format nil "%s%c" ?str 92)))
	(assert (escape)))

(defrule begin-string-escape-hexadecimal
	(filename ?name)
	?p <- (popped-char ?name 117) ; u
	?s <- (string (value ?str))
	(escape)
	(not (hexadecimal))
	=>
	(retract ?p)
	(modify ?s (value (format nil "%s%c" ?str 117)))
	(assert (hexadecimal)))

(defrule end-string-escape
	(filename ?name)
	?p <- (popped-char ?name ?char&~117) ; u
	?s <- (string (value ?str))
	?e <- (escape)
	(not (hexadecimal))
	=>
	(retract ?p ?e)
	(modify ?s (value (format nil "%s%c" ?str ?char))))

(defrule continue-string-escape-hexadecimal
	(filename ?name)
	?p <- (popped-char ?name ?char&48|49|50|51|52|53|54|55|56|57|97|98|99|100|101|102|65|66|67|68|69|70) ; 0-9 a-f A-F
	?s <- (string (value ?str))
	(escape)
	?h <- (hexadecimal $?hex&:(> 3 (length$ ?hex)))
	=>
	(retract ?p ?h)
	(modify ?s (value (format nil "%s%c" ?str ?char)))
	(assert (hexadecimal $?hex ?char)))

(defrule end-string-escape-hexadecimal
	(filename ?name)
	?p <- (popped-char ?name ?char&48|49|50|51|52|53|54|55|56|57|97|98|99|100|101|102|65|66|67|68|69|70) ; 0-9 a-f A-F
	?s <- (string (value ?str))
	?e <- (escape)
	?h <- (hexadecimal $?hex&:(= 4 (length$ ?hex)))
	=>
	(retract ?p ?e ?h)
	(modify ?s (value (format nil "%s%c" ?str ?char))))

(defrule start-number
	(filename ?name)
	?p <- (popped-char ?name ?char&45|48|49|50|51|52|53|54|55|56|57) ; - 0-9
	?e <- (element (value nil))
	(not (member (key ?e)))
	=>
	(retract ?p)
	(modify ?e (value (assert (number (value (format nil "%c" ?char)))))))

(defrule illegal-zero
	(filename ?name)
	(popped-char ?name ?char&~46|~69|~101|~-1) ; . E e EOF
	(number (value ?number&"0"|"-0"))
	=>
	(format t "ILLEGAL CHAR: %c found after %s" ?char ?number))

(defrule illegal-number
	(filename ?name)
	(popped-char ?name ?char&~46&~48&~49&~50&~51&~52&~53&~54&~55&~56&~57&~69&~101&~-1) ; . 0-9 E e EOF
	?n <- (number)
	?e <- (element (value ?n))
	(not (member (value ?e)))
	(not (array (values $? ?e)))
	=>
	(format t "ILLEGAL CHAR: %c found when parsing number" ?char))

(defrule continue-number
	(filename ?name)
	?p <- (popped-char ?name ?char&48|49|50|51|52|53|54|55|56|57) ; 0-9
	?n <- (number (value ?number))
	=>
	(retract ?p)
	(modify ?n (value (format nil "%s%c" ?number ?char))))

(defrule start-fraction
	(filename ?name)
	?p <- (popped-char ?name 46) ; .
	?n <- (number (value ?number))
	=>
	(retract ?p)
	(modify ?n (value ?number "")))

(defrule continue-fraction
	(filename ?name)
	?p <- (popped-char ?name ?char&48|49|50|51|52|53|54|55|56|57) ; 0-9
	?n <- (number (value ?number ?fraction))
	=>
	(retract ?p)
	(modify ?n (value ?number (format nil "%s%c" ?fraction ?char))))

(defrule illegal-fraction
	(filename ?name)
	(popped-char ?name ?char&~48&~49&~50&~51&~52&~53&~54&~55&~56&~57&~69&~101&~-1) ; 0-9 E e EOF
	?n <- (number (value ? ?))
	?e <- (element (value ?n))
	(not (member (value ?e)))
	(not (array (values $? ?e)))
	=>
	(format t "ILLEGAL CHAR: %c found when parsing fraction" ?char))

(defrule start-exponent
	(filename ?name)
	?p <- (popped-char ?name 69|101) ; E e
	?n <- (number (value ?number ?fraction))
	=>
	(retract ?p)
	(modify ?n (value ?number ?fraction "")))

(defrule find-exponent-sign
	(filename ?name)
	?p <- (popped-char ?name ?char&43|45) ; + -
	?n <- (number (value ?number ?fraction ""))
	=>
	(retract ?p)
	(modify ?n (value ?number ?fraction (format nil "%c" ?char) "")))

(defrule illegal-exponent-sign
	(filename ?name)
	(popped-char ?name ?char&~43|~45) ; 0-9 EOF
	(number (value ? ? ""))
	=>
	(format t "ILLEGAL CHAR: %c found when parsing exponent sign" ?char))

(defrule continue-exponent
	(filename ?name)
	?p <- (popped-char ?name ?char&48|49|50|51|52|53|54|55|56|57) ; 0-9
	?n <- (number (value ?number ?fraction ?sign ?exponent))
	=>
	(retract ?p)
	(modify ?n (value ?number ?fraction ?sign (format nil "%s%c" ?exponent ?char))))

(defrule illegal-exponent
	(filename ?name)
	(popped-char ?name ?char&~48&~49&~50&~51&~52&~53&~54&~55&~56&~57&~-1) ; 0-9 EOF
	(number (value ? ? ? ?))
	=>
	(format t "ILLEGAL CHAR: %c found when parsing exponent" ?char))

(defrule end-member
	(filename ?name)
	?m <- (member (key ?key) (value ?value&:(instancep ?value)))
	?o <- (obj (members $?members ?m))
	=>
	(retract ?m)
	(modify ?o (members ?members (make-instance of JSON-MEMBER (key ?key) (value ?value)))))

(defrule end-number
	(filename ?name)
	?n <- (number (value ?number))
	?e <- (element (value ?n))
	(or
		(popped-char ?name EOF)
		(popped-char ?name 9|10|13|32) ; whitespace
		(and
			(popped-char ?name 44|93) ; , ]
			(array (values $? ?e)))
		(and
			(popped-char ?name 44|125) ; , }
			?m <- (member (value ?e))
			(obj (members $? ?m))))
	=>
	(retract ?n)
	(modify ?e (value (make-instance of JSON-NUMBER (value (string-to-field ?number))))))

(defrule end-number-fraction
	(filename ?name)
	?n <- (number (value ?number ?fraction))
	?e <- (element (value ?n))
	(or
		(popped-char ?name EOF)
		(popped-char ?name 9|10|13|32) ; whitespace
		(and
			(popped-char ?name 44|93) ; , ]
			(array (values $? ?e)))
		(and
			(popped-char ?name 44|125) ; , }
			?m <- (member (value ?e))
			(obj (members $? ?m))))
	=>
	(retract ?n)
	(modify ?e (value (make-instance of JSON-NUMBER (value (string-to-field (format nil "%s.%s" ?number ?fraction)))))))

(defrule end-number-exponent
	(filename ?name)
	?n <- (number (value ?number ?fraction ?sign ?exponent))
	?e <- (element (value ?n))
	(or
		(popped-char ?name EOF)
		(popped-char ?name 9|10|13|32) ; whitespace
		(and
			(popped-char ?name 44|93) ; , ]
			(array (values $? ?e)))
		(and
			(popped-char ?name 44|125) ; , }
			?m <- (member (value ?e))
			(obj (members $? ?m))))
	=>
	(retract ?n)
	(modify ?e (value (make-instance of JSON-NUMBER (value (format nil "%s.%se%c%s" ?number ?fraction ?sign ?exponent))))))

(defrule start-false-null-true
	(filename ?name)
	?p <- (popped-char ?name ?char&102|110|116) ; f n t
	?e <- (element (value nil))
	(not (member (key ?e)))
	=>
	(retract ?p)
	(modify ?e (value (assert (false-null-true (values ?char))))))

(defrule false-find-a
	(filename ?name)
	?p <- (popped-char ?name 97) ; a
	?f <- (false-null-true (values 102))
	=>
	(retract ?p)
	(modify ?f (values 102 97)))

(defrule false-find-l
	(filename ?name)
	?p <- (popped-char ?name 108) ; l
	?f <- (false-null-true (values 102 97))
	=>
	(retract ?p)
	(modify ?f (values 102 97 108)))

(defrule false-find-s
	(filename ?name)
	?p <- (popped-char ?name 115) ; s
	?f <- (false-null-true (values 102 97 108))
	=>
	(retract ?p)
	(modify ?f (values 102 97 108 115)))

(defrule false-find-e
	(filename ?name)
	?p <- (popped-char ?name 101) ; e
	?f <- (false-null-true (values 102 97 108 115))
	=>
	(retract ?p)
	(modify ?f (values 102 97 108 115 101)))

(defrule end-false
	(filename ?name)
	?f <- (false-null-true (values 102 97 108 115 101))
	?e <- (element (value ?f))
	(or
		(popped-char ?name 9|10|13|32|-1) ; whitespace EOF
		(and
			(popped-char ?name 44|93) ; , ]
			(array (values $? ?e)))
		(and
			(popped-char ?name 44|125) ; , }
			(member (value ?e)))
	)
	=>
	(retract ?f)
	(modify ?e (value (make-instance of JSON-FALSE))))

(defrule true-find-r
	(filename ?name)
	?p <- (popped-char ?name 114) ; r
	?f <- (false-null-true (values 116))
	=>
	(retract ?p)
	(modify ?f (values 116 114)))

(defrule true-find-u
	(filename ?name)
	?p <- (popped-char ?name 117) ; u
	?f <- (false-null-true (values 116 114))
	=>
	(retract ?p)
	(modify ?f (values 116 114 117)))

(defrule true-find-e
	(filename ?name)
	?p <- (popped-char ?name 101) ; u
	?f <- (false-null-true (values 116 114 117))
	=>
	(retract ?p)
	(modify ?f (values 116 114 117 101)))

(defrule end-true
	(filename ?name)
	?f <- (false-null-true (values 116 114 117 101))
	?e <- (element (value ?f))
	(or
		(popped-char ?name 9|10|13|32|-1) ; whitespace EOF
		(and
			(popped-char ?name 44|93) ; , ]
			(array (values $? ?e)))
		(and
			(popped-char ?name 44|125) ; , }
			(member (value ?e)))
	)
	=>
	(retract ?f)
	(modify ?e (value (make-instance of JSON-TRUE))))

(defrule null-find-u
	(filename ?name)
	?p <- (popped-char ?name 117) ; u
	?f <- (false-null-true (values 110))
	=>
	(retract ?p)
	(modify ?f (values 110 117)))

(defrule null-find-l
	(filename ?name)
	?p <- (popped-char ?name 108) ; l
	?f <- (false-null-true (values 110 117))
	=>
	(retract ?p)
	(modify ?f (values 110 117 108)))

(defrule null-find-second-l
	(filename ?name)
	?p <- (popped-char ?name 108) ; l
	?f <- (false-null-true (values 110 117 108))
	=>
	(retract ?p)
	(modify ?f (values 110 117 108 108)))

(defrule end-null
	(filename ?name)
	?f <- (false-null-true (values 110 117 108 108))
	?e <- (element (value ?f))
	(or
		(popped-char ?name 9|10|13|32|-1) ; whitespace EOF
		(and
			(popped-char ?name 44|93) ; , ]
			(array (values $? ?e)))
		(and
			(popped-char ?name 44|125) ; , }
			(member (value ?e)))
	)
	=>
	(retract ?f)
	(modify ?e (value (make-instance of JSON-NULL))))

(defmessage-handler JSON-NUMBER print primary ()
	(print ?self:value))

(defmessage-handler JSON-TRUE print primary ()
	(print "true"))

(defmessage-handler JSON-FALSE print primary ()
	(print "false"))

(defmessage-handler JSON-NULL print primary ()
	(print "null"))

(defmessage-handler JSON-STRING print primary ()
	(print "\"" ?self:value "\""))

(defmessage-handler JSON-ARRAY print primary ()
	(print "[")
	(bind ?vals ?self:values)
	(bind ?len (length$ ?vals))
	(bind ?i 0)
	(foreach ?v ?vals
		(bind ?i (+ ?i 1))
		(send ?v print)
		(if (< ?i ?len) then (print ", ")))
	(print "]"))

(defmessage-handler JSON-MEMBER print primary ()
	(send ?self:key print)
	(print ": ")
	(send ?self:value print))

(defmessage-handler JSON-OBJECT print primary ()
	(print "{")
	(bind ?members ?self:members)
	(bind ?len (length$ ?members))
	(bind ?i 0)
	(foreach ?m ?members
		(bind ?i (+ ?i 1))
		(print crlf "  ")
		(send ?m print)
		(if (< ?i ?len) then (print ",") else (print crlf)))
	(print "}"))
