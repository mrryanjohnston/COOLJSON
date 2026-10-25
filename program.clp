; i cannot stop
; element
; ws value ws
;
; member
; ws string ws ':' element
(defclass JSON (is-a USER) (slot id))
(defclass JSON-NUMBER (is-a JSON) (slot value))
(defclass JSON-STRING (is-a JSON) (slot value))
(defclass JSON-ARRAY (is-a JSON) (multislot values))
(defclass JSON-OBJECT (is-a JSON) (multislot members))
(defclass JSON-MEMBER (is-a JSON) (slot key) (slot value))
(defclass JSON-FALSE (is-a JSON))
(defclass JSON-NULL (is-a JSON))
(defclass JSON-TRUE (is-a JSON))
(deftemplate json-string
	(slot name)
	(slot value))
(deftemplate json-number
	(slot name)
	(multislot value))
(deftemplate json-array
	(slot name)
	(multislot values))
(deftemplate json-obj
	(slot name)
	(multislot members))
(deftemplate json-element
	(slot name)
	(slot value (default nil)))
(deftemplate json-member
	(slot name)
	(slot key)
	(slot value (default nil)))
(deftemplate json-false-null-true
	(slot name)
	(multislot values))


;;; the start fact: a program asserts it with the
;;; logical name of an open file or socket and an id for the result, then
;;; asserts json-ready for the id and runs.  The codec reads one JSON value.
(deftemplate json-parse
	(slot stream)
	(slot id))

;;; the codec reads only while a ready fact for the id exists, and it
;;; retracts the fact when the value is complete
(deftemplate json-ready
	(slot id))

;;; one stream that the codec reads: the id of its facts and its logical name
(deftemplate json-stream
	(slot id)
	(slot stream))

;;; the end facts: the value, or the end of the stream before any value.
;;; The program that matches them retracts them.
(deftemplate json-done
	(slot id)
	(slot element))

(deftemplate json-eof
	(slot id))

(defrule json-start-parsing
	?p <- (json-parse (stream ?stream) (id ?name))
	=>
	(retract ?p)
	(assert (json-stream (id ?name) (stream ?stream)) (json-element (name ?name))))

(defrule json-pop-char
	(json-ready (id ?name))
	(json-stream (id ?name) (stream ?stream))
	(not (json-popped-char ?name ?))
	=>
	(assert (json-popped-char ?name (get-char ?stream))))

(defrule json-ignore-whitespace
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 9|10|13|32)
	(or
		(json-element (name ?name) (value nil))
		(json-obj (name ?name) (members $?members&:(= 0 (length$ ?members))|:(instancep (nth$ (length$ ?members) ?members))))
		(json-array (name ?name) (values $?values&:(= 0 (length$ ?values))|:(instancep (nth$ (length$ ?values) ?values))))
		(json-member (name ?name) (key ?key&:(instancep ?key)) (value nil)))
	=>
	(retract ?p))

(defrule json-init-element-for-empty-array
	(json-stream (id ?name))
	?a <- (json-array (name ?name) (values))
	(not (json-element (name ?name) (value nil)))
	=>
	(modify ?a (values (assert (json-element (name ?name))))))

(defrule json-init-element-for-empty-obj
	(json-stream (id ?name))
	(json-popped-char ?name 34)
	?o <- (json-obj (name ?name) (members))
	(not (json-element (name ?name) (value nil)))
	=>
	(modify ?o (members (assert (json-member (name ?name) (key (assert (json-element (name ?name)))))))))

(defrule json-start-array
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 91) ; [
	?e <- (json-element (name ?name) (value nil))
	(not (json-member (name ?name) (key ?e)))
	=>
	(retract ?p)
	(modify ?e (value (assert (json-array (name ?name))))))

(defrule json-array-unwrap-element
	(json-stream (id ?name))
	?e <- (json-element (name ?name) (value ?obj&:(instancep ?obj)))
	?a <- (json-array (name ?name) (values $?values ?e))
	=>
	(retract ?e)
	(modify ?a (values ?values ?obj)))

(defrule json-member-value-unwrap-element
	(json-stream (id ?name))
	?e <- (json-element (name ?name) (value ?obj&:(instancep ?obj)))
	?m <- (json-member (name ?name) (value ?e))
	=>
	(retract ?e)
	(modify ?m (value ?obj)))

(defrule json-array-find-comma-next-element
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 44) ; ,
	?a <- (json-array (name ?name) (values $?values&:(= 0 (length$ ?values))|:(instancep (nth$ (length$ ?values) ?values))))
	=>
	(retract ?p)
	(modify ?a (values ?values (assert (json-element (name ?name))))))

(defrule json-end-array
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 93) ; ]
	?a <- (json-array (name ?name) (values $?values ?value&:(instancep ?value)))
	?e <- (json-element (name ?name) (value ?a))
	=>
	(retract ?p ?a)
	(modify ?e (value (make-instance of JSON-ARRAY (values ?values ?value)))))

(defrule json-end-empty-array
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 93) ; ]
	?ee <- (json-element (name ?name) (value nil))
	?a <- (json-array (name ?name) (values ?ee))
	?e <- (json-element (name ?name) (value ?a))
	=>
	(retract ?ee ?p ?a)
	(modify ?e (value (make-instance of JSON-ARRAY))))

(defrule json-start-object
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 123) ; {
	?e <- (json-element (name ?name) (value nil))
	(not (json-member (name ?name) (key ?e)))
	=>
	(retract ?p)
	(modify ?e (value (assert (json-obj (name ?name))))))

(defrule json-end-member-key
	(json-stream (id ?name))
	?s <- (object (is-a JSON-STRING))
	?e <- (json-element (name ?name) (value =(instance-name ?s)))
	?m <- (json-member (name ?name) (key ?e))
	=>
	(retract ?e)
	(modify ?m (key (instance-name ?s))))

(defrule json-find-colon-member
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 58) ; :
	?m <- (json-member (name ?name) (key ?key&:(instancep ?key)) (value nil))
	=>
	(retract ?p)
	(modify ?m (value (assert (json-element (name ?name))))))

(defrule json-find-comma-next-member
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 44) ; ,
	?o <- (json-obj (name ?name) (members $?members&:(= 0 (length$ ?members))|:(instancep (nth$ (length$ ?members) ?members))))
	=>
	(retract ?p)
	(modify ?o (members ?members (assert (json-member (name ?name) (key (assert (json-element (name ?name)))))))))

(defrule json-end-object
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 125) ; }
	?o <- (json-obj (name ?name) (members $?members&:(= 0 (length$ ?members))|:(instancep (nth$ (length$ ?members) ?members))))
	?e <- (json-element (name ?name) (value ?o))
	=>
	(retract ?p ?o)
	(modify ?e (value (make-instance of JSON-OBJECT (members ?members)))))

(defrule json-start-string
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 34) ; "
	?e <- (json-element (name ?name) (value nil))
	=>
	(retract ?p)
	(modify ?e (value (assert (json-string (name ?name) (value ""))))))

(defrule json-continue-string
	(json-stream (id ?name))
	?p <- (json-popped-char ?name ?char&~34&~92) ; " \
	?s <- (json-string (name ?name) (value ?str))
	(not (json-escape ?name))
	=>
	(retract ?p)
	(modify ?s (value (format nil "%s%c" ?str ?char))))

(defrule json-end-string
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 34) ; "
	?s <- (json-string (name ?name) (value ?str))
	?e <- (json-element (name ?name) (value ?s))
	(not (json-escape ?name))
	=>
	(retract ?p ?s)
	(modify ?e (value (make-instance of JSON-STRING (value ?str)))))

(defrule json-begin-string-escape
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 92) ; \
	?s <- (json-string (name ?name) (value ?str))
	(not (json-escape ?name))
	=>
	(retract ?p)
	(modify ?s (value (format nil "%s%c" ?str 92)))
	(assert (json-escape ?name)))

(defrule json-begin-string-escape-hexadecimal
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 117) ; u
	?s <- (json-string (name ?name) (value ?str))
	(json-escape ?name)
	(not (json-hexadecimal ?name))
	=>
	(retract ?p)
	(modify ?s (value (format nil "%s%c" ?str 117)))
	(assert (json-hexadecimal ?name)))

(defrule json-end-string-escape
	(json-stream (id ?name))
	?p <- (json-popped-char ?name ?char&~117) ; u
	?s <- (json-string (name ?name) (value ?str))
	?e <- (json-escape ?name)
	(not (json-hexadecimal ?name $?))
	=>
	(retract ?p ?e)
	(modify ?s (value (format nil "%s%c" ?str ?char))))

(defrule json-continue-string-escape-hexadecimal
	(json-stream (id ?name))
	?p <- (json-popped-char ?name ?char&48|49|50|51|52|53|54|55|56|57|97|98|99|100|101|102|65|66|67|68|69|70) ; 0-9 a-f A-F
	?s <- (json-string (name ?name) (value ?str))
	(json-escape ?name)
	?h <- (json-hexadecimal ?name $?hex&:(> 3 (length$ ?hex)))
	=>
	(retract ?p ?h)
	(modify ?s (value (format nil "%s%c" ?str ?char)))
	(assert (json-hexadecimal ?name $?hex ?char)))

(defrule json-end-string-escape-hexadecimal
	(json-stream (id ?name))
	?p <- (json-popped-char ?name ?char&48|49|50|51|52|53|54|55|56|57|97|98|99|100|101|102|65|66|67|68|69|70) ; 0-9 a-f A-F
	?s <- (json-string (name ?name) (value ?str))
	?e <- (json-escape ?name)
	?h <- (json-hexadecimal ?name ? ? ?)
	=>
	(retract ?p ?e ?h)
	(modify ?s (value (format nil "%s%c" ?str ?char))))

(defrule json-start-number
	(json-stream (id ?name))
	?p <- (json-popped-char ?name ?char&45|48|49|50|51|52|53|54|55|56|57) ; - 0-9
	?e <- (json-element (name ?name) (value nil))
	(not (json-member (name ?name) (key ?e)))
	=>
	(retract ?p)
	(modify ?e (value (assert (json-number (name ?name) (value (format nil "%c" ?char)))))))

(defrule json-illegal-zero
	(json-stream (id ?name))
	(json-popped-char ?name ?char&~46&~69&~101&~-1&~9&~10&~13&~32&~44&~93&~125) ; . E e EOF ws , ] }
	(json-number (name ?name) (value ?number&"0"|"-0"))
	=>
	(format t "ILLEGAL CHAR: %c found after %s" ?char ?number))

(defrule json-illegal-number
	(json-stream (id ?name))
	(json-popped-char ?name ?char&~46&~48&~49&~50&~51&~52&~53&~54&~55&~56&~57&~69&~101&~-1) ; . 0-9 E e EOF
	?n <- (json-number (name ?name))
	?e <- (json-element (name ?name) (value ?n))
	(not (json-member (name ?name) (value ?e)))
	(not (json-array (name ?name) (values $? ?e)))
	=>
	(format t "ILLEGAL CHAR: %c found when parsing number" ?char))

(defrule json-continue-number
	(json-stream (id ?name))
	?p <- (json-popped-char ?name ?char&48|49|50|51|52|53|54|55|56|57) ; 0-9
	?n <- (json-number (name ?name) (value ?number))
	=>
	(retract ?p)
	(modify ?n (value (format nil "%s%c" ?number ?char))))

(defrule json-start-fraction
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 46) ; .
	?n <- (json-number (name ?name) (value ?number))
	=>
	(retract ?p)
	(modify ?n (value ?number "")))

(defrule json-continue-fraction
	(json-stream (id ?name))
	?p <- (json-popped-char ?name ?char&48|49|50|51|52|53|54|55|56|57) ; 0-9
	?n <- (json-number (name ?name) (value ?number ?fraction))
	=>
	(retract ?p)
	(modify ?n (value ?number (format nil "%s%c" ?fraction ?char))))

(defrule json-illegal-fraction
	(json-stream (id ?name))
	(json-popped-char ?name ?char&~48&~49&~50&~51&~52&~53&~54&~55&~56&~57&~69&~101&~-1) ; 0-9 E e EOF
	?n <- (json-number (name ?name) (value ? ?))
	?e <- (json-element (name ?name) (value ?n))
	(not (json-member (name ?name) (value ?e)))
	(not (json-array (name ?name) (values $? ?e)))
	=>
	(format t "ILLEGAL CHAR: %c found when parsing fraction" ?char))

(defrule json-start-exponent
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 69|101) ; E e
	?n <- (json-number (name ?name) (value ?number ?fraction))
	=>
	(retract ?p)
	(modify ?n (value ?number ?fraction "")))

(defrule json-start-exponent-no-fraction
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 69|101) ; E e
	?n <- (json-number (name ?name) (value ?number))
	=>
	(retract ?p)
	(modify ?n (value ?number "" "")))

(defrule json-find-exponent-sign
	(json-stream (id ?name))
	?p <- (json-popped-char ?name ?char&43|45) ; + -
	?n <- (json-number (name ?name) (value ?number ?fraction ""))
	=>
	(retract ?p)
	(modify ?n (value ?number ?fraction (format nil "%c" ?char) "")))

(defrule json-find-exponent-no-sign
	(json-stream (id ?name))
	?p <- (json-popped-char ?name ?char&48|49|50|51|52|53|54|55|56|57) ; 0-9
	?n <- (json-number (name ?name) (value ?number ?fraction ""))
	=>
	(retract ?p)
	(modify ?n (value ?number ?fraction "" (format nil "%c" ?char))))

(defrule json-illegal-exponent-sign
	(json-stream (id ?name))
	(json-popped-char ?name ?char&~43&~45&~48&~49&~50&~51&~52&~53&~54&~55&~56&~57&~-1) ; + - 0-9 EOF
	(json-number (name ?name) (value ? ? ""))
	=>
	(format t "ILLEGAL CHAR: %c found when parsing exponent sign" ?char))

(defrule json-continue-exponent
	(json-stream (id ?name))
	?p <- (json-popped-char ?name ?char&48|49|50|51|52|53|54|55|56|57) ; 0-9
	?n <- (json-number (name ?name) (value ?number ?fraction ?sign ?exponent))
	=>
	(retract ?p)
	(modify ?n (value ?number ?fraction ?sign (format nil "%s%c" ?exponent ?char))))

(defrule json-illegal-exponent
	(json-stream (id ?name))
	(json-popped-char ?name ?char&~48&~49&~50&~51&~52&~53&~54&~55&~56&~57&~-1&~9&~10&~13&~32&~44&~93&~125) ; 0-9 EOF ws , ] }
	(json-number (name ?name) (value ? ? ? ?))
	=>
	(format t "ILLEGAL CHAR: %c found when parsing exponent" ?char))

(defrule json-end-member
	(json-stream (id ?name))
	?m <- (json-member (name ?name) (key ?key) (value ?value&:(instancep ?value)))
	?o <- (json-obj (name ?name) (members $?members ?m))
	=>
	(retract ?m)
	(modify ?o (members ?members (make-instance of JSON-MEMBER (key ?key) (value ?value)))))

(defrule json-end-number
	(json-stream (id ?name))
	?n <- (json-number (name ?name) (value ?number))
	?e <- (json-element (name ?name) (value ?n))
	(or
		(json-popped-char ?name EOF)
		(json-popped-char ?name 9|10|13|32) ; whitespace
		(and
			(json-popped-char ?name 44|93) ; , ]
			(json-array (name ?name) (values $? ?e)))
		(and
			(json-popped-char ?name 44|125) ; , }
			?m <- (json-member (name ?name) (value ?e))
			(json-obj (name ?name) (members $? ?m))))
	=>
	(retract ?n)
	(modify ?e (value (make-instance of JSON-NUMBER (value (string-to-field ?number))))))

(defrule json-end-number-fraction
	(json-stream (id ?name))
	?n <- (json-number (name ?name) (value ?number ?fraction))
	?e <- (json-element (name ?name) (value ?n))
	(or
		(json-popped-char ?name EOF)
		(json-popped-char ?name 9|10|13|32) ; whitespace
		(and
			(json-popped-char ?name 44|93) ; , ]
			(json-array (name ?name) (values $? ?e)))
		(and
			(json-popped-char ?name 44|125) ; , }
			?m <- (json-member (name ?name) (value ?e))
			(json-obj (name ?name) (members $? ?m))))
	=>
	(retract ?n)
	(modify ?e (value (make-instance of JSON-NUMBER (value (string-to-field (format nil "%s.%s" ?number ?fraction)))))))

(defrule json-end-number-exponent
	(json-stream (id ?name))
	?n <- (json-number (name ?name) (value ?number ?fraction&~"" ?sign ?exponent))
	?e <- (json-element (name ?name) (value ?n))
	(or
		(json-popped-char ?name EOF)
		(json-popped-char ?name 9|10|13|32) ; whitespace
		(and
			(json-popped-char ?name 44|93) ; , ]
			(json-array (name ?name) (values $? ?e)))
		(and
			(json-popped-char ?name 44|125) ; , }
			?m <- (json-member (name ?name) (value ?e))
			(json-obj (name ?name) (members $? ?m))))
	=>
	(retract ?n)
	(modify ?e (value (make-instance of JSON-NUMBER (value (str-cat ?number "." ?fraction "e" ?sign ?exponent))))))

(defrule json-end-number-exponent-no-fraction
	(json-stream (id ?name))
	?n <- (json-number (name ?name) (value ?number "" ?sign ?exponent))
	?e <- (json-element (name ?name) (value ?n))
	(or
		(json-popped-char ?name EOF)
		(json-popped-char ?name 9|10|13|32) ; whitespace
		(and
			(json-popped-char ?name 44|93) ; , ]
			(json-array (name ?name) (values $? ?e)))
		(and
			(json-popped-char ?name 44|125) ; , }
			?m <- (json-member (name ?name) (value ?e))
			(json-obj (name ?name) (members $? ?m))))
	=>
	(retract ?n)
	(modify ?e (value (make-instance of JSON-NUMBER (value (str-cat ?number "e" ?sign ?exponent))))))

(defrule json-start-false-null-true
	(json-stream (id ?name))
	?p <- (json-popped-char ?name ?char&102|110|116) ; f n t
	?e <- (json-element (name ?name) (value nil))
	(not (json-member (name ?name) (key ?e)))
	=>
	(retract ?p)
	(modify ?e (value (assert (json-false-null-true (name ?name) (values ?char))))))

(defrule json-false-find-a
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 97) ; a
	?f <- (json-false-null-true (name ?name) (values 102))
	=>
	(retract ?p)
	(modify ?f (values 102 97)))

(defrule json-false-find-l
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 108) ; l
	?f <- (json-false-null-true (name ?name) (values 102 97))
	=>
	(retract ?p)
	(modify ?f (values 102 97 108)))

(defrule json-false-find-s
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 115) ; s
	?f <- (json-false-null-true (name ?name) (values 102 97 108))
	=>
	(retract ?p)
	(modify ?f (values 102 97 108 115)))

(defrule json-false-find-e
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 101) ; e
	?f <- (json-false-null-true (name ?name) (values 102 97 108 115))
	=>
	(retract ?p)
	(modify ?f (values 102 97 108 115 101)))

(defrule json-end-false
	(json-stream (id ?name))
	?f <- (json-false-null-true (name ?name) (values 102 97 108 115 101))
	?e <- (json-element (name ?name) (value ?f))
	(or
		(json-popped-char ?name 9|10|13|32|-1) ; whitespace EOF
		(and
			(json-popped-char ?name 44|93) ; , ]
			(json-array (name ?name) (values $? ?e)))
		(and
			(json-popped-char ?name 44|125) ; , }
			(json-member (name ?name) (value ?e)))
	)
	=>
	(retract ?f)
	(modify ?e (value (make-instance of JSON-FALSE))))

(defrule json-true-find-r
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 114) ; r
	?f <- (json-false-null-true (name ?name) (values 116))
	=>
	(retract ?p)
	(modify ?f (values 116 114)))

(defrule json-true-find-u
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 117) ; u
	?f <- (json-false-null-true (name ?name) (values 116 114))
	=>
	(retract ?p)
	(modify ?f (values 116 114 117)))

(defrule json-true-find-e
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 101) ; u
	?f <- (json-false-null-true (name ?name) (values 116 114 117))
	=>
	(retract ?p)
	(modify ?f (values 116 114 117 101)))

(defrule json-end-true
	(json-stream (id ?name))
	?f <- (json-false-null-true (name ?name) (values 116 114 117 101))
	?e <- (json-element (name ?name) (value ?f))
	(or
		(json-popped-char ?name 9|10|13|32|-1) ; whitespace EOF
		(and
			(json-popped-char ?name 44|93) ; , ]
			(json-array (name ?name) (values $? ?e)))
		(and
			(json-popped-char ?name 44|125) ; , }
			(json-member (name ?name) (value ?e)))
	)
	=>
	(retract ?f)
	(modify ?e (value (make-instance of JSON-TRUE))))

(defrule json-null-find-u
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 117) ; u
	?f <- (json-false-null-true (name ?name) (values 110))
	=>
	(retract ?p)
	(modify ?f (values 110 117)))

(defrule json-null-find-l
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 108) ; l
	?f <- (json-false-null-true (name ?name) (values 110 117))
	=>
	(retract ?p)
	(modify ?f (values 110 117 108)))

(defrule json-null-find-second-l
	(json-stream (id ?name))
	?p <- (json-popped-char ?name 108) ; l
	?f <- (json-false-null-true (name ?name) (values 110 117 108))
	=>
	(retract ?p)
	(modify ?f (values 110 117 108 108)))

(defrule json-end-null
	(json-stream (id ?name))
	?f <- (json-false-null-true (name ?name) (values 110 117 108 108))
	?e <- (json-element (name ?name) (value ?f))
	(or
		(json-popped-char ?name 9|10|13|32|-1) ; whitespace EOF
		(and
			(json-popped-char ?name 44|93) ; , ]
			(json-array (name ?name) (values $? ?e)))
		(and
			(json-popped-char ?name 44|125) ; , }
			(json-member (name ?name) (value ?e)))
	)
	=>
	(retract ?f)
	(modify ?e (value (make-instance of JSON-NULL))))

(defmessage-handler JSON-NUMBER pprint primary (?t)
	(printout ?t ?self:value))

(defmessage-handler JSON-TRUE pprint primary (?t)
	(printout ?t "true"))

(defmessage-handler JSON-FALSE pprint primary (?t)
	(printout ?t "false"))

(defmessage-handler JSON-NULL pprint primary (?t)
	(printout ?t "null"))

(defmessage-handler JSON-STRING pprint primary (?t)
	(printout ?t "\"" ?self:value "\""))

(defmessage-handler JSON-ARRAY pprint primary (?t)
	(printout ?t "[")
	(bind ?vals ?self:values)
	(bind ?len (length$ ?vals))
	(bind ?i 0)
	(foreach ?v ?vals
		(bind ?i (+ ?i 1))
		(send ?v pprint ?t)
		(if (< ?i ?len) then (printout ?t ",")))
	(printout ?t "]"))

(defmessage-handler JSON-MEMBER pprint primary (?t)
	(send ?self:key pprint ?t)
	(printout ?t ":")
	(send ?self:value pprint ?t))

(defmessage-handler JSON-OBJECT pprint primary (?t)
	(printout ?t "{")
	(bind ?members ?self:members)
	(bind ?len (length$ ?members))
	(bind ?i 0)
	(foreach ?m ?members
		(bind ?i (+ ?i 1))
		(send ?m pprint ?t)
		(if (< ?i ?len) then (printout ?t ",") else (printout ?t)))
	(printout ?t "}"))

;;; the value of the stream is complete when the top element holds an instance
;;; and no array or member holds that element
(defrule json-end
	(declare (salience 10))
	?e <- (json-element (name ?name) (value ?value&:(instancep ?value)))
	?s <- (json-stream (id ?name))
	(not (json-array (name ?name) (values $? ?e $?)))
	(not (json-member (name ?name) (value ?e)))
	(not (json-member (name ?name) (key ?e)))
	?r <- (json-ready (id ?name))
	=>
	(retract ?e ?r ?s)
	(assert (json-done (id ?name) (element ?value))))

(defrule json-end-of-stream
	"the stream ended before a value began"
	(declare (salience -10))
	?c <- (json-popped-char ?name -1)
	?r <- (json-ready (id ?name))
	?e <- (json-element (name ?name) (value nil))
	?s <- (json-stream (id ?name))
	=>
	(retract ?c ?r ?e ?s)
	(assert (json-eof (id ?name))))

(defrule json-drop-char-after-value
	"a character that came after the value, for example a newline, goes away"
	(declare (salience -10))
	?c <- (json-popped-char ?name ?)
	(json-stream (id ?name))
	(not (json-element (name ?name)))
	=>
	(retract ?c))
