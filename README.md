# COOLJSON

A JSON Parser written in
[CLIPS](https://clipsrules.net/)
and CLIPS's Object Oriented Language, COOL!

## Usage

You must have CLIPS installed and available on your system. Check out the
[official sourceforce page](https://sourceforge.net/projects/clipsrules/)
for download links or use
[CLIPSenv](https://github.com/mrryanjohnston/CLIPSenv)
to install. The file `program.bat` demonstrates parsing the `test*.json`
files present in `tests/cases`. Running it looks like this:

```
$ clips -f2 program.bat
#########%%%%%%%******************************************************~~~~~~~~
551 rules fired        Run time is 0.0019071102142334 seconds.
288918.802850356 rules per second.
15 mean number of facts (23 maximum).
23 mean number of instances (51 maximum).
4 mean number of activations (10 maximum).
         CLIPS (6.4.2 1/14/25)
CLIPS> 
```

The value of each stream is a `JSON-TEXT` instance with the stream's `id`.
Its `value` slot holds the top value, and it is the last instance the codec
makes for the stream, so a rule that matches it sees a complete value. Send
`pprint` to it, or to any instance below it, to write the JSON:

```
CLIPS> (do-for-all-instances ((?j JSON-TEXT)) TRUE (println ?j:id) (send ?j pprint t) (println))
test25.json
{"space before colon":1,"tab before colon":2,"newline before colon":3,"nested":{"inner key":[4,{"deep":"a b c"}]},"no space":5}
test24.json
[1e5,2,3.5,-0.5e+1,"not a number",true,null]
...
test3.json
{}
test2.json
[{"asdf":123,"four":[[],6,"jkl",[{}]]},789.02,"another",[567,"string",{"yo":1}],"hey"]
test.json
{"asdf":123,"four":[[],6,"jkl",[{}]]}
CLIPS> 
```

You can input your own custom JSON directly from the command line
by asserting the start fact for `stdin`:

```
CLIPS>(assert (json-parse (stream stdin) (id stdin)) (json-ready (id stdin)))
<Fact-349>
CLIPS> (run)
[ "My custom JSON", 987654321.2 ]
72 rules fired        Run time is 18.2761199474335 seconds.
3.93956705291327 rules per second.
20 mean number of facts (21 maximum).
52 mean number of instances (54 maximum).
1 mean number of activations (2 maximum).
CLIPS> (do-for-instance ((?j JSON-TEXT)) (eq ?j:id stdin) (send ?j pprint t))
["My custom JSON",987654321.2]CLIPS> 
```

Use functions like `do-for-all-instances` to query your JSON structures.
For example, in order to find all objects within the parsed `test*.json`
files that have a key/value pair with key `"asdf"`:

```
CLIPS> (do-for-all-instances ((?i JSON-OBJECT) (?member JSON-MEMBER) (?key JSON-STRING)) (and (member$ ?member ?i:members) (eq ?member:key ?key) (eq ?key:value "asdf")) (send ?i pprint t) (println)
{"asdf":false}
{"asdf":123,"four":[[],6,"jkl",[{}]]}
{"asdf":123,"four":[[],6,"jkl",[{}]]}
```

Or, as a `defrule`:

```
CLIPS> (defrule find-asdf
?key <- (object (is-a JSON-STRING) (value "asdf"))
?member <- (object (is-a JSON-MEMBER) (key =(instance-name ?key)))
?i <- (object (is-a JSON-OBJECT) (members $? =(instance-name ?member) $?))
=>
(send ?i print)
(println))
CLIPS> (run)
{"asdf":false}
{"asdf":123,"four":[[],6,"jkl",[{}]]}
{"asdf":123,"four":[[],6,"jkl",[{}]]}
3 rules fired        Run time is 9.79900360107422e-05 seconds.
30615.3576642336 rules per second.
15 mean number of facts (15 maximum).
51 mean number of instances (51 maximum).
2 mean number of activations (3 maximum).
```

## Testing

Run the test suite with `make test`. It'll run through the files in `tests/cases`
and make sure that the result of sending `pprint` to the instances
prints out the contents of the files named the same in `tests/expected`.

Every case decodes once under each of the conflict resolution strategies CLIPS
offers: `depth`, `breadth`, `lex`, `mea`, `complexity`, `simplicity` and
`random`. A codec that keeps only one rule on the agenda at a time decodes
the same way under all of them, so a case that fails under one strategy
and not another points at a rule set that relies on firing order. All the
cases decode in one run per strategy, so a strategy like `breadth` advances
every message a character at a time in lockstep: a rule whose patterns are
not all tied to the same message then matches facts from two messages at
once, which decoding one case at a time cannot show. Each failure names the
strategy it failed under, and the summary counts cases times strategies:

```
25 cases x 7 strategies = 175 tests, 175 passed, 0 failed
```

## Linting

Run `make lint` to check that the rules follow the practices of a COOL codec.
The lint script, `lint/lint.bat`, is written in CLIPS. It decodes every
file in `tests/cases` one rule at a time and checks:

1. only one rule is on the agenda at a time while one case is decoded
2. all the cases decode together in one run, each to the file named the same
   in `tests/expected`, firing as many rules as they fired alone put together
3. no rule has an `(if` in its right hand side
4. no deffunction called from a rule's right hand side, directly or through
   another deffunction, has an `(if`

It prints one line per violation, a summary, and exits with status 1 when
it found a violation.

## The start fact and the result

Each construct of this codec has the prefix `json-`,
and the codec has its own start fact, so it loads next to other codecs in
one environment. Open the stream and assert the start fact with the logical
name of the stream and an id for the results, then run:

    (open "input" input)
    (assert (json-parse (stream input) (id input)) (json-ready (id input)))
    (run)

The result is a `JSON-TEXT` instance whose `value` is the top value. Every
object the codec makes carries the `id` in its `id` slot, and the
`JSON-TEXT` is the last object it makes, so match it with an object pattern
or find it with `find-instance`:

    (find-instance ((?j JSON-TEXT)) (eq ?j:id input))

The codec only decodes and encodes. It does not record that a value was
received, sent or handled; a program that needs that keeps a fact of its
own, or unmakes the instances it is done with. To send a value, make the
objects and send `pprint` with the logical name of the stream:

    (send (make-instance of JSON-ARRAY (values
            (make-instance of JSON-STRING (value "pong"))
            (make-instance of JSON-NUMBER (value 1)))) pprint t)
    ; ["pong",1]

A protocol codec reads only while a `json-ready` fact for the id exists, and a complete message consumes it, so a server can poll between messages. A program that reads a file asserts the ready fact again until the codec asserts `json-eof`.
