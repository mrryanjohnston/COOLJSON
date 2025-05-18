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
files present in this directory. Running it looks like this:

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

Type `(facts)` to see the `element` facts which track the `JSON` instances:

```
CLIPS> (facts)
f-1     (filename test.json)
f-2     (filename test2.json)
f-3     (filename test3.json)
f-4     (filename test4.json)
f-5     (filename test5.json)
f-6     (element (name test5.json) (value [gen8]))
f-77    (popped-char test5.json 10)
f-78    (element (name test4.json) (value [gen15]))
f-123   (popped-char test4.json 10)
f-124   (element (name test3.json) (value [gen16]))
f-128   (popped-char test3.json 10)
f-129   (element (name test2.json) (value [gen39]))
f-278   (popped-char test2.json 10)
f-279   (element (name test.json) (value [gen51]))
f-348   (popped-char test.json 10)
For a total of 15 facts.
```

Each `element` fact has an instance name set in its `value` slot.
You may view the parsed JSON by `send`ind the `print` message
to any of the instance names like so:

```
CLIPS> (send [gen8] print)
{
  "\"": "woah\, dude",
  "hot dog": ["\u64aa yo"]
}CLIPS> (send [gen51] print)
{
  "asdf": 123,
  "four": [[], 6, "jkl", [{}]]
}CLIPS> (send [gen16] print)
{}CLIPS> (send [gen39] print)
[{
  "asdf": 123,
  "four": [[], 6, "jkl", [{}]]
}, 789.02, "another", [567, "string", {
  "yo": 1
}], "hey"]CLIPS> 
```

You can input your own custom JSON directly from the command line
by `assert`ing a `filename` fact for `stdin`:

```
CLIPS>(assert (filename stdin))
<Fact-349>
CLIPS> (run)
[ "My custom JSON", 987654321.2 ]
72 rules fired        Run time is 18.2761199474335 seconds.
3.93956705291327 rules per second.
20 mean number of facts (21 maximum).
52 mean number of instances (54 maximum).
1 mean number of activations (2 maximum).
CLIPS> (do-for-fact ((?f element)) (eq ?f:name stdin) (ppfact ?f))
(element 
   (name stdin) 
   (value [gen54]))
CLIPS> (send [gen54] print)
["My custom JSON", 987654321.2]CLIPS> 
```

Use functions like `do-for-all-instances` to query your JSON structures.
For example, in order to find all objects within the parsed `test*.json`
files that have a key/value pair with key `"asdf"`:

```
CLIPS> (do-for-all-instances ((?i JSON-OBJECT) (?member JSON-MEMBER) (?key JSON-STRING)) (and (member$ ?member ?i:members) (eq ?member:key ?key) (eq ?key:value "asdf")) (send ?i print))
{
  "asdf": false
}{
  "asdf": 123,
  "four": [[], 6, "jkl", [{}]]
}{
  "asdf": 123,
  "four": [[], 6, "jkl", [{}]]
}
```

Or, as a `defrule`:

```
CLIPS> (defrule find-asdf
?key <- (object (is-a JSON-STRING) (value "asdf"))
?member <- (object (is-a JSON-MEMBER) (key =(instance-name ?key)))
?i <- (object (is-a JSON-OBJECT) (members $? =(instance-name ?member) $?))
=>
(send ?i print))
CLIPS> (run)
{
  "asdf": 123,
  "four": [[], 6, "jkl", [{}]]
}{
  "asdf": 123,
  "four": [[], 6, "jkl", [{}]]
}{
  "asdf": false
}3 rules fired        Run time is 9.79900360107422e-05 seconds.
30615.3576642336 rules per second.
15 mean number of facts (15 maximum).
51 mean number of instances (51 maximum).
2 mean number of activations (3 maximum).
```
