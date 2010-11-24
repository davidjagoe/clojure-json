# NOTE #

Status: alpha.

This fork is an attempt to support the CLR platform. Currently unicode
is known not to work because unlike the Java BufferedReader which
correctly supports the concept of characters, I am using the .net
BufferedStream which provides no native support for characters beyonds
those that fit into bytes. Yay. Once I figure out a way around that, I
will also have to remove the (MemoryStream. (char-array string)) code
which suffers from the same problem.

# Installing #

clojure-json is on [clojars](http://clojars.org)! [http://clojars.org/org.danlarkin/clojure-json](http://clojars.org/org.danlarkin/clojure-json)

To install from source you can `lein install` but likely you can just add this project as a dependency like it says on [http://clojars.org/org.danlarkin/clojure-json](http://clojars.org/org.danlarkin/clojure-json).

# Using The Encoder #

    user=> (require '(org.danlarkin [json :as json]))
    nil
    user=> (print (json/encode [1 2 3 4 5]))
    [1,2,3,4,5]nil
    user=> (print (json/encode {:a 1 :b 2 :c 3}))
    {"a":1,"b":2,"c":3}nil
    user=> (print (json/encode [1 2 3 4 5] :indent 2))
    [
      1,
      2,
      3,
      4,
      5
    ]nil
    user=> (import '(java.io FileWriter))
    nil
    user=> (json/encode-to-writer [1 2 3 4 5] (FileWriter. "/tmp/foo.json"))
    #<FileWriter java.io.FileWriter@c93f91>
    user=> (import '(java.util Date))
    nil
    user=> (json/encode (Date.))
    java.lang.Exception: Unknown Datastructure: Sat Nov 5 19:00:00 GMT 1605 (NO_SOURCE_FILE:0)
    user=> (defn date-encoder
            [date writer pad current-indent start-token-indent indent-size]
            (.append writer (str start-token-indent \" date \")))
    #'user/date-encoder
    user=> (json/add-encoder java.util.Date date-encoder)
    #<MultiFn clojure.lang.MultiFn@da6c0d>
    user=> (print (json/encode (Date.)))
    "Sat Nov 5 19:00:00 GMT 1605"nil
    user=> (print (json/encode [(Date.) (Date.) (Date.)] :indent 2))
    [
      "Sat Nov 5 19:00:00 GMT 1605",
      "Sat Nov 5 19:00:00 GMT 1605",
      "Sat Nov 5 19:00:00 GMT 1605"
    ]nil
    user=> (print (json/encode {:foo (Date.) :bam 4 :quux 'bar} :indent 2))
    {
      "foo":"Sat Nov 5 19:00:00 GMT 1605",
      "bam":4,
      "quux":"bar"
    }nil

Obviously not all Clojure data structures have clear parallels in
JSON. In particular, keywords become strings upon encoding, and
strings used as keys become keywords upon decoding. For sets, you may
bind <tt>org.danlarkin.encoder/\*sets-as-maps\*</tt> to true in order to
get them encoded as maps of keys to themselves. This preserves O(1)
lookup and callability of sets at the expense of calling seq on them.

# Custom Encoding #

`clojure-json` uses a [multimethod](http://clojure.org/multimethods)
for custom encoding, dispatching on type.  If you're adding an encoder
function for a container type make sure to respect all of the
indentation arguments that your function will be passed and to call
encode-helper on each of the elements in your container.  I've left
the parameters to date-encoder un-hinted in this example in the
interest of brevity but their inclusion does seem to speed up
execution time a good bit so I suggest using them where speed matters.

# Using The Parser #

    user=> (require '(org.danlarkin [json :as json]))
    nil
    user=> (json/decode "[1, 2, 3, 4, 5]")
    [1 2 3 4 5]
    user=> (json/decode "{\"foo\":1, \"bar\":2, \"baz\":3}")
    {:foo 1, :bar 2, :baz 3}
    user=> (json/decode "{\"foo\":[1,2,\"superbam\"], \"bar\":{\"bam\":98.6}, \"baz\":3}")
    {:foo [1 2 "superbam"], :bar {:bam 98.6}, :baz 3}
    user=> (json/decode-from-reader (FileReader. "/tmp/foo.json"))
    [1 2 3 4 5]

# Special Thanks #
Special thanks go to Darrell Bishop of arubanetworks.com for writing the parser included in this distribution.
