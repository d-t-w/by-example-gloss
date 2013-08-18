;; ## Content
;; ---
;;
;; - ***the basics***
;;     - [frames](#frames)
;;     - [codec](#codec)
;;     - [nesting codec](#nesting)
;;     - [string frames](#strings)
;;     - [repeated frames](#repeated)
;;     - [transforms](#transforms)
;;     - [strict decoding](#decoding)
;; - ***a simple HTTP header codec***
;;     - [definition](#init-definition)
;;     - [transforms](#init-transforms)
;;     - [the codec](#init-codec)
;;     - [limitations](#init-limitations)
;; - ***a better HTTP codec***
;; &nbsp;

;; ## The Basics
;; ---
;; &nbsp;
;;
;; ***Gloss is a DSL for describing byte formats.***
;;
;; In Gloss, a byte format is called a *frame*, frames are compiled into *codec*, which allow you to:
;;
;; - encode a data structure into a ByteBuffer<sup>1</sup>; or,
;; - decode a ByteBuffer into a data structure.
;;
;; A frame is actually just a clojure data structure, that simple. As we will see, that data structure might
;; include other codec, meaning a complicated codec can be built from several smaller, simple codec.
;; It's turtles all the way down.
;;
;; Defining a complex codec by combining its composite parts allows us to test (and transform if needed)
;; at a granular level.
;;
;; <sup>1 ByteBuffers technically, but we'll get to that later</sup>
(ns by-example-gloss.core
  (:require [gloss.core :refer [compile-frame defcodec ordered-map string repeated]]
            [gloss.io :refer [encode decode to-byte-buffer contiguous]]
            [expectations :refer [expect run-all-tests]]))

;; <a id="frames"></a>*frames*
;; ---

;; Gloss recognizes certain keywords as primitive data types, and you can define a frame using any combination of them.

;; We'll start with :byte in our examples below, consider that inter-changeable with any of these.
[:byte, :int16, :int32, :int64, :float32, :float64, :ubyte, :uint16, :uint32, :uint64 ]

(def byte-frame
  "The simplest frame I could imagine, a single byte."
  :byte )

(def vector-bytes-frame
  "Frames are just clojure data structures, this frame is a vector of two bytes."
  [:byte :byte ])

(def map-bytes-frame
  "This frame contains the same data, but in map form rather than a vector."
  {:first :byte
   :second :byte})

;; <a id="codec"></a>*codec*
;; ---

(def byte-codec
  "We create a codec from our simplest frame with compile-frame"
  (compile-frame byte-frame))

;; Then we use this codec to encode a byte into a buffer.
;;
;; - *to-byte-buffer* is utility method provided by Gloss.
;; - *encode* always results in a sequence of buffers
(expect
  (to-byte-buffer 127)
  (first (encode byte-codec 127)))

;; And the inverse, decoding a buffer into a byte value.
(expect
  127
  (decode byte-codec (to-byte-buffer 127)))

;; Gloss provides a macro shortcut, defcodec, for defining codec.
(defcodec vector-bytes-codec
  vector-bytes-frame)

;; We can encode a vector into a buffer using our vector-bytes-codec
(expect
  (to-byte-buffer '(126 127))
  (first (encode vector-bytes-codec [126 127])))

;; and decode that vector from the buffer using the same.
(expect
  [126 127]
  (decode vector-bytes-codec (to-byte-buffer '(126 127))))

;; Rinse and repeat with our map-bytes-codec. Beware, Gloss encodes map values in a consistent
;; but arbitrary order. In my example the bytes could have been writte as '(127 126). If you require a particular
;; serialization order use Gloss' ordered-map.
(defcodec map-bytes-codec
  {:first :byte
   :second :byte})

;; We can encode/decode pretty much any clojure data structure using the primitives that Gloss provides.
(expect
  (to-byte-buffer '(126 127))
  (first (encode map-bytes-codec {:first 126
                                  :second 127})))

(expect
  {:first 126
   :second 127}
  (decode map-bytes-codec (to-byte-buffer '(126 127))))

;; <a id="nesting"></a>*nesting codec*
;; ---

;; As mentioned earlier, you can nest codec. This one is a vector of two previously defined codec,
;; one frame, and one map.
(defcodec nested-codec
  [vector-bytes-codec {:foo "bar"} byte-frame map-bytes-codec])

;; We can parse this complicated structure just as we did our simple codec.
(expect (to-byte-buffer '(123 124 125 126 127)) (first (encode nested-codec [[123 124]
                                                                             {:foo "bar"}
                                                                             125
                                                                             {:first 126 :second 127}])))

;; To my mind this is one of the most powerful aspects of Gloss. We can build really very complicated codec
;; from simple composite, well-tested parts. And when you throw in pre-encode and post-decode transforms, as we
;; will in a moment, it's a very powerful, expressive DSL.
(expect
  [[123 124]
   {:foo "bar"}
   125
   {:first 126 :second 127}]
  (decode nested-codec (to-byte-buffer '(123 124 125 126 127))))

;; &nbsp;
;;
;; <a id="strings"></a>*string frames*
;; ---

;; These primitives are all fine and well, but if we want to parse HTTP headers, we need to consume strings.

;; Luckily defining a stream of encoded text is simple in Gloss. The keyword argument specifies a character encoding,
;; all valid character encodings are supported.
(defcodec string-codec
  (string :utf-8 ))

;; For that codec to be any use we need to able to mix it with others, so we need to limit the string.
;; Gloss provides for either fixed length or delimiters. We're going to focus solely on delimiters.

;; Here's a string codec which is delimited by any of three character sequences. Any byte-sequence can be used
;; as a delimiter.
(defcodec dlm-string-codec
  (string :utf-8 :delimiters ["x" "xx" \y]))

;; When we decode a buffer with that codec, the text prior to the delimiter is extracted.
(expect
  "derek"
  (decode dlm-string-codec (to-byte-buffer "derekx")))
(expect
  "derek"
  (decode dlm-string-codec (to-byte-buffer "derekxx")))
(expect
  "derek"
  (decode dlm-string-codec (to-byte-buffer "dereky")))

;; We can include the delimiter in the extracted text with the argument:

;;     :strip-delimiters? false
(defcodec dlm-inclusive-codec
  (string :utf-8 :delimiters ["x" "xx" \y] :strip-delimiters? false))

;; It's easy to overlook a subtle but important fact, Gloss not only supports multiple delimiters, it supports
;; delimiters where one is a shorter version of another ("x" and "xx"). The largest delimiter is always matched.
;;
;; We utilize that fact when decoding HTTP headers where each header can be delimited by "\r\n" or "\r\n\r\n")
(expect
  "derekx"
  (decode dlm-inclusive-codec (to-byte-buffer "derekx")))
(expect
  "derekxx"
  (decode dlm-inclusive-codec (to-byte-buffer "derekxx")))
(expect
  "dereky"
  (decode dlm-inclusive-codec (to-byte-buffer "dereky")))

;; As with all codec, we can encode as well as decode. By default Gloss will encode with the first delimiter.
(expect
  (to-byte-buffer "derekx")
  (contiguous (encode dlm-string-codec "derek")))

;; If we want to select a specific delimiter while encoding, we can provide a function via the
;; :value->delimiter argument to our string codec which selects a delimiter at encoding time.
(defn choose-encoded-dlm [value]
  (condp = value
    "derek" ["x"]
    "kylie" ["xx"]
    ["y"]))

(defcodec dlm-selective-codec
  (string :utf-8 :delimiters ["x" "xx" \y] :value->delimiter choose-encoded-dlm))

;; Now when encoding we select a delimiter, rather than default.
(expect
  (to-byte-buffer "derekx")
  (contiguous (encode dlm-selective-codec "derek")))
(expect
  (to-byte-buffer "kyliexx")
  (contiguous (encode dlm-selective-codec "kylie")))
(expect
  (to-byte-buffer "kirstyy")
  (contiguous (encode dlm-selective-codec "kirsty")))

;; <a id="repeated"></a>*repeated frames*
;; ---

;; Gloss supports repeated sequences of frames.
(def rep-byte (repeated :byte ))

;; By default the encoded data is prefixed with a 32 bit integer which holds the number of repetitions
(expect
  (to-byte-buffer '(0 0 0 1 127))
  (first (encode rep-byte [127])))

;; Repeated sections always encode/decode vectors
(expect
  (to-byte-buffer '(0 0 0 2 126 127))
  (first (encode rep-byte [126 127])))
(expect
  [127]
  (decode rep-byte (to-byte-buffer '(0 0 0 1 127))))

;; Rather than use a prefix, we can provide termination delimiters.

;; Here we repeat delimited strings, the sequence also terminated by delimiter.
(def dlm-rep-string
  (repeated
    (string :utf-8 :delimiters ["\n"])
    :delimiters ["\0"]))

;; We could also specify a custom prefix or no prefix at all, in which case codec would have no bound and would
;; consume all the input.
(expect
  (to-byte-buffer "first\nsecond\n\0")
  (contiguous (encode dlm-rep-string ["first" "second"])))
(expect
  ["first" "second"]
  (decode dlm-rep-string (to-byte-buffer "first\nsecond\n\0")))

;; When decoding a repeated section, gloss scans first for the delimiter of the repeated section, then provides the
;; matched bytes to the inner frame to be consumed. Repeating the inner frame must end with all of the matched bytes
;; being completely consumed.
(expect
  Exception
  (decode dlm-rep-string (to-byte-buffer "first\nsecond\nleft-over-bytes\0")))

;; Similarly to the string frame, we can instruct gloss to leave the repeated-frame delimiter in the
;; matched bytes to be consumed by the inner frame. Again we use:

;;     :strip-delimiters? false
(def dlm-rep-string-x
  (repeated
    (string :utf-8 :delimiters ["\n" "\n\n"])
    :delimiters ["\n\n"]
    :strip-delimiters? false))

;; In this case Gloss matches the entire text as the repeated section then;
;;
;; - 'first\n' as the first string
;; - 'second\n' as the second
;; - 'third\n\n' as the third (always matches the largest delimiter)
(expect
  ["first" "second" "third"]
  (decode dlm-rep-string-x (to-byte-buffer "first\nsecond\nthird\n\n")))

;; &nbsp;
;;
;; <a id="transforms"></a>*pre-encode and post-decode transforms*
;; ---

;; When we compile a frame into a codec, we can supply functions which transform the input data before
;; encoding,
(defn transform-input [data]
  (vector (mapv name (keys data)) (vec (vals data))))

;; or the output data after decoding.
(defn transform-output [data]
  (zipmap (mapv keyword (first data)) (second data)))

;; This codec is defined with a frame which is vector based
(def trans-codec
  (compile-frame
    [[(string :utf-8 :delimiters [": "])] [(string :utf-8 :delimiters ["\n"])]]
    transform-input
    transform-output))

;; However its input and output are maps. The vector transformed.
(expect
  {:name "value"}
  (decode trans-codec (to-byte-buffer "name: value\n")))
(expect
  (to-byte-buffer "name: value\n")
  (contiguous (encode trans-codec {:name "value"})))

;; <a id="decoding"></a>*strict decoding*
;; ---

;; By default Gloss is strict, your codec is required to consume all of the bytes provided in the input, else
;; an exception is thrown.
(expect
  Exception
  (decode dlm-string-codec (to-byte-buffer "derekxkylie")))

;; However, Gloss can be instructed to disregard remaining bytes, acquire text.
(expect
  "derek"
  (decode dlm-string-codec (to-byte-buffer "derekxkylie") false))

;; &nbsp;
;;
;;
;;
;; ## An initial HTTP header codec
;; ---
;; &nbsp;

;; We'll start with a *very* simple definition of HTTP headers:
;;
;; - Repeated text of the format "name: value\r\n"
;; - The entire section ending with "\r\n\r\n"
;; - Headers of the same name can be repeated
(def ^:const rn "\r\n")
(def ^:const rnrn "\r\n\r\n")

;; Where a buffer with this text value (two headers, one repeated)
(def headers-buffer (to-byte-buffer "name: value\r\nname2: value2\r\nname2: value3\r\n\r\n"))

;; Will decode to this map structure
(def headers-data {:name ["value"]
                   :name2 ["value2" "value3"]})

;; <a id="init-definition"></a>*definition*
;; ---

;; First, a codec which matches a single header, decoding:
(defcodec header
  [(string :utf-8 :delimiters [": "]) [(string :utf-8 :delimiters [rn rnrn])]])

;; - "name: value\r\n" to  ["name" ["value"]] or;
(expect
  ["name" ["value"]]
  (decode header (to-byte-buffer "name: value\r\n")))

;; - "name: value\r\n\r\n" to the same
(expect
  ["name" ["value"]]
  (decode header (to-byte-buffer "name: value\r\n\r\n")))

;; As always, we can encode as well.
(expect
  (to-byte-buffer "name: value\r\n")
  (contiguous (encode header ["name" ["value"]])))

;; Then we repeat that codec. We don't consume the sequence delimiter, rather we allow our internal
;; frames to eat up those bytes.
(defcodec initial-headers
  (repeated header
    :delimiters [rnrn]
    :strip-delimiters? false))

;; Our test buffer is decoded to a fairly verbose vector.
(expect
  [["name" ["value"]] ["name2" ["value2"]] ["name2" ["value3"]]]
  (decode initial-headers headers-buffer))

;; <a id="init-transforms"></a>*tranforms*
;; ---

;; The next step is to create a post-decode transform method which will work that data into something more useable.
(defn output-to-map [data]
  (apply merge-with into (for [[k v] data] {(keyword k) v})))

;; We test our transforms at this level, confirming that transforming the output of our codec will give
;; the desired data structure.
(expect
  headers-data
  (output-to-map (decode initial-headers headers-buffer)))

;; Finally, we create the inverse of our post-decode method, breaking down input maps into a format that our
;; codec can accept. This will be applied as a pre-encode transform.
(defn input-to-vectors [data]
  (vec (apply concat (for [[k v] (vec data)]
                       (for [x v] [(name k) [x]])))))

;; Again we test our transform gives the desired output.
(expect
  (decode initial-headers headers-buffer)
  (input-to-vectors headers-data))

;; <a id="init-codec"></a>*the codec*
;; ---

;; A basic HTTP header codec
(def simple-headers
  (compile-frame
    (repeated header
      :delimiters [rnrn]
      :strip-delimiters? false)
    input-to-vectors
    output-to-map))

;; Decodes our sample data in a map correctly
(expect
  headers-data
  (decode simple-headers headers-buffer))

;; <a id="init-limitations"></a>*limitations*
;; ---

;; Unfortunately our encoding appends an extra "\r\n" to the buffer. This is because each header is delimited with rn,
;; and the entire sequence with rn rn.
(expect
  (to-byte-buffer "name: value\r\nname2: value2\r\nname2: value3\r\n\r\n\r\n")
  (contiguous (encode simple-headers headers-data)))

;; Gloss doesn't currently support specifying the delimiter to use when encoding a repeated sequence.
(defn specify-encoding-dlm [value]
  "\r\n")

;; We would like this instruct this sequence to use a single rn as its encoding delimiter, something that would
;; work for the string frame, but is currently a gloss limitation.
(def simple-headers-selective-dlm
  (compile-frame
    (repeated header
      :delimiters [rnrn]
      :strip-delimiters? false
      :value->delimiter specify-encoding-dlm)
  input-to-vectors
  output-to-map))

;; The :value->delimiter function is ignored.
(expect
  (to-byte-buffer "name: value\r\nname2: value2\r\nname2: value3\r\n\r\n\r\n")
  (contiguous (encode simple-headers-selective-dlm headers-data)))

;; Another limitation. Our codec fails if we have an empty set of headers.
(expect
  Exception
  (decode simple-headers (to-byte-buffer "\r\n\r\n")))

(run-all-tests)

