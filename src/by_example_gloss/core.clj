;; ## The Basics
;; ---
;;
;; ***Gloss is a DSL for describing, encoding, and decoding, byte formats.***
;;
;; In Gloss, a byte format is called a *frame*, frames are compiled into *codec*, which allow you to:
;;
;; - encode a data structure into a ByteBuffer<sup>1</sup>; or,
;; - decode a ByteBuffer into a data structure.
;;
;; A frame is actually just a clojure data structure, nothing more. As we will see, that data structure might
;; include other codec, meaning a complicated codec can be built from several smaller, simple codec.
;; It's turtles all the way down.
;;
;; Defining a complex codec by combining its simpler composite parts also allows us to test (and transform if needed)
;; at a granular level.
;;
;; <sup>1 ByteBuffers technically, but we'll get to that later</sup>
(ns by-example-gloss.core
  (:require [gloss.core :refer [compile-frame defcodec ordered-map string repeated]]
            [gloss.io :refer [encode decode to-byte-buffer contiguous]]
            [expectations :refer [expect run-all-tests]]))

;; *frames*
;; ---

;; Gloss recognizes certain keywords as primitive data types, and you can define a frame using any combination of them.

;; We'll stick with :byte in our examples below, consider that inter-changeable with any of these.
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

;; Lets compile these to codec and encode/decode some data

;; *codec*
;; ---

(def byte-codec
  "We create a codec from our simplest frame by using compile-frame"
  (compile-frame byte-frame))

;; Then we use this codec to encode a byte into a buffer.
;;
;; - *to-byte-buffer* is utility method provided by Gloss.
;; - *encode* always results in a sequence of buffers
(expect (to-byte-buffer 127) (first (encode byte-codec 127)))

;; And the inverse, decoding a buffer into a byte value.
(expect 127 (decode byte-codec (to-byte-buffer 127)))

;; Gloss provides a macro shortcut for defining named codec, we use it to create one from our vector frame.
(defcodec vector-bytes-codec
  vector-bytes-frame)

;; Then we encode a vector into a buffer using our vector-bytes-codec
(expect (to-byte-buffer '(126 127)) (first (encode vector-bytes-codec [126 127])))

;; and decode that vector from the buffer using the same.
(expect [126 127] (decode vector-bytes-codec (to-byte-buffer '(126 127))))

;; Rinse and repeat with our map-bytes-codec. Beware, Gloss encodes map values in a consistent
;; but arbitrary order. In my example the bytes could have been writte as '(127 126). If you require a particular
;; serialization order use Gloss' ordered-map.
(defcodec map-bytes-codec
  {:first :byte
   :second :byte})

;; We can encode/decode pretty much any clojure data structure using the primitives that Gloss provides.
(expect (to-byte-buffer '(126 127)) (first (encode map-bytes-codec {:first 126
                                                                    :second 127})))

(expect {:first 126
         :second 127} (decode map-bytes-codec (to-byte-buffer '(126 127))))

;; *nesting codec*
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
(expect [[123 124]
         {:foo "bar"}
         125
         {:first 126 :second 127}] (decode nested-codec (to-byte-buffer '(123 124 125 126 127))))

;; *string frames*
;; ---

;; These primitives are all fine and well, but in my case I need to parse HTTP headers, which means consuming strings.

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
(expect "derek" (decode dlm-string-codec (to-byte-buffer "derekx")))
(expect "derek" (decode dlm-string-codec (to-byte-buffer "derekxx")))
(expect "derek" (decode dlm-string-codec (to-byte-buffer "dereky")))

;; We can include the delimiter in the extraced text with the argument:

;;     :strip-delimiters? false
(defcodec dlm-inclusive-codec
  (string :utf-8 :delimiters ["x" "xx" \y] :strip-delimiters? false))

;; It's easy to overlook a subtle but important fact, Gloss not only supports multiple delimiters, it supports
;; delimiters where one is a shorter version of another ("x" and "xx"). The largest delimiter is always matched.
;;
;; We utilize that fact when decoding HTTP headers where each header can be delimited by "\r\n" or "\r\n\r\n")
(expect "derekx" (decode dlm-inclusive-codec (to-byte-buffer "derekx")))
(expect "derekxx" (decode dlm-inclusive-codec (to-byte-buffer "derekxx")))
(expect "dereky" (decode dlm-inclusive-codec (to-byte-buffer "dereky")))

;; As with all codec, we can encode as well as decode. By default Gloss will encode with the first delimiter.
(expect (to-byte-buffer "derekx") (contiguous (encode dlm-string-codec "derek")))

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
(expect (to-byte-buffer "derekx") (contiguous (encode dlm-selective-codec "derek")))
(expect (to-byte-buffer "kyliexx") (contiguous (encode dlm-selective-codec "kylie")))
(expect (to-byte-buffer "kirstyy") (contiguous (encode dlm-selective-codec "kirsty")))

;; *a note on decoding*
;; ---

;; By default Gloss is strict, your codec is required to consume all of the bytes provided in the input, else
;; an exception is thrown.
(expect Exception (decode dlm-string-codec (to-byte-buffer "derekxkylie")))

;; However, Gloss can be instructed to disregard remaining bytes, acquire text.
(expect "derek" (decode dlm-string-codec (to-byte-buffer "derekxkylie") false))

;; *repeated codec*
;; ---

;; So far all of our codec have been fixed, but this is where things get a little more interesting.

;; Gloss allows you to repeat a frame. Either frames or codecs. By default a repeated frame

(run-all-tests)

