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
;; - ***an initial HTTP header codec***
;;     - [definition](#init-definition)
;;     - [transforms](#init-transforms)
;;     - [the codec](#init-codec)
;;     - [limitations](#init-limitations)
;; - ***a better HTTP header codec***
;;     - [the codec](#better-codec)
;; - ***a complete HTTP header codec***
;;     - [the codec](#complete-codec)

;; &nbsp;
;; ## The Basics
;; ---

;; ***Gloss is a DSL for describing byte formats.***
;;
;; In Gloss, a byte format is called a *frame*, frames are compiled into *codec*, which allow you to:
;;
;; - encode a data structure into a ByteBuffer<sup>1</sup>; or,
;; - decode a ByteBuffer into a data structure.
;;
;; A frame is just a clojure data structure, that simple. That data structure might include other codec, meaning a
;; complicated codec can be built from several smaller, simple codec.
;;
;; Defining codec by their composite parts allows testing (and transformation if needed) at a granular level.
;;
;; <sup>1 technically Gloss encodes to a sequence of ByteBuffers</sup>
(ns by-example-gloss.core
  (:require [clojure.string :refer [trim
                                    lower-case]]
            [clojure.walk :refer [keywordize-keys
                                  stringify-keys]]
            [by-example-gloss.ext :refer [compile-frame-ext]]
            [gloss.core :refer [compile-frame
                                defcodec header
                                string
                                repeated
                                delimited-block]]
            [gloss.io :refer [encode
                              decode
                              to-byte-buffer
                              contiguous]]
            [expectations :refer [expect
                                  run-all-tests]]))

;; <a id="frames"></a>*frames*
;; ---

;; A frame can contain a number of different primitive data types.
;;
;; The examples below generally use :byte, consider that inter-changeable.
[:byte, :int16, :int32, :int64, :float32, :float64, :ubyte, :uint16, :uint32, :uint64 ]

(def byte-frame
  "A very simple frame, a single byte."
  :byte )

(def vector-bytes-frame
  "Frames are just clojure data structures, this frame is a vector of two bytes."
  [:byte :byte ])

(def map-bytes-frame
  "This frame contains the same data, but in map form rather than a vector."
  {:first :byte
   :second :byte})

(def map-bytes-frame-with-constant
  "Frames can have constant values which are not encoded, and always occur when decoded."
  {:first "constant-value"
   :second :byte})

;; <a id="codec"></a>*codec*
;; ---

(def byte-codec
  "A codec created from the simplest frame"
  (compile-frame byte-frame))

;; Can encode a byte into a buffer.
;;
;; - *to-byte-buffer* is utility method provided by Gloss.
;; - *encode* always results in a sequence of buffers
(expect
  (to-byte-buffer 127)
  (first (encode byte-codec 127)))

;; And the inverse, decode a buffer into a byte.
(expect
  127
  (decode byte-codec (to-byte-buffer 127)))

;; Gloss provides a codec defining macro, defcodec.
(defcodec vector-bytes-codec
  vector-bytes-frame)

;; A vector encoded into a buffer
(expect
  (to-byte-buffer '(126 127))
  (first (encode vector-bytes-codec [126 127])))

;; can be decoded from that buffer with the same codec.
(expect
  [126 127]
  (decode vector-bytes-codec (to-byte-buffer '(126 127))))

;; Rinse and repeat with the map-bytes-codec.
(defcodec map-bytes-codec
  {:first :byte
   :second :byte})

;; Gloss encodes map values in a consistent but arbitrary order. In this example the bytes could have
;; been written as '(127 126)
(expect
  (to-byte-buffer '(126 127))
  (first (encode map-bytes-codec {:first 126
                                  :second 127})))

;; If you require a particular serialization order use Gloss' ordered-map.
(expect
  {:first 126
   :second 127}
  (decode map-bytes-codec (to-byte-buffer '(126 127))))

;; <a id="nesting"></a>*nesting codec*
;; ---

;; A codec can be defined as a data structure which contains other codec.
(defcodec nested-codec
  [vector-bytes-codec {:foo "bar"} byte-frame map-bytes-codec])

;; Meaning more complex codec can be built from simpler parts.
(expect (to-byte-buffer '(123 124 125 126 127)) (first (encode nested-codec [[123 124]
                                                                             {:foo "bar"}
                                                                             125
                                                                             {:first 126 :second 127}])))

;; This can be advantageous if you want to test or transform at a granular level.
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

;; Aside from these primitives, Gloss supports parsing streams of text.

;; The first argument specifies a character encoding.
(defcodec unbound-codec
  (string :utf-8 ))

;; Gloss provides for either fixed length or delimited text. We're going to focus solely on delimited.

;; This string is delimited by any of three character sequences. Any byte-sequence can be used
;; as a delimiter.
(defcodec string-codec
  (string :utf-8 :delimiters ["x" "xx" \y]))

;; When applied to a buffer, the text prior to the delimiter is extracted.
(expect
  "derek"
  (decode string-codec (to-byte-buffer "derekx")))
(expect
  "derek"
  (decode string-codec (to-byte-buffer "derekxx")))
(expect
  "derek"
  (decode string-codec (to-byte-buffer "dereky")))

;; The delimiter can be included in the extracted text with:

;;     :strip-delimiters? false
(defcodec dlm-inclusive-codec
  (string :utf-8 :delimiters ["x" "xx" \y] :strip-delimiters? false))

(expect
  "derekx"
  (decode dlm-inclusive-codec (to-byte-buffer "derekx") false))

;; When decoding with multiple delimiters:

;; - the first delimiter is always matched; and,
(expect
  "dereky"
  (decode dlm-inclusive-codec (to-byte-buffer "derekyx") false))

;; - the longest delimiter is always matched
(expect
  "derekxx"
  (decode dlm-inclusive-codec (to-byte-buffer "derekxx")))

;; By default Gloss will encode with the first delimiter.
(expect
  (to-byte-buffer "derekx")
  (contiguous (encode string-codec "derek")))

;; To select a specific delimiter while encoding
(defn choose-encoded-dlm [value]
  (condp = value
    "derek" ["x"]
    "kylie" ["xx"]
    ["y"]))

;; provide a function via the :value->delimiter argument.
(defcodec dlm-selective-codec
  (string :utf-8 :delimiters ["x" "xx" \y] :value->delimiter choose-encoded-dlm))

;; Now the delimiter depends on the value being encoded.
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

;; Gloss supports repeating frames.
(def rep-byte (repeated :byte ))

;; By default encoded data is prefixed with a 32 bit integer which declares the number of repetitions.
(expect
  (to-byte-buffer '(0 0 0 1 127))
  (first (encode rep-byte [127])))

;; Repeated frames always encode/decode to vectors.
(expect
  (to-byte-buffer '(0 0 0 2 126 127))
  (first (encode rep-byte [126 127])))
(expect
  [127]
  (decode rep-byte (to-byte-buffer '(0 0 0 1 127))))

;; Repetition can be terminated by delimiter, rather than prefix.
(def dlm-rep-string
  (repeated
    (string :utf-8 :delimiters ["\n"])
    :delimiters ["\0"]))

;; Gloss also supports custom prefix or no prefix at all, the repeated frame would expect to consume all input.
(expect
  (to-byte-buffer "first\nsecond\n\0")
  (contiguous (encode dlm-rep-string ["first" "second"])))
(expect
  ["first" "second"]
  (decode dlm-rep-string (to-byte-buffer "first\nsecond\n\0")))

;; Gloss scans first for the delimiter of the repeated section, then provides the matched bytes to the inner frame
;; for consumption. All bytes must be consumed or an exception is thrown.
(expect
  Exception
  (decode dlm-rep-string (to-byte-buffer "first\nsecond\nleft-over-bytes\0")))

;; Both gloss/repeated and gloss/string support leaving the delimiter in the matched bytes.

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

;; When compiling a frame, we can supply functions that:

;; -  transform the input before encoding; and,
(defn transform-input [data]
  (first (stringify-keys data)))

;; - transform the output after decoding.
(defn transform-output [[k v]]
  {(keyword k) v})

;; This codec talks in vectors, but that is only an intermediary state.
(def trans-codec
  (compile-frame
    [(string :utf-8 :delimiters [": "]) (string :utf-8 :delimiters ["\n"])]
    transform-input
    transform-output))

;; The input to encode is transformed from a map
(expect
  (to-byte-buffer "name: value\n")
  (contiguous (encode trans-codec {:name "value"})))

;; and the output from decode back to a map.
(expect
  {:name "value"}
  (decode trans-codec (to-byte-buffer "name: value\n")))

;; <a id="decoding"></a>*strict decoding*
;; ---

;; Gloss is strict by default, codec are required to consume all input bytes.
(expect
  Exception
  (decode string-codec (to-byte-buffer "derekxkylie")))

;; However, Gloss can disregard remaining bytes, acquire result.
(expect
  "derek"
  (decode string-codec (to-byte-buffer "derekxkylie") false))

;; &nbsp;
;;
;;
;;
;; ## An initial HTTP header codec
;; ---
;; &nbsp;

;; A *very* simple definition of HTTP headers:
;;
;; - Repeated text of the format "name: value\r\n"
;; - The entire section ending with "\r\n\r\n"
(def ^:const rn "\r\n")
(def ^:const rnrn "\r\n\r\n")

;; A buffer with this text value
(def initial-buffer (to-byte-buffer "name: value\r\nname2: value2\r\n\r\n"))

;; will encode/decode from/to this map structure
(def initial-data {:name "value"
                   :name2 "value2"})

;; <a id="init-definition"></a>*definition*
;; ---

;; First, a codec which matches a single header, decoding:
(defcodec init-header
  [(string :utf-8 :delimiters [": "]) (string :utf-8 :delimiters [rn rnrn])])

;; - "name: value\r\n" to  ["name" "value"] or;
(expect
  ["name" "value"]
  (decode init-header (to-byte-buffer "name: value\r\n")))

;; - "name: value\r\n\r\n" to the same
(expect
  ["name" "value"]
  (decode init-header (to-byte-buffer "name: value\r\n\r\n")))

;; and encoding the data back to the intial buffer.
(expect
  (to-byte-buffer "name: value\r\n")
  (contiguous (encode init-header ["name" "value"])))

;; That header codec can be repeated. The repeated section leaves its delimiter in the matched bytes to be consumed
;; by the internal, repeated headers.
(defcodec initial-headers
  (repeated init-header
    :delimiters [rnrn]
    :strip-delimiters? false))

;; The buffer can be decoded into a vector.
(expect
  [["name" "value"] ["name2" "value2"]]
  (decode initial-headers initial-buffer))

;; <a id="init-transforms"></a>*tranforms*
;; ---

;; A post-decode transform works our output into a more practical form.
(defn output-to-map [data]
  (keywordize-keys (into {} data)))

(expect
  {:name "value"
   :name2 "value2"}
  (output-to-map [["name" "value"] ["name2" "value2"]]))

;; Similarly, a pre-encode transform breaks input into an acceptable form.
(defn input-to-vector [data]
  (vec (stringify-keys data)))

;; Meaning a map can be encoded into the simple header format.
(expect
  [["name" "value"] ["name2" "value2"]]
  (input-to-vector {:name "value"
                    :name2 "value2"}))

;; <a id="init-codec"></a>*the codec*
;; ---

;; A very basic HTTP header codec
(def simple-headers
  (compile-frame
    (repeated init-header
      :delimiters [rnrn]
      :strip-delimiters? false)
    input-to-vector
    output-to-map))

;; Decodes the sample data in a map correctly
(expect
  initial-data
  (decode simple-headers initial-buffer))

;; <a id="init-limitations"></a>*limitations*
;; ---

;; Unfortunately encoding emits the delimiter of each header, and the delimiter of the
;; repeated section, meaning the final written delimiter is wrong.
(expect
  (to-byte-buffer "name: value\r\nname2: value2\r\n\r\n\r\n")
  (contiguous (encode simple-headers initial-data)))

;; Gloss supports specifying an encoding delimiter for string frames, but not repeated sequences.
(defn specify-encoding-dlm [value]
  "\r\n")

;; Ideally gloss/repeated would allo an encoding-delimiter.
(def simple-headers-selective-dlm
  (compile-frame
    (repeated init-header
      :delimiters [rnrn]
      :encoding-delimiter rn ; <- encode this delimiter
      input-to-vector
      output-to-map)))

;; A final limitation, empty set of headers fails.
(expect
  Exception
  (decode simple-headers (to-byte-buffer rnrn)))

;; &nbsp;
;;
;;
;;
;; ## A slightly better HTTP header codec
;; ---
;; <sup>due to the limitations above, we're only concerned with decoding</sup>
;; &nbsp;

;; - Dont require a space after the colon separator.
(defcodec better-header
  [(string :utf-8 :delimiters [":"]) (string :utf-8 :delimiters [rn rnrn])])

;; - Repeated names combined into a single comma separated value.
;; - Names trimmed and lower-cased
;; - Values trimmed.
(defn output-to-merged-map [data]
  (apply merge-with #(str %1 "," %2)
    (map (fn [[k v]] {(keyword (-> k trim lower-case)) (trim v)}) data)))

(expect
  {:name "value"
   :name2 "value2,value3"}
  (output-to-merged-map [["name" "value"] ["name2" " value2"] ["name2" "value3 "]]))

;; <a id="better-codec"></a>*the codec*
;; ---

;; A slightly better HTTP header codec
(def better-headers
  (compile-frame
    (repeated better-header
      :delimiters [rnrn]
      :strip-delimiters? false)
    #(identity %)
    output-to-merged-map))

;; provides better HTTP header extraction.
(expect
  {:name "value"
   :name2 "value2,value3,value4"
   :name3 "value5"}
  (decode better-headers
    (to-byte-buffer (str
                      "name: value\r\n"
                      "name2:value2\r\n"
                      "name2:value3 \r\n"
                      "name3: value5 \r\n"
                      "name2:value4\r\n\r\n"))))

;; &nbsp;
;;
;;
;;
;; ## A complete HTTP header codec
;; ---
;; &nbsp;

;; - Http values can be folded over several lines
(def folded-buf
  (to-byte-buffer (str
                    "name: value\r\n"
                    "name2:value2\r\n"
                    " value3 \r\n"
                    "\tvalue3a \r\n"
                    "name3: value5 \r\n"
                    "name2:value4\r\n\r\n")))

;; - Where "\r\n " and "\r\n\t" should be parsed as " "
(def unfolded-buf
  (to-byte-buffer (str
                    "name: value\r\n"
                    "name2:value2 value3  value3a \r\n"
                    "name3: value5 \r\n"
                    "name2:value4 \r\n\r\n")))

;; - And eventually decoded into this structure
(def unfolded-data
  {:name "value"
   :name2 "value2 value3  value3a,value4"
   :name3 "value5"})

(def ^:const rn-space "\r\n ")
(def ^:const rn-tab "\r\n\t")
(def sp-buf (to-byte-buffer " "))
(def rnrn-buf (to-byte-buffer rnrn))

(defcodec part-unfold-codec
  (repeated
    (delimited-block [rn-space rn-tab rnrn] true)
    :delimiters [rnrn]
    :strip-delimiters? false))

(defn unfold [bufs]
  (if (= (first bufs) rnrn-buf)
    bufs
    (let [buf-seq (decode part-unfold-codec bufs)
          seq-size (count buf-seq)]
      (if (= seq-size 1)
        bufs
        (list (contiguous (interpose sp-buf (flatten (conj buf-seq rnrn-buf)))))))))

(expect unfolded-buf (first (unfold (seq [folded-buf]))))

;; <a id="complete-codec"></a>*the codec*
;; ---

;; A complete HTTP header codec
(def folding-headers
  (compile-frame-ext
    (repeated better-header
      :delimiters [rnrn]
      :strip-delimiters? false)
    #(identity %)
    unfold
    output-to-merged-map))

(expect
  unfolded-data
  (decode folding-headers folded-buf))

(run-all-tests)