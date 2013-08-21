;; ## Gloss Extensions
(ns by-example-gloss.ext
  (:use [gloss.core.structure]
        [clojure.walk]
        [gloss.core protocols]
        [gloss.data primitives bytes])
  (:require [clojure.zip :as z])
  (:import [java.nio
            ByteBuffer]))

(defn- compile-frame- [f]
  (cond
    (map? f) (convert-map (zipmap (keys f) (map compile-frame- (vals f))))
    (sequential? f) (convert-sequence (map compile-frame- f))
    :else f))

;; Allow a pre-decoder method when compiling a codec.
(defn compile-frame-ext
  ([frame pre-encoder pre-decoder post-decoder]
    (let [codec (compile-frame frame)
          read-codec (compose-callback
                       codec
                       (fn [x b]
                         [true (post-decoder x) b]))]
      (reify
        Reader
        (read-bytes [_ b]
          (read-bytes read-codec (pre-decoder b)))
        Writer
        (sizeof [_]
          (sizeof codec))
        (write-bytes [_ buf v]
          (write-bytes codec buf (pre-encoder v)))))))