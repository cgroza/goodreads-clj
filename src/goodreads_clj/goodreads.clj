(ns goodreads
  (:require [clj-http.client :as http]
            [clojure.java.io :as io])
  (:require [clojure.string :as string])
  (:require [clojure.xml :as xml])
  (:require [clojure.data.json :as json])
  (:require [clojure.java.io :as io])
  (:import [java.net URLEncoder]))

(def dev-key "XtAXRVJUr17UOCTqp8eDNw")
(def secret "UBdjKXrUX11U1OqbF6OBb5DdknQGGXJfklIjGjzjIu8")
(def url-review-by-title "https://www.goodreads.com/book/title.xml")
(def url-review-stats-isbns "https://www.goodreads.com/book/review_counts.json")
(def devkey-map {"key" dev-key})

(defn parametric [data]
  (string/join "&" (map (fn [[k v]] (str k "=" v)) data)))

(defn url-get [url data]
  (:body (http/get (str url "?" (parametric data)))))

(defn get-reviews-widget-by-title [title author & rating]
  (->> (let [data (assoc devkey-map "title" title "author" (string/replace author #"\s+" "+"))]
         (if (not-empty rating) (assoc data "rating" (str (first rating))) data))
       (url-get url-review-by-title)
       .getBytes
       java.io.ByteArrayInputStream.
       xml/parse))

(defn get-review-stats-isbns[isbns]
  (->> (assoc devkey-map "isbns" (string/join "," isbns))
       (url-get url-review-stats-isbns)
       json/read-str))
