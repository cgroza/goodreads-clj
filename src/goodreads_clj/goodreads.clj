(ns goodreads
  (:require [clj-http.client :as http])
  (:require [clojure.string :as string])
  (:require [clojure.xml :as xml])
  (:require [clojure.data.json :as json])
  (:import [java.net URLEncoder]))

;; Developers authentication
(def dev-key "XtAXRVJUr17UOCTqp8eDNw")
(def secret "UBdjKXrUX11U1OqbF6OBb5DdknQGGXJfklIjGjzjIu8")

;; Goodreads API URLs
(def root-url "https://www.goodreads.com")
(def url-review-title "/book/title.xml")
(def url-review-stats-isbns "/book/review_counts.json")
(def url-id-of-isbns "/book/isbn_to_id")
(def url-info-author-id "/author/show.xml")
(def url-author-id-name "/api/author_url")
(def url-author-books-page "/author/list.xml")
(def url-events-in-area "/event/index.xml")
(def url-find-group "/group/search.xml")
(def url-info-group-id "/group/show/")
(def url-listopia-book-id "/list/book/")
;; Default parameters
(def default-data {"key" dev-key})

(defn parametric [data]
  (string/join "&" (map (fn [[k v]] (str k "=" v)) data)))

(defn url-get [api-url data]
  (http/get (str root-url api-url "?" (parametric (conj default-data data)))))

(defn parse-xml-body [response]
  (->> response :body .getBytes java.io.ByteArrayInputStream. xml/parse))

(defn encode-name [name]
  (string/replace name #"\s+" "+"))

(defn id-author-name [author]
  (->>
   (url-get (str url-author-id-name "/" (encode-name author)) {})
   (parse-xml-body)))

(defn id-of-isbns [isbns]
  (->
   (url-get url-id-of-isbns {"isbn" (string/join "," isbns)})
   :body
   (string/split  #",")))

(defn info-author-id [id]
  (parse-xml-body (url-get url-info-author-id {"id" id})))

(defn author-books-page [id page]
  (parse-xml-body (url-get url-author-books-page {"id" id "page" (str page)})))

(defn reviews-widget-title [title author & rating]
  (->> (let [data {"title" title "author" (encode-name author)}]
         (if (not-empty rating) (assoc data "rating" (str (first rating))) data))
       (url-get url-review-title)
       parse-xml-body))

(defn review-stats-isbns[isbns]
  (->> {"isbns" (string/join "," isbns)}
       (url-get url-review-stats-isbns)
       :body
       json/read-str))

(defn events-in-area [latitude longitude country-code postal-code]
  (->> {"latitude" latitude
        "longitude" longitude
        "search[country_code]" country-code
        "search[postal_code]" postal-code}
       (url-get url-events-in-area)
       (parse-xml-body)))

(defn find-group
  ([name] (find-group name 1))
  ([name page]
   (->> (url-get url-find-group {"q" (encode-name name) "page" (str page)})
        parse-xml-body)))
(defn info-group-id [id sort order ]
  ;; sort: Field to sort topics by. One of 'comments_count', 'title', 'updated_at', 'views'
  ;; order: 'a' for ascending, 'd' for descending
  (->> (url-get (str url-info-group-id id ".xml") {"sort" sort "order" order})
       parse-xml-body))

(defn listopia-book-id [id]
  (->> (url-get (str url-listopia-book-id id ".xml"))
       parse-xml-body))
