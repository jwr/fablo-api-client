(ns fablo.api-client
  (:require [clj-http.client :as http]
            [clj-json.core :as json]
            [fablo.auth :as auth]
            [clojure.string :as string]))

;;; Bind those dynamically before using API functions
(def ^:dynamic *api-server* "localhost:8080")
(def ^:dynamic *api-customer* "dev")
(def ^:dynamic *api-auth-info* {:key-id "default" :key "example-api-key"})
                                        ; if I understand correctly: key-id is same as "keyname", and :key is most probably "secret"

;;; Our api-request is an http request wrapped in signature processing
(def api-request (auth/wrap-sign-request #'http/request))

;;; original
(comment(defmacro def-api-fn [name url-template & {:keys [request-method required-args optional-args url-template-args signature-required]}]
   (let [url-parameters (set url-template-args)]
     `(defn ~name [~@required-args & {:keys ~(vec (conj optional-args 'api-server 'api-customer 'api-auth-info))}]
        (let [request-map# (merge
                            {:url (str "http://"
                                       (string/join "/"
                                                    [(or ~'api-server ~'*api-server*) "api/2" (or ~'api-customer ~'*api-customer*)
                                                     (format ~url-template ~@url-template-args)]))
                             :method ~(or request-method :get)
                             :query-params (merge ~(into {} (map #(vector (str %) %) (remove url-parameters required-args)))
                                                  ~@(map (fn [x] `(if ~x {~(str x) (if (string? ~x) ~x (json/generate-string ~x))} {}))
                                                         optional-args))}
                            ~(when signature-required
                               `(when-let [auth-info# (or ~'api-auth-info ~'*api-auth-info*)]
                                  {:amazon-aws-auth [(or (:key-id auth-info#) "default") (:key auth-info#)]})))
              response# (api-request request-map#)
              result# (:status response#)]
          (when (and (>= result# 200) (< result# 300))
            (json/parse-string (:body response#))))))))

(defmacro def-api-fn [name url-template & {:keys [request-method required-args optional-args url-template-args signature-required]}]
  (let [url-parameters (set url-template-args)
        uri (gensym "uri-")]
    `(defn ~name [~@required-args & {:keys ~(vec (conj optional-args 'api-server 'api-customer 'api-auth-info))}]
       (let [~uri (string/join "/" ["/api/2" (or ~'api-customer ~'*api-customer*) (format ~url-template ~@url-template-args)]) ; TODO: extract or
             request-map# (merge
                           {:url (str "http://" (or ~'api-server ~'*api-server*) ~uri)
                            :method ~(or request-method :get)
                            :query-params (merge ~(into {} (map #(vector (str %) %) (remove url-parameters required-args)))
                                                 ~@(map (fn [x] `(if ~x {~(str x) (if (string? ~x) ~x (json/generate-string ~x))} {}))
                                                        optional-args))
                            :headers {"host" (or ~'api-server ~'*api-server*)}}
                           ~(when signature-required
                              `(when-let [auth-info# (or ~'api-auth-info ~'*api-auth-info*)]
                                 {:amazon-aws-auth [(or (:key-id auth-info#) "default") (:key auth-info#)]
                                  :uri ~uri})))
             ;; response# (api-request request-map#)
             ;; response# {:status 200, :body "{}" }
             response# (do #_(swank.core/break) (api-request request-map#))
             result# (:status response#)]
         (when (and (>= result# 200) (< result# 300)) ; checking result code might be unnecessary
           (json/parse-string (:body response#)))))))

;;; functions querying product base
(def-api-fn products-query "products/query" :optional-args [search-string start results category prefilter attributes return])
(def-api-fn product "products/id/%s" :required-args [id] :optional-args [return] :url-template-args [id])
(def-api-fn product-variants-query "products/id/%s/variants/query" :required-args [id] :optional-args [options return] :url-template-args [id])
(def-api-fn product-categories "products/categories" :optional-args [level])

(def-api-fn vendors "vendors")
(def-api-fn vendors-auth "vendors" :signature-required true)
(def-api-fn vendors-query "vendors/query" :optional-args [category])

(def-api-fn vendor-by-id "vendors/id/%s" :required-args [id] :url-template-args [id])

(def-api-fn vendor-categories "vendors/categories" :optional-args [level])

(def-api-fn special-offers "special-offers")
(def-api-fn special-offers-random "special-offers/random" :optional-args [number])

;;; functions modifying product base
;;; TODO: create function (or macro) to add signature in following functions
;;; and maybe modify existing macro to take into account admin path
(def-api-fn upload-db "admin/upload-db" :required-args [data-url] :optional-args [format check autoswitch]) ; does not work
(def-api-fn indexing-status "admin/indexing-status" :signature-required true)
(def-api-fn switch-db "admin/switch-db")
(def-api-fn config "admin/config")