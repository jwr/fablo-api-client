(ns fablo.api-client
  (:require [clj-http.client :as http]
            [clj-json.core :as json]
            [fablo.auth :as auth]
            [clojure.string :as string]))

;;; Bind those dynamically before using API functions
(def ^:dynamic *api-server* "localhost:8080")
(def ^:dynamic *api-customer* "dev")
(def ^:dynamic *api-auth-info* {:key-id "default" :key "example-api-key"})

;;; Our api-request is an http request wrapped in signature processing
(def api-request (auth/wrap-sign-request #'http/request))

(defmacro def-api-fn [name url-template & {:keys [request-method required-args optional-args url-template-args signature-required]}]
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
           (json/parse-string (:body response#)))))))

(def-api-fn products-query "products/query" :optional-args [search-string start results category prefilter attributes return])
(def-api-fn product "products/id/%s" :required-args [id] :optional-args [return] :url-template-args [id])
(def-api-fn product-variants-query "products/id/%s/variants/query" :required-args [id] :optional-args [options return] :url-template-args [id])
(def-api-fn product-categories "products/categories" :optional-args [level])

(def-api-fn vendors "vendors")
(def-api-fn vendors-query "vendors/query" :optional-args [category])

(def-api-fn vendor-by-id "vendors/id/%s" :required-args [id] :url-template-args [id])

(def-api-fn vendor-categories "vendors/categories" :optional-args [level])

(def-api-fn promotions "promotions")
(def-api-fn promotions-random "promotions/random" :optional-args [number])
