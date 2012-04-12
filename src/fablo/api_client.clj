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
  (let [url-parameters (set url-template-args)
        uri (gensym "uri-")]
    `(defn ~name [~@required-args & {:keys ~(vec (conj optional-args 'api-server 'api-customer 'api-auth-info 'throw-exceptions))}]
       (let [~uri (string/join "/" ["/api/2" (or ~'api-customer ~'*api-customer*) (format ~url-template ~@url-template-args)])
             request-map# (merge
                           {:url (str "http://" (or ~'api-server ~'*api-server*) ~uri) ; TODO: extract "or"
                            :method ~(or request-method :get)
                            :query-params (merge ~(into {} (map #(vector (str %) %) (remove url-parameters required-args)))
                                                 ~@(map (fn [x] `(if ~x {~(str x) (if (string? ~x) ~x (json/generate-string ~x))} {}))
                                                        optional-args))
                            :headers {"host" (or ~'api-server ~'*api-server*)}
                            :throw-exceptions (or ~'throw-exceptions false)}
                           ~(when signature-required
                              `(when-let [auth-info# (or ~'api-auth-info ~'*api-auth-info*)]
                                 {:amazon-aws-auth [(or (:key-id auth-info#) "default") (:key auth-info#)]
                                  :uri ~uri})))]
         (select-keys (api-request request-map#) [:status :body])))))

(def-api-fn products-query "products/query" :optional-args [search-string start results category prefilter attributes return sort weak-sort])
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

(def-api-fn version "admin/version")
(def-api-fn status "admin/status")

;;; functions requiring authentication

;;; GET functions
(def-api-fn indexing-status "admin/indexing-status" :signature-required true)
(def-api-fn config "admin/config" :signature-required true)
(def-api-fn features "admin/features" :signature-required true)

;;; POST functions
(def-api-fn config-set "admin/config" :request-method :post, :signature-required true
  :required-args [config])
(def-api-fn switch-db "admin/switch-db" :request-method :post, :signature-required true)
(def-api-fn upload-db "admin/upload-db" :request-method :post, :signature-required true
  :required-args [data-url] :optional-args [format autoswitch])
(def-api-fn feedback "recommendations/feedback/%s" :request-method :post :signature-required true
  :required-args [feedback-type user products] :optional-args [session-id] :url-template-args [feedback-type])

;;; DELETE functions
(def-api-fn config-delete-key "admin/config/%s" :request-method :delete, :signature-required true
  :required-args [config-key] :url-template-args [config-key]) ; not really sure how this should work
(def-api-fn config-delete-subkey "admin/config/%s/%s" :request-method :delete, :signature-required true
  :required-args [config-key config-subkey] :url-template-args [config-key config-subkey])