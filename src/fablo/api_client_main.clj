(ns fablo.api-client-main
  (:require [fablo.api-client :as client]
            [clojopts.core :as clojopts]
            [clj-json.core :as json]
            [clojure.java.io :as io])
  (:gen-class))

(def +fablorc-filename+ ".fablorc")

(defn- format-response [r]
  (format "Response status: %d \nResponse body: %s" (:status r) (:body r)))

(defmacro with-customer-params [params raw & body]
  `(let [parsed-params# ~params]
     (binding [client/*api-server* (str (parsed-params# "host") (if (get parsed-params# "port") (str ":" (parsed-params# "port")) ""))
               client/*api-customer* (parsed-params# "customer")
               client/*api-auth-info* {:key-id (parsed-params# "key"), :key (parsed-params# "secret")}]
       ~@body)))

(defn -main
  "Main function. Takes function with parameters as main parameter in format:
  api-function first-required-parameter-values second-required-parameter-values :optional-parameter-name optional-parameter-value
  e.g. upload-db http://fablo/data/opff/shopify.json :format OPFF :autoswitch 1
  By default api access parameters (customer name, secret and so on) are taken from ~/.fablorc file, which is in exactly the same format as fabloctl.
  Option -c specifies other customer than default in aforementioned file, while -p allows to pass them as parameter. 
  In that case ~/.fablorc is not even read.
  If user prefers returning of raw response from server passing -r parameter does just that."
  [& argv]
  (let [{:keys [params customer] :as opts}
        (clojopts/clojopts "fablo-api-client" argv
                           (clojopts/with-arg params p "Authentication parameters in JSON."
                             :type :str)
                           (clojopts/with-arg customer c "Fablorc customer. Ignored if 'params' paramter provided"
                             :type :str)
                           (clojopts/no-arg raw r "Returns raw response."))
        api-fn-params (:clojopts/more opts)
        api-fn (conj (map #(if (.startsWith % ":") (keyword (subs % 1)) %) (rest api-fn-params))
                     (symbol (str "fablo.api-client/" (first api-fn-params))))
        return-raw-response (contains? opts :raw)
        fablorc-path (io/file (get (System/getenv) "HOME") +fablorc-filename+)
        parsed-params (if params (json/parse-string params)
                          (do
                            (assert (.exists fablorc-path) "Neither params were provided, nor ~/.fablorc file exists.")
                            (with-open [r (io/reader fablorc-path)]
                              (let [fablorc-params-map (first (json/parsed-seq r))]
                                (if customer
                                  (do
                                    (assert (fablorc-params-map customer) "Provided customer does not exist in ~/.fablorc.")
                                    (fablorc-params-map customer))
                                  (fablorc-params-map "default"))))))]
    (assert api-fn-params "No query provided.")
    (with-customer-params parsed-params return-raw-response
      (let [r (eval api-fn)]
        (println (if return-raw-response r (format-response r)))))))