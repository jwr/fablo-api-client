(ns fablo.auth
  (:require [clojure.string :as string]
            [ring.util.codec :as codec]
            [clj-http.client :as http])
  (:import [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]))

(defn- hmac [algorithm data key]
  (when key
    (let [signing-key (SecretKeySpec. (.getBytes key) algorithm)
          mac (Mac/getInstance algorithm)
          encoder (sun.misc.BASE64Encoder.)]
      (.init mac signing-key)
      (string/trim-newline (.encodeBuffer encoder (.doFinal mac (.getBytes data)))))))

(def ^:private hmac-sha1 (partial hmac "HmacSHA1"))

(def ^:private hmac-sha256 (partial hmac "HmacSHA256"))

(defmacro ^:private replace-all
  ([str] str)
  ([str k v] `(string/replace ~str ~k ~v))
  ([str k v & args] `(replace-all (string/replace ~str ~k ~v) ~@args)))

;; Java's url-encoder has a bit different semantics than the one
;; mandated by AWS, so we provide a translation layer:
(defn url-encode-with-aws-semantics [s]
  (replace-all (codec/url-encode s)
               "*" "%2A"
               "%7E" "~"
               "+" "%20"))

(defn canonicalize-query-string [req]
  (let [names-and-values (remove #(= (first %) "Signature") (:params req))
        sorted-nv-pairs (sort-by first names-and-values)
        canonicalized-pairs (map (fn [[k v]] (str k "=" (url-encode-with-aws-semantics v))) sorted-nv-pairs)]
    (string/join "&" canonicalized-pairs)))

(defn string-to-sign [req]
  (string/join
   "\n"
   [(-> (or (:request-method req) (:method req)) name .toUpperCase)
    (or ((or (:headers req) {}) "host") "")
    (:uri req)
    (canonicalize-query-string req)]))

(defn check-signature
  "Check signature according to Amazon AWS specs. Takes a ring request and a key (string) as parameters, returns a
  vector with true/false indicating whether authentication succeeded and a string with the reason for failure."
  [{:keys [params] :as req} key]
  (let [version (params "SignatureVersion")
        method (params "SignatureMethod")
        key-id (params "AWSAccessKeyId")
        signature (params "Signature")]
    (assert key)
    (cond
     (not (and version method key-id signature))
     [false "Missing SignatureVersion, SignatureMethod, AWSAccessKeyId or Signature"]

     (not (#{"HmacSHA1" "HmacSHA256"} method))
     [false "Unknown signature method, accepted methods are HmacSHA1 and HmacSHA256"]

     (not= version "2")
     [false "Unsupported signature version, only version 2 is supported"]

     (not= signature (hmac method (string-to-sign req) key))
     [false (pr-str "Signature verification failed, got " signature " ; should be:" (hmac method (string-to-sign req) key))]

     true
     [true "Signature verification succesful"])))

(defn wrap-sign-request
  "Closure returning function, responsible for proper requests to Fablo."
  [client]
  (fn [req]
    (if-let [[key-id key] (:amazon-aws-auth req)]
      (let [params (merge {"SignatureVersion" "2"
                            "SignatureMethod" "HmacSHA256"
                            "AWSAccessKeyId" key-id}
                           (:query-params req))
            query-params (assoc params "Signature" (hmac-sha256 (string-to-sign (assoc req :params params)) key))
            cleaned-req (dissoc req :amazon-aws-auth :uri)] ; :amazon-aws-auth :uri are only temporary keys, they should not be send!
        (cond
         (= (:method req) :post)
         (client (assoc cleaned-req :body (http/generate-query-string query-params)
                   :content-type "application/x-www-form-urlencoded"))

         true                           ; default handling is get
         (client (assoc cleaned-req :query-params query-params))))
      (client req))))
