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

;; There is a lot of confusion around what needs to be percent-encoded for AWS and what should not be. Most descriptions
;; are unfortunately imprecise. The best description seems to be at
;; http:/docs.aws.amazon.comĄmazonSimpleDBłatest/DeveloperGuide/HMACAuth.html, stating:
;;
;; * Do not URL encode any of the unreserved characters that RFC 3986 defines. These unreserved characters are A-Z, a-z,
;;   0-9, hyphen ( -), underscore ( _), period ( .), and tilde ( ~).
;; * Percent encode all other characters with %XY, where X and Y are hex characters 0-9 and uppercase A-F.
;; * Percent encode the space character as %20 (and not +, as common encoding schemes do).
;;
;; This means we cannot use java.net.URLEncoder.encoder, nor ring.util.codec/url-encode, nor ring.util.codec/form-encode.
;;
;; This is an area where most software seems to be buggy. RFC3986 is imprecise, and how you encode a string depends on
;; where that string is used. What this all means is that there is no such thing as "URL encoding".
;;
;; Here are the differences between various encodings:
;;
;; (import [java.net URLEncoder])
;; (URLEncoder/encode "a b+c~-" "UTF-8")
;; "a+b%2Bc%7E-"
;;
;; (ring.util.codec/form-encode "a b+c~-")
;; "a+b%2Bc%7E-"
;;
;; (ring.util.codec/url-encode "a b+c~-")
;; "a%20b+c~-"
;;
;; (ring.util.codec/url-encode-for-aws "a b+c~-")
;; "a%20b%2Bc~-"
;;
;; The last one is what AWS requires. We call it 'url-encode-with-aws-semantics', because even though it is a strict
;; reading of section 2.3 of RFC3986 (any character that isn't listed as Unreserved should be encoded), most software
;; does not agree.

(defn url-encode-with-aws-semantics
  "Returns the url-encoded (as per section 2.3 of RFC3986) version of the given string, using either a specified
  encoding or UTF-8 by default."
  [unencoded & [encoding]]
  (string/replace
    unencoded
    #"[^A-Za-z0-9_~.-]+"
    #(ring.util.codec/percent-encode % encoding)))

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
     [false (pr-str "Signature verification failed, got " signature)]

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
            cleaned-req (dissoc req :amazon-aws-auth :uri :query-params)] ; :amazon-aws-auth :uri are only temporary keys,
                                        ; query-params is reattached later if necessary
        (cond
         (= (:method req) :post)
         (client (assoc cleaned-req :body (http/generate-query-string query-params)
                   :content-type "application/x-www-form-urlencoded"))

         :else                           ; default handling is get
         (client (assoc cleaned-req :query-params query-params))))
      (client req))))
