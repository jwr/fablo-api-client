(defproject fablo/api-client "1.0.1"
  :description "Fablo API client"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [clj-json "0.4.2"]
                 [commons-io/commons-io "2.0.1"]
                 [commons-codec "1.4"]
                 [clj-http "0.2.7"]     ; changed from 0.2.1, check if it works
                 [ring/ring-core "0.3.11"]]
  :dev-dependencies [[swank-clojure "1.3.2"]])
