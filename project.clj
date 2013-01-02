(defproject fablo/api-client "1.2.1"
  :main fablo.api-client-main
  :description "Fablo API client"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [clj-json "0.5.0"]
                 [commons-io/commons-io "2.0.1"]
                 [commons-codec "1.4"]
                 [clj-http "0.3.6"]
                 [ring/ring-core "0.3.11"]
                 [clojopts "0.3.2"]]
  :dev-dependencies [[swank-clojure "1.4.2"]])
