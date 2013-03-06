{:user
 {:plugins [[lein-difftest "1.3.8"]
            [lein-marginalia "0.7.1"]
            [lein-pprint "1.1.1"]
            [lein-swank "1.4.4"]
            [lein-iclojure "1.1"]
            [clj-website/lein-template "0.1.0"]
            [lein-kibit "0.0.7"]
            [lein-bikeshed "0.1.0"]
            [lein-ritz "0.6.0"]
            [migratus-lein "0.1.0"]]
  :dependencies [[ritz/ritz-nrepl-middleware "0.6.0"]]
  :repl-options {:nrepl-middleware
                 [ritz.nrepl.middleware.javadoc/wrap-javadoc
                  ritz.nrepl.middleware.simple-complete/wrap-simple-complete]}}}
