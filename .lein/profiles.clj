{:user
 {:plugins [[lein-bikeshed "0.1.0"]
            [lein-catnip "0.5.1"]
            [lein-difftest "1.3.8"]
            [lein-exec "0.3.0"]
            [lein-iclojure "1.1"]
            [lein-kibit "0.0.7"]
            [lein-marginalia "0.7.1"]
            [lein-pprint "1.1.1"]
            [lein-protobuf "0.3.1"]
            [lein-swank "1.4.4"]
            [lein-ritz "0.6.0"]]
  :dependencies [[ritz/ritz-nrepl-middleware "0.6.0"]]
  :repl-options {:nrepl-middleware
                 [ritz.nrepl.middleware.javadoc/wrap-javadoc
                  ritz.nrepl.middleware.simple-complete/wrap-simple-complete]}}}
