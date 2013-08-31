{:user {:plugins [[lein-datomic "0.2.0"]
                  [lein-ritz "0.7.0"]
                  [lein-bikeshed "0.1.3"]
                  [lein-try "0.3.0"]
                  [lein-exec "0.3.1"]
                  [lein-vanity "0.1.0"]
                  [jonase/eastwood "0.0.2"]
                  [lein-kibit "0.0.8"]
                  [com.aphyr/prism "0.1.1"]
                  [lein-marginalia "0.7.1"]
                  [lein-cloverage "1.0.2"]]
        :datomic {:install-location "~/code/datomic-free-0.8.4138"}
        :aliases {"eval" ["run" "-m" "clojure.main/main" "-e"]}
        :dependencies [[ritz/ritz-nrepl-middleware "0.7.0"]
                       [alembic "0.2.0"]
                       [com.cemerick/pomegranate "0.2.0"]
                       [com.aphyr/prism "0.1.1"]
                       [jark "0.4.3"]]}}

;; Documentation

;; lein try [clj-time "0.5.1"]
;; lein try clj-time
