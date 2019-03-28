(package! telega :recipe
;;   (:fetcher file
;;    :path "~/devel/ext/telega.el"
;;    :files (:defaults "README.md" "etc" "server")))
  (:fetcher github
   :repo "zevlg/telega.el"
   :branch "telega-with-inserters"
   :files (:defaults "README.md" "etc" "server")))
