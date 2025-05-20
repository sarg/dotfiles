#!/usr/bin/env bb

(ns git2rss
  (:require [clojure.zip :as zip]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.data.zip.xml :refer [xml1-> text]]))

(defn commits-for [gitdir last-seen before]
  (->> (shell/with-sh-dir gitdir
         (shell/sh "git" "fetch" "origin" "master")
         (shell/sh "git" "log"
                   (str "--format=%H%n%an%n%cI%n%s")
                   (str "--before=" before "T00:00:00Z")
                   (str last-seen "..origin/master")))
       :out
       str/split-lines
       (partition 4)
       (map (partial zipmap '(:commit :author :date :subject)))))

(let* [filename (first *command-line-args*)

       now (-> (java.time.Instant/now)
               (.truncatedTo java.time.temporal.ChronoUnit/SECONDS))

       yesterday (-> now
                     (.atZone java.time.ZoneOffset/UTC)
                     .toLocalDate
                     (.minusDays 1))

       feed (-> filename
                io/reader
                (xml/parse :namespace-aware false
                           :skip-whitespace true)
                zip/xml-zip)

       ;; state from the input file
       last-seen (or (xml1-> feed :git2rss :last-seen text) "origin/master~10")
       gitdir (xml1-> feed :git2rss :dir text)
       commit-url (xml1-> feed :git2rss :commit-url text)
       range-url (xml1-> feed :git2rss :range-url text)]

  (def commits (commits-for gitdir last-seen (.plusDays yesterday 1)))
  (println (count commits) " new commits.")
  (when (empty? commits) (System/exit 0))

  (def latest-commit (:commit (first commits)))

  (def new-entry
    (xml/sexp-as-element
     [:entry
      [:title (str "Commits for " yesterday)]
      [:id (str "urn:git2rss:" yesterday)]
      [:link {:rel "alternate"
              :href (format range-url last-seen latest-commit)}]
      [:updated now]
      [:content {:type "xhtml"}
       [:div {:xmlns "http://www.w3.org/1999/xhtml"}
        [:p "Commits"
         [:ul
          (for [c commits]
            [:li [:a {:href (format commit-url (:commit c))} (:subject c)]])]]]]]))

  (spit filename
        (-> feed

            (xml1-> :updated)
            (zip/replace (xml/element :updated nil (.toString now)))
            zip/up

            (xml1-> :git2rss :last-seen)
            (zip/replace (xml/element :last-seen nil latest-commit))
            zip/up

            zip/rightmost
            (zip/insert-right new-entry)

            zip/root
            xml/indent-str)))
