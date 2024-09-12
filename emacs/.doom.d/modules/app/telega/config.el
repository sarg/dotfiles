;; -*- lexical-binding:t -*-
;; (defalias 'sarg/telega-ins--message (symbol-function 'telega-ins--message))

;; restore custom inserters
;; (cond
;;  (saved-msgs-p
;;   (telega-ins--date (plist-get msg :date)))
;;  (private-chat-p
;;   (telega-ins (if (telega-msg-by-me-p msg) "-> " "<- ")))
;;  ((not channel-post-p)
;;   (telega-ins "<" (telega-user-title sender 'full-name) "> ")))

(defun sarg/telega-get-code ()
  "Extract confirmation code from latest message of telegram user BenderBot."
  (interactive)
  (let* ((chat (cl-find "BenderBot" telega--ordered-chats
                        :test (lambda (needname chat)
                                (string= (telega-chat-title chat)
                                         needname))))

         (text (telega--tl-get chat :last_message :content :text :text))

         (re-list (list (rx (or "Code: " "Kod " "Код " "код: "
                                "Пароль для подтвердения - ")
                            (group (1+ digit))))))

    (set-text-properties 0 (length text) nil text)
    (when (-any (lambda (re) (string-match re text)) re-list)
      (kill-new (match-string 1 text)))))

(defun telega-set-my-name (first last)
  (interactive "sFirst name: \nsLast name: ")
  (telega-server--call (list :@type "setName" :first_name first :last_name last)))

(use-package! telega
  :commands (telega)

  ;; This fixes the issue with closing buffer when it is visible in other window.
  ;; The logic is as follows:
  ;;   kill-this-buffer is advised with doom*switch-to-fallback-buffer-maybe
  ;;   this function delegates to original kill-this-buffer if the buffer isn't doom-real-buffer-p
  ;;   then doom|protect-visible-functions in kill-buffer-query-functions prevents the close.
  ;; So, to fix this make telega.el chat buffers real.
  :hook (telega-chat-mode . doom-mark-buffer-as-real-h)

  ;; :init
  ;; (setq telega-inserter-for-msg-button #'sarg/telega-ins--message)

  :config
  (add-hook! telega-root-mode (cd telega-directory))
  (add-hook! telega-chat-mode (cd telega-directory))

  (setq
   telega-root-show-avatars nil
   telega-chat-show-avatars nil
   telega-chat-use-date-breaks nil
   telega-chat-input-markups '("markdown2" nil)
   telega-animation-play-inline nil
   telega-sticker-size '(8 . 48)
   telega-emoji-custom-alist '((":s:" . "¯\\_(ツ)_/¯")))

  (advice-add #'telega-ins--sponsored-message :override #'ignore)

  (setq telega-hide-previews 't)
  ;; show previews for photo/video webpages
  (advice-add #'telega-ins--webpage :before-while
              (lambda (msg &rest args)
                (let ((ht (telega--tl-get msg :content :web_page :type)))
                  (and telega-hide-previews (-contains? '("video" "photo") ht)))))

  ;; insert just content of a message, no headers needed
  ;; -> message text
  ;;
  ;; instead of
  ;;
  ;; XX Username
  ;; XX message text                                                    08.04.20✓
  (advice-add #'telega-msg--pp
              :override
              (apply-partially #'telega-button--insert 'telega-msg))

  (advice-add
   'telega-logout
   :before-while (lambda (&rest r) (y-or-n-p "Really log out from current account?")))

  (set-popup-rule! (regexp-quote "*Telega Instant View*")
    :ignore t)

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :side 'left
    :size 0.25
    :ttl nil
    :quit t
    :select t)

  (after! dired
    (load "telega-dired-dwim.el"))

  (when (featurep! :editor evil)
    (map!
     (:map telega-msg-button-map
           "k" nil
           "l" nil))))
