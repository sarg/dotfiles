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
  :custom
  (telega-chat-show-deleted-messages-for '(not saved-messages))
  (telega-root-show-avatars nil)
  (telega-root-default-view-function #'telega-view-compact)
  (telega-chat-button-width 30)
  (telega-filter-custom-one-liners '(custom))
  (telega-filter-custom-show-folders nil)
  (telega-filters-custom
   '(("lng_filters_type_no_archived" . archive)
     ("Unread" . (and main unread))))
  (telega-chat-show-avatars nil)
  (telega-chat-use-date-breaks nil)
  (telega-chat-input-markups '("markdown2" nil))
  (telega-animation-play-inline nil)
  (telega-sticker-size '(8 . 48))
  (telega-emoji-use-images nil)
  (telega-emoji-custom-alist '((":s:" . "¯\\_(ツ)_/¯")))
  (telega-msg-ignore-predicates '(telega-msg-special-p))

  :config
  (add-hook! telega-root-mode (cd telega-directory))
  (add-hook! telega-chat-mode (cd telega-directory))

  ;; disable not used minor modes
  (global-telega-url-shorten-mode 1)
  (telega-patrons-mode -1)
  (telega-active-locations-mode -1)
  (telega-active-video-chats-mode -1)
  (telega-contact-birthdays-mode -1)
  (telega-active-stories-mode -1)

  (setq telega-hide-previews 't)
  ;; show previews for photo/video webpages
  (advice-add #'telega-ins--link-preview :before-while
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

  (advice-add #'telega-chatbuf--sponsored-messages-fetch
              :override #'ignore)

  (advice-add
   'telega-logout
   :before-while (lambda (&rest r) (y-or-n-p "Really log out from current account?")))

  (set-face-attribute 'telega-msg-heading nil :background nil)
  (set-popup-rule! (regexp-quote "*Telega Instant View*")
    :ignore t)

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :side 'left
    :size 36
    :ttl nil
    :quit t
    :select t)

  (after! dired
    (load "telega-dired-dwim.el"))

  (when (modulep! :editor evil)
    (map!
     (:map telega-msg-button-map
           "k" nil
           "l" nil))))
