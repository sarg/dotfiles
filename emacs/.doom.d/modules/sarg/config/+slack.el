(slack-register-team
 :name "intelgro"
 :default t
 :client-id (auth-source-pass-get "client-id" "Work/Horti/intelgro.slack.com")
 :client-secret (auth-source-pass-get "client-secret" "Work/Horti/intelgro.slack.com")
 ;; also token_legacy
 :token (auth-source-pass-get "token" "Work/Horti/intelgro.slack.com")
 :full-and-display-names t)

(after! alert
  (setq alert-default-style 'notifications))
