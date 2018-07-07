(slack-register-team
 :name "intelgro"
 :default t
 :client-id (auth-source-pass-get "client-id" "Sites/intelgro.slack.com")
 :client-secret (auth-source-pass-get "client-secret" "Sites/intelgro.slack.com")
 :token (auth-source-pass-get "token" "Sites/intelgro.slack.com")
 :full-and-display-names t)
