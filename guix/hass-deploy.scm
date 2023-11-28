(use-service-modules networking ssh)
(use-package-modules bootloaders)

(list (machine
       (operating-system (load "./hass.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "hass")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDjhWyUVTxopjREWDp9KY76c7qOz/XObo37/x9jzz9RT")
                       (system "x86_64-linux")
                       (user "sarg")))))
