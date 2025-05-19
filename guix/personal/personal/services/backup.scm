(define-module (personal services backup)
  #:use-module (gnu services)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu services backup)
  #:use-module (personal services supercron)
  #:use-module (srfi srfi-1)

  #:export (restic-backup-service-type)
  #:re-export (restic-backup-configuration
               restic-backup-job))

(define (supercron-backup-task job)
  (let ((name (restic-backup-job-name job))
        (schedule (restic-backup-job-schedule job)))
    #~(make <task>
        #:name (string-append "backup-" #$name)
        #:environment '(#$(string-append "HOME=" (getenv "HOME")))
        #:schedule (list (apply make <interval> #$schedule))
        #:arguments '(#$(restic-backup-job-program job)))))

(define restic-backup-service-type
  (service-type (name 'restic-backup)
                (extensions
                 (list
                  (service-extension home-profile-service-type
                                     restic-backup-service-profile)
                  (service-extension supercron-service-type
                                     (lambda (conf)
                                       (map supercron-backup-task
                                            (restic-backup-configuration-jobs conf))))))
                (compose concatenate)
                (extend
                 (lambda (config jobs)
                   (restic-backup-configuration
                    (inherit config)
                    (jobs (append (restic-backup-configuration-jobs config)
                                  jobs)))))
                (default-value (restic-backup-configuration))
                (description
                 "This service configures supercron tasks for running backups
with restic.")))
