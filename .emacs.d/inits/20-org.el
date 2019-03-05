(defun ladicle/task-clocked-time ()
  "Return a string with the clocked time and effort, if any"
  (interactive)
  (let* ((clocked-time (org-clock-get-clocked-time))
         (h (truncate clocked-time 60))
         (m (mod clocked-time 60))
         (work-done-str (format "%d:%02d" h m)))
    (if org-clock-effort
        (let* ((effort-in-minutes
                (org-duration-to-minutes org-clock-effort))
               (effort-h (truncate effort-in-minutes 60))
               (effort-m (truncate (mod effort-in-minutes 60)))
               (effort-str (format "%d:%02d" effort-h effort-m)))
          (format "%s/%s" work-done-str effort-str))
      (format "%s" work-done-str))))

(use-package org-pomodoro
  :after org-agenda
  :custom
  (org-pomodoro-ask-upon-killing t)
  (org-pomodoro-format "%s")
  (org-pomodoro-short-break-format "%s")
  (org-pomodoro-long-break-format  "%s")
  :custom-face
  (org-pomodoro-mode-line ((t (:foreground "#ff5555"))))
  (org-pomodoro-mode-line-break   ((t (:foreground "#50fa7b"))))
  :hook
  (org-pomodoro-started . (lambda () (notifications-notify
                                      :title "org-pomodoro"
                                      :body "Let's focus for 25 minutes!"
                                      :app-icon "~/.emacs.d/img/001-food-and-restaurant.png")))
  (org-pomodoro-finished . (lambda () (notifications-notify
                                       :title "org-pomodoro"
                                       :body "Well done! Take a break."
                                       :app-icon "~/.emacs.d/img/004-beer.png")))
  :config
  :bind (:map org-agenda-mode-map
              ("p" . org-pomodoro)))
