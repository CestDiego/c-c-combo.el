(defvar C-c-combo--last-time  0
  "Last Keystroke TimeStamp")
(defvar C-c-combo--curr-cps  0
  "Moving Average Rate in Characters per second")
(defvar C-c-combo--target-wpm 50
  "Words per Minute Target Rate")
(defvar C-c-combo--target-cps 4.1666
  "Characters per second Target Rate")
(defvar C-c-combo-check-timer nil
  "Timer that checks if we are over the target CPS")

(defun C-c-combo-set-target-rate (rate)
  (setq C-c-combo--target-wpm rate
        C-c-combo--target-cps (C-c-combo--wpm-to-cps rate)))

(defun C-c-combo--current-time-in-seconds ()
  (string-to-number (format-time-string "%s.%3N" (current-time))))

(defun C-c-combo--wpm-to-cps (rate-wpm)
  (let* ((chars-in-word 5)
         (chars-per-min (* rate-wpm chars-in-word))
         (chars-per-sec (/ chars-per-min 60.0)))
    chars-per-sec))

(defun C-c-combo--check-if-over-target-rate ()
  (setq C-c-combo--curr-cps (C-c-combo--compute-cps))
  (if (> C-c-combo--curr-cps C-c-combo--target-cps)
      (message "Now")
    (message "not yet")))

(defun C-c-combo--compute-cps ()
  (let* ((now      (C-c-combo--current-time-in-seconds))
         (interval (- now C-c-combo--last-time))
         (exponent (* interval C-c-combo--target-cps))
         (base     (- 1.0 (/ 1.0 C-c-combo--target-cps)))
         (decay-factor (expt base exponent))
         (new-rate (* C-c-combo--curr-cps decay-factor)))
    new-rate))

(defun C-c-combo--process ()
  (with-demoted-errors "Error while running C-c combo: %s"
    (when (and (this-command-keys)
               (derived-mode-p 'prog-mode))
      (let ((now      (C-c-combo--current-time-in-seconds))
            (new-rate (+ 1 (C-c-combo--compute-cps))))
        ;; weird
        (setq C-c-combo--curr-cps new-rate
              C-c-combo--last-time now)))))

;;;###autoload
(defun C-c-combo--activate ()
  "Activates Combo mode."
  (setq C-c-combo-check-timer (run-at-time
                               "1 second"
                               1
                               'C-c-combo--check-if-over-target-rate))
  (add-hook 'pre-command-hook #'C-c-combo--process))

;;;###autoload
(defun C-c-combo--deactivate ()
  "Deactivates Combo mode, and deletes timer."
  (remove-hook 'pre-command-hook #'C-c-combo--process)
  (cancel-timer C-c-combo-check-timer))

;;;###autoload
(defun C-c-combo--toggle ()
  "Toggle Combo mode."
  (interactive)
  (if C-c-combo-mode
      (C-c-combo--deactivate)
    (C-c-combo--activate)))

;;;###autoload
(define-minor-mode C-c-combo-mode
  "C-c-combo"
  :lighter ""
  :init-value nil
  :global t
  (C-c-combo--toggle))
