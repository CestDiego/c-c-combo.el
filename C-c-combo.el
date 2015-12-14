;; Copyright (C) 2015  Diego Berrocal

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; This mode is super awesome and you should feel the same about it or else..
;;

(defvar C-c-combo--last-key  '(("timestamp" . 0)
                               ("key" . nil)
                               ("repeated?". nil))
  "Cons Cell, first item is timestamp second is the key")
(defvar C-c-combo--curr-cps  0
  "Moving Average Rate in Characters per second")
(defvar C-c-combo--target-wpm 60.0
  "Words per Minute Target Rate")
(defvar C-c-combo--target-cps 5.83
  "Characters per second Target Rate")
(defvar C-c-combo-check-timer nil
  "Timer that checks if we are over the target CPS")
(defvar C-c-combo--counter 0
  "Stores how many seconds you have been with acceptable wpm")

;; (defconst C-c-combo--files-path (file-name-directory load-file-name))
(setq C-c-combo--files-path "/home/jarvis/Projects/C-c-combo/")
(setq C-c-combo--announcer-files-path '())

(defun get-announcer-file-paths ()
  (unless (= 4 (length C-c-combo--announcer-files-path))
    (setq C-c-combo--announcer-files-path
          (mapcar (lambda (file)
                    (expand-file-name
                     (concat file ".wav")
                     C-c-combo--files-path))
                  '("fatality"
                    "flawless_spree"
                    "unstoppable"
                    "stop_this_modafoca")))))

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

;;;###autoload
(defun C-c-combo--play-sound-file (path)
  (if (eq system-type 'darwin)
      (start-process "*Messages*" nil "afplay" path)
    (start-process "*Messages*" nil "aplay" path)))

(defun C-c-combo--play-announcer-sound ()
  "This will end when our list ends."
  (let ((current-sound (pop C-c-combo--announcer-files-path)))
    (when current-sound
      (C-c-combo--play-sound-file current-sound))))

(defun C-c-combo--encourage-user ()
  (when (equal (mod C-c-combo--counter 5) 0)
    (C-c-combo--play-announcer-sound))
  (setq C-c-combo--counter (1+ C-c-combo--counter)))

(defun C-c-combo--check-if-over-target-rate ()
  (let ((repeated? (assoc-default "repeated?" C-c-combo--last-key))
        (computed-cps (C-c-combo--compute-cps)))
    (setq C-c-combo--curr-cps computed-cps)
    (if (and (not repeated?) (> computed-cps C-c-combo--target-cps))
        (C-c-combo--encourage-user)
      (get-announcer-file-paths)
      (setq C-c-combo--counter 0))))

(defun C-c-combo--compute-cps ()
  (let* ((now        (C-c-combo--current-time-in-seconds))
         (last-time  (assoc-default "timestamp" C-c-combo--last-key))
         (interval   (- now last-time))
         (exponent   (* interval C-c-combo--target-cps))
         (base       (- 1.0 (/ 1.0 C-c-combo--target-cps)))
         (decay-factor (expt base exponent))
         (new-rate (* C-c-combo--curr-cps decay-factor)))
    new-rate))

(defun C-c-combo--process ()
  (with-demoted-errors "Error while running C-c combo: %s"
    (when (and (this-command-keys)
               (derived-mode-p 'prog-mode))
      (let ((now      (C-c-combo--current-time-in-seconds))
            (key      (this-command-keys))
            (last-key (assoc-default "key" C-c-combo--last-key))
            (new-rate (+ 1 (C-c-combo--compute-cps))))
        (setq C-c-combo--curr-cps new-rate
              C-c-combo--last-key `(("timestamp" . ,now)
                                    ("key"       . ,key)
                                    ("repeated?" . ,(equal key last-key))))))))

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
      (C-c-combo--activate)
    (C-c-combo--deactivate)))

;;;###autoload
(define-minor-mode C-c-combo-mode
  "C-c-combo"
  :global t
  :lighter nil
  :keymap nil
  :init-value nil
  (C-c-combo--toggle))
