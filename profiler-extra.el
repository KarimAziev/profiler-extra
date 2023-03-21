;;; profiler-extra.el --- Utils for Native Profiler -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/profiler-extra
;; Version: 0.1.0
;; Keywords: tools lisp
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Utils for Native Profiler

;;; Code:


(require 'transient)

(defcustom profiler-extra-reset-after-report t
  "Non-nil means reset all profiling info after results are displayed.
Results are displayed with the `profiler-extra-toggle' command."
  :type 'boolean
  :group 'profiler-extra)


;;;###autoload
(defun profiler-extra-toggle ()
  "Run profiler if not running, otherwise show report."
  (interactive)
  (require 'profiler)
  (if (not (or (profiler-cpu-running-p)
               (profiler-memory-running-p)))
      (profiler-start
       (if (not (fboundp 'profiler-cpu-start))
           'mem
         (intern (completing-read
                  "Mode (default cpu): "
                  '("cpu" "mem" "cpu+mem")
                  nil t nil nil "cpu"))))
    (let ((wns (window-list)))
      (when (fboundp 'profiler-stop)
        (profiler-stop))
      (when (fboundp 'profiler-report)
        (profiler-report))
      (when (and (fboundp 'profiler-reset)
                 profiler-extra-reset-after-report)
        (profiler-reset))
      (when-let ((wnd (seq-find (lambda (w)
                                  (when (not (memq w wns))
                                    (with-selected-window w
                                      (derived-mode-p 'profiler-report-mode))))
                                (window-list))))
        (select-window wnd)
        (profiler-extra-buffer-menu)))))

(defun profiler-extra-description ()
  "Run profiler if not running, otherwise show report."
  (let ((cpu-p (and (fboundp 'profiler-cpu-running-p)
                    (profiler-cpu-running-p)))
        (memory-p (and (fboundp 'profiler-memory-running-p)
                       (profiler-memory-running-p))))
    (concat (concat "CPU " (if cpu-p (propertize "(on) "
                                                 'face 'success)
                             "(off)"))
            " + "
            (concat "Mem " (if memory-p (propertize "(on) "
                                                    'face 'success)
                             "(off)")))))

(defvar-local profiler-extra-expand-all-entries-expanded nil)

;;;###autoload
(defun profiler-extra-expand-all-entries ()
  "Show all entries in `profiler-report-mode'."
  (interactive)
  (when (and (fboundp 'profiler-report-expand-entry)
             (fboundp 'profiler-report-move-to-entry))
    (save-excursion
      (goto-char (point-min))
      (profiler-report-expand-entry t)
      (while (= (forward-line 1) 0)
        (profiler-report-move-to-entry)
        (profiler-report-expand-entry t))
      (setq profiler-extra-expand-all-entries-expanded t))))


;;;###autoload
(defun profiler-extra-toggle-tree-entry ()
  "Expand all entries."
  (interactive)
  (when (fboundp 'profiler-report-toggle-entry)
    (profiler-report-toggle-entry t)))


;;;###autoload
(defun profiler-extra-hide-all-entries ()
  "Hide all entries in `profiler-report-mode'."
  (interactive)
  (when (and (fboundp 'profiler-report-collapse-entry)
             (fboundp 'profiler-report-move-to-entry))
    (save-excursion
      (goto-char (point-min))
      (profiler-report-collapse-entry)
      (while (= (forward-line 1) 0)
        (profiler-report-move-to-entry)
        (profiler-report-collapse-entry))
      (setq profiler-extra-expand-all-entries-expanded nil))))


;;;###autoload
(defun profiler-extra-toggle-all-entries ()
  "Toggle showing all entries in `profiler-report-mode'."
  (interactive)
  (if profiler-extra-expand-all-entries-expanded
      (profiler-extra-hide-all-entries)
    (profiler-extra-expand-all-entries)))

(defun profiler-extra-inapt-fn ()
  "Return non nil if profiler is running."
  (and
   (featurep 'profiler)
   (fboundp 'profiler-running-p)
   (profiler-running-p)))

;;;###autoload
(defun profiler-extra-toggle-sorting ()
  "Toggle sorting in `profiler-report-mode'."
  (interactive)
  (let ((fn (if (and (boundp 'profiler-report-order)
                     (eq profiler-report-order 'ascending))
                'profiler-report-descending-sort
              'profiler-report-ascending-sort)))
    (funcall fn)))

;;;###autoload
(defun profiler-extra-change-sampling-interval ()
  "Change sampling profiler sampling interval."
  (interactive)
  (and
   (featurep 'profiler)
   (let ((value
          (string-trim (read-string
                        "Sampling interval in nanoseconds: "
                        (when (boundp 'profiler-sampling-interval)
                          (number-to-string profiler-sampling-interval))))))
     (setq value (when (string-match-p "^[0-9]+$" value)
                   (string-to-number value)))
     (unless (natnump value)
       (user-error "Must be positive integer "))
     (customize-set-value 'profiler-sampling-interval value)
     (when transient-current-command
       (transient--redisplay)))))

;;;###autoload (autoload 'profiler-extra-menu "profiler-extra.el" nil t)
(transient-define-prefix profiler-extra-menu ()
  "Transient menu for common Profiling commands."
  [[("l" profiler-extra-change-sampling-interval
     :description
     (lambda ()
       (concat "Sampling  "
               (if (boundp 'profiler-sampling-interval)
                   (propertize
                    (format "%d" profiler-sampling-interval)
                    'face
                    'transient-argument)
                 "")
               " ms"))
     :transient t)
    ("t" profiler-extra-toggle
     :description (lambda ()
                    (if (and (fboundp 'profiler-running-p)
                             (profiler-running-p))
                        (concat "Start  " (profiler-extra-description))
                      (concat "Stop and report " (profiler-extra-description)))))
    ("s" "Start Native Profiler..." profiler-start)
    ("h" "Show Profiler Report" profiler-report
     :inapt-if-not profiler-extra-inapt-fn)
    ("S" "Stop Native Profiler" profiler-stop
     :inapt-if-not
     profiler-extra-inapt-fn)]
   ["ELP"
    ("i" "Instrument Function..." elp-instrument-function)
    ("n" "Instrument Package..." elp-instrument-package)
    ("o" "Show Profiling Results" elp-results)
    ("r" "Reset Counters for Function..." elp-reset-function)
    ("e" "Reset Counters for All Functions" elp-reset-all)
    ("m" "Remove Instrumentation for All Functions" elp-restore-all)
    ("v" "Remove Instrumentation for Function..."
     elp-restore-function)]])


;;;###autoload (autoload 'profiler-extra-buffer-menu "profiler-extra.el" nil t)
(transient-define-prefix profiler-extra-buffer-menu ()
  "Transient menu for `profiler-report-mode'."
  :transient-non-suffix #'transient--do-stay
  [[("n" "Next Entry" profiler-report-next-entry :transient t)
    ("p" "Previous Entry" profiler-report-previous-entry :transient t)
    ""
    ("i" "Toggle Entry" profiler-report-toggle-entry
     :inapt-if-not
     (lambda ()
       (when (fboundp 'profiler-report-calltree-at-point)
         (profiler-report-calltree-at-point)))
     :transient t)
    ("<tab>" "Toggle tree" profiler-extra-toggle-tree-entry
     :inapt-if-not
     (lambda ()
       (when (fboundp 'profiler-report-calltree-at-point)
         (profiler-report-calltree-at-point)))
     :transient t)
    ("<backtab>" "Toggle all" profiler-extra-toggle-all-entries
     :transient t)
    ""
    ("j" "Find Entry" profiler-report-find-entry :inapt-if-not
     (lambda ()
       (when (fboundp 'profiler-report-calltree-at-point)
         (profiler-report-calltree-at-point))))
    ("d" "Describe Entry" profiler-report-describe-entry :inapt-if-not
     (lambda ()
       (when (fboundp 'profiler-report-calltree-at-point)
         (profiler-report-calltree-at-point))))
    ("C" "Show Calltree" profiler-report-render-calltree :inapt-if-not
     (lambda ()
       (when (boundp 'profiler-report-reversed)
         profiler-report-reversed)))
    ("B" "Show Reversed Calltree" profiler-report-render-reversed-calltree
     :inapt-if
     (lambda ()
       (when (boundp 'profiler-report-reversed)
         profiler-report-reversed)))
    ("s" profiler-extra-toggle-sorting
     :description
     (lambda ()
       (format "Toggle sorting (%s)" (if (boundp 'profiler-report-order)
                                         profiler-report-order
                                       "none")))
     :transient t)]
   [("=" "Compare Profile..." profiler-report-compare-profile)
    ("w" "Write Profile..." profiler-report-write-profile)
    ("r" profiler-extra-change-sampling-interval
     :description
     (lambda ()
       (concat "Sampling  "
               (if (boundp 'profiler-sampling-interval)
                   (propertize
                    (format "%d" profiler-sampling-interval)
                    'face
                    'transient-argument)
                 "")
               " ms"))
     :transient t)
    ("t" profiler-extra-toggle
     :description (lambda ()
                    (if (and (fboundp 'profiler-running-p)
                             (profiler-running-p))
                        (concat "Start  " (profiler-extra-description))
                      (concat "Stop and report " (profiler-extra-description)))))
    ("a" "Start Profiler" profiler-start :inapt-if profiler-running-p)
    ("P" "Stop Profiler" profiler-stop :inapt-if-not profiler-running-p)
    ("e" "New Report" profiler-report :inapt-if-not profiler-running-p)]])

(defvar profiler-extra-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-h ?") #'profiler-extra-buffer-menu)
    (define-key map (kbd ".") #'profiler-extra-buffer-menu)
    (define-key map (kbd "C-.") #'profiler-extra-buffer-menu)
    (define-key map (kbd "s") #'profiler-extra-toggle-sorting)
    (define-key map (kbd "<tab>") #'profiler-extra-toggle-tree-entry)
    (define-key map (kbd "<backtab>") #'profiler-extra-toggle-all-entries)
    map))


;;;###autoload
(define-minor-mode profiler-extra-buffer-mode
  "Add commands to `profiler-report-mode'."
  :lighter " profiler+"
  :keymap profiler-extra-map)

(provide 'profiler-extra)
;;; profiler-extra.el ends here