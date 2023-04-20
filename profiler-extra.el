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
Results are displayed with the commands `profiler-extra-report' and
`profiler-extra-toggle-and-report'."
  :type 'boolean
  :group 'profiler-extra)

(defvar profiler-extra-last-mode nil)
(defvar profiler-extra-recording nil)

(defun profiler-extra-description ()
  "Return description with status of profiler."
  (let ((cpu-p (and (fboundp 'profiler-cpu-running-p)
                    (profiler-cpu-running-p)
                    (propertize "(on)"
                                'face 'success)))
        (memory-p (and (fboundp 'profiler-memory-running-p)
                       (profiler-memory-running-p)
                       (propertize "(on)"
                                   'face 'success))))
    (concat (propertize "CPU" 'face 'transient-heading)
            " "
            (or cpu-p
                (propertize "(off) "
                            'face 'transient-inactive-value))
            " "
            (propertize "Mem" 'face 'transient-heading)
            " "
            (or memory-p (propertize "(off) "
                                     'face 'transient-inactive-value)))))

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
  "Change sampling profiler interval."
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


;;;###autoload
(defun profiler-extra-start-or-stop ()
  "Change sampling profiler sampling interval."
  (interactive)
  (require 'profiler)
  (cond ((or (and (fboundp 'profiler-cpu-running-p)
                  (profiler-cpu-running-p))
             (and (fboundp 'profiler-memory-running-p)
                  (profiler-memory-running-p)))
         (when (fboundp 'profiler-stop)
           (profiler-stop)))
        (t (setq profiler-extra-last-mode
                 (or (when transient-current-command
                       (let* ((args (transient-args
                                     transient-current-command))
                              (cpu (car (member "cpu" args)))
                              (mem (car (member "mem" args)))
                              (val (string-join (remove nil (list cpu mem)) "+")))
                         (if (string-empty-p val)
                             nil
                           val)))
                     profiler-extra-last-mode
                     (completing-read
                      "Mode (default cpu): "
                      '("cpu+mem" "cpu" "mem")
                      nil t nil nil "cpu")
                     "cpu+mem"))
           (profiler-start (intern profiler-extra-last-mode))))
  (when transient-current-command
    (transient--redisplay)))

;;;###autoload
(defun profiler-extra-toggle-and-report ()
  "Run profiler if not running, otherwise show report."
  (interactive)
  (require 'profiler)
  (cond ((or (and (fboundp 'profiler-cpu-running-p)
                  (profiler-cpu-running-p))
             (and (fboundp 'profiler-memory-running-p)
                  (profiler-memory-running-p)))
         (setq profiler-extra-recording nil)
         (when (fboundp 'profiler-stop)
           (profiler-stop))
         (profiler-extra-report))
        (t (setq profiler-extra-last-mode
                 (or (when transient-current-command
                       (let* ((args (transient-args
                                     transient-current-command))
                              (cpu (car (member "cpu" args)))
                              (mem (car (member "mem" args)))
                              (val (string-join (remove nil (list cpu mem)) "+")))
                         (if (string-empty-p val)
                             nil
                           val)))
                     profiler-extra-last-mode
                     (completing-read
                      "Mode (default cpu): "
                      '("cpu+mem" "cpu" "mem")
                      nil t nil nil "cpu")
                     "cpu+mem"))
           (setq profiler-extra-recording t)
           (profiler-start (intern profiler-extra-last-mode)))))


;;;###autoload
(defun profiler-extra-report ()
  "Report and reset profiler if `profiler-extra-reset-after-report' is non nil."
  (interactive)
  (require 'profiler)
  (when (fboundp 'profiler-report)
    (profiler-report))
  (setq profiler-extra-recording nil)
  (when (and (fboundp 'profiler-reset)
             profiler-extra-reset-after-report)
    (profiler-reset)))


;;;###autoload (autoload 'profiler-extra-elp "profiler-extra.el" nil t)
(transient-define-prefix profiler-extra-elp ()
  "ELP menu."
  ["Emacs Lisp Profiler"
   ("i" "Instrument Function..." elp-instrument-function)
   ("n" "Instrument Package..." elp-instrument-package)
   ""
   ("s" "Show Profiling Results" elp-results)
   ("r" "Reset Counters for Function..." elp-reset-function :transient t)
   ("e" "Reset Counters for All Functions" elp-reset-all :transient t)
   ""
   ("K" "Remove Instrumentation for All Functions" elp-restore-all
    :transient t)
   ("v" "Remove Instrumentation for Function..."
    elp-restore-function :transient t)]
  [("l" "Native Profiler" profiler-extra-menu)])


;;;###autoload (autoload 'profiler-extra-menu "profiler-extra.el" nil t)
(transient-define-prefix profiler-extra-menu ()
  "Native profiler menu."
  :transient-non-suffix #'transient--do-exit
  :value
  (lambda ()
    (or
     (when profiler-extra-last-mode
       (remove ""
               (split-string
                profiler-extra-last-mode "+" t)))
     (list "cpu" "mem")))
  [:if-derived
   profiler-report-mode
   [:description
    "Profiler Report"
    ("n" "Next Entry" profiler-report-next-entry :transient t)
    ("p" "Previous Entry" profiler-report-previous-entry :transient
     t)
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
    ("d" "Describe Entry" profiler-report-describe-entry
     :inapt-if-not
     (lambda ()
       (when (fboundp 'profiler-report-calltree-at-point)
         (profiler-report-calltree-at-point))))
    ("C" "Show Calltree" profiler-report-render-calltree
     :inapt-if-not
     (lambda ()
       (when (boundp 'profiler-report-reversed)
         profiler-report-reversed)))
    ("B" "Show Reversed Calltree"
     profiler-report-render-reversed-calltree
     :inapt-if
     (lambda ()
       (when (boundp 'profiler-report-reversed)
         profiler-report-reversed)))
    ("S" profiler-extra-toggle-sorting
     :description
     (lambda ()
       (format "Toggle sorting (%s)" (if (boundp
                                          'profiler-report-order)
                                         profiler-report-order
                                       "none")))
     :transient t)]
   [("=" "Compare Profile..." profiler-report-compare-profile)
    ("w" "Write Profile..." profiler-report-write-profile)]]
  [["Native Profiler"
    ("m" "Memory" "mem")
    ("c" "CPU" "cpu")
    ("l" profiler-extra-change-sampling-interval
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
     :transient t)]
   [:description
    profiler-extra-description
    ("RET"
     profiler-extra-toggle-and-report
     :description (lambda ()
                    (if (and (fboundp 'profiler-running-p)
                             (profiler-running-p))
                        (concat "Stop, report and exit")
                      (concat "Start and exit"))))
    ("s" profiler-extra-start-or-stop
     :description (lambda ()
                    (if (and (fboundp 'profiler-running-p)
                             (profiler-running-p))
                        "Stop"
                      "Start"))
     :transient t)
    ("h" profiler-extra-report
     :description (lambda ()
                    (propertize "Report"
                                'face
                                (if (or (and (fboundp 'profiler-running-p)
                                             (profiler-running-p))
                                        (bound-and-true-p profiler-memory-log)
                                        (bound-and-true-p profiler-cpu-log))
                                    'transient-enabled-suffix
                                  'transient-inapt-suffix))))]]
  [("e" "Emacs Lisp Profiler" profiler-extra-elp)]
  (interactive)
  (if (not profiler-extra-recording)
      (transient-setup 'profiler-extra-menu)
    (profiler-extra-report)))

(defvar profiler-extra-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-h ?") #'profiler-extra-menu)
    (define-key map (kbd ".") #'profiler-extra-menu)
    (define-key map (kbd "C-.") #'profiler-extra-menu)
    (define-key map (kbd "s") #'profiler-extra-toggle-sorting)
    (define-key map (kbd "<tab>") #'profiler-extra-toggle-tree-entry)
    (define-key map (kbd "<backtab>") #'profiler-extra-toggle-all-entries)
    map))

(easy-menu-add-item nil '("emacs-lisp")
                    ["Profiler extra" profiler-extra t])


;;;###autoload
(define-minor-mode profiler-extra-buffer-mode
  "Add commands to `profiler-report-mode'."
  :lighter " profiler+"
  :keymap profiler-extra-map)

(provide 'profiler-extra)
;;; profiler-extra.el ends here