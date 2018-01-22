;; Copyright © 2015-2018  Bozhidar Batsov, Artur Malabarba and CIDER contributors

;; Author: Juan E. Maya <jmayaalv@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides coarse-grained interactive profiling support.

;;; Code:

(require 'cider-client)

(defconst cider-profile-buffer "*cider-profile*")

;;;###autoload
(defun cider-profile-samples (&optional query)
  "Displays current max-sample-count. If optional QUERY is
  specified, set max-sample-count and display new value."
  (interactive "P")
  (cider-ensure-op-supported "set-max-samples")
  (cider-ensure-op-supported "get-max-samples")
  (if (not (null query))
      (cider-nrepl-send-request
       (let ((max-samples (if (numberp query) query '())))
         (message (format "query: %s" max-samples))
         (list "op" "set-max-samples" "max-samples" max-samples))
       (nrepl-make-response-handler
        (current-buffer)
        (lambda (_buffer value)
          (let ((value (if (zerop (length value)) "unlimited" value)))
            (message (format "max-sample-count is now %s." value))))
        '()
        '()
        '()))
    (cider-nrepl-send-request
     (list "op" "get-max-samples")
     (nrepl-make-response-handler
      (current-buffer)
      (lambda (_buffer value)
        (let ((value (if (zerop (length value)) "unlimited" value)))
          (message (format "max-sample-count is now %s." value))))
      '()
      '()
      '())))
  query)

;;;###autoload
(defun cider-profile-var-profiledp (query)
  "Displays the profiling status of var under point. Prompts for
var if none under point or prefix argument is present."
  (interactive "P")
  (cider-ensure-op-supported "is-var-profiled")
  (cider-read-symbol-name
   "Report profiling status for var: "
   (lambda (sym)
     (let ((ns (cider-current-ns)))
       (cider-nrepl-send-request
        (list "op" "is-var-profiled"
              "ns" ns
              "sym" sym)
        (nrepl-make-response-handler
         (current-buffer)
         (lambda (_buffer value)
           (cond ((equal value "profiled")
                  (message (format "profiling %s/%s." ns sym)))
                 ((equal value "unprofiled")
                  (message (format "not profiling %s/%s." ns sym)))
                 ((equal value "unbound")
                  (message (format "%s/%s is not bound." ns sym)))))
         '()
         '()
         '()))))
   query))

;;;###autoload
(defun cider-profile-ns-toggle (&optional query)
  "Toggle profiling for the ns associated with optional QUERY or,
  if nil current ns."
  (interactive "P")
  (cider-ensure-op-supported "toggle-profile-ns")
  (let ((ns (if query (completing-read
                       "Toggle profiling for ns: " (cider-sync-request:ns-list))
              (cider-current-ns))))
    (cider-nrepl-send-request
     (list "op" "toggle-profile-ns"
           "ns" ns)
     (nrepl-make-response-handler
      (current-buffer)
      (lambda (_buffer value)
        (cond ((equal value "profiled")
               (message (format "profiling %s." ns)))
              ((equal value "unprofiled")
               (message (format "not profiling %s." ns)))))
      '()
      '()
      '())))
  query)

;;;###autoload
(defun cider-profile-toggle (query)
  "Toggle profiling for the given QUERY.
Defaults to the symbol at point.  With prefix arg or no symbol at
point, prompts for a var."
  (interactive "P")
  (cider-ensure-op-supported "toggle-profile")
  (cider-read-symbol-name
   "Toggle profiling for var: "
   (lambda (sym)
     (let ((ns (cider-current-ns)))
       (cider-nrepl-send-request
        (list "op" "toggle-profile"
              "ns" ns
              "sym" sym)
        (nrepl-make-response-handler
         (current-buffer)
         (lambda (_buffer value)
           (cond ((equal value "profiled")
                  (message (format "profiling %s/%s." ns sym)))
                 ((equal value "unprofiled")
                  (message (format "not profiling %s/%s." ns sym)))
                 ((equal value "unbound")
                  (message (format "%s/%s is not bound." ns sym)))))
         '()
         '()
         '()))))
   query))

(defun cider-profile-display-stats (stats-response)
  (let ((table (nrepl-dict-get stats-response "err")))
    (if cider-profile-buffer
        (let ((buffer (cider-make-popup-buffer cider-profile-buffer)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t)) (insert table)))
          (display-buffer buffer)
          (let ((window (get-buffer-window buffer)))
            (set-window-point window 0)
            (select-window window)
            (fit-window-to-buffer window)))
      (cider-repl-emit-err-output (cider-current-repl-buffer) table))))

;;;###autoload
(defun cider-profile-summary (query)
  "Display a summary of currently collected profile data."
  (interactive "P")
  (cider-ensure-op-supported "profile-summary")
  (cider-profile-display-stats
   (nrepl-send-sync-request (list "op" "profile-summary")))
  query)

;;;###autoload
(defun cider-profile-var-summary (query)
  "Display profile data for var under point, prompting if none or
prefix argument given."
  (interactive "P")
  (cider-ensure-op-supported "profile-var-summary")
  (cider-read-symbol-name
   "Profile-summary for var: "
   (lambda (sym)
     (cider-profile-display-stats
      (nrepl-send-sync-request
       (list "op" "profile-var-summary"
             "ns" (cider-current-ns)
             "sym" sym)))))
  query)

;;;###autoload
(defun cider-profile-clear (query)
  "Clear any collected profile data."
  (interactive "P")
  (cider-ensure-op-supported "clear-profile")
  (cider-nrepl-send-request
   (list "op" "clear-profile")
   (nrepl-make-response-handler
    (current-buffer)
    (lambda (_buffer value)
      (when (equal value "cleared")
        (message "cleared profile data.")))
    '()
    '()
    '()))
  query)

;;;###autoload
(define-minor-mode cider-profile-mode
  "Toggle cider-profile-mode."
  nil
  nil
  `((,(kbd "C-c =") . cider-profile-toggle)
    (,(kbd "C-c _") . cider-profile-clear)
    (,(kbd "C-c -") . cider-profile-summary)
    (,(kbd "C-c M--") . cider-profile-var-summary)
    (,(kbd "C-c +") . cider-profile-ns-toggle)
    (,(kbd "C-c M-=") . cider-profile-var-profiledp)
    (,(kbd "C-c M-+") . cider-profile-samples)))

(provide 'cider-profile)

;;; cider-profile.el ends here
