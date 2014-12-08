;;; igv.el --- 

;; Copyright (C) 2014 Stefano Barbi
;; Author: Stefano Barbi <stefanobarbi@gmail.com>
;; Version: 0.1

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
;;
;;
;;

;;; Code:

(defgroup igv nil "Options to customize IGV interaction")

(defcustom igv-port
  60151
  "IGV port."
  :group 'igv
  :type 'integer)

(defcustom igv-host
  "127.0.0.1"
  "IGV address."
  :group 'igv :type 'string)

(defcustom igv-path
  "~/Bioinfo/IGV_2.1.23/igv.sh"
  "Full path specification of the igv.sh file."
  :group 'igv :type 'string)

(defvar igv-connection)

(setq igv-connection (make-network-process :name "igv" :host igv-host :service igv-port))

(defun igv-start ()
  "Start the IGV process."
  (interactive)
  (if (process-live-p "igv-process")
      (message "igv already started")
    (start-process "igv-process" "igv-output" igv-path)))

(defun  igv-check-connection ()
  "Check that IGV connection is alive."
  (if igv-connection
      (string= (process-status igv-connection) "open")
    nil))

(defun igv-connect ()
  "Connect Emacs to an existing IGV process."
  (interactive)
  (condition-case err
      (if (igv-check-connection)
	  igv-connection
	(setq igv-connection (make-network-process :name "igv" :host igv-host :service igv-port)))
    (file-error
     (message "A connection to the server cannot be opened. Please, check that IGV is running."))))

(defun igv-send (str)
  "Helper function to send messages to IGV.
STR is a string."
  (cond ((igv-check-connection)
	 (process-send-string igv-connection str)
	 (message str))
	(t (error "no connection established"))))

(defun igv-goto ()
  "Goto location at point."
  (interactive)
  (save-excursion
    (let ((cur (point))
          (beg nil)
          (end nil)
          (address nil))
      (progn
	;; move point to an hypotethical beginning of an address
	(skip-chars-backward "chrMXY0-9:,")
	(setq beg (point))
	(setq end (re-search-forward "chr[0-9XMY]+:[0-9,]+"))
	(when (>= (point) cur)
	  (setq address (buffer-substring-no-properties beg end))
	  (igv-send (format "goto %s\n" address)))))))

(defvar-local igv-re "\\(^\\|\\W+\\)\\(\\(chr\\)?[12MXY][0-9]?:[0-9,]+\\(-[0-9,]+\\)?\\)\\(\\W+\\|$\\)")

(defun igv-last-location ()
  "Search backward for a location."
  (interactive)
  (save-excursion
    ;; finish this tomorrow!
    (skip-chars-forward "chrMXY0-9:,")
    (re-search-backward igv-re (point-at-bol))
    (message (match-string-no-properties 2))))


;; (message (format "//variation[@vid=\'%s\']" str)))))))

(defun igv-load-file (filename)
  "Load a .bam file into IGV.
FILENAME is the path of bam file."
  (interactive "f")
  (igv-send (format "load %s\n" filename)))

(defun igv-sort ()
  "Sort current IGV track by position."
  (interactive)
  (igv-send "sort position"))

(defun igv-load-url (url)
  "Open a remote url.
URL is an address pointing to a .bam file."
  (interactive "r")
  (igv-send (format "load %s\n" url)))

(defun igv-load-url-at-point ()
  "Open url at point."
  (interactive)
  (let ((f (thing-at-point 'url)))
    (if f
	(igv-send (format "load %s\n" f))
      (error "malformed url"))))

(defun igv-load-file-at-point ()
  "Load the bam file at point into IGV."
  (interactive)
  (igv-load-file (thing-at-point 'filename)))

(defun igv-set-snapshot-directory (dir)
  "Set directory "
  (interactive "D")
  (igv-send (format "snapshotDirectory %s\n" dir)))

(defun igv-snapshot (filename)
  "Take a snapshot of the current portview.
FILENAME is the path where the snapshot will be saved."
  (interactive "F")
  (igv-send (format "snapshot %s\n" filename)))

(define-minor-mode igv-mode
  "toggle igv-mode"
  nil
  :keymap
  '(([C-c u] . igv-load-url-at-point)
    ([C-c g] . igv-goto))
  :group 'igv)

(provide 'igv)

;;; igv.el ends here
