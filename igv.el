;; Author: Stefano Barbi <stefanobarbi@gmail.com>

(defgroup igv nil "options to customize IGV interaction")
(defcustom igv-port
  60151
  "sets the port where the IGV service is listening"
  :group 'igv :type 'integer)

(defcustom igv-host
  "127.0.0.1"
  "sets the host where the IGV service is listening"
  :group 'igv :type 'string)

(defcustom igv-path
  "~/Bioinfo/IGV_2.1.23/igv.sh"
  "full path specification of the igv.sh file"
  :group 'igv :type 'string)

(setq igv-connection (make-network-process :name "igv" :host igv-host :service igv-port))

(defun igv-start ()
  "start the igv process"
  (interactive)
  (if (process-live-p "igv-process")
      (message "igv already started")
    (start-process "igv-process" "igv-output" igv-path)
    )
  )

(defun  igv-check-connection ()
  "check that igv-connection is alive"
  (if igv-connection
      (string= (process-status igv-connection) "open")
    nil)
  )

(defun igv-connect ()
  "connect emacs to an existing igv process\n"
  (interactive)
  (condition-case err
      (if (igv-check-connection)
	  igv-connection
	(setq igv-connection (make-network-process :name "igv" :host igv-host :service igv-port)))
    (file-error
     (message "A connection with the server cannot be opened. Please, check that IGV is up and running."))
    )
)

(defun igv-send (str)
  (cond ((igv-check-connection)
	 (process-send-string igv-connection str)
	 (message str)
	 ))
  )

(defun igv-goto ()
  "goto location at point"
  (interactive)
  (save-excursion
    (let* ((cur (point))
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
	  (igv-send (format "goto %s\n" address))))))
)

(defun igv-load-file (filename)
  "tell igv to load a .bam file"
  (interactive "f")
  (igv-send (format "load %s\n" filename)))

(defun igv-load-file-at-point ()
  "tell igv to load the file at point"
  (interactive)
  (let* ((f (thing-at-point 'filename)))
    (igv-send (format "load %s\n" f))))

(defun igv-set-snapshot-directory (dir)
  (interactive "D")
  (igv-send (format "snapshotDirectory %s\n" dir)))

(defun igv-snapshot (filename)
  "take a snapshot of the current portview"
  (interactive "F")
  (igv-send (format "snapshot %s\n" filename)))
