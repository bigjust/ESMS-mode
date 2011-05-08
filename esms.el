(define-derived-mode esms-mode nil "ESMS"
  "Major mode for setting up a ESMS lineup")

(defvar roster-file nil)
(defvar teamsheet-file nil)

(defvar positions '("GK" "DF" "DM" "MF" "AM" "FW"))

(defmacro traverse-file (filename start at start-line &rest body)
  `(let ((moreLines t))
     (with-temp-buffer
       (insert-file-contents ,filename)
       (goto-line ,start-line)
       (let (( moreLines t))
	 (while moreLines
	   (setq current-line (split-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
	   ,@body
	   (setq moreLines (= 0 (forward-line 1))))))))

(defun get-unavailable-players ()
  (interactive)
  (setq unavailable-players nil)
  (let ((unavailable-players nil))
    (traverse-file roster-file start at 3
		     (if (> (length current-line) 24)
			 (if (or (> (string-to-number (nth 24 current-line)) 0)
				 (> (string-to-number (nth 25 current-line)) 0))
			     (setq unavailable-players (cons (nth 0 current-line) unavailable-players)))))
    (reverse unavailable-players)))

(defun get-roster-players ()
  (let ((players nil))
    (traverse-file roster-file start at 3
		   (if (> (length current-line) 0)
		       (setq players (cons (car current-line) players))))
    (reverse players)))

(defun get-lineup-players ()
  (let ((players nil))
    (traverse-file teamsheet-file start at 4
		     (if (member (nth 0 current-line) positions)		   
			 (setq players (cons (nth 1 current-line) players))))
  (reverse players)))

(defun check-lineup ()
  (interactive)
  (let ((roster (get-roster-players))
	(lineup (get-lineup-players))
	(errors (list)))
    (dolist (player lineup errors)
      (if (not (member player roster))
	  (setq errors (cons player errors))))
    (if errors
	errors
      "No Errors!")))
    
