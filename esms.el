(defvar roster-file "~/SSL/roster/ccy.txt")
(defvar teamsheet-file "~/SSL/teamsheets/ccysht.txt")

;; TODO check against lineup
(defun check-players ()
  (interactive)
  (find-file roster-file)
  (get-unavailable-players))

(defmacro traverse-file (filename start at start-line &rest body)
  `(let ((moreLines t))
    (find-file ,filename)
    (goto-line ,start-line)
    (let (( moreLines t))
      (while moreLines
	(setq current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	(setq current-line (split-string current-line))
	,@body
	(setq moreLines (= 0 (forward-line 1)))))))

(defun get-unavailable-players ()
  (setq unavailable-players nil)
  (let ((unavailable-players nil))
    (traverse-roster roster-file start at 3
		     (if (> (length current-line) 0)
			 (if (or (> (string-to-number (nth 24 current-line)) 0)
				 (> (string-to-number (nth 25 current-line)) 0))
			     (message "%s" (nth 0 current-line))
			   (append unavailable-players (list (nth 0 current-line))))))
    (message "%s" (combine-and-quote-strings unavailable-players))))

(defun get-lineup-players ()
  (interactive)
  (let ((lineup-players nil)
	(nil-count 0))
    (traverse-roster teamsheet-file start at 4
		     (if (and (< nil-count 2) (> (length current-line) 1))
			 (message "%s" (nth 1 current-line))
		       (setq nil-count (+ nil-count 1))))
    (message "%s" (combine-and-quote-strings lineup-players))))