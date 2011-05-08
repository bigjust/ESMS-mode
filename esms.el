(define-derived-mode esms-mode nil "ESMS"
  "Major mode for setting up a ESMS lineup")

(defvar roster-file nil)
(defvar teamsheet-file nil)

(defvar positions '("PK:" "GK" "DF" "DM" "MF" "AM" "FW"))

(defmacro traverse-file (filename start at start-line &rest body)
  "traverses esms user generated files (rosters and teamsheets)"
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
  "Find players in roster who are either injured (crocked) or suspended (bad boys)"
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
  "Return a list of each player in the roster"
  (let ((players nil))
    (traverse-file roster-file start at 3
		   (if (> (length current-line) 0)
		       (setq players (cons (car current-line) players))))
    (reverse players)))

(defun get-lineup-players ()
  "Returns a list of each player on the teamsheet"
  (let ((players nil))
    (traverse-file teamsheet-file start at 4
		     (if (member (nth 0 current-line) positions)
			 (setq players (cons (nth 1 current-line) players))))
  (reverse players)))

(defun check-lineup ()
  "Checks the teamsheet for:
 1. TODO unavailable players 
 2. players not found on the roster
 3. valid penalty kicker
 4. TODO check substitutes not in starting lineup"
  (interactive)
  (let ((roster (get-roster-players))
	(lineup (get-lineup-players))
	(errors (list)))
    (dolist (player lineup errors)
      (if (not (member player roster))
	  (setq errors (cons (concat player " not found in roster") errors))))
    (if (not (check-lineup-pk lineup))
    	  (setq errors (cons "Penalty Taker not in starting lineup" errors)))
    (if errors
	errors
      "No Errors!")))

(defun starting-lineup (lineup)
  "returns the first 11 players on the lineup.  assumes a list of 17 players.
   11 (starting lineup) + 5 (substitutes) + 1 (penalty taker)"
  (nbutlast lineup 6))

(defun check-lineup-pk (lineup)
  "Checks the teamsheet for a valid penalty kicker. returns nil if "
  (member (car (last lineup)) (starting-lineup lineup)))
