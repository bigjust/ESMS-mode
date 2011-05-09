;; esms.el -- ESMS mode for PBEM football teamsheet checking
;; https://github.com/bigjust/ESMS-mode
;;
;; TODO:
;;  1. auto-detect roster file for current teamsheet
;;  2. output errors to temp buffer
;;  3. highlight errors
;;  4. syntax table
;;  5. run every time the teamsheet is saved

(defvar roster-file nil)
(defvar teamsheet-file nil)
(defvar *roster-directory* "~/SSL/rosters/" "Default directory for rosters")

(defvar positions '("PK:" "GK" "DF" "DM" "MF" "AM" "FW"))

(define-derived-mode esms-mode text-mode "ESMS"
  "Major mode for setting up a ESMS lineup"
  (setq teamsheet-file (buffer-file-name))
  (setq roster-file (concat *roster-directory* (file-name-nondirectory (buffer-file-name)))))

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
 1. unavailable players 
 2. players not found on the roster
 3. valid penalty kicker
 4. check substitutes not in starting lineup"
  (interactive)
  (let ((roster (get-roster-players))
	(lineup (get-lineup-players))
	(crocked (get-unavailable-players))
	(errors (list)))
    (dolist (player lineup errors)
      (if (not (member player roster))
	  (setq errors (cons (concat player " not found in roster") errors)))
      (if (member player crocked)
	  (setq errors (cons (concat player " not available to play") errors))))
    (dolist (player (get-substitutes lineup) errors)
      (if (member player (starting-lineup lineup))
	  (setq errors (cons (concat player " is listed as starting and sub") errors))))
    (if (not (check-lineup-pk lineup))
	(setq errors (cons "Penalty Taker not in starting lineup" errors)))
    (if (> (length errors) 0)
	(princ errors)
      (princ "No Errors!"))
    ))

(defun get-substitutes (lineup)
  "return subsitutes from lineup (12-16)"
  (nthcdr 11 (butlast lineup)))

(defun starting-lineup (lineup)
  "returns the first 11 players on the lineup.  assumes a list of 17 players.
   11 (starting lineup) + 5 (substitutes) + 1 (penalty taker)"
  (butlast lineup 6))

(defun check-lineup-pk (lineup)
  "Checks the teamsheet for a valid penalty kicker. returns nil if "
  (member (car (last lineup)) (starting-lineup lineup)))
