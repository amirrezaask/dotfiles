;; Acme-like functionalities
;; so we want acme
;; we need to main functions
;;  Middle click => Execute (if any thing selected use that as stdin else use current word)
;;  Right click => Load search for the selected text or current word as a file in pwd first else pass it to plumber program

(defvar acme-plumber (lambda (text) (message "plumber received %s" text)) "Plumber program")

(defun --acme-open-file-if-exists (filename)
  "tries to open FILENAME if exists in current dir"
  (if (file-exists-p filename)
      (progn 
        (split-window-right)
        (other-window 1)
        (find-file filename))
    nil))

(defun --acme-pass-to-plumber (text)
  "pass the text into plumber function"
  (funcall acme-plumber text))

(defun --acme-get-selected-text ()
  (interactive)
  (if (use-region-p)
      (let ((regionp (buffer-substring (region-beginning) (region-end))))
        regionp) nil))

(defun --acme-get-region-or-word ()
  "gets selected region if non current word under cursor"
  (interactive)
  (if (eq (--acme-get-selected-text) nil) (current-word) (--acme-get-selected-text)))


(defun acme-load ()
  "Loads the selected region or current word under cursor
    first tries to open it as a file
    if no file found, pass it to plumber function"
  (interactive)
  (unless (eq (--acme-open-file-if-exists (--acme-get-region-or-word)) t) (--acme-pass-to-plumber (--acme-get-region-or-word))))


(defun acme-execute ()
  "Tries to execute passed command.
   if first char is '|' it will pipe selected text as input and reads stdout and replaces the region
   if first char is '>' it will pipe selected text as input.
   if first char is '<' it will just replace region with stdout.")




