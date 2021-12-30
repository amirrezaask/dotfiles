;; Acme-like functionalities
;;  So we want acme
;;  we need to main functions
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

(defun --acme-get-cmd-prefix (cmd)
  (let (
        (prefix (nth 0 (string-to-list cmd)))
        )
    (cond
     ((eq prefix (string-to-char "|")) "|")
     ((eq prefix (string-to-char ">")) ">")
     ((eq prefix (string-to-char "<")) "<")
     (t nil)
     )
  ))


(defun --acme-get-actual-cmd (cmd)
  (if (eq nil (--acme-get-cmd-prefix cmd))
      cmd (apply 'string (cdr (string-to-list cmd)))))


(defun --acme-execute-noop (cmd)
  "TODO check if buffer is open already in any window just use that don't other window it"
  "Just executes the command and writes the output to another window" 
  (let* ((procname (format "%s %s" (buffer-name) cmd))
        (bufname (format "%s+Errors" (buffer-name) cmd))
        (splitted (split-string cmd " "))
        (program (car splitted))
        (args (string-join (cdr splitted))))
            (start-process procname bufname program args)
            (split-window-right)
            (other-window 1)
            (switch-to-buffer bufname)
    )
  )


(defun --acme-execute-pipe (cmd)
  "Pipe selected region to cmd and pipe back the output and replace region with it"
  (make-pipe-process )
  (split-window-right)
  (other-window 1)
  (insert (shell-command-to-string cmd))
  )

(defun --acme-execute-write (cmd)
  "executes the cmd and pipes region or word into it's stdin")

(defun --acme-execute-read (cmd)
  "executes the cmd and reads the stdout and replaces the region with it")

(defun acme-execute ()
  "Tries to execute passed command.
   if first char is '|' it will pipe selected text as input and reads stdout and replaces the region
   if first char is '>' it will pipe selected text as input.
   if first char is '<' it will just replace region with stdout."
  (interactive)
  (let* ((usercmd (--acme-get-region-or-word))
         (cmdprefix (--acme-get-cmd-prefix usercmd))
         (useractualcmd (--acme-get-actual-cmd usercmd)))
    (message "user cmd -> %s" usercmd)
    (message "cmd prefix -> %s" cmdprefix)
    (message "actual cmd -> %s" useractualcmd)
    (cond
     ((string= "|" cmdprefix) (--acme-execute-pipe useractualcmd))
     ((string= ">" cmdprefix) (--acme-execute-write useractualcmd))
     ((string= "<" cmdprefix) (--acme-execute-read useractualcmd))
     (t (--acme-execute-noop useractualcmd)))
    )
  )

|ls





