(defpackage #:trivial-glob/pattern
  (:use #:cl)
  (:local-nicknames
   (#:re #:cl-ppcre))
  (:export
   #:glob-to-regex
   #:match-pattern))
(in-package #:trivial-glob/pattern)

(defvar *match-dotfiles* nil
  "When NIL, wildcards do not match leading dots in filenames.
When T, wildcards match leading dots like any other character.")

(defun glob-to-regex (pattern &key pathname period casefold)
  "Convert a glob pattern to a regular expression string.

PATHNAME - If true, '/' characters are not matched by wildcards.
PERIOD - If true, leading '.' must be matched explicitly.
CASEFOLD - If true, perform case-insensitive matching."
  (declare (ignore casefold)) ; handled by cl-ppcre scan
  (with-output-to-string (out)
    (write-string "^" out)
    (loop with i = 0
          with len = (length pattern)
          while (< i len)
          for char = (char pattern i)
          do (case char
               (#\*
                ;; * matches zero or more characters (excluding / if pathname mode)
                (if pathname
                    (write-string "[^/]*" out)
                    (write-string ".*" out))
                (incf i))
               (#\?
                ;; ? matches exactly one character (excluding / if pathname mode)
                (if pathname
                    (write-string "[^/]" out)
                    (write-string "." out))
                (incf i))
               (#\[
                ;; Bracket expression - convert glob [!...] to regex [^...]
                ;; Find the closing bracket
                (let ((close-pos (position #\] pattern :start (1+ i))))
                  (if close-pos
                      (let ((bracket-content (subseq pattern i (1+ close-pos))))
                        ;; Convert [! to [^ for negation
                        (when (and (> (length bracket-content) 2)
                                   (char= (char bracket-content 1) #\!))
                          (setf bracket-content
                                (concatenate 'string
                                             "["
                                             "^"
                                             (subseq bracket-content 2))))
                        (write-string bracket-content out)
                        (setf i (1+ close-pos)))
                      ;; No closing bracket - treat [ as literal
                      (progn
                        (write-string "\\[" out)
                        (incf i)))))
               (#\\
                ;; Escape sequence
                (when (< (1+ i) len)
                  (write-string "\\\\" out)
                  (write-char (char pattern (1+ i)) out)
                  (incf i 2)))
               (otherwise
                ;; Regular character - escape if it's a regex metachar
                (when (find char ".^$+(){}|")
                  (write-char #\\ out))
                (write-char char out)
                (incf i))))
    ;; Handle period flag - if period is true and pattern doesn't start with dot
    ;; the regex should not match strings starting with dot
    (when (and period (> (length pattern) 0) (char/= (char pattern 0) #\.))
      ;; This is handled by the pattern itself, not the regex
      ;; We'll need to check this in match-pattern
      nil)
    (write-string "$" out)))

(defun match-pattern (pattern string &key pathname period casefold)
  "Test whether STRING matches the glob PATTERN.

PATHNAME - If true, '/' characters are not matched by wildcards.
PERIOD - If true, leading '.' must be matched explicitly.
CASEFOLD - If true, perform case-insensitive matching.

Returns T if the string matches, NIL otherwise."
  ;; Handle period flag: if true and string starts with '.',
  ;; pattern must also start with '.'
  (when (and period
             (> (length string) 0)
             (char= (char string 0) #\.)
             (or (= (length pattern) 0)
                 (char/= (char pattern 0) #\.)))
    (return-from match-pattern nil))

  (let* ((regex (glob-to-regex pattern
                               :pathname pathname
                               :period period
                               :casefold casefold))
         ;; Add case-insensitive flag to regex if needed
         (final-regex (if casefold
                          (concatenate 'string "(?i)" regex)
                          regex)))
    (and (re:scan final-regex string) t)))
