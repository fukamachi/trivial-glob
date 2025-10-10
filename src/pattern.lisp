(defpackage #:trivial-glob/pattern
  (:use #:cl)
  (:export
   #:match-pattern
   #:compile-pattern))
(in-package #:trivial-glob/pattern)

(defvar *match-dotfiles* nil
  "When NIL, wildcards do not match leading dots in filenames.
When T, wildcards match leading dots like any other character.")

(defun char-matches-p (pattern-char string-char casefold)
  "Check if a single character matches, with optional case-folding."
  (if casefold
      (char-equal pattern-char string-char)
      (char= pattern-char string-char)))

(defun char-in-bracket-p (char bracket-content casefold)
  "Check if CHAR matches the bracket expression BRACKET-CONTENT.
BRACKET-CONTENT should be the content between [ and ], e.g., 'a-z' or '!abc'."
  (let ((negated nil)
        (content bracket-content))
    ;; Check for negation
    (when (and (> (length content) 0)
               (or (char= (char content 0) #\!)
                   (char= (char content 0) #\^)))
      (setf negated t)
      (setf content (subseq content 1)))

    ;; Check if char matches any character or range in the bracket
    (let ((matched nil))
      (loop with i = 0
            while (< i (length content))
            do (cond
                 ;; Range like a-z
                 ((and (< (+ i 2) (length content))
                       (char= (char content (1+ i)) #\-))
                  (let ((start (char content i))
                        (end (char content (+ i 2))))
                    (when (if casefold
                              (and (char<= start (char-upcase char))
                                   (char<= (char-upcase char) end))
                              (and (char<= start char)
                                   (char<= char end)))
                      (setf matched t)
                      (return)))
                  (incf i 3))
                 ;; Individual character
                 (t
                  (when (char-matches-p (char content i) char casefold)
                    (setf matched t)
                    (return))
                  (incf i))))
      ;; Apply negation if needed
      (if negated (not matched) matched))))

(defun match-glob-internal (pattern pstart pend string sstart send
                            &key pathname period casefold)
  "Internal recursive glob matcher.
Returns T if pattern[pstart:pend] matches string[sstart:send]."
  (cond
    ;; Both exhausted - match
    ((and (>= pstart pend) (>= sstart send))
     t)

    ;; Pattern exhausted but string remains - no match
    ((>= pstart pend)
     nil)

    ;; String exhausted but pattern has only * left - match
    ((>= sstart send)
     (and (< pstart pend)
          (char= (char pattern pstart) #\*)
          (match-glob-internal pattern (1+ pstart) pend string sstart send
                               :pathname pathname :period period :casefold casefold)))

    ;; Match current pattern character
    (t
     (let ((pchar (char pattern pstart))
           (schar (char string sstart)))
       (case pchar
         (#\*
          ;; * matches zero or more characters (excluding / in pathname mode)
          (or
           ;; Try matching zero characters (skip the *)
           (match-glob-internal pattern (1+ pstart) pend string sstart send
                                :pathname pathname :period period :casefold casefold)
           ;; Try matching one or more characters
           (and (not (and pathname (char= schar #\/)))
                (match-glob-internal pattern pstart pend string (1+ sstart) send
                                     :pathname pathname :period period :casefold casefold))))

         (#\?
          ;; ? matches exactly one character (excluding / in pathname mode)
          (and (not (and pathname (char= schar #\/)))
               (match-glob-internal pattern (1+ pstart) pend string (1+ sstart) send
                                    :pathname pathname :period period :casefold casefold)))

         (#\[
          ;; Bracket expression
          (let ((close-pos (position #\] pattern :start (1+ pstart) :end pend)))
            (if close-pos
                (let ((bracket-content (subseq pattern (1+ pstart) close-pos)))
                  (and (char-in-bracket-p schar bracket-content casefold)
                       (match-glob-internal pattern (1+ close-pos) pend
                                            string (1+ sstart) send
                                            :pathname pathname :period period :casefold casefold)))
                ;; No closing bracket - treat [ as literal
                (and (char-matches-p pchar schar casefold)
                     (match-glob-internal pattern (1+ pstart) pend
                                          string (1+ sstart) send
                                          :pathname pathname :period period :casefold casefold)))))

         (#\\
          ;; Escape sequence - match next character literally
          (when (< (1+ pstart) pend)
            (let ((escaped-char (char pattern (1+ pstart))))
              (and (char-matches-p escaped-char schar casefold)
                   (match-glob-internal pattern (+ pstart 2) pend
                                        string (1+ sstart) send
                                        :pathname pathname :period period :casefold casefold)))))

         (otherwise
          ;; Literal character
          (and (char-matches-p pchar schar casefold)
               (match-glob-internal pattern (1+ pstart) pend
                                    string (1+ sstart) send
                                    :pathname pathname :period period :casefold casefold))))))))

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

  (match-glob-internal pattern 0 (length pattern)
                       string 0 (length string)
                       :pathname pathname
                       :period period
                       :casefold casefold))

(defun compile-pattern (pattern &key pathname period casefold)
  "Compile a glob pattern into a function for repeated matching.

Returns a function that takes a string and returns T if it matches."
  (lambda (string)
    (match-pattern pattern string
                   :pathname pathname
                   :period period
                   :casefold casefold)))
