(defpackage #:trivial-glob
  (:use #:cl)
  (:local-nicknames
   (#:pattern #:trivial-glob/pattern)
   (#:fs #:trivial-glob/filesystem))
  (:export
   #:glob
   #:glob-match
   #:compile-glob
   #:*match-dotfiles*))
(in-package #:trivial-glob)

(defparameter *match-dotfiles* nil
  "When NIL, wildcards do not match leading dots in filenames.
When T, wildcards match leading dots like any other character.")

(defun glob (pathname-or-pattern &key follow-symlinks)
  "Return a list of pathnames matching the glob pattern.

PATHNAME-OR-PATTERN - A pathname designator or glob pattern string.
FOLLOW-SYMLINKS - If true, follow symbolic links during traversal.

Examples:
  (glob \"*.txt\")
  (glob \"src/**/*.lisp\")
  (glob \"{foo,bar}/*.c\")

Returns a list of pathnames matching the pattern."
  (fs:glob-filesystem pathname-or-pattern :follow-symlinks follow-symlinks))

(defun glob-match (pattern string &key pathname period casefold)
  "Test whether STRING matches the glob PATTERN.

PATTERN - A glob pattern string.
STRING - The string to test.
PATHNAME - If true, '/' characters are not matched by wildcards.
PERIOD - If true, leading '.' must be matched explicitly.
CASEFOLD - If true, perform case-insensitive matching.

Returns T if the string matches, NIL otherwise.

Examples:
  (glob-match \"*.txt\" \"file.txt\") => T
  (glob-match \"test?.c\" \"test1.c\") => T
  (glob-match \"[a-z]*\" \"hello\") => T"
  (pattern:match-pattern pattern string
                         :pathname pathname
                         :period period
                         :casefold casefold))

(defun compile-glob (pattern &key pathname period casefold)
  "Compile a glob pattern into a function for repeated matching.

Returns a function that takes a string and returns T if it matches.

Example:
  (let ((matcher (compile-glob \"*.txt\")))
    (funcall matcher \"file.txt\")) => T"
  (let* ((regex (pattern:glob-to-regex pattern
                                       :pathname pathname
                                       :period period
                                       :casefold casefold))
         ;; Add case-insensitive flag to regex if needed
         (final-regex (if casefold
                          (concatenate 'string "(?i)" regex)
                          regex))
         (scanner (cl-ppcre:create-scanner final-regex)))
    (lambda (string)
      (and (cl-ppcre:scan scanner string) t))))
