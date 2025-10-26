(defpackage #:trivial-glob/filesystem
  (:use #:cl)
  (:local-nicknames
   (#:compiler #:trivial-glob/compiler))
  (:export
   #:glob-filesystem))
(in-package #:trivial-glob/filesystem)

(defstruct (exclude-pattern-spec
            (:copier nil)
            (:predicate nil))
  "Specification for an exclude pattern with optional negation.
NEGATION - T if this is a negation pattern (starts with !), NIL for regular exclusion.
MATCHER - Compiled path matcher function (pathname) -> boolean."
  (negation nil :type boolean :read-only t)
  (matcher nil :type function :read-only t))

(defun has-wildcards-p (string)
  "Check if STRING contains glob wildcard characters."
  (some (lambda (char) (find char "*?[")) string))

(defun split-pattern-string (pattern-string)
  "Split pattern string into directory components and filename pattern.
Returns (values dir-components filename-pattern) where:
- dir-components: list of directory component strings
- filename-pattern: filename portion (string)

Examples:
  \"*.txt\" => (NIL \"*.txt\")
  \"dir/*.txt\" => ((\"dir\") \"*.txt\")
  \"dir/sub/**/*.lisp\" => ((\"dir\" \"sub\" \"**\") \"*.lisp\")
  \"/abs/path/*.c\" => ((\"\" \"abs\" \"path\") \"*.c\")  ; empty string = absolute"
  (let ((components (uiop:split-string pattern-string :separator "/")))
    (if (null components)
        (values nil "")
        (let ((filename (car (last components)))
              (dirs (butlast components)))
          (values dirs filename)))))

(defun split-filename-pattern (filename-pattern)
  "Split filename pattern into name and type (extension) parts.
Returns (values name-pattern type-pattern) where both are strings or NIL.

Examples:
  \"*.lisp\" => (\"*\" \"lisp\")
  \"*-test.lisp\" => (\"*-test\" \"lisp\")
  \"test*\" => (\"test*\" NIL)
  \"test.tar.gz\" => (\"test.tar\" \"gz\")  ; split on last dot only"
  (let ((parts (uiop:split-string filename-pattern :separator '(#\.) :max 2)))
    (cond
      ((null parts) (values nil nil))
      ((= (length parts) 1) (values (first parts) nil))
      (t (values (first parts) (second parts))))))

(defun symlink-p (pathname)
  "Check if PATHNAME is a symbolic link.
Note: This uses a heuristic that works on most Unix systems but may not be 100% accurate
on all platforms. It resolves relative paths to absolute before checking."
  (handler-case
      (let* ((absolute-path (uiop:ensure-absolute-pathname pathname *default-pathname-defaults*))
             (truename-result (truename absolute-path))
             (namestr-abs (namestring absolute-path))
             (namestr-true (namestring truename-result)))
        ;; Compare absolute path with its truename
        ;; On Unix, symlinks will have different paths
        (not (equal namestr-abs namestr-true)))
    (error () nil)))

(defun expand-tilde (pattern-string)
  "Expand tilde (~) in PATTERN-STRING to home directory if present.
Returns the expanded pattern string."
  (cond
    ((and (> (length pattern-string) 0)
          (char= (char pattern-string 0) #\~))
     ;; Replace ~ or ~/ with home directory
     (let ((home-dir (namestring (user-homedir-pathname))))
       (cond
         ((= (length pattern-string) 1)
          ;; Just "~"
          home-dir)
         ((char= (char pattern-string 1) #\/)
          ;; "~/..."
          (concatenate 'string home-dir (subseq pattern-string 2)))
         (t
          ;; "~user/..." - not supported, use as-is
          pattern-string))))
    (t pattern-string)))

(defun should-exclude-p (pathname exclude-specs)
  "Check if PATHNAME should be excluded according to EXCLUDE-SPECS.
Returns T if the pathname should be excluded, NIL if it should be included.
Processes patterns sequentially; last match wins."
  (when exclude-specs
    ;; Start with included, process patterns sequentially
    (let ((included t))
      (dolist (spec exclude-specs)
        (when (funcall (exclude-pattern-spec-matcher spec) pathname)
          ;; This pattern matches - update inclusion state
          (setf included (exclude-pattern-spec-negation spec))))
      ;; Return T if excluded (included is NIL)
      (not included))))

(defun filter-excluded (pathnames exclude-specs)
  "Filter pathnames according to exclude patterns with optional negation.
PATHNAMES - List of pathnames to filter.
EXCLUDE-SPECS - List of exclude-pattern-spec structures, or NIL.
                Patterns are processed sequentially; last match wins.
Returns filtered list of pathnames."
  (if (null exclude-specs)
      pathnames
      (remove-if (lambda (pathname)
                   (should-exclude-p pathname exclude-specs))
                 pathnames)))

(defun glob-filesystem (pathname-or-pattern &key follow-symlinks exclude)
  "Return a list of pathnames matching the glob pattern.

PATHNAME-OR-PATTERN - A pathname designator or glob pattern string.
FOLLOW-SYMLINKS - If true, follow symbolic links during traversal.
EXCLUDE - A pattern string or list of pattern strings to exclude from results.

Returns a list of pathnames matching the pattern."
  ;; Compile exclude patterns once
  (let ((exclude-specs
          (when exclude
            (let ((patterns (if (listp exclude) exclude (list exclude))))
              (mapcar (lambda (pattern)
                        (if (and (< 0 (length pattern))
                                 (char= (char pattern 0) #\!))
                            ;; Negation pattern - strip '!' and set negation flag
                            (make-exclude-pattern-spec
                             :negation t
                             :matcher (compiler:compile-path-pattern (subseq pattern 1)))
                            ;; Regular exclusion pattern
                            (make-exclude-pattern-spec
                             :negation nil
                             :matcher (compiler:compile-path-pattern pattern))))
                      patterns))))
        (pattern-string (etypecase pathname-or-pattern
                          (pathname (namestring pathname-or-pattern))
                          (string (expand-tilde pathname-or-pattern)))))

    ;; Parse pattern string ourselves to avoid CL pathname corruption
    (multiple-value-bind (dir-components filename-pattern)
        (split-pattern-string pattern-string)

      ;; Split filename into name and type parts
      (multiple-value-bind (name-pattern type-pattern)
          (split-filename-pattern filename-pattern)

        ;; Check for wildcards
        (let* ((has-name-wildcard-p (and name-pattern (has-wildcards-p name-pattern)))
               (has-type-wildcard-p (and type-pattern (has-wildcards-p type-pattern)))
               (has-dir-wildcard-p (some (lambda (comp)
                                           (or (string= comp "**")
                                               (has-wildcards-p comp)))
                                         dir-components)))

          ;; If no wildcards, just check if file exists
          (when (and (not has-dir-wildcard-p)
                     (not has-name-wildcard-p)
                     (not has-type-wildcard-p))
            (let ((path (pathname pattern-string)))
              (return-from glob-filesystem
                (when (uiop:file-exists-p path)
                  (filter-excluded (list (truename path)) exclude-specs)))))

          ;; Compile filename patterns once
          (let ((name-matcher (when has-name-wildcard-p
                                (compiler:compile-pattern name-pattern
                                                          :period (not compiler:*match-dotfiles*))))
                (type-matcher (when has-type-wildcard-p
                                (compiler:compile-pattern type-pattern))))

            ;; If only name/type have wildcards (no directory wildcards)
            (when (and (or has-name-wildcard-p has-type-wildcard-p)
                       (not has-dir-wildcard-p))
              ;; Build directory path
              (let* ((dir-path (if dir-components
                                   (make-pathname :directory (cons (if (string= (first dir-components) "")
                                                                       :absolute
                                                                       :relative)
                                                                   (if (string= (first dir-components) "")
                                                                       (rest dir-components)
                                                                       dir-components))
                                                  :name nil
                                                  :type nil)
                                   (uiop:getcwd))))
                (unless (uiop:directory-exists-p dir-path)
                  (error 'file-error
                         :pathname dir-path
                         :format-control "Directory does not exist: ~A"
                         :format-arguments (list dir-path)))
                (when (and (not follow-symlinks) (symlink-p dir-path))
                  (return-from glob-filesystem nil))
                (return-from glob-filesystem
                  (filter-excluded
                   (match-files-in-directory dir-path
                                             name-pattern name-matcher
                                             type-pattern type-matcher)
                   exclude-specs))))

            ;; Has directory wildcards - perform custom pattern matching
            (filter-excluded
             (glob-match-filesystem dir-components
                                    name-pattern name-matcher
                                    type-pattern type-matcher
                                    follow-symlinks
                                    exclude-specs)
             exclude-specs)))))))

(defun match-files-in-directory (directory name-pattern name-matcher type-pattern type-matcher)
  "Match files in DIRECTORY using compiled matchers.
NAME-PATTERN and TYPE-PATTERN are strings (or NIL for no pattern).
NAME-MATCHER and TYPE-MATCHER are compiled functions (or NIL for no wildcard)."
  (let ((files (uiop:directory-files directory))
        (results nil))
    (dolist (file files)
      (let ((file-name (pathname-name file))
            (file-type (pathname-type file)))
        (when (and (or (null name-pattern)
                       (if name-matcher
                           (funcall name-matcher (if file-name (string file-name) ""))
                           (string= name-pattern (if file-name (string file-name) ""))))
                   (or (null type-pattern)
                       (if type-matcher
                           (funcall type-matcher (if file-type (string file-type) ""))
                           (string= type-pattern (if file-type (string file-type) "")))))
          (push file results))))
    (nreverse results)))

(defun glob-match-filesystem (dir-components name-pattern name-matcher
                              type-pattern type-matcher follow-symlinks
                              exclude-specs)
  "Match pattern against filesystem using compiled matchers."
  ;; Split directory components into base (no wildcards) and remaining (with wildcards)
  (let* ((split-pos (position-if (lambda (comp)
                                   (or (string= comp "**")
                                       (has-wildcards-p comp)))
                                 dir-components))
         (base-components (if split-pos
                              (subseq dir-components 0 split-pos)
                              dir-components))
         (remaining-components (when split-pos
                                 (subseq dir-components split-pos))))

    ;; Build base directory pathname
    (let ((base-dir (if base-components
                        (make-pathname :directory (cons (if (string= (first base-components) "")
                                                            :absolute
                                                            :relative)
                                                        (if (string= (first base-components) "")
                                                            (rest base-components)
                                                            base-components))
                                       :name nil
                                       :type nil)
                        (uiop:getcwd))))

      ;; Check if base directory exists
      (unless (uiop:directory-exists-p base-dir)
        (error 'file-error
               :pathname base-dir
               :format-control "Directory does not exist: ~A"
               :format-arguments (list base-dir)))

      ;; If no remaining dir components, match files in base directory
      (if (null remaining-components)
          (match-files-in-directory base-dir name-pattern name-matcher
                                    type-pattern type-matcher)
          ;; Otherwise, need to match through subdirectories
          (let ((visited (make-hash-table :test 'equal)))
            (match-through-directories base-dir remaining-components
                                       name-pattern name-matcher
                                       type-pattern type-matcher
                                       follow-symlinks visited exclude-specs))))))

(defun match-through-directories (base-dir remaining-dir-components
                                  name-pattern name-matcher
                                  type-pattern type-matcher
                                  follow-symlinks visited exclude-specs)
  "Walk through REMAINING-DIR-COMPONENTS using compiled matchers.
Uses uiop:collect-sub*directories for memory-efficient iteration.
Skips directories that match exclusion patterns for performance."
  (if (null remaining-dir-components)
      (match-files-in-directory base-dir name-pattern name-matcher
                                type-pattern type-matcher)
      (let ((current-component (first remaining-dir-components))
            (rest-components (rest remaining-dir-components))
            (results nil))

        ;; Handle ** pattern specially
        (cond
          ((string= current-component "**")
           ;; ** matches zero or more directory levels
           ;; First, try matching at current level (zero directories)
           (setf results (nconc results
                                (match-through-directories
                                 base-dir rest-components
                                 name-pattern name-matcher
                                 type-pattern type-matcher
                                 follow-symlinks visited exclude-specs)))

           ;; Then use collect-sub*directories for memory-efficient recursive search
           (uiop:collect-sub*directories
            base-dir
            ;; collectp - process all directories
            (constantly t)
            ;; recursep - control recursion based on symlinks, visited, and exclusions
            (lambda (dir)
              (and (or follow-symlinks (not (symlink-p dir)))
                   (let ((truename-str (namestring (truename dir))))
                     (not (gethash truename-str visited)))
                   ;; Skip recursing into excluded directories for performance
                   ;; Note: Per gitignore semantics, you cannot re-include files if their
                   ;; parent directory is excluded, so this is safe
                   (not (should-exclude-p dir exclude-specs))))
            ;; collector - process each directory found
            (lambda (subdir)
              ;; Skip the base directory itself
              (unless (equal (truename subdir) (truename base-dir))
                (let ((truename-str (namestring (truename subdir))))
                  (unless (gethash truename-str visited)
                    (setf (gethash truename-str visited) t)
                    ;; Continue the ** search in this subdirectory
                    (let ((sub-results (match-through-directories
                                        subdir
                                        remaining-dir-components ; Keep the **
                                        name-pattern name-matcher
                                        type-pattern type-matcher
                                        follow-symlinks
                                        visited
                                        exclude-specs)))
                      (setf results (nconc results sub-results)))
                    (remhash truename-str visited)))))))

          (t
           ;; Regular directory component (not **)
           ;; Use collect-sub*directories to get only immediate children
           (let ((dir-matcher (when (has-wildcards-p current-component)
                                (compiler:compile-pattern current-component
                                                          :period (not compiler:*match-dotfiles*)))))
             (uiop:collect-sub*directories
              base-dir
              ;; collectp - only collect immediate children
              (lambda (dir)
                (and (not (equal (truename dir) (truename base-dir)))
                     (= (length (pathname-directory dir))
                        (1+ (length (pathname-directory base-dir))))))
              ;; recursep - don't recurse (only want immediate children)
              (lambda (dir)
                ;; Only recurse one level to find immediate children
                (= (length (pathname-directory dir))
                   (length (pathname-directory base-dir))))
              ;; collector - process matching directories
              (lambda (subdir)
                (let ((subdir-name (first (last (pathname-directory subdir)))))
                  (when (and subdir-name
                             (if dir-matcher
                                 (funcall dir-matcher subdir-name)
                                 (string= current-component subdir-name))
                             (or follow-symlinks (not (symlink-p subdir))))
                    (let ((truename-str (namestring (truename subdir))))
                      (unless (gethash truename-str visited)
                        (setf (gethash truename-str visited) t)
                        ;; Recursively match in this subdirectory
                        (let ((sub-results (match-through-directories
                                            subdir
                                            rest-components
                                            name-pattern name-matcher
                                            type-pattern type-matcher
                                            follow-symlinks
                                            visited
                                            exclude-specs)))
                          (setf results (nconc results sub-results)))
                        (remhash truename-str visited))))))))))

        results)))
