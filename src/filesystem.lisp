(defpackage #:trivial-glob/filesystem
  (:use #:cl)
  (:local-nicknames
   (#:pattern #:trivial-glob/pattern))
  (:export
   #:glob-filesystem))
(in-package #:trivial-glob/filesystem)

(defun has-wildcards-p (string)
  "Check if STRING contains glob wildcard characters."
  (some (lambda (char) (find char "*?[")) string))

(defun has-wildcard-component-p (component)
  "Check if a pathname component contains wildcards.
Returns T for :WILD, :WILD-INFERIORS, or implementation-specific pattern objects."
  (not (or (null component)
           (stringp component))))

(defun pathname-component-to-string (component)
  "Convert a pathname component to a string pattern.
Returns NIL for NIL, \"*\" for :WILD, and the string itself for strings."
  (cond
    ((null component) nil)
    ((eq component :wild) "*")
    ((stringp component) component)
    ;; For other cases (like SBCL pattern objects), return wildcard
    (t "*")))

(defun glob-filesystem (pathname-or-pattern &key follow-symlinks)
  "Return a list of pathnames matching the glob pattern.

PATHNAME-OR-PATTERN - A pathname designator or glob pattern string.
FOLLOW-SYMLINKS - If true, follow symbolic links during traversal.

Returns a list of pathnames matching the pattern."
  (declare (ignore follow-symlinks)) ; TODO: implement symlink following

  (let ((path (etypecase pathname-or-pattern
                (pathname pathname-or-pattern)
                (string (pathname pathname-or-pattern)))))

    ;; Check if this is a literal path (no wildcards)
    (let* ((dir-components (pathname-directory path))
           (name (pathname-name path))
           (type (pathname-type path))
           (has-name-wildcard-p (has-wildcard-component-p name))
           (has-type-wildcard-p (has-wildcard-component-p type))
           (has-dir-wildcard-p (or (find :wild dir-components)
                                   (find :wild-inferiors dir-components)
                                   (some (lambda (comp)
                                           (and (stringp comp) (has-wildcards-p comp)))
                                         dir-components))))

      ;; If no wildcards, just check if file exists
      (when (and (not has-dir-wildcard-p)
                 (not has-name-wildcard-p)
                 (not has-type-wildcard-p))
        (return-from glob-filesystem
          (when (uiop:file-exists-p path)
            (list (truename path)))))

      ;; If only name/type have wildcards (no directory wildcards)
      (when (and (or has-name-wildcard-p has-type-wildcard-p)
                 (not has-dir-wildcard-p))
        (return-from glob-filesystem
          ;; For patterns with both name and type wildcards (or specific patterns),
          ;; use directory and filter. Otherwise use uiop:directory-files.
          (cond
            ((and has-name-wildcard-p (or has-type-wildcard-p (stringp type)))
             ;; Has specific pattern like file*.txt or *.*
             (remove-if #'uiop:directory-pathname-p (directory path)))
            (has-name-wildcard-p
             ;; Has only name wildcard like *, match all files
             (uiop:directory-files (uiop:pathname-directory-pathname path)))
            (t
             ;; Has only type wildcard
             (remove-if #'uiop:directory-pathname-p (directory path))))))

      ;; Has directory wildcards - perform custom pattern matching
      (glob-match-filesystem path))))

(defun glob-match-filesystem (pattern-path)
  "Match PATTERN-PATH against the filesystem."
  (let* ((dir-components (pathname-directory pattern-path))
         (name (pathname-name pattern-path))
         (type (pathname-type pattern-path))
         ;; Split into base (no wildcards) and remaining (may have wildcards)
         (split-pos (position-if (lambda (comp)
                                   (or (eq comp :wild)
                                       (eq comp :wild-inferiors)
                                       (and (stringp comp) (has-wildcards-p comp))))
                                 dir-components))
         (base-components (if split-pos
                              (subseq dir-components 0 split-pos)
                              dir-components))
         (remaining-components (when split-pos
                                 (subseq dir-components split-pos))))

    ;; Build base directory (ensure it's a directory pathname with no name/type)
    (let ((base-dir (if base-components
                        (make-pathname :directory base-components
                                       :name nil
                                       :type nil
                                       :defaults pattern-path)
                        (uiop:getcwd))))

      ;; If base doesn't exist, return empty
      (unless (uiop:directory-exists-p base-dir)
        (return-from glob-match-filesystem nil))

      ;; If no remaining dir components, match files in base directory
      (if (null remaining-components)
          (match-files-in-directory base-dir name type)
          ;; Otherwise, need to match through subdirectories
          (match-through-directories base-dir remaining-components name type)))))

(defun match-files-in-directory (directory name-component type-component)
  "Match files in DIRECTORY against NAME-COMPONENT and TYPE-COMPONENT patterns."
  (let ((files (uiop:directory-files directory))
        (results nil)
        (name-pattern (pathname-component-to-string name-component))
        (type-pattern (pathname-component-to-string type-component)))

    (dolist (file files)
      (let ((file-name (pathname-name file))
            (file-type (pathname-type file)))
        (when (and (or (null name-pattern)
                       (pattern:match-pattern name-pattern
                                              (if file-name (string file-name) "")
                                              :period (not pattern:*match-dotfiles*)))
                   (or (null type-pattern)
                       (pattern:match-pattern type-pattern
                                              (if file-type (string file-type) ""))))
          (push file results))))

    (nreverse results)))

(defun match-through-directories (base-dir remaining-dir-components
                                  name-component type-component)
  "Walk through REMAINING-DIR-COMPONENTS from BASE-DIR and match files."
  (if (null remaining-dir-components)
      (match-files-in-directory base-dir name-component type-component)
      (let ((current-component (first remaining-dir-components))
            (rest-components (rest remaining-dir-components))
            (results nil))

        ;; Handle :wild-inferiors (** pattern) specially
        (cond
          ((eq current-component :wild-inferiors)
           ;; ** matches zero or more directory levels
           ;; First, try matching at current level (zero directories)
           (let ((zero-match (match-through-directories base-dir
                                                        rest-components
                                                        name-component
                                                        type-component)))
             (setf results (nconc results zero-match)))

           ;; Then recursively search all subdirectories
           (let ((subdirs (uiop:subdirectories base-dir)))
             (dolist (subdir subdirs)
               ;; For each subdirectory, continue the ** search
               (let ((sub-results (match-through-directories
                                   subdir
                                   remaining-dir-components ; Keep the ** component
                                   name-component
                                   type-component)))
                 (setf results (nconc results sub-results))))))

          (t
           ;; Regular directory component (not **)
           (let ((subdirs (uiop:subdirectories base-dir)))
             (dolist (subdir subdirs)
               ;; Check if subdirectory name matches current component
               (let ((subdir-name (first (last (pathname-directory subdir)))))
                 (when (or (eq current-component :wild)
                           (and (stringp current-component)
                                (stringp subdir-name)
                                (pattern:match-pattern current-component subdir-name
                                                       :period (not pattern:*match-dotfiles*))))
                   ;; Recursively match in this subdirectory
                   (let ((sub-results (match-through-directories
                                       subdir
                                       rest-components
                                       name-component
                                       type-component)))
                     (setf results (nconc results sub-results)))))))))

        results)))
