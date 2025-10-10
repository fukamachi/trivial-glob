(defpackage #:trivial-glob/tests/filesystem
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:glob #:trivial-glob)
   (#:a #:alexandria)))
(in-package #:trivial-glob/tests/filesystem)

(defvar *test-dir*
  (merge-pathnames "test-fixtures/"
                   (asdf:system-source-directory :trivial-glob)))

(deftest literal-path-matching
  (testing "Matching literal filenames (no wildcards)"
    (let ((results (glob:glob (merge-pathnames "README.md" *test-dir*))))
      (ok (= (length results) 1))
      (ok (search "README.md" (namestring (first results))))))

  (testing "Non-existent file returns empty list"
    (let ((results (glob:glob (merge-pathnames "nonexistent.txt" *test-dir*))))
      (ok (null results)))))

(deftest wildcard-filename-matching
  (testing "* matches files in directory"
    (let ((results (glob:glob (merge-pathnames "*.txt" *test-dir*))))
      (ok (>= (length results) 2))
      (ok (every (lambda (p) (string= (pathname-type p) "txt")) results))))

  (testing "* matches all files"
    (let ((results (glob:glob (merge-pathnames "*" *test-dir*))))
      (ok (> (length results) 0))
      ;; Should include README.md, *.txt, *.log files
      (ok (some (lambda (p) (search "README" (namestring p))) results))))

  (testing "Pattern with prefix"
    (let ((results (glob:glob (merge-pathnames "file*.txt" *test-dir*))))
      (ok (>= (length results) 2))
      (ok (every (lambda (p)
                   (and (string= (pathname-type p) "txt")
                        (a:starts-with-subseq "file" (pathname-name p))))
                 results)))))

(deftest directory-pattern-matching
  (testing "Match files in subdirectory"
    (let ((results (glob:glob (merge-pathnames "src/*.lisp" *test-dir*))))
      (ok (>= (length results) 1))
      (ok (every (lambda (p)
                   (and (string= (pathname-type p) "lisp")
                        (search "src" (namestring p))))
                 results))))

  (testing "Match files two levels deep"
    (let ((results (glob:glob (merge-pathnames "src/core/*.lisp" *test-dir*))))
      (ok (>= (length results) 2))
      (ok (every (lambda (p)
                   (and (string= (pathname-type p) "lisp")
                        (search "core" (namestring p))))
                 results)))))

(deftest recursive-globbing
  (testing "** matches all subdirectories recursively"
    (let ((results (glob:glob (merge-pathnames "**/*.lisp" *test-dir*))))
      ;; Should find .lisp files in src/, src/core/, src/utils/
      (ok (>= (length results) 4))
      (ok (every (lambda (p) (string= (pathname-type p) "lisp")) results))
      ;; Should include files from different subdirectory levels
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results))
      (ok (some (lambda (p) (search "src/core/" (namestring p))) results))))

  (testing "**/ in middle of pattern"
    (let ((results (glob:glob (merge-pathnames "src/**/*.lisp" *test-dir*))))
      ;; Should find .lisp files in src/ and all its subdirectories
      (ok (>= (length results) 4))
      (ok (every (lambda (p)
                   (and (string= (pathname-type p) "lisp")
                        (search "src" (namestring p))))
                 results))))

  (testing "** matches zero directories"
    (let ((results (glob:glob (merge-pathnames "src/**/*.lisp" *test-dir*))))
      ;; Should include src/main.lisp (zero directories between src and file)
      (ok (some (lambda (p) (search "src/main.lisp" (namestring p))) results)))))
