;;; cpputils-cmake.el --- Easy real time C++ syntax check and IntelliSense if you use CMake.

;; Copyright (C) 2014 Chen Bin
;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/cpputils-cmake
;; Keywords: CMake IntelliSense Flymake Flycheck
;; Version: 0.4.16

;; This file is not part of GNU Emacs.

;; This file is free software (GPLv3 License)

;; How to set it up:
;; See README.org which is distributed with this file

;;; Code:

;; The basic algorithm is simple:
;; 1. Find the CMakeLists.txt from current directory.
;; 2. Extract information from it and fill in cppcm-hash
(defcustom cppcm-proj-max-dir-level 16 "maximum level of the project directory tree"
  :type 'number
  :group 'cpputils-cmake)

(defcustom cppcm-build-dirname "build" "The directory name of build directory"
  :type 'string
  :group 'cpputils-cmake)

(defcustom cppcm-reload-all-hook nil
  "hook after cppcm-reload-all is called. You can modify the global variables set up by cppcm-reload-all"
  :type 'hook
  :group 'cpputils-cmake)

(defvar cppcm-extra-preprocss-flags-from-user nil "Value example: (\"-I/usr/src/include\" \"-I./inc\" \"-DNDEBUG\").")

(defvar cppcm-build-dir nil "The full path of build directory")
(defvar cppcm-src-dir nil "The full path of root source directory")
(defvar cppcm-include-dirs nil "Value example: (\"-I/usr/src/include\")")
(defvar cppcm-preprocess-defines nil "Value example: (\"-DNDEBUG\" \"-D_WXGTK_\")")

(defvar cppcm-hash (make-hash-table :test 'equal))
(defconst cppcm-prog "cpputils-cmake")
(defcustom cppcm-makefile-name "Makefile" "The filename for cppcm makefiles"
  :type 'string
  :group 'cpputils-cmake)

(defvar cppcm-cmake-target-regex
  "^\s*[^#]*\s*\\(add_\\(?:executable\\|library\\)\\)\s*(\\([^\s]+\\)"
  "Regex for matching a CMake target definition")

(defvar cppcm-cmake-exe-regex
  "^\\(add_executable\\)$"
  "Regex for matching a CMake target definition")

(defcustom cppcm-write-flymake-makefile t "Toggle cpputils-cmake writing Flymake Makefiles"
  :type 'boolean
  :group 'cpputils-cmake)

(defvar cppcm-compile-list
  '(cppcm-compile-in-current-exe-dir
    compile
    cppcm-compile-in-root-build-dir)
  "The list of compile commands.
The sequence is the calling sequence when give prefix argument.

For example:
  If you use the default sequence, such as
  '(cppcm-compile-in-current-exe-dir
    compile
    cppcm-compile-in-root-build-dir)
  then you can run following commands.
'M-x cppcm-compile'         => `cppcm-compile-in-current-exe-dir'
'C-u M-x cppcm-compile'     => `compile'
'C-u C-u M-x cppcm-compile' => `cppcm-compile-in-root-build-dir'.
")

(defvar cppcm-debug nil "enable debug mode")

(defun cppcm--cmakelists-dot-txt (dir)
  (concat (file-name-as-directory dir) "CMakeLists.txt"))

(defun cppcm--exe-hashkey (dir)
  (let (k)
    (setq k (concat (file-name-as-directory dir) "exe-full-path"))
    k))

(defun cppcm--flags-hashkey (dir)
  (let (k)
    (setq k (concat (file-name-as-directory dir) "cpp-flags"))
    k))

(defun cppcm-share-str (msg)
  (kill-new msg)
  (with-temp-buffer
    (insert msg)
    (shell-command-on-region (point-min) (point-max)
                             (cond
                              ((eq system-type 'cygwin) "putclip")
                              ((eq system-type 'darwin) "pbcopy")
                              (t "xsel -ib")
                              ))))

(defun cppcm-readlines (FILE)
    "Return a list of lines of a file at FILE."
      (with-temp-buffer
            (insert-file-contents FILE)
                (split-string (buffer-string) "\n" t)))

(defun cppcm-parent-dir (d) (file-name-directory (directory-file-name d)))

(defun cppcm-query-var (f re)
  (let (v lines)
    (setq lines (cppcm-readlines f))
    (catch 'brk
      (dolist (l lines)
        (when (string-match re l)
          (setq v (match-string 1 l))
          (throw 'brk t)
          )))
    v
    ))

(defun cppcm-query-var-from-last-matched-line (f re)
  "get the last matched line"
  (let (vlist lines)
    (setq lines (cppcm-readlines f))
    (dolist (l lines)
      (if (string-match re l)
        (push (match-string 1 l) vlist)
        ))
    ;; Select the top-level directory assuming it is the one with shorter path
    (if vlist (car (sort vlist #'string-lessp)))
    ))

;; get all the possible targets
(defun cppcm-query-targets (f)
  (let ((vars ())
        lines)
    (setq lines (cppcm-readlines f))
    (dolist (l lines)
      (if (string-match cppcm-cmake-target-regex l)
        (push (list (downcase (match-string 1 l)) (match-string 2 l)) vars)
        ))
    vars
    ))

;; get all the possible targets
;; @return matched line, use (match-string 2 line) to get results
(defun cppcm-match-all-lines (f)
  (let ((vars ())
        lines)
    (setq lines (cppcm-readlines f))
    (catch 'brk
      (dolist (l lines)
        (if (string-match cppcm-cmake-target-regex l)
          (push l vars)
          )))
    vars
    ))

(defun cppcm-query-match-line (f re)
  "return match line"
  (let (ml lines)
    (setq lines (cppcm-readlines f))
    (catch 'brk
      (dolist (l lines)
        (when (string-match re l)
          (setq ml l)
          (throw 'brk t)
          )))
    ml
    ))

;; grep Project_SOURCE_DIR if it exists
;; if Project_SOURCE_DIR does not exist, grep first what_ever_SOURCE_DIR
;; the result is assume the root source directory,
;; kind of hack
;; Please enlighten me if you have better result
(defun cppcm-get-root-source-dir (d)
  (let (rlt)
    (setq lt (cppcm-query-var-from-last-matched-line (concat d "CMakeCache.txt") "Project_SOURCE_DIR\:STATIC\=\\(.*\\)"))
    (if (not rlt)
        (setq rlt (cppcm-query-var-from-last-matched-line (concat d "CMakeCache.txt") "[[:word:]]+_SOURCE_DIR\:STATIC\=\\(.*\\)")))
    rlt
    ))

(defun cppcm-get-dirs ()
  "search from current directory to the parent to locate build directory
return (found possible-build-dir build-dir src-dir)"
  (let ((crt-proj-dir (file-name-as-directory (file-name-directory buffer-file-name)))
        (i 0)
        found
        build-dir
        src-dir
        possible-build-dir)
    (setq cppcm-build-dir nil)
    (setq cppcm-src-dir nil)
    (catch 'brk
           (while (and (< i cppcm-proj-max-dir-level) (not found) )
                  (setq build-dir (concat crt-proj-dir (file-name-as-directory cppcm-build-dirname)))
                  (cond
                   ((and build-dir (file-exists-p (concat build-dir "CMakeCache.txt")))
                    (setq found t)
                    (setq cppcm-build-dir build-dir)
                    )
                   (t ;not a real build directory,
                    (if (file-exists-p build-dir)
                        (setq possible-build-dir build-dir))
                    ;; keep looking up the parent directory
                    (setq crt-proj-dir (cppcm-parent-dir crt-proj-dir))
                    ))
                  (setq i (+ i 1)))
           (when found
             (setq src-dir (cppcm-get-root-source-dir build-dir))
             (setq cppcm-src-dir src-dir)
             ))
    (list found possible-build-dir build-dir src-dir)))

(defun cppcm-guess-var (var cm)
  (let (rlt r)
    (cond
     ((string= var "PROJECT_NAME")
      (setq r (concat "\s*project(\s*\\([^ ]+\\)\s*)")))
     (t
      (setq r (concat "\s*set(\s*" var "\s+\\([^ ]+\\)\s*)" ))
      ))
   (setq rlt (cppcm-query-var cm r))
    rlt))

(defun cppcm-strip-prefix (prefix str)
  "strip prefix from str"
  (if (string-equal (substring str 0 (length prefix)) prefix)
      (substring str (length prefix))
    str)
  )

(defun cppcm-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun cppcm-trim-compiling-flags (cppflags flag-prefix)
  (let (tks
        (next-tk-is-included-dir nil)
        (v ""))
    (setq tks (split-string (cppcm-trim-string cppflags) "\s+" t))
    (dolist (tk tks v)
      (if next-tk-is-included-dir
          (progn
            (setq v (concat v " " flag-prefix tk))
            (setq next-tk-is-included-dir nil)
            )
        (if (string= (substring tk 0 2) flag-prefix)
            (setq v (concat v " " tk))
          ;; corner case for "-I"
          (if (string= tk "-isystem") (setq next-tk-is-included-dir t))
          )))
    v
  ))

;; I don't consider the win32 environment because cmake support Visual Studio
;; @return full path of executable and we are sure it exists
(defun cppcm-guess-exe-full-path (exe-dir tgt)
  (let (p
        base-exe-name
        (type (car tgt))
        (e (cadr tgt)))

    (setq base-exe-name (concat exe-dir "lib" e))
    (when cppcm-debug
      (message "cppcm-guess-exe-full-path: tgt=%s" tgt)
      (message "cppcm-guess-exe-full-path: exe-dir=%s" exe-dir)
      (message "cppcm-guess-exe-full-path: cppcm-cmake-exe-regex=%s" cppcm-cmake-exe-regex)
      (message "cppcm-guess-exe-full-path: base-exe-name=%s" base-exe-name)
      )

    (if (string-match cppcm-cmake-exe-regex type)
        (progn
          ;; application bundle on OS X?
          (setq p (concat exe-dir e (if (eq system-type 'darwin) (concat ".app/Contents/MacOS/" e))))
          ;; maybe the guy on Mac prefer raw application? try again.
          (if (not (file-exists-p p)) (setq p (concat exe-dir e)))
          (if (not (file-exists-p p)) (setq p nil))
          )


      (cond
       ((file-exists-p (concat base-exe-name ".a"))
        (setq p (concat base-exe-name ".a")))

       ((file-exists-p (concat base-exe-name ".so"))
        (setq p (concat base-exe-name ".so")))

       ((file-exists-p (concat base-exe-name ".dylib"))
        (setq p (concat base-exe-name ".dylib")))

       (t (setq p nil))
       )
      )
    (if cppcm-debug (message "cppcm-guess-exe-full-path: p=%s" p))
    p
    ))

(defun cppcm-get-exe-dir-path-current-buffer ()
  (file-name-directory (cppcm-get-exe-path-current-buffer))
  (cppcm-get-exe-path-current-buffer))

(defun cppcm-handle-one-executable (root-src-dir build-dir src-dir tgt)
  "Find information for current executable. My create Makefile for flymake.
Require the project be compiled successfully at least once."
  (let (flag-make
        base-dir
        mk
        cm
        c-flags
        c-defines
        exe-dir
        exe-full-path
        (executable (cadr tgt))
        queried-c-flags
        queried-c-defines
        is-c)

    (setq base-dir (cppcm--guess-dir-containing-cmakelists-dot-txt src-dir))
    (setq cm (cppcm--cmakelists-dot-txt base-dir))
    (setq exe-dir (concat
                   (directory-file-name build-dir)
                   (cppcm-strip-prefix root-src-dir (file-name-directory cm))))

    (setq flag-make
          (concat
           exe-dir
           "CMakeFiles/"
           executable
           ".dir/flags.make"
           ))
    ;; try to guess the executable file full path
    (setq exe-full-path (cppcm-guess-exe-full-path exe-dir tgt))

    (when exe-full-path
      (puthash (cppcm--exe-hashkey base-dir) exe-full-path cppcm-hash)
      (when (and (file-exists-p flag-make)
              (setq queried-c-flags (cppcm-query-match-line flag-make "\s*\\(CX\\{0,2\\}_FLAGS\\)\s*=\s*\\(.*\\)"))
              )

        (setq is-c (if (string= (match-string 1 queried-c-flags) "C_FLAGS") "C" "CXX"))
        (setq c-flags (cppcm-trim-compiling-flags (match-string 2 queried-c-flags) "-I"))

        (setq queried-c-defines (cppcm-query-match-line flag-make "\s*\\(CX\\{0,2\\}_DEFINES\\)\s*=\s*\\(.*\\)"))
        ;; (setq c-defines (cppcm-trim-compiling-flags (match-string 2 queried-c-defines) "-D"))
        ;; just what ever preprocess flag we got
        (setq c-defines (match-string 2 queried-c-defines))

        (puthash (cppcm--flags-hashkey base-dir) (list c-flags c-defines) cppcm-hash)

        (when cppcm-write-flymake-makefile
            (setq mk (concat (file-name-as-directory src-dir) cppcm-makefile-name))
            (with-temp-file mk
              (insert (concat "# Generated by " cppcm-prog ".\n"
                              "include " flag-make "\n"
                              ".PHONY: check-syntax\ncheck-syntax:\n\t${"
                              (if (string= is-c "C") "CC" "CXX")
                              "} -o /dev/null ${"
                              is-c
                              "_FLAGS} ${"
                              is-c
                              "_DEFINES} "
                              (mapconcat 'identity cppcm-extra-preprocss-flags-from-user " ")
                              " -S ${CHK_SOURCES}"
                              ))))
        ))
    ))

(defun cppcm-scan-info-from-cmake(root-src-dir src-dir build-dir)
  (let ((base src-dir)
        cm
        subdir
        possible-targets
        tgt
        e)

    ;; search all the subdirectory for CMakeLists.txt
    (setq cm (cppcm--cmakelists-dot-txt src-dir))

    ;; open CMakeLists.txt and find
    (when (file-exists-p cm)
      (setq possible-targets (cppcm-query-targets cm))

      (dolist (tgt possible-targets)
        ;; if the target is ${VAR_NAME}, we need query CMakeLists.txt to find actual value
        ;; of the target
        (setq e (cadr tgt))

        (setq e (if (and (> (length e) 1) (string= (substring e 0 2) "${"))  (cppcm-guess-var (substring e 2 -1) cm) e))
        (setcar (nthcdr 1 tgt) e)

        (cppcm-handle-one-executable root-src-dir build-dir src-dir tgt))
      )
    (dolist (f (directory-files base))
      (setq subdir (concat (file-name-as-directory base) f))
      (when (and (file-directory-p subdir)
                 (not (equal f ".."))
                 (not (equal f "."))
                 (not (equal f ".git"))
                 (not (equal f cppcm-build-dirname))
                 (not (equal f ".svn"))
                 (not (equal f ".hg"))
                 )
        (cppcm-scan-info-from-cmake root-src-dir subdir build-dir)
        ))))

(defun cppcm--guess-dir-containing-cmakelists-dot-txt (&optional src-dir)
  (let ((i 0)
        dir
        found)
    (if src-dir (setq dir src-dir)
      (setq dir (file-name-as-directory (file-name-directory buffer-file-name))))

    (while (and (< i cppcm-proj-max-dir-level) (not found) )
      (cond
       ((file-exists-p (cppcm--cmakelists-dot-txt dir))
        (setq found t)
        )
       (t
        ;; keep looking up the parent directory
        (setq dir (cppcm-parent-dir dir))
        ))
      (setq i (+ i 1)))
    (unless found
      (setq dir nil))
    dir))

;;;###autoload
(defun cppcm-get-exe-path-current-buffer ()
  (interactive)
  (let (exe-path
        dir)
    (setq dir (cppcm--guess-dir-containing-cmakelists-dot-txt))

    (if dir (setq exe-path (gethash (cppcm--exe-hashkey dir) cppcm-hash)))

    (if exe-path
        (progn
          (cppcm-share-str exe-path)
          (message "%s => clipboard" exe-path)
          )
      (message "executable missing! Please run 'M-x compile' at first.")
      )
    exe-path
    ))

(defun cppcm-set-c-flags-current-buffer ()
  (interactive)
  (let ((dir (cppcm--guess-dir-containing-cmakelists-dot-txt))
        c-compiling-flags-list
        c-flags
        c-defines)

    (when dir
      (setq c-compiling-flags-list (gethash (cppcm--flags-hashkey dir) cppcm-hash))
      (setq c-flags (nth 0 c-compiling-flags-list))
      (setq c-defines (nth 1 c-compiling-flags-list))

      (when c-flags
        (setq cppcm-include-dirs (split-string c-flags "\s+" t))
        (setq cppcm-include-dirs (delq nil
                                       (mapcar (lambda (str)
                                                 (if (string-match "^-I *" str)
                                                     ;; well, make sure the include is absolute path containing NO dot character
                                                     (concat "-I" (file-truename (replace-regexp-in-string "^-I *" "" str)))
                                                   str))
                                               cppcm-include-dirs))
              ))
      (setq cppcm-preprocess-defines (if c-defines (split-string c-defines "\s+" t)))
      )
    ))

(defun cppcm-compile-in-current-exe-dir ()
  "compile the executable/library in current directory."
  (interactive)
  (setq compile-command (concat "make -C \"" (cppcm-get-exe-dir-path-current-buffer) "\""))
  (call-interactively 'compile))

(defun cppcm-compile-in-root-build-dir ()
  "compile in build directory"
  (interactive)
  (setq compile-command (concat "make -C \"" cppcm-build-dir "\""))
  (call-interactively 'compile))

;;;###autoload
(defun cppcm-version ()
  (interactive)
  (message "0.4.16"))

;;;###autoload
(defun cppcm-compile (&optional prefix)
  "compile the executable/library in current directory,
default compile command or compile in the build directory.
You can specify the sequence which compile is default
by customize `cppcm-compile-list'."
  (interactive "p")
  (when (and cppcm-build-dir (file-exists-p (concat cppcm-build-dir "CMakeCache.txt")))
    (let ((index (round (log prefix 4))))
      (call-interactively (nth index cppcm-compile-list))
      )))

;;;###autoload
(defun cppcm-recompile ()
  "make clean;compile"
  (interactive)
  (let (previous-compile-command
        recompile-command)
    (setq previous-compile-command compile-command)
    (setq recompile-command (concat compile-command " clean;" compile-command))
    (compile recompile-command)
    (setq compile-command previous-compile-command)
    ))

;;;###autoload
(defun cppcm-reload-all ()
  "reload and reproduce everything"
  (interactive)
  (let (dirs )
    (when buffer-file-name
      (setq dirs (cppcm-get-dirs))
      (cond
       ((car dirs)
        ;; looks normal, we find buid-dir and soure-dir
        ;; create info for other plugins at first
        ;; cppcm-include-dirs will be set here
        ;; create makefiles may fail if the executable does not exist yet
        (condition-case nil
            (progn
              ;; the order is fixed
              (cppcm-scan-info-from-cmake (nth 3 dirs) (nth 3 dirs) (nth 2 dirs))
              (cppcm-set-c-flags-current-buffer))
          (error
           (message "Failed to create Makefile for flymake. Continue anyway.")))
        )
       ((nth 1 dirs)
        ;; build-dir is found, but flags in build-dir need be created
        ;; warn user.
        (message "Please run cmake in %s at first" (nth 1 dirs))
        )
       (t
        (message "Build directory is missing! Create it and run cmake in it.")))
      )
    )

  (if cppcm-debug (message "cppcm-include-dirs=%s" cppcm-include-dirs))

  (when cppcm-include-dirs
    ;; for auto-complete-clang
    (setq ac-clang-flags (append cppcm-include-dirs cppcm-preprocess-defines cppcm-extra-preprocss-flags-from-user))
    (setq company-clang-arguments (append cppcm-include-dirs cppcm-preprocess-defines cppcm-extra-preprocss-flags-from-user))
    ;; unlike auto-complete and company-mode, flycheck prefer make things complicated
    (setq flycheck-clang-include-path (delq nil
                                            (mapcar (lambda (str)
						      (if (string-match "^-I *" str) (replace-regexp-in-string "^-I *" "" str)))
                                                    ac-clang-flags)))

    (setq flycheck-clang-definitions (delq nil
                                            (mapcar (lambda (str)
						      (if (string-match "^-D *" str) (replace-regexp-in-string "^-D *" "" str)))
                                                    ac-clang-flags)))
    ;; set cc-search-directories automatically, so ff-find-other-file will succeed
    (add-hook 'ff-pre-find-hook
              '(lambda ()
                 (let (inc-dirs)
                   (setq inc-dirs (mapcar (lambda (item)
                                            (when (string-match "^-I[ \t]*" item) (replace-match "" nil nil item)))
                                          cppcm-include-dirs))
                   ;; append the directories into the cc-search-directories
                   ;; please note add-to-list won't insert duplicated items
                   (dolist (x inc-dirs) (add-to-list 'cc-search-directories x))
                   ))))
  (when (and cppcm-build-dir (file-exists-p (concat cppcm-build-dir "CMakeCache.txt")))
    (setq compile-command (concat "make -C \"" cppcm-build-dir "\""))
    )
  (run-hook-with-args 'cppcm-reload-all-hook)
  )

(provide 'cpputils-cmake)

;;; cpputils-cmake.el ends here
