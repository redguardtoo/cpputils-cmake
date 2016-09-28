;;; main.el --- tests for cpputils-cmake

;; Copyright (C) 2016 Chen Bin
;;

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(require 'cpputils-cmake)

(defvar cpputils-root nil)

(defun my-ert-text-exists-in-file (re file)
  "regular expression RE exists in FILE."
  (let* (rlt)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (search-forward-regexp re (point-max) t)
          (setq rlt t)))
    rlt))

(defun cpputils-test-build-example ()
  (let* (args)
    (setq cpputils-root (directory-file-name (file-truename (locate-dominating-file default-directory "cpputils-cmake.el"))))
    (message "cpputils-root=%s" cpputils-root)
    (shell-command (format "rm -rf %s/build/;cd %s/example;find . -name Makefiles | xargs rm"
                              cpputils-root
                              cpputils-root))
    (shell-command (format "mkdir -p %s/build && cd %s/build && cmake ../example"
                           cpputils-root
                           cpputils-root))))

;; startup
(cpputils-test-build-example)

;; test cases
(ert-deftest cpputils-test-no-compliation-error ()
  ;; Makefile exists
  (should (executable-find "cmake"))
  (should (file-exists-p (format "%s/build/Makefile" cpputils-root)))
  ;; example project could be successfully built
  (should (= 0 (shell-command (format "make -C %s/build/" cpputils-root))))
  (should (string-match-p "hello" "hello")))

(ert-deftest cpputils-test-all ()
  (let* ((cppcm-write-flymake-makefile t)
         (cppcm-debug nil)
         (main-cpp (format "%s/example/src/main.cpp" cpputils-root))
         (flymake-makefile (format "%s/example/Makefile" cpputils-root))
         (flags-make (format "%s/build/CMakeFiles/project_x_exe.dir/flags.make" cpputils-root)))
    (message "main-cpp=%s" main-cpp)
    (should (file-exists-p main-cpp))
    (find-file main-cpp)
    (should (eq major-mode 'c++-mode))
    (should (string= default-directory (file-truename (format "%s/example/src/" cpputils-root))))
    (shell-command (format "rm %s" flymake-makefile))
    (cppcm-reload-all)
    (should (file-exists-p flymake-makefile))
    ;; flags.make contains include directories
    (should (file-exists-p flags-make))
    (message "flymake-makefile=%s \nflags-make=%s" flymake-makefile flags-make)
    (should (my-ert-text-exists-in-file (format "^include %s" flags-make) flymake-makefile))
    (should (my-ert-text-exists-in-file "\${CXX} -o /dev/null \${CXX_FLAGS} \${CXX_DEFINES} \${CXX_INCLUDES}  *-S \${CHK_SOURCES}" flymake-makefile))
    ;; include directories
    (should (equal cppcm-include-dirs
                   (list (format "-I\"%s/example/src/\"" cpputils-root)
                         (format "-I\"%s/example/project_a/src\"" cpputils-root)
                         (format "-I\"%s/example/project_z/src\"" cpputils-root)
                         (format "-I\"%s/example/src\"" cpputils-root))))))