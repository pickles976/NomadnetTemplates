#!/usr/bin/csi -script

;;; csi -s ./pages/subpages/file-browser.mu - File browser for Nomadnet

(import (chicken process-context)) ; for accessing environment variables
(import (chicken file))            ; for directory listing
(import (chicken file posix))      ; for file stats
(import fmt fmt-c)
(import (chicken string))
(import micron)

(load "/home/pi/.nomadnetwork/storage/pages/app/templates/subpage-header.scm")

(define page-name "file-browser")

(define current-dir (or (get-environment-variable "var_current_dir") "/home/pi/.nomadnetwork/storage/files"))

;; Get list of files and directories, sorted alphabetically
(define entries (directory current-dir #t))

(define (get-file-size filepath)
    (let* ((size (file-size filepath))
           (mb (/ size (* 1024.0 1024.0))))
        (fmt #f "(" (fix 2 mb) " MB)")))

(define (display-dir dir)
    (let ((full-path (conc current-dir "/" dir)))
        (submit-field (conc "/" dir) "/page/subpages/file-browser.mu" page-name
                      (string-append "current_dir=" full-path))))

(define (display-file file)
    (let* ((full-path (conc current-dir "/" file))
           (nomad-path (string-append "/file/" file)))
        (file-link nomad-path (conc file " " (get-file-size full-path)))))

(define (display-current-directory)
    (apply conc (map (lambda (entry)
        (let ((full-path (string-append current-dir "/" entry)))
            (if (directory? full-path)
                (conc (style '(fg "0f0")) (display-dir entry) nl (reset-style))
                (conc (display-file entry) nl))))
    entries)))

(define (current-dir->pseudo-path dir)
    (string-append "/file"
                   (substring dir (string-length "/home/pi/.nomadnetwork/storage/files"))))

(print
    (subpage-header (current-dir->pseudo-path current-dir))
    (display-current-directory))
