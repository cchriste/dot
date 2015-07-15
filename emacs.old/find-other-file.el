;;;
;;; File:	find-other-file.el
;;; Author:	Robert Mecklenburg
;;; Created:	March 22, 2000
;;; 
;;; Description: Find the matching cxx/hxx file for the current buffer.
;;;               
;;; (c) Copyright 2000, Parametric Technology Corporation, all rights reserved.
;;;


;;;----------------------------------------------------------------
;;; find-other-source-file
;;; 
;;; Assuming the current buffer is associated with a cxx/hxx/h file,
;;; find the matching file with the "opposite" suffix.
;;; 
(defun find-other-source-file ()
  "Assuming we are visiting a C++ source file and that the current file is one
of a pair (cxx and hxx files with the same base name), visit the other file in
another window."
  (interactive)
  (let* ((base-name (file-name-nondirectory (buffer-file-name)))
         (base-noext (file-name-sans-extension base-name)))

    ;; Are we visiting an h/hxx file?
    (if (string-match "\\.h\\(xx\\)?" base-name)
      (let* ((other-buffer-cxx (get-buffer (concat base-noext ".cxx"))))
		
        (if other-buffer-cxx
          (switch-to-buffer-other-window other-buffer-cxx)
          
          ;; No cxx buffer, look for a cpp buffer.
          (let* ((other-buffer-cpp (get-buffer (concat base-noext ".cpp"))))
			
            (if other-buffer-cpp
              (switch-to-buffer-other-window other-buffer-cpp)

              (find-tag-other-window
               (concat base-noext "\\.c\\(c\\|pp\\|xx\\)?") nil t))))))
	
	
    ;; Are we visiting a cxx file?
    (if (string-match "\\.c\\(xx\\|pp\\)?" base-name)
      (let* ((other-buffer-hxx (get-buffer (concat base-noext ".hxx"))))
		
        (if other-buffer-hxx
          (switch-to-buffer-other-window other-buffer-hxx)
		  
          ;; No hxx buffer, look for an h buffer.
          (let* ((other-buffer-h (get-buffer (concat base-noext ".h"))))
			
            (if other-buffer-h
              (switch-to-buffer-other-window other-buffer-h)
			  
              (find-tag-other-window
               (concat base-noext "\\.h\\(xx\\|pp\\)?") nil t))))))))