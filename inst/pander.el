;;; pander.el --- ESS integration of pander (R) package

;; Copyright (C) 2012 Gergely Daróczi
;;
;; Author: Gergely Daróczi <gergely@snowl.net>
;; Version: 0.1
;; Package-Requires: ((ess))
;; Keywords: ESS, R, report
;; X-URL: https://rapporter.github.io/pander/

;; This file is not part of GNU Emacs.

;; This "program" is free software; you can redistribute it and/or
;; modify it under the terms of the Affero General Public License
;; as published by the Free Software Foundation; version 3.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; AGPL for more details: http://www.gnu.org/licenses/agpl-3.0.html

;;; Installation and load

;; This file can be found on your filesystem if pander R package
;; is installed. If you do not know where it is, run the following
;; command in *R*:
;;
;;	system.file('pander.el', package='pander')
;;
;; To use this minor, simply add the above firectory to your path
;; and load pander.el, e.g.:
;;
;;	(add-to-list 'load-path "/usr/lib/R/library/pander/")
;;	(require 'pander)
;;

;;; Feedback

;; This minor-mode is under heavy development. Any suggestion/feedback
;; really welcomed at GH issue tracker:
;; 
;;	https://github.com/rapporter/pander/issues

;;; TODO:

;;   * escaping issue (now changing all double quotes to single quotes in selection)

;;; Code:

;; minor mode

(defgroup pander nil
  "ESS integration of pander R package"
  :group 'ess)

(defvar pander-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c p b") 'pander-brew)
    (define-key keymap (kbd "C-c p B") 'pander-brew-export)
    (define-key keymap (kbd "C-c p e") 'pander-eval)
    keymap)
  "Keymap for pander-mode.")

(define-minor-mode pander-mode
  "Toggle pander mode.
With no argument, this command toggles the mode. 
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When pander mode is enabled, some keybindigs
are activated for pander functions."
  :lighter " pander"
  :group 'pander
  )

;; Define key-bindings for calling above functions

;; (global-set-key (kbd "C-c p e") 'pander-evals-region)
;; (global-set-key (kbd "C-c p r") 'pander-region)
;; (global-set-key (kbd "C-c p c") 'pander-chunk)
;; (global-set-key (kbd "C-c p p") 'pander-region-or-chunk)
;; (global-set-key (kbd "C-c p R") 'pander-region-export)
;; (global-set-key (kbd "C-c p C") 'pander-chunk-export)
;; (global-set-key (kbd "C-c p P") 'pander-region-or-chunk-export)


(defcustom pander-clipboard nil
  "If non-nil then the result of pander-* functions would be copied to clipboard."
  :type 'boolean
  :group 'pander)

(defcustom pander-show-source nil
  "If non-nil then the source of R commands would also show up in generated documents while running 'pander-eval'. This would not affect 'brew' function ATM."
  :type 'boolean
  :group 'pander)


;; functions

(defun pander-postprocess-output ()

  "Prettify results in *ess-output* and optionally copy to clipboard."

  ;; remove possible "+" chars at the beginning of the result
  (set-buffer "*ess-output*")
  (beginning-of-line)
  (while (re-search-forward "\\+ " (min (point-at-eol)) 'go)
    (replace-match ""))
  
  ;; copy to clipboard
  (if pander-clipboard
      (clipboard-kill-ring-save (point-min) (point-max))
    )
  
  )


(defun pander-brew ()
  "Run Pandoc.brew on current buffer or region (if mark is active), show results in *ess-output* and (optionally) copy results to clipboard while setting working directory to tempdir() temporary."
    (interactive)

    (save-excursion
      (if mark-active
	  (let (
		(selection (buffer-substring-no-properties (region-beginning) (region-end))))
	    (ess-execute (format "require(pander, quietly=T);wd<-getwd();setwd(tempdir());Pandoc.brew(text=\"%s\");setwd(wd)\n" (replace-regexp-in-string "\"" "'" selection)))
	    )
	(ess-execute (format "require(pander, quietly=T);wd<-getwd();setwd(tempdir());Pandoc.brew(\"%s\");setwd(wd)\n" buffer-file-name))
	)
      (pander-postprocess-output)
      )

    )


(defun pander-brew-export ()
  "Run Pandoc.brew on current buffer or region (if mark is active) and export results to specified (auto-complete in minibuffer) format. Also tries to open exported document."
    (interactive)
    
    (save-excursion
      (let ((output-format (completing-read  "Output format: "
					     '(("html" 1) ("pdf" 2) ("odt" 3) ("docx" 4)) nil nil "html")))
	(if mark-active
	    (let (
		  (selection (buffer-substring-no-properties (region-beginning) (region-end))))
	      (ess-command (format "require(pander, quietly=T);wd<-getwd();setwd(tempdir());Pandoc.brew(text=\"%s\",output=tempfile(),convert=\"%s\" );setwd(wd)\n" (replace-regexp-in-string "\"" "'" selection) output-format))
	      )
	  (ess-command (format "require(pander, quietly=T);wd<-getwd();setwd(tempdir());Pandoc.brew(\"%s\",output=tempfile(),convert=\"%s\");setwd(wd)\n" buffer-file-name output-format))
	  )
	)
      )
    )
    

(defun pander-eval ()
  "Run pander on (automatically evaluated) region *or* current chunk (if marker is not set), show results (of last returned R object) in *ess-output* and (optionally) copy those to clipboard while setting working directory to tempdir() temporary. Chunk is recognized by opening '<%' or '<%=', and closing '%>' tags."
  (interactive)

  (save-excursion
    (let ((show-src
	   (if pander-show-source
	       (concat "TRUE")
	     (concat "FALSE"))
	   ))

      (if mark-active
	  (let (
		(selection (buffer-substring-no-properties (region-beginning) (region-end))))
	    (if (= (length selection) 0)
	      (message "Nothing selected in region.")
	    (ess-execute (format "pander:::ess.pander.evals(\"%s\", show.src=%s)\n" (replace-regexp-in-string "\"" "'" selection) show-src))))
	
	(let (p1 p2)
	  (skip-chars-backward "^<%[=]+") (setq p1 (point))
	  (skip-chars-forward "^%>") (setq p2 (point))
	  (let (
		(selection (buffer-substring-no-properties p1 p2)))
	    (if (= (length selection) 0)
		(message "Pointer is not inside a chunk!")
	      (ess-execute (format "pander:::ess.pander.evals(\"%s\", show.src=%s)\n" (replace-regexp-in-string "\"" "'" selection) show-src))))))

      )(pander-postprocess-output))
  
  )


(provide 'pander)
