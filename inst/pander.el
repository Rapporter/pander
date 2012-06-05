;;; pander.el --- ESS integration of pander (R) package

;; Copyright (C) 2012 Gergely Daróczi
;;
;; Author: Gergely Daróczi <gergely@snowl.net>
;; Version: 0.1
;; Keywords: ESS, R, report
;; X-URL: http://daroczig.github.com/pander/

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Affero General Public License
;; as published by the Free Software Foundation; version 3.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; AGPL for more details: http://www.gnu.org/licenses/agpl-3.0.html



;; 
;; TODO list:
;;   * escaping issue (now changing all double quotes to single quotes in selection)
;;


;; Code:

(provide 'ess-pander)

;; Run Pandoc.brew on current buffer and show results in *ess-output* while setting working directory to tempdir() temporary.
(defun pander-brew ()
    (interactive)
    (ess-execute (format "require(pander);wd<-getwd();setwd(tempdir());Pandoc.brew(\"%s\");setwd(wd)\n" buffer-file-name))
)


;; Run Pandoc.brew on current buffer and export results in HTML. Also tries to open that automatically in default browser.
(defun pander-brew-to-HTML ()
    (interactive)
    (ess-command (format "require(pander);wd<-getwd();setwd(tempdir());Pandoc.brew(\"%s\", output = tempfile(), convert = 'html');setwd(wd)\n" buffer-file-name))
)


;; Run evals on region and show results in *ess-output* while setting working directory to tempdir() temporary.
(defun pander-evals-region ()
    (interactive)
    (if mark-active
	(let (
	      (selection (buffer-substring-no-properties (region-beginning) (region-end))))
	  (if (= (length selection) 0)
	      (message "Nothing to pass to evals.")
	    (ess-execute (format "pander:::ess.evals(\"%s\")\n" (replace-regexp-in-string "\"" "'" selection))))
	  )
      (error "mark not active"))
)


;; Run pander (after eval) on region and show results in *ess-output* while setting working directory to tempdir() temporary.
(defun pander-region ()
  (interactive)
  (if mark-active
	(let (
	      (selection (buffer-substring-no-properties (region-beginning) (region-end))))
	  (if (= (length selection) 0)
	      (message "Nothing to pass to evals.")
	    (ess-execute (format "pander:::ess.pander.evals(\"%s\")\n" (replace-regexp-in-string "\"" "'" selection))))
	  )
    (error "mark not active"))
  )

(defun pander-chunk ()
  "Run pander (after eval) on current chunk (in which the pointer is) and show results in *ess-output* while setting working directory to tempdir() temporary. Chunk is recognized by opening '<%' or '<%=', and closing '%>' tags."
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "^<%[=]+") (setq p1 (point))
    (skip-chars-forward "^%>") (setq p2 (point))
    (let (
	  (selection (buffer-substring-no-properties p1 p2)))
      (if (= (length selection) 0)
	  (message "Pointer is not inside a chunk!")
	(ess-execute (format "pander:::ess.pander.evals(\"%s\")\n" (replace-regexp-in-string "\"" "'" selection))))
      )
    )
  )


;; Run pander (after eval) on region or current chunk (if marker is not set) and show results in *ess-output* while setting working directory to tempdir() temporary. Chunk is recognized by opening '<%' or '<%=', and closing '%>' tags.
(defun pander-region-or-chunk ()
  (interactive)
  (if mark-active
      (pander-region)
    (pander-chunk))
  )


;; Define key-bindings for calling above functions
(global-set-key (kbd "C-c p b") 'pander-brew)
(global-set-key (kbd "C-c p B") 'pander-brew-to-HTML)
(global-set-key (kbd "C-c p e") 'pander-evals-region)
(global-set-key (kbd "C-c p r") 'pander-region)
(global-set-key (kbd "C-c p c") 'pander-chunk)
(global-set-key (kbd "C-c p p") 'pander-region-or-chunk)
