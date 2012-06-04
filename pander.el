(defun pander-brew ()
    "Run Pandoc.brew on current buffer and show results in *ess-output* while setting working directory to tempdir() temporary."
    (interactive)
    (ess-execute (format "require(pander);wd<-getwd();setwd(tempdir());Pandoc.brew(\"%s\");setwd(wd)\n" buffer-file-name))
)

(defun pander-brew-to-HTML ()
    "Run Pandoc.brew on current buffer and export results in HTML. Also tries to open that automatically in default browser."
    (interactive)
    (ess-command (format "require(pander);wd<-getwd();setwd(tempdir());Pandoc.brew(\"%s\", output = tempfile(), convert = 'html');setwd(wd)\n" buffer-file-name))
)

(defun pander-evals-region ()
    "Run evals on region and show results in *ess-output* while setting working directory to tempdir() temporary."
    (interactive)
    (if mark-active
	(let (
	      (selection (buffer-substring-no-properties (region-beginning) (region-end))))
	  (if (= (length selection) 0)
	      (message "Nothing to pass to evals.")
	    (ess-execute (format "require(pander);wd<-getwd();setwd(tempdir());evals(\"%s\");setwd(wd)\n" selection)))
	  )
      (error "mark not active"))
)

(defun pander-region ()
    "Run pander (after eval) on region and show results in *ess-output* while setting working directory to tempdir() temporary."
    (interactive)
    (if mark-active
	(let (
	      (selection (buffer-substring-no-properties (region-beginning) (region-end))))
	  (if (= (length selection) 0)
	      (message "Nothing to pass to evals.")
	    (ess-execute (format "require(pander);wd<-getwd();setwd(tempdir());pander(evals(\"%s\")[[1]]);setwd(wd)\n" selection)))
	  )
      (error "mark not active"))
)

(global-set-key (kbd "C-c p b") 'pander-brew)
(global-set-key (kbd "C-c p B") 'pander-brew-to-HTML)
(global-set-key (kbd "C-c p e") 'pander-evals-region)
(global-set-key (kbd "C-c p r") 'pander-region)
