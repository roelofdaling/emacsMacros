(defun step-lines ( buf1)
   (interactive
      (list (getBuffers "select buffer: "))
   ) 
   (
    let ( (bufferName (concat "TST_" buf1 )) (pat "TS\\w+/IPF/PressureMaintenance") (cl nil) (nl "\n") )
     (get-buffer-create bufferName)
     (print bufferName)
     (save-excursion
     (goto-char (point-min))
     (while (not (eobp) )
       (setq cl (thing-at-point 'line t))
       (if (string-match pat cl)
	   (progn
	     ;;;(setq cl (replace-regexp-in-string nl "" cl))
	     ;;;(print cl)
	     (with-current-buffer bufferName (insert cl))
	   )  
       )	 
       (forward-line 1)
     )
     )
   )
  ) 


(defun getBuffers ( prompt )
  (interactive)
  (read-buffer prompt (other-buffer (current-buffer))
                   (confirm-nonexistent-file-or-buffer))
  
)  


(defun psf-goto-ipftsb (ts)
  "goto start of timestep looking for IPF prior solve"
  (interactive "Mtimestep: ")
  (
   let ((srch " ") (begin 0) (end 0))
    (beginning-of-buffer)    
    (setq srch (concat "TS" ts "/IPF: From"))
    (search-forward srch nil (return nil))
  )
  (point)
)

(defun ret-nil() (nil))

(defun psf-next-ipftsb()
  "goto to the next timestep w.r.t the current cursor"  
  (interactive)
  (
   let ((srch (concat "TS.*/IPF: From" )) (current "") (begin 0) ( end 0) (res t))
    (beginning-of-line)
    (setq begin (point))
    (end-of-line)
    (setq end (point))
    (setq current (buffer-substring-no-properties begin end))
    (beginning-of-line)
    (if ( string-match srch current) 
	(progn
          (search-forward-regexp srch  )
	)  
    )
   (end-of-line) 
   (setq res (search-forward-regexp srch ))
  ;;(if res
  ;;   (point)
  ;;   nil
  ;;)
  )  
)


( defun psf-replace-timecolb ()
  "replace the time colummn with YYY. To help comparing buffers"
  (interactive)
  (
   let( (srch-time "^\\w\\w\\w\\w/\\w\\w/\\w\\w\\s-*\\w\\w:\\w\\w:\\w\\w")
	;;(srch-ipfts "\\(TS.*\\)/IPF: From\\(.*\\)\\s-+.*:\\s-+.*to\\(.*\\)step.*$")
	(srch-ipfts "\\(TS.*\\)/IPF: From\\(.*\\)\s-+.*:.*to\s-\\(.*\\\)\s-.*days$")
	;;(srch-ipfts "TS.*/IPF: From.*to.*step.*$")
	(start 0) (end 0) (from "") (to "") (ts ""))
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward-regexp srch-ipfts nil t)
          (setq ts (match-string-no-properties 1))
          (setq from (match-string-no-properties 2))
          (setq to (match-string-no-properties 3))
	  (print (concat ts from to))
      )
    )
  )
) 
