;; Lisp functions to assist in navigating and analysing psflow log files
;; All functions start with psf (for logsearch) 


(defun log(buffer info)
  (with-current-buffer buffer
    (goto-char(point-max))
    (insert "\n")
    (insert info)
    (insert "\n")
    )
)  

(defconst POINT-REGISTER 0
  "Register to save the current point in.")


(defcustom saved-point nil
  "Whether a point is saved in `POINT-REGISTER`."
  :type 'boolean)

(defun save-or-goto-saved-point ()
  "Interactively memorise a point and return to it."
  (if saved-point
      (progn (register-to-point POINT-REGISTER)
             (setq saved-point nil))
    (progn (point-to-register POINT-REGISTER)
           (setq saved-point t))))




( defun psf-replace-timecol (  )
  "replace the wall-time colummn with a string shownig timestep info. To help comparing buffers"
  (interactive)  
  (
   let(
       (srch-time24 "^\\w\\w\\w\\w/\\w\\w/\\w\\w\\s-*\\w\\w:\\w\\w:\\w\\w")
       (srch-time12 "^\\w.*?[AP]M")
       (srch-time "")
       (srch-ipfts "\\(TS.*\\)/IPF:\\s-From\\s-+\\(\\w+/\\w+/\\w+\\).*to\\s-\\(\\w+/\\w+/\\w+\\).*$")	
       (start 0) (end 0) (from "") (to "") (ts ""))
    (setq srch-time srch-time12)
    (save-excursion
      (end-of-buffer)
      (setq end (point))
      (while (search-backward-regexp srch-ipfts nil t)
      (search-backward-regexp srch-ipfts nil t)
          (setq ts (match-string-no-properties 1))
          (setq from (match-string-no-properties 2))
          (setq to (match-string-no-properties 3))
	  (setq col (concat ts "(" from " -- " to ")"))
	  (search-backward "PRE TIMESTEP")
	  (beginning-of-line)
	  (setq start (point))
	  (print (concat "B: " col "--" (number-to-string start) "--" (number-to-string end)))
          (replace-regexp-in-region srch-time col start end)
	  (setq end(point))
	  (print (concat "A: " col "--" (number-to-string start) "--" (number-to-string end)))
       )
      )
   )
)      


( defun psf-replace-timecol-simple ()
  "replace the time colummn with YYY. To help comparing buffers"
  (interactive)
  (
   let( (srch "^\\w\\w\\w\\w/\\w\\w/\\w\\w\\s-*\\w\\w:\\w\\w:\\w\\w") )
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward srch nil t)
        (replace-match "YYYY/MM/DD")
      )
    )
  )
) 



(defun psf-goto-ipfts (ts)
  "goto start of timestep looking for IPF prior solve"
  (interactive "Mtimestep: ")
  (
   let ((srch " ") (begin 0) (end 0))
    (beginning-of-buffer)    
    (setq srch (concat "TS" ts "/IPF: From"))
    (condition-case nil (search-forward srch) (error nil))
  )
  (point)
)


(defun psf-goto-ipfts-old (ts)
  "goto start of timestep looking for IPF prior solve"
  (interactive "Mtimestep: ")
  (
   let ((srch " ") (begin 0) (end 0))
    (beginning-of-buffer)    
    (setq srch (concat "TS" ts "/IPF: From"))
    (search-forward srch)
  )
  (point)
)


(defun psf-goto-cflts (ts)
  "goto start of timestep looking for coflow string"
  (interactive "Mtimestep: ")
  (
   let ((srch " ") (begin 0) (end 0))
    (beginning-of-buffer)    
    (setq srch (concat "at beginning of timestep " ts))
    (condition-case nil (search-forward srch) (error nil))
  )
  (point)
)


(defun psf-goto-ts (ts)
  "goto start of timestep looking for IPF or CFL prior solve"
  (interactive "Mtimestep: ")
  (
   let ((pnt nil) )
   (setq pnt ( psf-goto-ipfts ts ))
   (if ( eq pnt 1)
      (progn
         ( setq pnt (psf-goto-cflts ts) )
       )
   )
   (print pnt)
  )
)


(defun psf-prev-ipfts()
  "goto to the previous timestep w.r.t the current cursor"
  (interactive)
  (
   let ((srch (concat "TS.*/IPF: From" )) (current "") (begin 0) ( end 0))
    (beginning-of-line)
    (setq begin (point))
    (end-of-line)
    (setq end (point))
    (setq current (buffer-substring-no-properties begin end)) 
    (if ( not (string-match srch current) )
	(progn
          (search-backward-regexp srch)
	)  
      )
   (beginning-of-line) 
   (search-backward-regexp srch)
   )
  (point)
)


(defun psf-current-ipfts()
  "goto to the current timestep w.r.t the current cursor"  
  (interactive)
  (
   let ((srch (concat "TS.*/IPF: From" )) (current "") (begin 0) ( end 0))
    (beginning-of-line)
    (setq begin (point))
    (end-of-line)
    (setq end (point))
    (setq current (buffer-substring-no-properties begin end)) 
    (if ( not (string-match srch current) )
	(progn
          (search-backward-regexp srch)
	)  
      )
   (beginning-of-line) 
   )
  (point)
)

(defun psf-next-ipfts()
  "goto to the next timestep w.r.t the current cursor"  
  (interactive)
  (
   let ((srch (concat "TS.*/IPF: From" )) (current "") (begin 0) ( end 0))
    (beginning-of-line)
    (setq begin (point))
    (end-of-line)
    (setq end (point))
    (setq current (buffer-substring-no-properties begin end))
    (beginning-of-line)
    (if ( string-match srch current) 
	(progn
          (search-forward-regexp srch)
	)  
    )
   (end-of-line) 
   (search-forward-regexp srch)
   )
  (point)
)


(defun psf-goto-mainsolve(ts)
  "goto start of main solve in selected timestep"
  (interactive
    (list (read-string "timestep: "))
   )
  ( psf-goto-ts ts)
  ( gotoMainSolve)
)

(defun psf-goto-nr-mainsolve(ts it)
  "goto start of main solve in selected timestep"
  (interactive
   (list
    (read-string "timestep: ")
    (read-string "iteration: ")
    )
   )
  ( psf-goto-mainsolve ts)
  ( gotoNewtonIteration it)
)



(defun psf-goto-solspace (solspace ts)
  "goto start of selected solution space in selected timestep"
  (interactive
   (list
    (getSolSpaces)
    (read-number "timestep: ")
    )
   )
  (
   let ( (srch ""))
   (beginning-of-buffer) 
  (setq srch (concat "TS" (number-to-string ts) "/IPF/" solspace))
  (search-forward srch)
  
  )
) 

(defun psf-goto-ipr-cfl (well ts)
  "search for Coflow native IPR of well at start of selected timestep: The IPR which will be used in the prior solve of the selected timestep "
  (interactive
   (list
    (getWellArgs)
    (read-number "timestep: ")
    )
  )
  ( let ((srch " "))
    (beginning-of-buffer)
    (setq qwell (concat "'" well "'"))
    (setq srch (concat "IPR table of well: " qwell))
    (psf-goto-ts (number-to-string ts))
    (search-backward-regexp srch)
    (PrintCurrentTimestep)
  )
)


(defun psf-goto-ipr (well ts)
  "search for IPR of well at start of selected timestep: The IPR which will be used in the prior solve of the selected timestep "
  (interactive
   (list
    (getWellArgs)
    (read-number "timestep: ")
    )
  )
  ( let ((srch " "))
    (beginning-of-buffer)
    (setq qwell (concat "'" well "'"))
    (setq srch (concat "After mapping: IPR table for well " qwell))
    (psf-goto-ts (number-to-string ts))
    (search-backward srch)
  )
)

(defun psf-goto-target-control (well ts)
  "go to the line with the IMEX target control for selected well at selected timestep"
  (interactive
   (list
    (getWellArgs)
    (read-number "timestep: ")
    )
  )
  ( let ((srch " "))
    (psf-goto-ts (number-to-string ts))
    (setq qwell (concat "'" well "'"))
    (setq srch (concat "Target control.*for the simulator well:.*" qwell))
    (search-forward-regexp srch)
  )
)  

(defun psf-goto-well-summary (well ts)
  "go to the line summarizing the selected well results at end of main solve of the selected timestep"
  (interactive
   (list
    (getWellArgs)
    (read-number "timestep: ")
    )
   )
  ( let ((srch "Well Summary:") (nowell "No operational well") (current:""))
    (psf-goto-ts (number-to-string ts))
    (search-forward-regexp srch)
    (setq srch well)
    (search-forward-regexp srch)
    (beginning-of-line)
  )
)  
  


(defun psf-copy-imex-information (well ts &optional toName)
  "copy the imex information for well at timestep ts to buffer"
  (interactive
   (list
    (getWellArgs)
    (read-number "timestep: ")
    )
  )
  ( let ((srch " ") (inoperative "Inoperative") (current "") (begin 0) (end 0) (toBufferName "cfl-info"))
    (if toName
      (setq toBufferName toName)
      )
    (log toBufferName (concat "----- Targets for IMEX, TS " (number-to-string ts) " ------- "))     
    (save-excursion
     (psf-goto-ts (number-to-string ts))
     (setq qwell (concat "'" well "'"))
     (setq srch (concat "CoFlow information for well:.*" qwell))
     (search-forward-regexp srch)
     (beginning-of-line)
     (push-mark (point))    
     (setq begin (point))
     ;;(forward-line)
     ;;(setq current (buffer-substring (line-beginning-position) (line-end-position) ))
     ;;(if ( string-match inoperative current) 
     (setq srch "IMEX/GEM Explicit: ************")
     ;;)	       
     ;;(setq srch (concat "Target control.*for the simulator well:.*" qwell))
      (search-forward srch)
      (end-of-line)
      (setq end (point))
      (append-to-buffer toBufferName begin end)
    ) 
  )
)  

(defun psf-copy-well-summary (well ts &optional toName)
  "copy the end of main solve well summary at timestep ts to buffer"
  (interactive
   (list
    (getWellArgs)
    (read-number "timestep: ")
    )
  )
  ( let ((srch " ") (begin 0) (end 0) (toBufferName "cfl-info"))
    (if toName
      (setq toBufferName toName)
      )
    (log toBufferName (concat "----- Well summary at end of timestep " (number-to-string ts) " ------- "))
    (psf-goto-well-summary well ts)
    (beginning-of-line)
    (setq begin(point))
    (end-of-line)
    (setq end(point))
    (append-to-buffer toBufferName begin end)
    )
  )

(defun psf-copy-ipr (well ts &optional toName) 
  "copy  IPR of well at start of specified timestep"
  (interactive
   (list
    (getWellArgs)
    (read-number "timestep: ")
    )
   )
  (
   let ((srch "Bottomhole") (begin 0) (end 0) (toBufferName (DefaultOutputBuffer) ))
    (if toName
	(setq toBufferName toName)
      )
    (log toBufferName (concat "----- IPR used for prior solve TS " (number-to-string ts) " ------- ")) 
    (save-excursion
     (psf-goto-ipr well ts)
     (beginning-of-line)
     (push-mark (point))
     (setq begin (point))
     (setq srch "Bottomhole")
     (search-forward srch)
     (setq srch "IMEX")
     (search-forward srch)
     (beginning-of-line)
     (setq end (point))
     (append-to-buffer toBufferName begin end)
    )
    )
   ;;;(if (not toName)  (SelectDefaultOutputBuffer))
)

(defun psf-ipr-tsrange (well ts1 ts2)
  (interactive
   (list
    (getWellArgs)
    (read-number "timestep 1:")
    (read-number "timestep 2:")
    )
   )
  (let ((ts "") (bufferName (concat "iprs-" well)))
     (get-buffer-create bufferName)
     (setq ts ts1)
     (while (<= ts ts2)
        (psf-copy-ipr well ts bufferName)
	(setq ts (+ ts 1))
	)
     (switch-to-buffer-other-frame bufferName)
   )
)

(defun psf-well-info-tsrange (well ts1 ts2)
  (interactive
   (list
    (getWellArgs)
    (read-number "timestep 1:")
    (read-number "timestep 2:")
    )
   )
  (let ((ts "") (bufferName (concat "info-timerange-" well)))
     (get-buffer-create bufferName)
     (setq ts ts1)
     (while (<= ts ts2)
        (psf-extract-info-ts well ts bufferName)
	(setq ts (+ ts 1))
	)
     (switch-to-buffer-other-frame bufferName)
   )
)



(defun psf-imex-obdbg()
  "find imex working directory"
  (interactive)
  (
   let ((srch " ") (f " "))
    (beginning-of-buffer)    
    (setq srch "Starting process:.*-f")
    (search-forward-regexp srch)
    (setq  f (buffer-substring (+ (point) 2) (progn (search-forward ".dat") (point)) ) )
    (setq f (string-replace ".dat" ".obdbg" f))
    (switch-to-buffer (find-file-other-frame f))
   )
)


(defun map-glb-eu (f)
  "convert folder name"
  (
   let ( (glb "/glb/ams/") (eu "//europe.shell.com/tcs/ams/"))
    ( setq f (string-replace glb eu f))
    )
)  
     
(defun psf-imex-file( reservoir ext)
  "find imex file with extenstion ext"
  (
   let ((srch (concat "Finish creating wells dataset section for reservoir " reservoir))
	(srch2 "IMEX data file:\\s-.*\\.dat")
	(first "IMEX data file: '")
	;(last "\\s-.*created")
	(f " "))
    (save-excursion
      (beginning-of-buffer)
      (search-forward-regexp srch)
      (search-forward-regexp srch2)
      (setq f (match-string-no-properties 0))
      (print f)
      (setq f (string-replace first "" f))
      (setq f (string-replace ".dat" ext f))
      (setq f (map-glb-eu f))
      (print f )
      (switch-to-buffer (find-file-other-frame f))
    )
   )
)


(defun psf-imex-dat( reservoir)
  "find imex dat file and open in new buffer"
  (interactive
  (list
   (getUTHReservoirArgs)
   )
  )
  (
   let ((f " "))
   (setq f (psf-imex-file reservoir ".dat")) 
   (switch-to-buffer (find-file-other-frame f))
  )
)

(defun psf-imex-out( reservoir)
  "find imex out file and open in new buffer"  
  (interactive
  (list
   (getUTHReservoirArgs)
   )
  )
  (
   let ((f " "))
   (setq f (psf-imex-file reservoir ".out")) 
   (switch-to-buffer (find-file-other-frame f))
  )
)


(defun psf-imex-obdbg( reservoir)
  "find imex obdbg file and open in new buffer"
  (interactive
  (list
   (getUTHReservoirArgs)
   )
  )
  (
   let ((f " "))
   (setq f (psf-imex-file reservoir ".obdbg")) 
   (switch-to-buffer (find-file-other-frame f))
   ;(read-only-mode)
  )
)


(defun imx-goto-ts (ts)
  "goto to the specified timestep in the obdbg file by searching for occurences of DTIMEPREV"
  (interactive "ntimestep: ")
  (
   let ((srch " ") )
    (beginning-of-buffer)
    (setq srch "DTIMEPREV")
    (search-forward srch nil nil ts)
    )
)  

(defun imx-goto-ipr (well ts)
  "search for IPR of well at start of specified timestep in IMEX obdbg file"
  (interactive
   (list
    (read-string "well-to-search-for: ")
    (read-number "timestep: ")
    )
  )
  ( let ((srch " "))
    (setq qwell (concat "'" well "'"))
    (setq srch (concat "WELL.*" qwell))
    (imx-goto-ts ts)
    (search-forward-regexp srch)
  )
)

(defun imx-pdrain-profile-new ( well )
  "extract PDRAIN as function of time for well"
  (interactive
   (list
    (read-string "well: ")
    )
   )
  ( let ((ts 0)
	 (srchDateLine "Date:")
	 (srchDate ".*\\s-\\([[:digit:]]+\\)/\\([[:digit:]]+\\)/\\([[:digit:]]+\\).*")
	 (srchPdrainLine "") (srchPdrainValue "")(current "") (toBufferName "pdrain-profile-")
	 (tabName "") (day "") (month "") (year "")
	 (pdrain "")
	 )
    (setq srchPdrainLine (concat "PDRAIN.*" well))
    (setq toBufferName (concat toBufferName well) )
    (get-buffer-create toBufferName)
    (setq tabName (concat well "_pdrain"))
    (log toBufferName "PRESSURE=KPA")
    (log toBufferName (concat "TABLE (INTEGER ts, LONGTIME TIME, PRESSURE pdrain) " tabName)) 
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward srchPdrainLine nil t)
	(setq ts (+ ts 1) )
        (setq current (buffer-substring (line-beginning-position) (line-end-position) ))
        (setq srchPdrainValue (concat "PDRAIN.*'" well "'\\s-\\(.*\\)" ) )
	(if (string-match srchPdrainValue current)
	    (setq pdrain (match-string 1 current))
	)
	(re-search-forward srchDateLine nil t)
        (setq current (buffer-substring (line-beginning-position) (line-end-position) ))	
        (if (string-match srchDate current)
          (progn 
           (setq month (match-string 1 current))
           (setq day (match-string 2 current))
           (setq year (match-string 3 current))
           (log toBufferName (concat tabName "," (number-to-string ts) ",DATE(" day "," month "," year ")," pdrain) )
          )
        )
      )
    )
  )
)

(defun imx-pdrain-profile ( well )
  "extract PDRAIN as function of time for well"
  (interactive
   (list
    (read-string "well: ")
    )
   )
  ( let ((ts 0)
	 (srchDateLine "Date:")
	 (srchDate ".*\\s-\\([[:digit:]]+\\)/\\([[:digit:]]+\\)/\\([[:digit:]]+\\).*")
	 (srchPdrainLine "") (srchPdrainValue "")(current "") (toBufferName "pdrain-profile-")
	 (tabName "") (day "") (month "") (year "")
	 )
    (setq srchPdrainLine (concat "PDRAIN.*" well))
    (setq toBufferName (concat toBufferName well) )
    (get-buffer-create toBufferName)
    (setq tabName (concat well "_pdrain"))
    (log toBufferName "PRESSURE=KPA")
    (log toBufferName (concat "TABLE (INTEGER ts, LONGTIME TIME, PRESSURE pdrain) " tabName)) 
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward srchDateLine nil t)
	(setq ts (+ ts 1) )
        (setq current (buffer-substring (line-beginning-position) (line-end-position) ))
        (if (string-match srchDate current)
          (progn 
           (setq month (match-string 1 current))
           (setq day (match-string 2 current))
           (setq year (match-string 3 current))
	   (re-search-forward srchPdrainLine nil t)
           (setq current (buffer-substring (line-beginning-position) (line-end-position) ))
           (setq srchPdrainValue (concat "PDRAIN.*'" well "'\\s-\\(.*\\)" ) )
	   (if (string-match srchPdrainValue current)
               (log toBufferName (concat tabName "," (number-to-string ts) ",DATE(" day "," month "," year ")," (match-string 1 current)) )
	   )
          )
        )
      )
    )
  )
)




(defun print-info(info)
  "print info to the cfl-info buffer"
  (DefaultOutputBuffer)
  ;(switch-to-buffer "cfl-info")
  (insert-for-yank (concat info "\n"))
  (switch-to-prev-buffer)
)  

(defun psf-extract-info-ts(well ts &optional toName)
  (interactive
   (list
    (getWellArgs)
    (read-number "timestep: ")
    )
  )
  (
   let ((bufferName (concat "info-" well "-" (number-to-string ts))))
    (if toName
	(setq bufferName toName)
      ( progn
	(
         (get-buffer-create bufferName)
        )  
       )
    )    
    (psf-copy-ipr well ts bufferName)
    (psf-copy-imex-information well ts bufferName)
    (if ( isOperativeUth well ts)
	(psf-copy-well-summary well ts bufferName)
    )
  )
)
 


(defun annotate-for-org-mode ()
  (interactive)
  ( let ( (a (list))
	  (srch "")
	  (end 0)
	  (orgstr "")
	  (white-re "")
	  (f "") (key "") (end "") (start "") (empt "") (top "") (n 0))
    (setq a ())
    (setq white-re "\\s-+")
    (setq start "#Start\\s-*")
    (setq end "#End\\s-*")	  
    (setq srch "#Start.*\\|#End.*")
    (print "annotating log regions")
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward-regexp srch nil t)
	(setq f (match-string-no-properties 0))
	(if (string= (substring f 0 2) "#S")
	    (progn
	       (setq key (replace-regexp-in-string start empt f) )
	       (setq key (replace-regexp-in-string white-re "&" key))	      
	       (push key a)
	       (beginning-of-line)
               (setq orgstr (concat "#+BEGIN_" key))
	       (insert (concat orgstr "\n"))
	       )
	  ;End
	  (setq key (replace-regexp-in-string end empt f) )	  
	  (setq key (replace-regexp-in-string white-re "&" key))	      
	  (setq top (pop a))
	  (if (not (string= top key)) ( print (concat "error end" key top) ))
	  (beginning-of-line)
          (setq orgstr (concat "#+END_" key))	  
	  (insert (concat orgstr "\n"))
	)
        (end-of-line)
	)
      )
    )
  (org-mode)
  (org-hide-block-all)
  (beginning-of-buffer)
  (search-forward-regexp "#\\+BEGIN" nil t)
  
)

(defun convert-ipfts-to-org-blocks()
  (interactive)
  ( let ( (a (list))
	  (srch "")
	  (end 0)
	  (orgstr "")
	  (white-re "")
	  (apprEndTS "")
	  (f "") (key "") (prevkey "") (end "") (start "") (empt "") (top "") (n 0))
    (setq a ())
    (setq white-re "\\s-+")
    (setq start "Starting the")
    (setq end "#End\\s-*")
    (setq apprEndTS ".*POST TIMESTEP.*")
    (setq srch "TS.*/IPF: From.*$")
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward-regexp srch nil t)
	
	(setq f (match-string-no-properties 0))
  	(setq key (replace-regexp-in-string start empt f) )
	(setq key (replace-regexp-in-string white-re "---" key))	      
        (setq orgstr (concat "#+BEGIN_" key))

        (save-or-goto-saved-point)	
	(search-backward start)
	(beginning-of-line)
	(insert (concat prevkey "\n"))
	(insert (concat orgstr "\n"))
        (save-or-goto-saved-point)	
	(end-of-line)
	(setq prevkey (concat "#+END_" key))
      )	
      (search-forward-regexp apprEndTS nil t )
      (beginning-of-line)
      (insert (concat prevkey "\n"))	
      )
    )
    (org-mode)
    (org-hide-block-all)
    (beginning-of-buffer)
    (search-forward-regexp "#\\+BEGIN" nil t)
  )

(defun clear-annotate-for-org-mode ()
  (interactive)
  (org-show-block-all)
  (let ( (srch "") (end "") )
    (setq srch "#\\+BEGIN.*\\|#\\+END.*")
    (save-excursion
    (beginning-of-buffer)
    (while(search-forward-regexp srch nil t)
      (beginning-of-line)
      (kill-line)
      (kill-line)
      )
    )
  )
)  


(defun psf-show-calculation-path ( )
  "Prints the sequence of log-regions leading to the curent line. If no log-regions available only shows the IPF timestep"
  (interactive)
  ( let ( (stack (list))
	  (srch "")
	  (end 0)
	  (orgstr "")
	  (white-re "")
	  (f "") (key "") (end "") (start "") (empt "") (top "") (lineno 0))
    (setq a ())
    (setq white-re "\\s-+")
    (setq start "#\\+BEGIN_")
    (setq end "#\\+END_")	  
    (setq srch "#\\+BEGIN_.*\\|#\\+END_.*")
    (setq lineno (line-number-at-pos))
    (save-excursion
      (while(search-backward-regexp srch nil t)
	(setq f (match-string-no-properties 0))
	(if (string= (substring f 0 3) "#+E")
	    ; ENDs are pushed, after removing the leading #+END_ part
	    (progn
	       (setq key (replace-regexp-in-string end empt f) )	      
               (push key stack)
	       )
	    ;If a BEGIN matches the END on the stack the END is popped 
	    ;Else the Begin is pushed. 
	  (setq key (replace-regexp-in-string start empt f) )
	    (if (equal (car stack) key)
		( pop stack)
	        (push key stack)
	    )
         )
	)
      (print (concat "-------- line number " (number-to-string lineno) "-------------------------"))
      (while stack
	(print (car stack ))
	(setq stack (cdr stack) )
      )
      )
    )
  )


(defun getWellList()
  (interactive)
  (
   let ((wlst nil) (argList nil) (f "") (srch "Prepared Well.*\\.") (done nil) (n 0 ) )
    (save-excursion
    (beginning-of-buffer)  
    (while (and (not done) (search-forward-regexp srch nil t))
      (setq f (match-string-no-properties 0))
      (setq f (replace-regexp-in-string "Prepared Well\\s-*" "" f) )
      (setq f (replace-regexp-in-string "\\s-*\\.*\\s-*" "" f) )
      (setq f (replace-regexp-in-string "'" "" f) )
      (if (not (member f wlst) )
        (setq wlst (cons f wlst))
	(setq done t)
      )
      )
    )
    (while wlst
      ( setq n ( + n 1) )
      ( setq argList (cons   (cons (car wlst) n ) argList))
      ( setq wlst (cdr wlst))
    )
    argList
   )
)


(defun getTimestepList()
    (
     let ((tslst nil) (argList nil) (f "") (srch "TS.*/IPF: From.*$")  (last "/IPF: From.*$") (n 0))
     (save-excursion 
      (beginning-of-buffer)
      (while (search-forward-regexp srch nil t)
        (setq f (match-string-no-properties 0))
	(print f)
        (setq f (replace-regexp-in-string last "" f) )
        (setq f (replace-regexp-in-string "TS" "" f) )
	(setq tslst (cons f tslst))
	)
    )  
    (while tslst
      ( setq n ( + n 1) )
      ( setq argList (cons   (cons (car tslst) n ) argList))
      ( setq tslst (cdr tslst))
    )
    argList
   )
)      

(defun getUTHReservoirList()
    (
     let ((reslst nil) (argList nil) (f "")
	  (first "Finish creating wells dataset section for reservoir ")
	  (srch "Finish creating wells dataset section for reservoir.*")
	  (last "\\.$") (n 0))
     (save-excursion 
      (beginning-of-buffer)
      (while (search-forward-regexp srch nil t)
        (setq f (match-string-no-properties 0))
        (setq f (replace-regexp-in-string last "" f) )
        (setq f (replace-regexp-in-string first "" f) )
	(setq reslst (cons f reslst))
	)
    )  
    (while reslst
      ( setq n ( + n 1) )
      ( setq argList (cons   (cons (car reslst) n ) argList))
      ( setq reslst (cdr reslst))
    )
    argList
   )
)      


(defun getSolSpaceList()
  (
   let((solList()) (argList()) (srch "^.*Solving Solution Space") (f "") (begin 0) (end 0) (done nil) (n 0))
    ( save-excursion
      (beginning-of-buffer)
      (setq begin (psf-next-ipfts))
      (save-excursion
        (setq end (psf-next-ipfts))
      )	
      (print begin)
      (print end)
      ( while (and (not done) (search-forward-regexp srch nil t))
	(setq done (> (point) end))
	(if (not done)
	  (progn
	    (setq f (match-string-no-properties 0))
	    (setq f (replace-regexp-in-string "^.*/IPF/" "" f) )
	    (setq f (replace-regexp-in-string "/.*" "" f) )
            (setq solList (cons f solList))
	    )
	)
      )
      )
    (while solList
      ( setq n ( + n 1) )
      ( setq argList (cons   (cons (car solList) n ) argList))
      ( setq solList (cdr solList))
    )
    argList
  )      
)  


(defun getWellArgs()
 (interactive)
 (completing-read
   "Complete wellname: "
   (getWellList)
   nil t "")
)

(defun getUTHReservoirArgs()
 (interactive)
 (completing-read
   "reservoir name: "
   (getUTHReservoirList)
   nil t "")
)

(defun getIPFTS()
 (interactive)
 (completing-read
   "Complete timestep: "
   (getTimestepList)
   nil t "")
)

(defun getSolSpaces()
 (interactive)
 (completing-read
   "Complete Solution Space: "
   (getSolSpaceList)
   nil t "")
)

(defun getBuffers ( )
  (interactive)
  (ido-read-buffer "prompt" (other-buffer (current-buffer))
                   (confirm-nonexistent-file-or-buffer))
  
  (read-buffer-to-switch "Select buffer")
)  

(defun gotoMainSolve ( )
  (
   let ((srch "Starting main solve"))
   (search-forward srch)
   (beginning-of-line)
   )
 )

(defun gotoNewtonIteration ( it )
  (
   let ((srch "NR iteration "))
   (setq srch (concat srch it)) 	
   (search-forward srch)
   (beginning-of-line)
   )
 )


(defun notAlreadyOrgBlockAnnotated()  
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "#\\+BEGIN" nil 1) nil  t)
  )  
)  

(defun DefaultOutputBuffer ()
  (
   let ((bufferName "PSFLOW-Log-Info"))
    (get-buffer-create bufferName)
    bufferName
   ) 
  )

(defun isUTH()
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "Start creating.*dataset" nil 1) t nil)
  )  
)  

(defun hasRegions()
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "#Start" nil 1) t nil)
  )  
)  

(defun isIPF()
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "TS.*/IPF" nil 1) t nil)
  )  
)  

(defun isCFLLOG()
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "guided task execution" nil 1) t nil)
  )  
)  

(defun hasIMEX-IPR()
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "Before mapping: IPR table for well" nil 1) t nil )
  )
)  


(defun SelectDefaultOutputBuffer ()
  (interactive)
  (
   let ((bufferName (DefaultOutputBuffer)))
    (switch-to-buffer-other-frame bufferName)
   ) 
  )

(defun GetCurrentTimestep()
  (interactive)
  (
   let(       
     (srch-ipfts "\\(TS.*\\)/IPF:\\s-From\\s-+\\(\\w+/\\w+/\\w+\\).*to\\s-\\(\\w+/\\w+/\\w+\\).*$")	
     (ts ""))
   (save-excursion
     (search-backward-regexp srch-ipfts nil t)
     (setq ts (match-string-no-properties 1))
     )
   ts
  )
)

(defun PrintCurrentTimestep()
  (interactive)
  (
    let ((ts ""))
   (setq ts (GetCurrentTimestep) )
    (print ts)
  )
)

(defun isOperativeUth ( well ts)
  ( let ((srch " ") (inoperative "Inoperative"))
    (save-excursion
     (psf-goto-ts (number-to-string ts))
     (setq qwell (concat "'" well "'"))
     (setq srch (concat "CoFlow information for well:.*" qwell))
     (search-forward-regexp srch)
     (beginning-of-line)
     (forward-line)
     (setq current (buffer-substring (line-beginning-position) (line-end-position) ))
     (if ( string-match inoperative current) nil t)
    )
  )
)


(require `org-mouse)
(setq search-invisible t)


(font-lock-add-keywords 'org-mode '(("#\\+BEGIN.*$\\|#\\*END.*$" . 'secondary-selection)))

(setq log-highlights
      '(("DIVERGING" . 'font-lock-warning-face)))

(define-derived-mode log-mode org-mode 
  "major mode for coloring simulation log."
  (setq font-lock-defaults '(log-highlights)))

(generate-new-buffer "cfl-info")

(defvar org-related-menu
  (let((menu (make-sparse-keymap "ORG")))
  (bindings--define-key menu [ psf-annotate-for-org-mode-ipfts] '(menu-item "IPF Timestep to collapsible blocks"  convert-ipfts-to-org-blocks
									  :help "convert every IPF timestep to a collapsible block. Activates org-mode"
                                                                          :enable (and  (notAlreadyOrgBlockAnnotated) (isIPF) ) ))
  (bindings--define-key menu [ annotate-for-org-mode] '(menu-item "Log regions to collapsible blocks"  annotate-for-org-mode
									  :help "convert the IPF log-regions to collapsible blocks. Activates org-mode"
                                                                          :enable (and (notAlreadyOrgBlockAnnotated) (hasRegions)) ))
  (bindings--define-key menu [ clear-annotate-for-org-mode] '(menu-item "Remove block annotation"  clear-annotate-for-org-mode
								     :help "remove all collapsible block annotations"
								     :enable (not (notAlreadyOrgBlockAnnotated) )))
  (bindings--define-key menu [ psf-collapse-org] '(menu-item "Collapse blocks"  org-hide-block-all
							   :help "Collapse all blocks.Only show top level block headers"
							   :enable (not (notAlreadyOrgBlockAnnotated) )))
  (bindings--define-key menu [ psf-open-org] '( menu-item "Show all blocks"  org-show-all
					      :help "Open all levels of blocks. Requires collapsible blocks to be created "
					      :enable (not (notAlreadyOrgBlockAnnotated) )))
  (bindings--define-key menu [ psf-show-calculation-path] '(menu-item "Show containing regions/blocks"  psf-show-calculation-path
								    :help "Prints the sequence of log-regions/blocks containing current line. Requires collapsible blocks to be created"
								    :enable (not (notAlreadyOrgBlockAnnotated) )))  
  
  menu
  ))
  
(defvar ipf-related-menu
  (let ((menu (make-sparse-keymap "IPF")))
    (bindings--define-key menu [ psf-goto-ipfts ] '(menu-item "Goto start of IPF timestep"  psf-goto-ipfts :help "goto start of timestep")  )
    (bindings--define-key menu [ psf-current-ipfts ] '( menu-item "Goto current IPF timestep"  psf-current-ipfts :help "goto start of timestep w.r.t. current line"))
    (bindings--define-key menu [ psf-next-ipfts ] '(menu-item "Goto next IPF timestep"  psf-next-ipfts :help "goto the beginning of the next timestep w.r.t. current line"))
    (bindings--define-key menu [ psf-prev-ipfts ] '(menu-item "Goto previous IPF timestep"  psf-prev-ipfts :help "goto the beginning of the previous timestep w.r.t. current line"))
    (bindings--define-key menu [ psf-goto-solspace ] '(menu-item "Goto IPF Solution space"  psf-goto-solspace :help "goto start of selected solution space in selected timestep"))
    (bindings--define-key menu [ psf-goto-ipr-cfl ] '(menu-item "Goto CFL IPR"  psf-goto-ipr-cfl :help "goto Coflow calculated IPR")  )
    
     menu
     ))

(defvar uth-related-menu
  (let ((menu (make-sparse-keymap "UTH")))
    (bindings--define-key menu [ psf-goto-ipr ] '(menu-item "Goto IPR for well"  psf-goto-ipr :enable (hasIMEX-IPR)
							     :help "Goto IPR of selected well and timestep. Requires UTH in Explicit Coupling mode"))    
    (bindings--define-key menu [ psf-copy-ipr ] '(menu-item "Copy IPR for well"  psf-copy-ipr :enable (hasIMEX-IPR)
							     :help "Copy IPR of selected well and timestep to PSFLOW-Log-Info buffer. Requires UTH in Explicit Coupling mode"))
    (bindings--define-key menu [ psf-extract-info] '(menu-item "Well info at TS"  psf-extract-info-ts :enable (hasIMEX-IPR)
								:help "Extracts IPR/Targets/Result for well at specified timestep. Requires UTH in Explicit Coupling mode"))
    (bindings--define-key menu [ psf-imex-dat ] '(menu-item "Open IMEX dat file"  psf-imex-dat
							     :help "Open the Imex .dat file for selected reservoir in a new frame."))        
    (bindings--define-key menu [ psf-imex-out ] '(menu-item "Open IMEX out file"  psf-imex-out
							     :help "Open the Imex .out file for selected reservoir in a new frame."))        
    (bindings--define-key menu [ psf-imex-obdbg ] '(menu-item "Open IMEX obdbg file"  psf-imex-obdbg
							     :help "Open the Imex .obdbg file for selected reservoir in a new frame."))        
    menu
    ))
(defvar psf-log-menu-map (let ((psf-log-menu (make-sparse-keymap "PSFlowNavigation")))
(bindings--define-key psf-log-menu [ ipf-navigation ] `(menu-item "IPF navigation",  ipf-related-menu :enable (and (isCFLLOG) (isIPF) ) ) )
(bindings--define-key psf-log-menu [ uth-navigation ] `(menu-item "UTH navigation",  uth-related-menu :enable (and (isCFLLOG) (isUTH) ) ) )			   			   
(bindings--define-key psf-log-menu [ org-log-regions ] `(menu-item "Log-Regions",  org-related-menu :enable (isCFLLOG) ) )
(bindings--define-key psf-log-menu [ psf-replace-timecol ] '(menu-item "Replace timecol"  psf-replace-timecol
								       :enable (isCFLLOG)
								       :help "replace the timecol to remove irrelevant differences between buffers")  )

psf-log-menu

))

(define-key-after (lookup-key global-map [menu-bar]) [psf-log-menu]
                            (cons "PSFFLOW-Log" psf-log-menu-map) 'Help)

