;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.


(setq overlaylist () )

(defun create-overlay (regions)
  (
   let ((ov-list () ) (r ()) (o nil))
    (while regions
      (setq r (car regions))
      (setq o (make-overlay (car r) (cdr r)))
      (setq regions (cdr regions))
      (setq ov-list (cons o ov-list))
      )
    ov-list
    )
)  

(defun apply-overlays ( ovs)
  (
   let( (r nil) (ov nil))
    (while ovs 
      (setq ov (car ovs))
      (while ov
	(setq r (car ov))
	(overlay-put r 'face '(:foreground "green"))
	(setq ov (cdr ov))
      )	
     (setq ovs (cdr ovs))
     )
    )
 ) 


(defun push-overlay (begin end key)
  (
   let ((x (make-overlay begin end)))
   (print key) 
   (overlay-put x 'face '(:foreground "red"))
   (push (cons key x) overlaylist)
   )
)

(defun pop-overlay (begin end key)
  (
   let ((o nil))
    (setq o (cdr (assoc key overlaylist)))
    (print key)
    (delete-overlay o)
  ) 
)


(defun skip-to (end-key)
  (interactive)
  (
   let((end 0))
   (print (concat "going to " end-key))
   (search-forward end-key)
   (beginning-of-line)
   (next-line)
   (setq end (point))
   )
)


(defun color-log-section(name)
  (interactive
   (list
    (read-string "region name: ")
    )
  ) 
  (
   let (  (ovs () )(f "") (fm "") (begin 0) (end 0) (srch "") (key-begin "") (key-end "") (done nil))
    (setq key-begin (concat "#\\+BEGIN_" name))
    (setq key-end (concat "#+END_" name))
    (setq srch "#\\+BEGIN.*\\|#\\+END.*")
     (save-excursion
      (search-backward-regexp key-begin)
      (next-line)
      (beginning-of-line)
      (setq begin (point))
      (while (and (not done ) (search-forward-regexp srch nil t) )
	(setq f (match-string-no-properties 0))

	( if (equal f key-end)
	    (progn
	      (setq done t)
	      (previous-line)
	      (end-of-line)
	      (setq end (point) )
              (setq ovs  (cons (create-overlay  ( cons ( cons begin end ) nil))  ovs ) )		 
	      )
	  (previous-line)
	  (end-of-line)
	  (setq end (point) )
          (setq ovs  (cons (create-overlay  ( cons ( cons begin end ) nil))  ovs ) )
	  
	  (setq fm (replace-regexp-in-string "BEGIN" "END" f))
          (setq begin (skip-to fm))		
	)	       
      )
     ) 
    (apply-overlays ovs) 
  )
)


	
(defun uncolor-log-section()
  (interactive)
  (
   let (  (key "") (begin 0) (end 0) (lcontent "") (srch "") )
    (setq srch "#\\+BEGIN")
    (save-excursion
      (search-backward-regexp srch)
      (setq lcontent (buffer-substring-no-properties (line-beginning-position) (line-end-position) ))    
      (beginning-of-line)
      (setq begin (point))
      (setq srch (replace-regexp-in-string "BEGIN" lcontent))
      (search-forward srch)
      (setq end (point))
      (setq key (concat srch (number-to-string begin) (number-to-string end))) 
      (pop-overlay begin end key)
    )
   ) 
 )

(defun select-log-section(section)
  (interactive
   (list
    (read-string "log section: ")
    )
  ) 
  (
   let (  (ovs () )(f "") (fm "") (begin 0) (end 0) (srch "") (key-begin "") (key-end "") (done nil))
    (setq key-begin (concat "TS\\w+/IPF/name"))
    (setq key-end (concat "#+END_" name))
    (setq srch "#\\+BEGIN.*\\|#\\+END.*")
     (save-excursion
      (search-backward-regexp key-begin)
      (next-line)
      (beginning-of-line)
      (setq begin (point))
      (while (and (not done ) (search-forward-regexp srch nil t) )
	(setq f (match-string-no-properties 0))

	( if (equal f key-end)
	    (progn
	      (setq done t)
	      (previous-line)
	      (end-of-line)
	      (setq end (point) )
              (setq ovs  (cons (create-overlay  ( cons ( cons begin end ) nil))  ovs ) )		 
	      )
	  (previous-line)
	  (end-of-line)
	  (setq end (point) )
          (setq ovs  (cons (create-overlay  ( cons ( cons begin end ) nil))  ovs ) )
	  
	  (setq fm (replace-regexp-in-string "BEGIN" "END" f))
          (setq begin (skip-to fm))		
	)	       
      )
     ) 
    (apply-overlays ovs) 
  )
)
