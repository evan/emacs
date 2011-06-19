;; be more like a mac app

(defun new-frame-with-scratch ()
  "Open a new frame with scratch buffer selected"
  (interactive)
  (let ((frame (make-frame)))
	(select-frame-set-input-focus frame)
	
	(if (get-buffer "*scratch*")
	    (switch-to-buffer "*scratch*" 'norecord))))
  
(global-set-key [(super n)] 'new-frame-with-scratch)


;; emulate Cmd-W behavior. gleaned from aquamacs
(defun close-window ()
  "Emulate Mac default Cmd-W behavior: close the current window and 
    recursively whatever its container is up to the last frame."
  (interactive)
  ;; quit from minibuffer
  (when (minibuffer-window-active-p
	 (minibuffer-window (selected-frame)))
    (abort-recursive-edit))

  ;; handle tabs
  (cond
   ((and (boundp 'tabbar-mode) tabbar-mode)
    (let ((tabbar-retain-windows-when-tab-deleted nil))
      (tabbar-close-tab)))
   (t (delete-window-or-frame))))

(defun delete-window-or-frame (&optional window)
  "Deletes window and its frame if it is the only one"
  (let ((w (or window (selected-window))))
    (select-window w)
    (if (one-window-p t)
	(delete-frame-or-hide)
      (delete-window (selected-window)))))

(defun delete-frame-or-hide (&optional frame)
  "Deletes the frame, or hides it if it is the last one"
  (condition-case nil
      (delete-frame (or frame (selected-frame)) 'force)
    (error
     (let ((f (or frame (selected-frame))))
       (run-hook-with-args 'delete-frame-functions f)
       (let ((confirm-nonexistent-file-or-buffer)
	     (tabbar-mode nil))
	 (set-window-dedicated-p (selected-window) nil)
	 (if (get-buffer "*scratch*")
	     (switch-to-buffer "*scratch*" 'norecord)))
       (make-frame-invisible f t)))))

(global-set-key [(super w)] 'close-window)
