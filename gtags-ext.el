;;; gtags-ext.el --- gtags extention for Emacs
;; Time-stamp: <2012-04-12 18:00:00 bitswitcher>
;; Copyright (C) 2012 bitswitcher

(defun gtags-goto-tag (tagname flag &optional other-win)
  (let (option context save prefix buffer lines flag-char)
    (setq save (current-buffer))
    (setq flag-char (string-to-char flag))
    ; Use always ctags-x format.
    (setq option "-x")
    (if (char-equal flag-char ?C)
        (setq context (concat "--from-here=" (number-to-string (gtags-current-lineno)) ":" buffer-file-name))
        (setq option (concat option flag)))
    (cond
     ((char-equal flag-char ?C)
      (setq prefix "(CONTEXT)"))
     ((char-equal flag-char ?P)
      (setq prefix "(P)"))
     ((char-equal flag-char ?g)
      (setq prefix "(GREP)"))
     ((char-equal flag-char ?I)
      (setq prefix "(IDUTILS)"))
     ((char-equal flag-char ?s)
      (setq prefix "(S)"))
     ((char-equal flag-char ?r)
      (setq prefix "(R)"))
     (t (setq prefix "(D)")))
    ;; load tag
    (if gtags-select-buffer-single
        (progn
          ; delete "*GTAGS SELECT*" buffer info from gtags-buffer-stack and gtags-point-stack
          (let (now-gtags-buffer-stack now-buffer now-gtags-point-stack now-point)
            (setq now-gtags-buffer-stack (reverse gtags-buffer-stack))
            (setq now-gtags-point-stack (reverse gtags-point-stack))
            (setq gtags-buffer-stack nil)
            (setq gtags-point-stack nil)
            (while now-gtags-buffer-stack
              (setq now-buffer (car now-gtags-buffer-stack))
              (setq now-point (car now-gtags-point-stack))
              (if (and (buffer-name now-buffer) (not (string-match "*GTAGS SELECT*" (buffer-name now-buffer))))
                  (progn
                    (setq gtags-buffer-stack (cons now-buffer gtags-buffer-stack))
                    (setq gtags-point-stack (cons now-point gtags-point-stack))))
              (setq now-gtags-buffer-stack (cdr now-gtags-buffer-stack))
              (setq now-gtags-point-stack (cdr now-gtags-point-stack))))
          ; kill "*GTAGS SELECT*" buffer
          (let (now-buffer-list now-buffer)
            (setq now-buffer-list (buffer-list))
            (while now-buffer-list
              (setq now-buffer (car now-buffer-list))
              (if (string-match "*GTAGS SELECT*" (buffer-name now-buffer))
                  (kill-buffer now-buffer))
              (setq now-buffer-list (cdr now-buffer-list))))))
    (setq buffer (generate-new-buffer (generate-new-buffer-name (concat "*GTAGS SELECT* " prefix tagname))))
    (set-buffer buffer)
    ;
    ; Path style is defined in gtags-path-style:
    ;   root: relative from the root of the project (Default)
    ;   relative: relative from the current directory
    ;	absolute: absolute (relative from the system root directory)
    ;
    (cond
     ((equal gtags-path-style 'absolute)
      (setq option (concat option "a")))
     ((equal gtags-path-style 'root)
      (let (rootdir)
        (if gtags-rootdir
          (setq rootdir gtags-rootdir)
         (setq rootdir (gtags-get-rootpath)))
        (if rootdir (cd rootdir)))))
    (message "Searching %s ..." tagname)
    (if (not (= 0 (if (equal flag "C")
                      (call-process "global" nil t nil option "--encode-path=\" \t\"" context tagname)
                      (call-process "global" nil t nil option "--encode-path=\" \t\"" tagname))))
	(progn (message (buffer-substring (point-min)(1- (point-max))))
               (gtags-pop-context))
      (goto-char (point-min))
      (setq lines (count-lines (point-min) (point-max)))
      (cond
       ((= 0 lines)
         (cond
          ((char-equal flag-char ?P)
           (message "%s: path not found" tagname))
          ((char-equal flag-char ?g)
           (message "%s: pattern not found" tagname))
          ((char-equal flag-char ?I)
           (message "%s: token not found" tagname))
          ((char-equal flag-char ?s)
           (message "%s: symbol not found" tagname))
          (t
           (message "%s: tag not found" tagname)))
	(gtags-pop-context)
	(kill-buffer buffer)
	(set-buffer save))
       ((= 1 lines)
	(message "Searching %s ... Done" tagname)
	(gtags-select-it t other-win))
       (t
        (if (null other-win)
            (switch-to-buffer buffer)
          (switch-to-buffer-other-window buffer))
        ;; added by tani
        (goto-char (point-min))
        (while (not (eobp))
          (put-text-property (point) (+ 16 (point)) 'invisible t)
          (forward-line 1))
        (goto-char (point-min))
	(switch-to-buffer buffer)
	(gtags-select-mode))))))

(provide 'gtags-ext)

;;; gtags-ext.el ends here
