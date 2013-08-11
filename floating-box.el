(defun floating-box-mode ())



(defun old-create-popup ()
  (interactive)
  (let* ((x (current-column))
         (y (line-number-at-pos))
         (x$ (window-width (selected-window)))
         (popup (popup-create 1 30 10 :margin-left 10 :margin-right 20)))
    (popup-set-list popup '("quid" "est" "veritas?"))
    (popup-draw popup)
    (setq floating-boxes (cons popup floating-boxes))))

(defun destroy-popup ()
  (interactive)
  (loop for box in floating-boxes
        doing (delete-overlay box)
        finally do (setq floating-boxes nil)))

(defun place ()
  "Return \(POINT PADDING) where POINT is the most neareat
logical position to the right-edge of the window, and PADDING is
a positive number of padding to the edge."
  (save-excursion

    (let* ((window-width (window-width))
           (window-margin (destructuring-bind (left-margin . right-margin)
                              (window-margins)
                            (+ (or left-margin 0) (or right-margin 0))))
           (column-bol (progn (yascroll:vertical-motion (cons 0 0))
                              (current-column)))
           (column-eol (progn (yascroll:vertical-motion
                               (cons (- window-width 1 (if window-system 0 1)) 0))
                              (current-column)))
           (column-eol-visual (- column-eol column-bol))

           (padding (- window-width
                       window-margin
                       column-eol-visual
                       (if window-system 0 1))))
      (list (point) padding))))


(defun time-widget ()
  '(function (lambda () (format "Time: %s" (format-time-string "%Hh%M")))
             face '((t (:background "black" :foreground "green")))))

(defun calendar-widget ()
  '(function (lambda () (shell-command-to-string "cal"))
             face '((t (:background "maroon" :foreground "golden")))))

(defun buffer-info-mode ()
  '(function (lambda () (concat
                           (propertize "*Current Major Mode*"
                                       'font-lock-face `(:weight bold))
                           "\n"
                           (format "%s" (major-mode (current-buffer)))))
             face '((t (:background "#9f0" :foreground "black")))))

(defun love-widget ()
  '(function (lambda () (format "< k+t 3"))
             margin-right 15
             face '((t (:background "red" :height 2.1 :foreground "white")))))

(defun gen-gnu ()
  )

;; Todo
;;; - Window width resizing
;;; - When line is large

(defvar floating-boxes nil)

(defun create-popup (message i j face)
  (interactive)
  (save-excursion
    (goto-char (window-start))
    (next-line i)
    (destructuring-bind (edge-pos edge-padding) (place)
      (if (= edge-pos (line-end-position))
          (let* ((overlay (make-overlay edge-pos edge-pos))
                 (first-line (line-number-at-pos (window-start)))
                 (len (- edge-padding (length message) j))

                 (len (- edge-padding (length message) j))
                 )
            (let ((after-string
                   (concat (make-string len ?\ )
                           (propertize message 'face face))))
              (put-text-property 0 1 'cursor t after-string)
              (overlay-put overlay 'after-string after-string)
              (overlay-put overlay 'window (selected-window))
              (setq floating-boxes (cons overlay floating-boxes))
              ))
        (message "FIXME!!!!!!!!!!!!!!!")))))


(defun create-popup-with-various-lines (widget start)
  (interactive)
  (let* ((target (funcall widget))
         (fun (plist-get target 'function))
         (face (plist-get target 'face))
         (padding-h 2)
         (padding-v 1)
         (margin-top 1)
         (margin-right (or (plist-get target 'margin-right) 3))
         (splat (split-string (funcall fun) "\n"))
         (size (apply #'max (mapcar #'length splat)))
         (splat (mapcar (lambda (x) (concat
                                       (make-string padding-h ? )
                                       x (make-string (- size (length x)) ? )
                                       (make-string padding-h ? )))
                        (append (make-list padding-v " ") splat (make-list padding-v " ")))))
    (loop for x in splat
          and i = start then (1+ i)
          doing (create-popup x (+ i margin-top) margin-right face)
          finally return i)))

(defun create-various-widgets ()
  (let ((widgets '(time-widget calendar-widget buffer-info-mode love-widget))
        (spacing 1))
    (loop for widget in widgets
          and pos = 0 then
          (+ spacing (create-popup-with-various-lines widget pos)))))

(defun redraw-popup (&rest bar)
  (destroy-popup)
  (message "Redrawing")
  (create-various-widgets))

(defun add-or-remove-hooks (addp)
  (if addp
      (progn
        ;;(add-hook 'before-change-functions 'yascroll:before-change nil t)
        (add-hook 'window-scroll-functions 'redraw-popup)
        ;;(add-hook 'window-configuration-change-hook
        ;;'yascroll:after-window-configuration-change nil t)
        )
    (remove-hook 'window-scroll-functions 'redraw-popup t)))



(provide 'floating-box)
