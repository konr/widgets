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



;; Todo
;;; - Window width resizing
;;; - When line is large

(defun create-popup ()
  (interactive)
  (save-excursion
    (goto-char (window-start))
    (destructuring-bind (edge-pos edge-padding) (place)
      (if (= edge-pos (line-end-position))
          (let* ((overlay (make-overlay edge-pos edge-pos))
                 (first-line (line-number-at-pos (window-start)))
                 (message "[ HUAHUEHUAHUE ]")
                 (after-string
                  (concat (make-string (- edge-padding (length message) 1) ?\ )
                          (propertize message 'face 'yascroll:thumb-text-area))))
            (put-text-property 0 1 'cursor t after-string)
            (overlay-put overlay 'after-string after-string)
            (overlay-put overlay 'window (selected-window))
            (setq floating-boxes (cons overlay floating-boxes))
            )
        (let ((overlay (make-overlay edge-pos (1+ edge-pos)))
              (display-string
               (propertize " "
                           'face 'yascroll:thumb-text-area
                           'cursor t)))
          (overlay-put overlay 'display display-string)
          (overlay-put overlay 'window (selected-window))
          (setq floating-boxes (cons overlay floating-boxes)))))))

(defun redraw-popup (&rest bar)
  (destroy-popup)
  (message "Redrawing")
  (create-popup))

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
