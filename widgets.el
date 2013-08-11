;;; widgets.el --- Widgets

;; Author: Konrad Scorciapino <konr@konr.mobi>
;; Keywords: widgets, productivity
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; Todo
;;; - Window width resizing
;;; - When line is large

(defun widgets/destroy-all ()
  (interactive)
  (loop for widget in widgets/live
        doing (delete-overlay widget)
        finally do (setq widgets/live nil)))

(defun widgets/place ()
  "(from yascroll.el)

Return \(POINT PADDING) where POINT is the most neareat
logical position to the right-edge of the window, and PADDING is
a positive number of padding to the edge."
  (save-excursion

    (let* ((window-width (window-width))
           (window-margin (destructuring-bind (left-margin . right-margin)
                              (window-margins)
                            (+ (or left-margin 0) (or right-margin 0))))
           (column-bol (progn (vertical-motion (cons 0 0))
                              (current-column)))
           (column-eol (progn (vertical-motion
                               (cons (- window-width 1 (if window-system 0 1)) 0))
                              (current-column)))
           (column-eol-visual (- column-eol column-bol))

           (padding (- window-width
                       window-margin
                       column-eol-visual
                       (if window-system 0 1))))
      (list (point) padding))))

(setq widgets/live nil)

(defun widgets/create-line (text i)
  "Returns the "
  (interactive)
  (save-excursion
    (goto-char (window-start))

    (let* ((left-to-walk (forward-line i))
           (wra (widgets/place)) (edge-pos (car wra)) (edge-padding (cadr wra))
           (padding (- edge-padding (length text)))
           (fixanda (not (= edge-pos (line-end-position))))
           (overlay (if (> left-to-walk 0)
                        (make-overlay (+ left-to-walk edge-pos 10)
                                      (+ (if fixanda 1 0) edge-pos left-to-walk) )
                      (make-overlay edge-pos (+ (if fixanda 1 0) edge-pos))))
           (after-string
            (if (> padding 0)
                (concat (make-string padding ?\ ) text)
              (progn (substring text (- padding))))))
      (put-text-property
       0 1 'cursor t after-string)
      (if fixanda
          (overlay-put overlay 'display after-string)
        (overlay-put overlay 'after-string after-string))
      (overlay-put overlay 'window (selected-window))
      (setq widgets/live (cons overlay widgets/live))
      (= (forward-line 2) 0))))


(defun widgets/format-widget (target)
  (interactive)
  (let* ((fun (plist-get target 'function)) (face (plist-get target 'face))
         (padding-h 2)
         (padding-v 1)
         (margin-right 3)
         (spacing 1)
         (splat (split-string (funcall fun) "\n"))
         (size (apply #'max (mapcar #'length splat)))
         (splat (mapcar (lambda (x)
                          (concat (propertize (concat
                                               (make-string padding-h ? )
                                               x (make-string (- size (length x)) ? )
                                               (make-string padding-h ? ))
                                              'face face)
                                  (make-string margin-right ? )))
                        (append (make-list padding-v " ") splat (make-list padding-v " ")))))
    (append splat (make-list spacing " "))))

(defun widgets/create-widgets ()
  (interactive)
  (let* ((widgets (list (time-widget) (calendar-widget) (buffer-info-mode)
                        ;; (love-widget)
                        (ascii-art-widget clippy)
                        ))
         (margin-top 1)
         (lines (mapcan #'widgets/format-widget widgets)))
    (loop for x in lines
          and i = 0 then (1+ i)
          and cont = t then (widgets/create-line (nth i lines) (+ i margin-top))
          while cont
          finally (let ((remainder (reverse (subseq lines i))))
                    (loop for y in remainder
                          doing (widgets/create-line y (+ i margin-top)))))))


(defun widgets/redraw (&rest bar)
  (destroy-popup)
  ;;(message "Redrawing")
  (widgets/create-various-widgets))

(defun widgets/hookit (addp)
  (if addp
      (progn
        ;;(add-hook 'before-change-functions 'yascroll:before-change nil t)
        (add-hook 'window-scroll-functions 'widgets/redraw)
        ;;(add-hook 'window-configuration-change-hook
        ;;'yascroll:after-window-configuration-change nil t)
        )
    (remove-hook 'window-scroll-functions 'widgets/redraw t)))



(provide 'widgets)

;;
