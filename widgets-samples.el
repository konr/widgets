
(setq clippy
      "
        _.-;:q=._
      .' j=\"\"^k;:\\.
     ; .F       \";`Y
    ,;.J_        ;'j
  ,-;\"^7F       : .F
 ,-'-_<.        ;gj. _.,--
;  _,._`\\.     : `T\"5,
: `?8w7 `J  ,-'\" -^q. `
 \\;._ _,=' ;   n58L Y.
   F;\";  .' k_ `^'  j'
   J;:: ;     \"y:-='
    L;;==      |:;   jT\\   _________________________
    L;:;J      J:L  7:;'   / It looks like you want /
    I;|:.L     |:k J:.' , / to build an emacs pkg  /
    |;J:.|     ;.I F.:   /_/----------------------/
    J;:L::     |.| |.J  /,
    J:`J.`.    :.J |. L .
     L :k:`._ ,',j J; |  `
     I :`=.:.\"_\".'  L J
     |.:  `\"-=-'    |.J
     `: :           ;:;
       J: :         /.;'
       k;.\\.    _.;:Y'
         `Y;.\"-=';:='
           `\"===\"")

(defgroup widgets nil
  "Widgets!"
  :group 'convenience
  :prefix "widgets/")

(defface time-face
  '((t (:background "black" :foreground "green")))
  "Face for text-area scroll bar thumb."
  :group 'floating-box)

(defface time-face-2
  '((t (:background "slateblue" :foreground "golden")))
  "Face for text-area scroll bar thumb."
  :group 'floating-box)


(defface time-face-3
  '((t (:background "blue" :foreground "white")))
  "Face for text-area scroll bar thumb."
  :group 'floating-box)

(defface time-face-4
  '((t (:background "#9f0" :foreground "black")))
  "Face for text-area scroll bar thumb."
  :group 'floating-box)

(defun time-widget ()
  '(function (lambda () (format "Time: %s" (format-time-string "%Hh%M")))
             face time-face))

(defun calendar-widget ()
  '(function (lambda () (shell-command-to-string "cal"))
             face time-face-2))


(defun ascii-art-widget (ascii)
  `(function (lambda () ,ascii)
             face time-face-3))

(defun buffer-info-mode ()
  '(function (lambda () (concat
                    (propertize "*Current Major Mode*"
                                'font-lock-face `(:weight bold))
                    "\n"
                    (format "%s" (major-mode (current-buffer)))))
             face time-face-4))

(defun love-widget ()
  '(function (lambda () (format "< k+t 3"))
             margin-right 15
             face time-face-3))
