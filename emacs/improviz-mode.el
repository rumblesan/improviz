(defvar improviz-mode-hook nil)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pz\\'" . improviz-mode))

(defun improviz-mode ()
  "Major mode for Improviz editing"
  (interactive)
  (general-nvmap
   :prefix ","
   "e" 'send-improviz-program
   ))

(defun send-improviz-program ()
  (interactive)
  (request
   "http://localhost:3000/read"
   :type "POST"
   :data (buffer-string)
   :parser 'json-read
   :success (function*
             (lambda ()
               (message "Sent program"))))
  )

(provide 'improviz)
