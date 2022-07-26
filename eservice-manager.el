(setq log-buffer (get-buffer-create "esm-log-buffer"))

(setq esm/services nil)

(defun start-process-once (name shell-cmd)
  (interactive)
  (if (not (position-if (lambda (s) (equal name s)) esm/services))
   (progn (start-process-shell-command name log-buffer shell-cmd)
		  (setq esm/services (push name esm/services)))))

(start-process-once "some" "/bin/pwd")



