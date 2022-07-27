(setq log-buffer (get-buffer-create "esm-log-buffer"))

(setq esm/services nil)

(defun esm/start-process (name shell-cmd &optional dedicated-log-buffer)
  (interactive)
  (start-process-shell-command
		 name
		 (if dedicated-log-buffer
			 (get-buffer-create name)
		   log-buffer)
		 shell-cmd))

(defun esm/start-process-once (name shell-cmd &optional dedicated-log-buffer)
  (interactive)
  (if (not (assq (intern name) esm/services))
	  (progn
		(let ((proc (esm/start-process name shell-cmd dedicated-log-buffer)))
		  (setq esm/services (push (cons (intern name) 'proc) esm/services))))))

(defun esm/kill-process (name &optional kill-dedicated-buffer)
  (interactive)
  (switch-to-buffer log-buffer)
  (kill-process (assoc name esm/services))
  (when kill-dedicated-buffer (kill-buffer name)))

(esm/start-process-once "some" "/bin/pwd")
(esm/kill-process "some")

