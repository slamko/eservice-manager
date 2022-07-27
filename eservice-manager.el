(setq esm/log-buffer (get-buffer-create "esm-log-buffer"))

(setq esm/services nil)

(defstruct procd
  proc-obj
  (log-buffer esm/log-buffer))

(defun esm/start-process (name shell-cmd &optional dedicated-log-buffer)
  (interactive)
  (start-process-shell-command
		 name
		 (if dedicated-log-buffer
			 (get-buffer-create name)
		   esm/log-buffer)
		 shell-cmd))

(defun esm/start-process-once (name shell-cmd &optional dedicated-log-buffer)
  (interactive)
  (if (not (assq (intern name) esm/services))
	  (progn
		(let ((proc (esm/start-process name shell-cmd dedicated-log-buffer)))
		  (setq esm/services
				(push (cons (intern name)
							(make-procd
							 :proc-obj proc
							 :log-buffer (if dedicated-log-buffer (get-buffer-create name) esm/log-buffer)))
					  esm/services))))))

(defun esm/kill-process (name &optional kill-dedicated-buffer)
  (interactive)
  (let ((proc (cdr (assq (intern name) esm/services))))
	(when proc
	  (with-current-buffer (procd-log-buffer proc)
		(kill-process (procd-proc-obj proc))
		(setq esm/services (assq-delete-all (intern name) esm/services))
		(when kill-dedicated-buffer (kill-buffer name))))))

(esm/start-process-once "meer" "/bin/pnmixer")
(esm/kill-process "meer")

