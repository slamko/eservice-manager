(defconst esm/log-buffer (get-buffer-create "esm-log-buffer")
  "Default buffer running all the services")

(defvar-local esm/services nil)

(defstruct procd
  proc-obj
  (log-buffer esm/log-buffer))

(defun esm/start-process (name shell-cmd &optional dedicated-log-buffer)
  (interactive)
  (let* ((proc-buffer (if dedicated-log-buffer (get-buffer-create name) esm/log-buffer))
		(proc (start-process-shell-command
			   name
			   proc-buffer
			   shell-cmd)))
	(make-procd
	 :proc-obj proc
	 :log-buffer proc-buffer)))

(defun esm/start-process-once (name shell-cmd &optional dedicated-log-buffer)
  (interactive)
  (if (not (assq (intern name) esm/services))
	  (progn
		(let ((new-proc (esm/start-process name shell-cmd dedicated-log-buffer)))
		  (setq esm/services
				(push (cons (intern name) new-proc) esm/services))))))

(defun esm/do-kill-proc (proc &optional kill-proc-buffer)
  (when proc
	  (with-current-buffer (procd-log-buffer proc)
		(kill-process (procd-proc-obj proc)))))

(defun esm/kill-process (name &optional kill-process-buffer)
  "Kill process with name NAME"
  (interactive)
  (let ((proc (cdr (assq (intern name) esm/services))))
	(esm/do-kill-proc proc kill-process-buffer)
	(setq esm/services (assq-delete-all (intern name) esm/services))
	(when kill-proc-buffer (kill-buffer name))))

(defun esm/kill-all (&optional kill-process-buffers)
  "Kill all services under the control of 'eservice-manager'"
  (interactive)
  (dolist (proc esm/services)
	(esm/do-kill-proc (cdr proc) kill-process-buffers)
	(setq esm/services (assq-delete-all (car proc) esm/services))
	(when kill-proc-buffer (kill-buffer name))))
  
(provide 'eservice-manager)
