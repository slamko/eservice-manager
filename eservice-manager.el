;; eservice-manager.el - GNU Emacs extension for managing services when using Emacs as a Desktop Environment. 

;; Copyright (C) 2022 Viacheslav Chepelyk-Kozhin.
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later version.
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


(require 'cl-lib)

(defconst esm/log-buffer (get-buffer-create "esm-log-buffer")
  "Default buffer running all the services")

(defvar-local esm/services nil)

(cl-defstruct procd
  (proc-obj nil)
  (log-buffer nil))

(defun esm/start-process (name shell-cmd use-log-buffer &optional dedicated-buffer)
  (interactive)
  (let* ((proc-buffer
          (when use-log-buffer (if dedicated-buffer (get-buffer-create name) esm/log-buffer)))
		(proc (start-process-shell-command
			   name
			   proc-buffer
			   shell-cmd)))
	(make-procd
	 :proc-obj proc
	 :log-buffer proc-buffer)))

(defun esm/start-process-once (name shell-cmd use-log-buffer &optional dedicated-buffer)
  (interactive)
  (if (not (assq (intern name) esm/services))
	  (progn
		(let ((new-proc (esm/start-process name shell-cmd use-log-buffer dedicated-buffer)))
		  (setq esm/services
				(push (cons (intern name) new-proc) esm/services))))))

(defun esm/do-kill-proc (proc)
  (when proc
	(progn
	  (with-current-buffer (procd-log-buffer proc)
		  (kill-process (procd-proc-obj proc))
	  t))))

(defun esm/do-kill-buffer (name)
  (let ((kill-buffer-std-query (copy-tree kill-buffer-query-functions)))
	(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
	(kill-buffer name)
	(setq kill-buffer-query-functions kill-buffer-std-query)))

(defun esm/kill-process (name &optional kill-proc-buffer)
  "Kill process with name NAME"
  (interactive)
  (let ((proc (cdr (assq (intern name) esm/services))))
	(let ((result (esm/do-kill-proc proc kill-proc-buffer)))
	  (setq esm/services (assq-delete-all (intern name) esm/services))
	  (when (kill-proc-buffer (procd-proc-obj proc)) (esm/do-kill-buffer name))
	  result)))

(defun esm/kill-all (&optional kill-proc-buffers)
  "Kill all services under the control of 'eservice-manager'"
  (interactive)
  (let (result)
	(dolist (proc esm/services result)
	  (setq result t)
	  (esm/do-kill-proc (cdr proc) kill-proc-buffers)
	  (setq esm/services (assq-delete-all (car proc) esm/services))
	  (when (and kill-proc-buffers (procd-proc-obj proc)) (esm/do-kill-buffer (car proc))))))
  
(provide 'eservice-manager)
