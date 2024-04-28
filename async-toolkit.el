;;; async-toolkit.el --- Asynchronous file and buffer operations for Emacs

;; Copyright (C) 2024

;; Author: Your Name <your-email@example.com>
;; URL: https://github.com/zawatton21/async-toolkit
;; Keywords: lisp, async
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This Emacs package provides enhanced asynchronous file and buffer
;; operations, allowing for non-blocking saves and buffer manipulations.

;;; Requirements:

;; - `async` package
;; - `cl-lib` package

;;; Code:
(require 'cl-lib)
(require 'async)

(defvar enable-async-save-buffer-advice nil
  "Control whether to redirect `save-buffer` to `async-external-save-buffer-with-update`.")

(defvar async-save-in-progress nil
  "A flag to check if an asynchronous save is in progress.")

;;;###autoload
(defun file-locked-p ()
  "Return t if the current buffer's file is locked."
  (and buffer-file-name
       (file-exists-p (concat buffer-file-name ".#"))))

;;;###autoload
(defun async-save-buffer-internal (orig-func &rest args)
  "Advice to optionally redirect `save-buffer` based on `enable-async-save-buffer-advice`."
  (if (not enable-async-save-buffer-advice)
      (apply orig-func args)  ; Always run the original function if the control variable is disabled
    (if async-save-in-progress
        (apply orig-func args)  ; Avoid recursive call if async save is already in progress
      (let ((async-save-in-progress t))
        (async-external-save-buffer)))))  ; Redirect to async version

(advice-add 'save-buffer :around #'async-save-buffer-internal)

(defun toggle-async-save-buffer-advice ()
  "Toggle the `async-save-buffer-internal` advice on or off."
  (interactive)
  (setq enable-async-save-buffer-advice (not enable-async-save-buffer-advice))
  (message "Async save buffer advice is now %s"
           (if enable-async-save-buffer-advice "enabled" "disabled")))

(defvar enable-async-save-some-buffers-advice nil
  "Control whether to use asynchronous behavior for `save-some-buffers`.")

;;;###autoload
(defun async-save-some-buffers (orig-fun &optional arg pred)
  "Conditional advice to use `async-external-save-buffer-with-update` for `save-some-buffers`.
Uses `enable-async-save-some-buffers-advice` to determine behavior."
  (if enable-async-save-some-buffers-advice
      (let* ((switched-buffer nil)
             save-some-buffers--switch-window-callback
             queried autosaved-buffers
             files-done inhibit-message)
        (unwind-protect
            (save-window-excursion
              (dolist (buffer (buffer-list))
                (with-current-buffer buffer
                  (when (and buffer-save-without-query (buffer-modified-p))
                    (push (buffer-name) autosaved-buffers)
                    (async-external-save-buffer))))
              (setq files-done
                    (map-y-or-n-p
                     (lambda (buffer)
                       (if arg
                           t
                         (setq queried t)
                         (if (buffer-file-name buffer)
                             (format "Asynchronously save file %s? " (buffer-file-name buffer))
                           (format "Asynchronously save buffer %s? " (buffer-name buffer)))))
                     (lambda (buffer)
                       (with-current-buffer buffer
                         (async-external-save-buffer)))
                     (files--buffers-needing-to-be-saved pred)
                     '("buffer" "buffers" "save")
                     save-some-buffers-action-alist))
              (dolist (func save-some-buffers-functions)
                (setq inhibit-message (or (funcall func nil arg) inhibit-message)))
              (or queried (> files-done 0) inhibit-message
                  (cond
                   ((null autosaved-buffers)
                    (message "(No files need saving)"))
                   ((= (length autosaved-buffers) 1)
                    (message "(Asynchronously saved %s)" (car autosaved-buffers)))
                   (t
                    (message
                     "(Asynchronously saved %d files: %s)" (length autosaved-buffers)
                     (mapconcat 'identity autosaved-buffers ", "))))))
          (when switched-buffer
            (pop-to-buffer-same-window switched-buffer))))
    (funcall orig-fun arg pred)))


(advice-add 'save-some-buffers :around #'async-save-some-buffers)

(defun toggle-async-save-some-buffers-advice ()
  "Toggle the `async-save-some-buffers` advice on or off."
  (interactive)
  (setq enable-async-save-some-buffers-advice (not enable-async-save-some-buffers-advice))
  (message "Async save-some-buffers advice is now %s"
           (if enable-async-save-some-buffers-advice "enabled" "disabled")))

(defvar auto-save-all-buffers-enabled nil
  "If non-nil, enable automatic saving of all buffers.")

(defvar auto-save-all-buffers-interval 300
  "Time in seconds between automatic saves of all buffers.")

(defvar auto-save-all-timers-timer nil
  "Timer used to schedule `save-all-buffers`.")

;;;###autoload
(defun async-save-all-buffers ()
  "Save all buffers that are associated with a file and have been modified."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (buffer-modified-p))
        (org-updated-timestamp)
        (increment-review-counter)
         (async-external-save-buffer)))))

;;;###autoload
(defun enable-auto-save-all-buffers ()
  "Enable automatic saving of all modified file buffers."
  (interactive)
  (when auto-save-all-buffers-enabled
    ;; タイマーが既に存在していればキャンセルする
    (when (timerp auto-save-all-timers-timer)
      (cancel-timer auto-save-all-timers-timer))
    ;; 新しいタイマーを設定
    (setq auto-save-all-timers-timer
          (run-with-idle-timer auto-save-all-buffers-interval t 'async-save-all-buffers))))

(defun toggle-auto-save-all-buffers ()
  "Toggle the automatic saving of all buffers."
  (interactive)
  (setq auto-save-all-buffers-enabled (not auto-save-all-buffers-enabled))
  (if auto-save-all-buffers-enabled
      (enable-auto-save-all-buffers)
    (when (timerp auto-save-all-timers-timer)
      (cancel-timer auto-save-all-timers-timer)
      (setq auto-save-all-timers-timer nil)))
  (message "Auto-save all buffers is %s"
           (if auto-save-all-buffers-enabled "enabled" "disabled")))

(defvar enable-async-revert-advice nil
  "Control whether `async-revert-buffer' should be used.")

;;; グローバル変数の定義
(defvar async-diff-command "diff --speed-large-files"
  "The command used to compare files in `my-async-replace-buffer-contents-with`.")

;;;###autoload
(defun async-replace-buffer-contents-with (temp-buffer-name)
  "Replace the current buffer's contents with that of TEMP-BUFFER-NAME asynchronously, if there are differences."
  (let* ((target-buffer (current-buffer))
         (file-name (buffer-file-name target-buffer)) ; ファイル名を変数に保存
         (temp-buffer (get-buffer temp-buffer-name)))
    (unless temp-buffer
      (error "Temporary buffer does not exist"))
    (let ((temp-file (with-current-buffer temp-buffer
                      (make-temp-file "emacs-async-replace" nil ".tmp"))))
      ;; 一時ファイルに内容を書き込む
      (write-region (point-min) (point-max) temp-file nil 'silent)
      (let ((diff-cmd (format "%s %s %s" 
                              async-diff-command
                              (shell-quote-argument temp-file) 
                              (shell-quote-argument file-name))))
        ;; 非同期diffコマンドを開始
        (async-start
         `(lambda ()
            (shell-command-to-string ,diff-cmd))
         `(lambda (diff-output)
            (when (buffer-live-p ,target-buffer)
              (if (string-empty-p diff-output)
                  (message "No changes detected.")
                (progn
                  ;; 差分がある場合のみバッファを更新
                  (with-current-buffer ,target-buffer
                    (erase-buffer)
                    (insert-file-contents ,temp-file)
                    (let ((new-modtime (nth 5 (file-attributes ,file-name))))
                      (set-visited-file-modtime new-modtime)))
                  (message "Buffer updated asynchronously.")))
              (delete-file ,temp-file))))))))

;;;###autoload
(defun async-revert-buffer ()
  "Asynchronously revert the current buffer using an external process, keeping as much buffer state as possible."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (unless file-name
      (error "Buffer is not visiting a file"))
    (let ((temp-buffer-name (generate-new-buffer " *temp file content*")))
      (with-current-buffer temp-buffer-name
        (insert-file-contents file-name))
      (async-replace-buffer-contents-with temp-buffer-name))))

;;;###autoload
(defun async-revert-advice (original-revert &rest args)
  "Advice to use `async-revert-buffer' instead of `revert-buffer' when `enable-async-revert-advice' is t."
  (if enable-async-revert-advice
      (async-revert-buffer)
    (apply original-revert args)))

(advice-add 'revert-buffer :around #'async-revert-advice)

(provide 'async-toolkit)

;;; async-emacs-toolkit.el ends here
