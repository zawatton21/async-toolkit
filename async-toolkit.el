;;; async-emacs-toolkit.el --- Asynchronous file and buffer operations for Emacs

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
(defun async-external-save-buffer ()
  "Save the current buffer using an external process asynchronously and measure elapsed time for debugging."
  (interactive)
  (require 'cl-lib)
  (require 'async)

  (unless (buffer-file-name)
    (message "Buffer is not associated with a file. Skipping external save.")
    (cl-return-from async-external-save-buffer))

  (let* ((start-time (current-time))
         (file-name (buffer-file-name))
         (buf-content (buffer-string))
         (temp-file (make-temp-file "emacs-external-save" nil ".tmp" buf-content)))

    (let* ((win-temp-file (replace-regexp-in-string "/" "\\\\" temp-file))
           (win-file-name (replace-regexp-in-string "/" "\\\\" file-name))
           (cmd (if (eq system-type 'windows-nt)
                    (format "move /Y \"%s\" \"%s\"" (shell-quote-argument win-temp-file) (shell-quote-argument win-file-name))
                  (format "mv %s %s" (shell-quote-argument temp-file) (shell-quote-argument file-name)))))

      ;; Convert file paths for Windows, if necessary
      (when (eq system-type 'windows-nt)
        (setq win-temp-file (encode-coding-string win-temp-file 'utf-8)
              win-file-name (encode-coding-string win-file-name 'utf-8)))

      ;; Use async-start to run the shell command asynchronously
      (async-start
       `(lambda ()
          (shell-command ,cmd))
       `(lambda (result)
          (if (= result 0)
              (progn
                (message "Buffer saved using external process asynchronously.")
                (with-current-buffer ,(current-buffer)
                  ;; Update the buffer's file modification time to the latest
                  (let ((new-modtime (nth 5 (file-attributes ,file-name))))
                    (set-visited-file-modtime new-modtime))
                  (revert-buffer t t t)
                  (set-buffer-modified-p nil)
                  (run-hooks 'after-save-hook))
                ;; Clean up the temporary file
                (delete-file ,temp-file))
            ;; If the operation failed, display the output buffer for diagnostics
            (with-current-buffer (get-buffer-create "*External Save Output*")
              (insert "Error during external save: " (number-to-string result))
              (display-buffer (current-buffer)))))))))

;;;###autoload
(defun async-save-buffer-internal (orig-func &rest args)
  "Advice to optionally redirect `save-buffer` based on `enable-async-save-buffer-advice`."
  (if enable-async-save-buffer-advice
      (if async-save-in-progress
          (apply orig-func args)  ; async-save-buffer からの再帰呼び出しを避ける
        (async-external-save-buffer-with-update))  ; 通常の save-buffer 呼び出しを async-save-buffer にリダイレクト
    (apply orig-func args)))  ; 制御用変数が無効な場合は、常に元の関数を実行


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
                    (async-external-save-buffer-with-update))))
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
                         (async-external-save-buffer-with-update)))
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
(defun enable-auto-save-all-buffers ()
  "Enable automatic saving of all modified file buffers."
  (interactive)
  (when auto-save-all-buffers-enabled
    ;; タイマーが既に存在していればキャンセルする
    (when (timerp auto-save-all-timers-timer)
      (cancel-timer auto-save-all-timers-timer))
    ;; 新しいタイマーを設定
    (setq auto-save-all-timers-timer
          (run-with-idle-timer auto-save-all-buffers-interval t 'save-all-buffers))))

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

;;;###autoload
(defun async-replace-buffer-contents-with (temp-buffer-name)
  "Replace the current buffer's contents with that of TEMP-BUFFER-NAME asynchronously, if there are differences."
  (let* ((target-buffer (current-buffer))
         (temp-buffer (get-buffer temp-buffer-name)))
    (unless temp-buffer
      (error "Temporary buffer does not exist"))
    (let ((temp-file (with-current-buffer temp-buffer
                      (make-temp-file "emacs-async-replace" nil ".tmp"))))
      ;; 一時ファイルに内容を書き込む
      (write-region (point-min) (point-max) temp-file nil 'silent)
      (let ((diff-cmd (format "diff --speed-large-files %s %s" (shell-quote-argument temp-file) (shell-quote-argument (buffer-file-name target-buffer)))))
        ;; 非同期diffコマンドを開始
        (let ((start-time (current-time)))
          (async-start
           `(lambda ()
              (let ((diff-start-time (current-time)))
                ;; diffコマンドの実行と、開始時刻とのペアを返す
                (cons (shell-command-to-string ,diff-cmd) diff-start-time)))
           `(lambda (result)
              ;; diffの結果と開始時刻を分解
              (let* ((diff-output (car result))
                     (diff-start-time (cdr result))
                     (end-time (current-time)))
                (when (buffer-live-p ,target-buffer)
                  (if (string-empty-p diff-output)
                      nil  ;; 差分がない場合は何もしない
                    (with-current-buffer ,target-buffer
                      (erase-buffer)
                      (insert-file-contents ,temp-file)
                      ;; ファイルの最新の修正時刻を使用してバッファの変更時刻を更新
                      (let ((new-modtime (nth 5 (file-attributes (buffer-file-name)))))
                        (set-visited-file-modtime new-modtime)))))
                  (delete-file ,temp-file)))))))))

;;;###autoload
(defun async-revert-buffer ()
  "Asynchronously revert the current buffer using an external process, keeping as much buffer state as possible."
  (interactive)
  (let ((start-time (current-time))
        (file-name (buffer-file-name)))
    (unless file-name
      (error "Buffer is not visiting a file"))
    ;; 外部コマンドの代わりに、一時バッファを用いる
    (let ((output (with-temp-buffer
                    (insert-file-contents file-name)
                    (buffer-string)))
          (read-end-time (current-time)))
      ;;(message "Time to read file into temp buffer: %fs" 
      ;;          (float-time (time-subtract read-end-time start-time)))
      ;; 現在のバッファと一時バッファの内容を置き換え
      (let ((temp-buffer (generate-new-buffer " *temp file content*")))
        (with-current-buffer temp-buffer
          (insert output))
        (async-replace-buffer-contents-with temp-buffer)
        ;;(replace-buffer-contents temp-buffer)
        (kill-buffer temp-buffer)
        (let ((end-time (current-time)))
          ;;(message "Total async revert time: %fs" 
          ;;         (float-time (time-subtract end-time start-time)))
          )))))

;;;###autoload
(defun async-revert-advice (original-revert &rest args)
  "Advice to use `async-revert-buffer' instead of `revert-buffer' when `enable-async-revert-advice' is t."
  (if enable-async-revert-advice
      (async-revert-buffer)
    (apply original-revert args)))

(advice-add 'revert-buffer :around #'async-revert-advice)

(provide 'async-emacs-toolkit)

;;; async-emacs-toolkit.el ends here
