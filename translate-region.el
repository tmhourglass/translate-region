;;; init-translate-region.el --- Translate region between Chinese and English

;; Copyright (C) 2025

;; Author: tmhourglass
;; Keywords: translation, convenience
;; Version: 0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides a function to translate the selected region
;; between Chinese and English.  If the selected region contains Chinese,
;; it will be translated to English, and vice versa.

;; Requirements:
;; - translate-shell (trans) command line tool must be installed.
;;   - macOS: brew install translate-shell
;;   - Linux: apt-get install translate-shell (or use your package manager)
;;   - Windows: can be installed through WSL or other methods
;; - (Optional) gptel package for using LLM translation.

;; Installation:
;; 1. Install the translate-shell command line tool
;; 2. (Optional) Install the gptel package if you want to use LLM translation
;; 3. Place this file in your Emacs load path
;; 4. Add the following to your Emacs configuration:
;;    (require 'translate-region)
;;    (global-set-key (kbd "C-c t") 'translate-region-zh-en)
;;    (global-set-key (kbd "C-c T") 'translate-region-naming-interactive)

;; Usage:
;; 1. Select a region of text
;; 2. Call `translate-region-zh-en` command (M-x translate-region-zh-en)
;;    or use the key binding you defined (e.g., C-c t)
;; 3. The selected text will be replaced with its translation
;; 4. You can switch between translation backends (translate-shell or gptel)
;;    using M-x translate-region-toggle-backend
;; 5. For variable or function naming, select text and use:
;;    - M-x translate-region-naming (uses default style)
;;    - M-x translate-region-naming-interactive (choose style interactively)

;; Customization:
;; - `translate-region-chinese-threshold`: Adjust the threshold for
;;   determining if text is Chinese (default: 0.1)
;; - `translate-region-trans-command`: Change the command used for
;;   translation if needed (default: "trans")
;; - `translate-region-backend`: Choose the translation backend
;;   ('translate-shell or 'gptel)
;; - `translate-region-gptel-system-message`: System message for LLM translation
;; - `translate-region-gptel-model`: LLM model to use for translation
;; - `translate-region-naming-style`: Default naming style for variable and function naming
;; - `translate-region-gptel-naming-system-message`: System message for LLM naming translation

;; Troubleshooting:
;; - If you get an error about the translate-shell command not being found,
;;   make sure it's installed and in your PATH
;; - If translations fail, check your internet connection as translate-shell
;;   requires internet access to work
;; - For gptel backend, ensure gptel is properly configured with an API key

;;; Code:

(require 'gptel)
(require 'gptel-curl)

(defgroup translate-region nil
  "Translation utilities for Emacs."
  :group 'convenience
  :prefix "translate-region-")

(defcustom translate-region-chinese-threshold 0.1
  "Threshold for determining if text is Chinese.
If the ratio of Chinese characters to total characters is greater than
this value, the text is considered Chinese."
  :type 'float
  :group 'translate-region)

(defcustom translate-region-trans-command "trans"
  "Command to call translate-shell."
  :type 'string
  :group 'translate-region)

(defcustom translate-region-backend 'translate-shell
  "Backend to use for translation.
Possible values:
- translate-shell: Use the translate-shell command line tool
- gptel: Use gptel to call LLM for translation"
  :type '(choice (const :tag "translate-shell" translate-shell)
                 (const :tag "gptel" gptel))
  :group 'translate-region)

(defcustom translate-region-gptel-system-message
  "You are a helpful translation assistant. Your task is to translate the given text between Chinese and English. If the input is in Chinese, translate it to English. If the input is in English, translate it to Chinese. Provide only the translation without any additional explanation or notes."
  "System message to send to the LLM when using gptel for translation."
  :type 'string
  :group 'translate-region)

(defcustom translate-region-gptel-model nil
  "The LLM model to use for translation when using gptel.
If nil, use the default model set in gptel."
  :type '(choice (const :tag "Default" nil)
                 (symbol :tag "Model"))
  :group 'translate-region)

(defcustom translate-region-naming-style 'camelCase
  "Default naming style for variable and function naming.
Possible values:
- camelCase: First word lowercase, subsequent words capitalized (e.g., myVariableName)
- PascalCase: All words capitalized (e.g., MyVariableName)
- snake_case: All lowercase with underscore separators (e.g., my_variable_name)
- kebab-case: All lowercase with hyphen separators (e.g., my-variable-name)
- SCREAMING_SNAKE_CASE: All uppercase with underscore separators (e.g., MY_VARIABLE_NAME)"
  :type '(choice (const :tag "camelCase" camelCase)
                 (const :tag "PascalCase" PascalCase)
                 (const :tag "snake_case" snake_case)
                 (const :tag "kebab-case" kebab-case)
                 (const :tag "SCREAMING_SNAKE_CASE" SCREAMING_SNAKE_CASE))
  :group 'translate-region)

(defcustom translate-region-gptel-naming-system-message
  "You are a helpful translation assistant specialized in creating programming variable and function names. Your task is to translate the given text between Chinese and English, and format it in a way that would be suitable for a variable or function name. If the input is in Chinese, translate it to English. If the input is in English, translate it to Chinese. Provide only the translation without any additional explanation or notes. The translation should be concise and use appropriate programming terminology."
  "System message to send to the LLM when using gptel for naming translation."
  :type 'string
  :group 'translate-region)

(defun translate-region-is-chinese (text)
  "Determine if TEXT contains Chinese characters.
Returns t if the ratio of Chinese characters to total characters
is greater than `translate-region-chinese-threshold'."
  (let ((chinese-count 0)
        (total-count (length text)))
    (when (> total-count 0)
      (dolist (char (string-to-list text))
        ;; Check if character is in Chinese Unicode ranges
        ;; Basic Chinese (including CJK ideographs, Bopomofo, Kanbun, etc.)
        (when (or (and (>= char #x4E00) (<= char #x9FFF))
                  ;; CJK Unified Ideographs Extension A
                  (and (>= char #x3400) (<= char #x4DBF))
                  ;; CJK Unified Ideographs Extension B
                  (and (>= char #x20000) (<= char #x2A6DF))
                  ;; CJK Unified Ideographs Extension C
                  (and (>= char #x2A700) (<= char #x2B73F))
                  ;; CJK Unified Ideographs Extension D
                  (and (>= char #x2B740) (<= char #x2B81F))
                  ;; CJK Unified Ideographs Extension E
                  (and (>= char #x2B820) (<= char #x2CEAF))
                  ;; CJK Unified Ideographs Extension F
                  (and (>= char #x2CEB0) (<= char #x2EBEF))
                  ;; CJK Compatibility Ideographs
                  (and (>= char #xF900) (<= char #xFAFF))
                  ;; CJK Compatibility Ideographs Supplement
                  (and (>= char #x2F800) (<= char #x2FA1F)))
          (setq chinese-count (1+ chinese-count))))
      (> (/ (float chinese-count) total-count) translate-region-chinese-threshold))))

(defun translate-region-check-command ()
  "Check if translate-shell command is available."
  (unless (executable-find translate-region-trans-command)
    (user-error "Cannot find %s command. Please install translate-shell first"
                translate-region-trans-command)))

(defun translate-region-with-translate-shell (text from-lang to-lang)
  "Translate TEXT using translate-shell.
FROM-LANG is the source language code.
TO-LANG is the target language code."
  (translate-region-check-command)
  (let ((command (format "%s -proxy http://127.0.0.1:7890 -b %s:%s \"%s\""
                         translate-region-trans-command
                         from-lang to-lang
                         (replace-regexp-in-string "\"" "\\\\\"" text)))
        (translated-text))
    (message "Translating from %s to %s..."
             (if (string= from-lang "zh") "Chinese" "English")
             (if (string= to-lang "zh") "Chinese" "English"))
    (condition-case err
        (with-temp-buffer
          (when (= 0 (call-process-shell-command command nil t))
            (setq translated-text (buffer-string))
            ;; Remove trailing newline if present
            (when (string-match-p "\n\\'" translated-text)
              (setq translated-text (substring translated-text 0 -1)))
            translated-text)
          (unless translated-text
            (user-error "Translation failed. Check your network connection")))
      (error (user-error "Translation error: %s" (error-message-string err))))

    ;; 返回翻译结果
    translated-text))

(defun translate-region-with-gptel (text is-chinese)
  "Translate TEXT using gptel.
If IS-CHINESE is non-nil, translate from Chinese to English.
Otherwise, translate from English to Chinese."
  (unless (require 'gptel nil t)
    (user-error "Package 'gptel' is not installed"))

  (let* ((from-lang (if is-chinese "Chinese" "English"))
         (to-lang (if is-chinese "English" "Chinese"))
         (prompt (format "Translate the following %s text to %s:\n\n%s"
                         from-lang to-lang text))
         (response nil)
         (done nil))

    (message "Translating from %s to %s using LLM..." from-lang to-lang)

    ;; 使用gptel-request函数发送翻译请求
    (gptel-request
        prompt
      :system translate-region-gptel-system-message
      ;; :model translate-region-gptel-model
      :callback (lambda (resp info)
                  (setq response resp
                        done t)
                  (when (plist-get info :error)
                    (user-error "Translation error: %s" (plist-get info :error))))
      :stream nil)

    ;; 等待响应
    (while (not done)
      (sleep-for 0.1))

    ;; 返回翻译结果
    response))

(defun translate-region-convert-to-naming-style (text style)
  "Convert TEXT to the specified naming STYLE.
STYLE can be one of: 'camelCase, 'PascalCase, 'snake_case, 'kebab-case, or 'SCREAMING_SNAKE_CASE."
  ;; 首先将文本按空格分割成单词
  (let* ((words (split-string text "[ \t\n]+" t))
         (processed-words nil))

    ;; 根据不同的命名风格处理单词
    (cond
     ;; 驼峰命名法：第一个单词首字母小写，后续单词首字母大写
     ((eq style 'camelCase)
      (setq processed-words
            (cons (downcase (car words))
                  (mapcar (lambda (word)
                            (concat (upcase (substring word 0 1))
                                    (downcase (substring word 1))))
                          (cdr words))))
      (mapconcat 'identity processed-words ""))

     ;; 帕斯卡命名法：所有单词首字母大写
     ((eq style 'PascalCase)
      (setq processed-words
            (mapcar (lambda (word)
                      (concat (upcase (substring word 0 1))
                              (downcase (substring word 1))))
                    words))
      (mapconcat 'identity processed-words ""))

     ;; 蛇形命名法：所有单词小写，单词之间用下划线连接
     ((eq style 'snake_case)
      (setq processed-words (mapcar 'downcase words))
      (mapconcat 'identity processed-words "_"))

     ;; 连字符命名法：所有单词小写，单词之间用连字符连接
     ((eq style 'kebab-case)
      (setq processed-words (mapcar 'downcase words))
      (mapconcat 'identity processed-words "-"))

     ;; 全大写蛇形命名法：所有字母大写，单词之间用下划线连接
     ((eq style 'SCREAMING_SNAKE_CASE)
      (setq processed-words (mapcar 'upcase words))
      (mapconcat 'identity processed-words "_"))

     ;; 默认情况下，保持原文本不变
     (t text))))

(defun translate-region-with-gptel-naming (text is-chinese)
  "Translate TEXT using gptel for naming purposes.
If IS-CHINESE is non-nil, translate from Chinese to English.
Otherwise, translate from English to Chinese."
  (unless (require 'gptel nil t)
    (user-error "Package 'gptel' is not installed"))

  (let* ((from-lang (if is-chinese "Chinese" "English"))
         (to-lang (if is-chinese "English" "Chinese"))
         (prompt (format "Translate the following %s text to %s for use as a variable or function name:\n\n%s"
                         from-lang to-lang text))
         (response nil)
         (done nil))

    (message "Translating from %s to %s using LLM for naming..." from-lang to-lang)

    ;; 使用gptel-request函数发送翻译请求
    (gptel-request
        prompt
      :system translate-region-gptel-naming-system-message
      ;; :model translate-region-gptel-model
      :callback (lambda (resp info)
                  (setq response resp
                        done t)
                  (when (plist-get info :error)
                    (user-error "Translation error: %s" (plist-get info :error))))
      :stream nil)

    ;; 等待响应
    (while (not done)
      (sleep-for 0.1))

    ;; 返回翻译结果
    response))

;;;###autoload
(defun translate-region-zh-en ()
  "Translate the selected region between Chinese and English.
If the region contains Chinese, translate to English.
If the region contains English, translate to Chinese."
  (interactive)
  (if (use-region-p)
      (let* ((begin (region-beginning))
             (end (region-end))
             (text (buffer-substring-no-properties begin end))
             (is-chinese (translate-region-is-chinese text))
             (from-lang (if is-chinese "zh" "en"))
             (to-lang (if is-chinese "en" "zh"))
             (translated-text))

        (setq translated-text
              (cond
               ((eq translate-region-backend 'translate-shell)
                (translate-region-with-translate-shell text from-lang to-lang))
               ((eq translate-region-backend 'gptel)
                (translate-region-with-gptel text is-chinese))
               (t (user-error "Unknown translation backend: %s" translate-region-backend))))

        ;; Replace region with translated text
        (delete-region begin end)
        (goto-char begin)
        (insert translated-text)
        (message "Translated %s to %s successfully"
                 (if is-chinese "Chinese" "English")
                 (if is-chinese "English" "Chinese")))
    (user-error "No region selected")))

;;;###autoload
(defun translate-region-toggle-backend ()
  "Toggle between translation backends."
  (interactive)
  (setq translate-region-backend
        (if (eq translate-region-backend 'translate-shell)
            'gptel
          'translate-shell))
  (message "Switched translation backend to %s"
           (if (eq translate-region-backend 'translate-shell)
               "translate-shell"
             "gptel")))

;;;###autoload
(defun translate-region-naming (style)
  "Translate the selected region and convert it to a naming style.
If the region contains Chinese, translate to English.
If the region contains English, translate to Chinese.
STYLE specifies the naming style to use."
  (interactive (list translate-region-naming-style))
  (if (use-region-p)
      (let* ((begin (region-beginning))
             (end (region-end))
             (text (buffer-substring-no-properties begin end))
             (is-chinese (translate-region-is-chinese text))
             (from-lang (if is-chinese "zh" "en"))
             (to-lang (if is-chinese "en" "zh"))
             (translated-text))

        ;; 使用适当的翻译后端获取翻译
        (setq translated-text
              (cond
               ((eq translate-region-backend 'translate-shell)
                (translate-region-with-translate-shell text from-lang to-lang))
               ((eq translate-region-backend 'gptel)
                (translate-region-with-gptel-naming text is-chinese))
               (t (user-error "Unknown translation backend: %s" translate-region-backend))))

        ;; 将翻译后的文本转换为指定的命名风格
        (when (and translated-text (not (string= translated-text "")))
          (setq translated-text (translate-region-convert-to-naming-style translated-text style)))

        ;; 替换区域内容
        (delete-region begin end)
        (goto-char begin)
        (insert translated-text)
        (message "Translated and converted to %s naming style" style))
    (user-error "No region selected")))

;;;###autoload
(defun translate-region-naming-interactive ()
  "Translate the selected region and convert it to a user-selected naming style.
If the region contains Chinese, translate to English.
If the region contains English, translate to Chinese."
  (interactive)
  (let ((style (intern (completing-read "Select naming style: "
                                        '("camelCase" "PascalCase" "snake_case"
                                          "kebab-case" "SCREAMING_SNAKE_CASE")
                                        nil t nil nil "camelCase"))))
    (translate-region-naming style)))

(provide 'translate-region)
;;; translate-region.el ends here
