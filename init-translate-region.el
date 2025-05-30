;;; init-translate-region.el --- Configuration for translate-region

;;; Commentary:

;; 这个文件提供了 translate-region 包的配置示例。
;; 将此文件放置在您的 Emacs 配置目录中，并在您的 init.el 中加载它：
;; (load "~/.emacs.d/init-translate-region.el")
;;
;; 或者如果您使用 use-package：
;; (use-package translate-region
;;   :load-path "path/to/translate-region"
;;   :config
;;   (load "~/.emacs.d/init-translate-region.el"))

;;; Code:

;; 确保 translate-region 已加载
(require 'translate-region)

;; ==============================
;; 基本设置
;; ==============================

;; 设置快捷键
(global-set-key (kbd "C-c t") 'translate-region-zh-en)         ;; 基本翻译
(global-set-key (kbd "C-c T") 'translate-region-naming-interactive)  ;; 命名风格翻译
(global-set-key (kbd "C-c C-t") 'translate-region-toggle-backend)    ;; 切换翻译后端

;; ==============================
;; 翻译后端设置
;; ==============================

;; 设置默认翻译后端
;; 选项: 'translate-shell 或 'gptel
(setq translate-region-backend 'translate-shell)

;; 如果您使用 gptel 作为后端，请确保已安装并配置 gptel
;; (require 'gptel)
;; (setq gptel-api-key "your-api-key-here")
;; (setq gptel-model "gpt-3.5-turbo")  ;; 或其他您想使用的模型

;; 设置 gptel 模型用于翻译（可选）
;; 如果为 nil，则使用 gptel 的默认模型
;; (setq translate-region-gptel-model 'gpt-4)

;; ==============================
;; 命名风格设置
;; ==============================

;; 设置默认命名风格
;; 选项: 'camelCase, 'PascalCase, 'snake_case, 'kebab-case, 'SCREAMING_SNAKE_CASE
(setq translate-region-naming-style 'camelCase)

;; ==============================
;; 高级设置
;; ==============================

;; 调整中文检测阈值（默认为 0.1）
;; 如果中文字符占比超过此值，则认为文本是中文
;; (setq translate-region-chinese-threshold 0.1)

;; 更改 translate-shell 命令（如果不在标准路径中）
;; (setq translate-region-trans-command "/usr/local/bin/trans")

;; ==============================
;; 代理设置
;; ==============================

;; 如果您需要通过代理使用 translate-shell，可以自定义函数
;; 下面是一个示例，使用 http://127.0.0.1:7890 作为代理
(defun my-translate-region-with-translate-shell (text from-lang to-lang)
  "使用代理翻译 TEXT，从 FROM-LANG 到 TO-LANG。"
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
    translated-text))

;; 取消下面的注释以使用自定义的代理翻译函数
;; (advice-add 'translate-region-with-translate-shell :override #'my-translate-region-with-translate-shell)

;; ==============================
;; 自定义系统消息
;; ==============================

;; 自定义 LLM 翻译的系统消息
;; (setq translate-region-gptel-system-message
;;       "You are a helpful translation assistant. Your task is to translate the given text between Chinese and English.
;; If the input is in Chinese, translate it to English. If the input is in English, translate it to Chinese.
;; Provide only the translation without any additional explanation or notes.
;; Ensure the translation is accurate and maintains the original meaning and tone.")

;; 自定义 LLM 命名翻译的系统消息
;; (setq translate-region-gptel-naming-system-message
;;       "You are a helpful translation assistant specialized in creating programming variable and function names.
;; Your task is to translate the given text between Chinese and English, and format it in a way that would be suitable for a variable or function name.
;; If the input is in Chinese, translate it to English. If the input is in English, translate it to Chinese.
;; Provide only the translation without any additional explanation or notes.
;; The translation should be concise, accurate, and use appropriate programming terminology.")

;; ==============================
;; 扩展功能
;; ==============================

;; 示例：创建一个函数，直接将选定文本翻译为特定命名风格
(defun translate-region-to-snake-case ()
  "将选定区域翻译并转换为 snake_case 命名风格。"
  (interactive)
  (translate-region-naming 'snake_case))

(defun translate-region-to-pascal-case ()
  "将选定区域翻译并转换为 PascalCase 命名风格。"
  (interactive)
  (translate-region-naming 'PascalCase))

;; 为这些函数设置快捷键
(global-set-key (kbd "C-c t s") 'translate-region-to-snake-case)
(global-set-key (kbd "C-c t p") 'translate-region-to-pascal-case)

(provide 'init-translate-region)
;;; init-translate-region.el ends here
