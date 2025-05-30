# translate-region - 中英文翻译工具

`translate-region` 是一个 Emacs 包，提供在中文和英文之间快速翻译文本的功能。它能自动检测选定区域的语言，并将中文翻译为英文或将英文翻译为中文。此外，它还提供了将翻译结果转换为各种编程命名风格的功能，非常适合在编程过程中快速创建变量或函数名称。

## 功能亮点

- **自动语言检测**：自动识别选定文本是中文还是英文
- **双向翻译**：支持中文到英文和英文到中文的翻译
- **多种翻译后端**：
  - `translate-shell`：使用命令行工具进行翻译
  - `gptel`：使用 LLM (如 ChatGPT) 进行翻译
- **命名风格转换**：将翻译结果转换为各种编程命名风格
  - camelCase：`myVariableName`
  - PascalCase：`MyVariableName`
  - snake_case：`my_variable_name`
  - kebab-case：`my-variable-name`
  - SCREAMING_SNAKE_CASE：`MY_VARIABLE_NAME`
- **交互式命名风格选择**：在翻译时选择所需的命名风格

## 先决条件

### translate-shell

要使用 `translate-shell` 后端，您需要安装 [translate-shell](https://github.com/soimort/translate-shell) 命令行工具：

- **macOS**：`brew install translate-shell`
- **Linux**：`apt-get install translate-shell`（或使用您的包管理器）
- **Windows**：可以通过 WSL 或其他方法安装

### gptel（可选）

要使用 LLM 翻译功能，您需要安装 [gptel](https://github.com/karthink/gptel) 包：

```elisp
;; 使用 package.el
M-x package-install RET gptel RET

;; 或使用 straight.el
(straight-use-package 'gptel)

;; 或使用 use-package
(use-package gptel
  :ensure t)
```

并配置您的 API 密钥：

```elisp
(setq gptel-api-key "your-api-key-here")
```

## 安装

### 手动安装

1. 下载 `translate-region.el` 文件
2. 将其放置在您的 Emacs 加载路径中
3. 在您的 Emacs 配置中添加：

```elisp
(require 'translate-region)
(global-set-key (kbd "C-c t") 'translate-region-zh-en)
(global-set-key (kbd "C-c T") 'translate-region-naming-interactive)
```

### 使用 use-package

```elisp
(use-package translate-region
  :load-path "path/to/translate-region"
  :bind (("C-c t" . translate-region-zh-en)
         ("C-c T" . translate-region-naming-interactive)))
```

### 使用 straight.el

```elisp
(straight-use-package
 '(translate-region :type git :host github :repo "your-username/translate-region"))

(global-set-key (kbd "C-c t") 'translate-region-zh-en)
(global-set-key (kbd "C-c T") 'translate-region-naming-interactive)
```

## 基本用法

### 文本翻译

1. 选择要翻译的文本区域
2. 使用 `M-x translate-region-zh-en` 或您定义的快捷键（如 `C-c t`）
3. 选定的文本将被替换为其翻译

### 切换翻译后端

使用 `M-x translate-region-toggle-backend` 在 translate-shell 和 gptel 后端之间切换。

## 高级用法

### 变量/函数命名翻译

#### 使用默认命名风格

1. 选择要翻译的文本区域
2. 使用 `M-x translate-region-naming`
3. 文本将被翻译并转换为默认命名风格（默认为 camelCase）

#### 交互式选择命名风格

1. 选择要翻译的文本区域
2. 使用 `M-x translate-region-naming-interactive` 或您定义的快捷键（如 `C-c T`）
3. 从提示中选择所需的命名风格
4. 文本将被翻译并转换为所选的命名风格

### 命名风格示例

假设选定的中文文本是 "用户登录状态"：

- **camelCase**：`userLoginStatus`
- **PascalCase**：`UserLoginStatus`
- **snake_case**：`user_login_status`
- **kebab-case**：`user-login-status`
- **SCREAMING_SNAKE_CASE**：`USER_LOGIN_STATUS`

## 自定义选项

### 基本设置

```elisp
;; 设置默认翻译后端
(setq translate-region-backend 'gptel)  ; 或 'translate-shell

;; 设置默认命名风格
(setq translate-region-naming-style 'snake_case)  ; 默认为 'camelCase
```

### 高级设置

```elisp
;; 调整中文检测阈值（默认为 0.1）
(setq translate-region-chinese-threshold 0.2)

;; 更改 translate-shell 命令（如果不在标准路径中）
(setq translate-region-trans-command "/path/to/trans")

;; 自定义 LLM 翻译的系统消息
(setq translate-region-gptel-system-message "您的自定义系统消息")

;; 自定义 LLM 命名翻译的系统消息
(setq translate-region-gptel-naming-system-message "您的自定义命名系统消息")

;; 设置 gptel 模型（如果需要）
(setq translate-region-gptel-model 'gpt-4)
```

### 代理设置

如果您需要通过代理使用 translate-shell，可以修改 `translate-region-with-translate-shell` 函数中的命令格式，或者在您的系统环境中设置代理。

## 故障排除

### translate-shell 找不到

确保 translate-shell 已安装并在您的 PATH 中。您可以在终端中运行 `which trans` 来检查。

### 翻译失败

- 检查您的网络连接
- 如果使用代理，确保代理设置正确
- 对于 translate-shell 后端，尝试在终端中手动运行 `trans` 命令来测试

### gptel 相关问题

- 确保 gptel 包已正确安装
- 检查您的 API 密钥是否已正确设置
- 查看 gptel 的文档以获取更多故障排除信息

## 贡献

欢迎贡献！请随时提交问题报告、功能请求或拉取请求。

## 许可证

本项目采用 MIT 许可证 - 详见 LICENSE 文件。 
