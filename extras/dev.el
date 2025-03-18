;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; project.el config
(setq project-vc-extra-root-markers '("*.csproj" 
                                    ".projectile" 
                                    ".project"    
                                    "package.json"
                                    "pom.xml"
                                    "requirements.txt"
                                    "pyproject.toml"
                                    "Gemfile" 
                                    "*.gemspec"
                                    "autogen.sh"))  


;;; mise
(use-package mise
  :ensure t
  :defer t
  :config
  (add-hook 'after-init-hook #'global-mise-mode))


;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package forge
  :ensure t
  :defer t
  :after magit)


(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((c-sharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp" "v0.23.1"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (devicetree . ("https://github.com/joelspadin/tree-sitter-devicetree"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))


  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (csharp-mode . csharp-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    :custom
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("/home/dean/src/public/combobulate")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language mode setup

(use-package devicetree-ts-mode
  :ensure t)

(add-hook 'python-mode-hook
          (lambda ()
            (uv-activate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot

(setq eldoc-echo-area-prefer-doc-buffer t
      eldoc-echo-area-use-multiline-p nil)

(use-package eglot
  :ensure nil
  :hook
  ((python-ts-mode csharp-ts-mode) . eglot-ensure)

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot--semantic-tokens-mode +1)
  (eglot--semantic-tokens-queue-update)
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider))
  (eglot-stay-out-of '(yasnippet))
  
  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log everywhere event
  (add-to-list 'eglot-server-programs
               '(csharp-ts-mode . ("OmniSharp" "-lsp")))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "uv" "run" "basedpyright-langserver" "--stdio")))
  
(use-package eldoc-box
  :ensure t
  :after eglot
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))



(add-to-list 'eglot-server-programs
             '((python-mode python-ts-mode)
               "basedpyright-langserver" "--stdio"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  HELPER Functions

(defun uv-activate ()
  "Activate Python environment managed by uv based on current
project directory.Looks for .venv directory in project root and
activates the Python interpreter."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (venv-path (expand-file-name ".venv" project-root))
         (python-path (expand-file-name
                       (if (eq system-type 'windows-nt)
                           "Scripts/python.exe"
                         "bin/python")
                       venv-path)))
    (if (file-exists-p python-path)
        (progn
          ;; Set Python interpreter path
          (setq python-shell-interpreter python-path)

          ;; Update exec-path to include the venv's bin directory
          (let ((venv-bin-dir (file-name-directory python-path)))
            (setq exec-path (cons venv-bin-dir
                                  (remove venv-bin-dir exec-path))))

          ;; Update PATH environment variable
          (setenv "PATH" (concat (file-name-directory python-path)
                                 path-separator
                                 (getenv "PATH")))

          ;; Update VIRTUAL_ENV environment variable
          (setenv "VIRTUAL_ENV" venv-path)

          ;; Remove PYTHONHOME if it exists
          (setenv "PYTHONHOME" nil)

          (message "Activated UV Python environment at %s" venv-path))
      (error "No UV Python environment found in %s" project-root))))


(defun schmo/reapply-csharp-ts-mode-font-lock-settings ()
  "Fixes csharp-ts-mode font lock with latest version of parser"
  (interactive)
  (setq csharp-ts-mode--keywords
        '("this" "add" "alias" "as" "base" "break" "case" "catch" "checked" "class" "continue"
          "default" "delegate" "do" "else" "enum" "event" "explicit" "extern" "finally"
          "for" "foreach" "global" "goto" "if" "implicit" "interface" "is" "lock"
          "namespace" "notnull" "operator" "params" "return" "remove" "sizeof"
          "stackalloc" "static" "struct" "switch" "throw" "try" "typeof" "unchecked"
          "using" "while" "new" "await" "in" "yield" "get" "set" "when" "out" "ref" "from"
          "where" "select" "record" "init" "with" "let"))

  (let ((ops '("--" "-" "-=" "&" "&=" "&&" "+" "++" "+=" "<" "<=" "<<" "<<=" "="
               "==" "!" "!=" "=>" ">" ">=" ">>" ">>=" ">>>" ">>>=" "|" "|=" "||"
               "?" "??" "??=" "^" "^=" "~" "*" "*=" "/" "/=" "%" "%=" ":")))
    (setq csharp-ts-mode--font-lock-settings
          (treesit-font-lock-rules
           :language 'c-sharp
           :feature 'bracket
           '((["(" ")" "[" "]" "{" "}" (interpolation_brace)]) @font-lock-bracket-face)

           :language 'c-sharp
           :feature 'delimiter
           `((["," ":" ";"]) @font-lock-delimiter-face
             ([,@ops]) @font-lock-operator-face
             )

           :language 'c-sharp
           :override t
           :feature 'comment
           '((comment) @font-lock-comment-face)

           :language 'c-sharp
           :override t
           :feature 'keyword
           `([,@csharp-ts-mode--keywords] @font-lock-keyword-face
             (modifier) @font-lock-keyword-face
             (implicit_type) @font-lock-keyword-face)

           :language 'c-sharp
           :override t
           :feature 'property
           `((attribute name: (identifier) @font-lock-property-use-face))

           :language 'c-sharp
           :override t
           :feature 'literal
           `((integer_literal) @font-lock-number-face
             (real_literal) @font-lock-number-face
             (null_literal) @font-lock-constant-face
             (boolean_literal) @font-lock-constant-face)

           :language 'c-sharp
           :override t
           :feature 'string
           `([(character_literal)
              (string_literal)
              (raw_string_literal)
              (verbatim_string_literal)
              ;; (interpolated_string_expression)
              (string_content)
              (interpolation_start)
              (interpolation_quote)] @font-lock-string-face)

           :language 'c-sharp
           :override t
           :feature 'escape-sequence
           '((escape_sequence) @font-lock-escape-face)

           :language 'c-sharp
           :feature 'type
           :override t
           '((generic_name (identifier) @font-lock-type-face)
             (type_parameter (identifier) @font-lock-type-face)
             (parameter type: (identifier) @font-lock-type-face)
             (type_argument_list (identifier) @font-lock-type-face)
             (as_expression right: (identifier) @font-lock-type-face)
             (is_expression right: (identifier) @font-lock-type-face)
             (_ type: (identifier) @font-lock-type-face)
             (predefined_type) @font-lock-builtin-face
             )

           :language 'c-sharp
           :feature 'definition
           :override t
           '((interface_declaration name: (identifier) @font-lock-type-face)
             (class_declaration name: (identifier) @font-lock-type-face)
             (enum_declaration name: (identifier) @font-lock-type-face)
             (struct_declaration (identifier) @font-lock-type-face)
             (record_declaration (identifier) @font-lock-type-face)
             (namespace_declaration name: (identifier) @font-lock-type-face)
             (constructor_declaration name: (identifier) @font-lock-constructor-face)
             (destructor_declaration name: (identifier) @font-lock-constructor-face)
             (base_list (identifier) @font-lock-type-face)
             (enum_member_declaration (identifier) @font-lock-variable-name-face)
             (parameter name: (identifier) @font-lock-variable-name-face)
             (implicit_parameter) @font-lock-variable-name-face
             )

           :language 'c-sharp
           :feature 'function
           '((method_declaration name: (identifier) @font-lock-function-name-face)
             (local_function_statement name: (identifier) @font-lock-function-name-face)
             (invocation_expression
              function: (member_access_expression
                         name: (identifier) @font-lock-function-call-face))
             (invocation_expression
              function: (identifier) @font-lock-function-call-face)
             (invocation_expression
              function: (member_access_expression
                         name: (generic_name (identifier) @font-lock-function-call-face)))
             (invocation_expression
              function: (generic_name (identifier) @font-lock-function-call-face)))

           :language 'c-sharp
           :feature 'expression
           '((identifier) @font-lock-variable-use-face)

           :language 'c-sharp
           :feature 'directives
           :override t
           '((preproc_if
              "#if" @font-lock-preprocessor-face)
             (preproc_if
              "#endif" @font-lock-preprocessor-face)
             (preproc_elif
              "#elif" @font-lock-preprocessor-face)
             (preproc_else
              "#else" @font-lock-preprocessor-face)
             ;; (preproc_endif) @font-lock-preprocessor-face
             (preproc_define
              "#define" @font-lock-preprocessor-face
              (preproc_arg) @font-lock-constant-face)
             (preproc_undef
              "#undef" @font-lock-preprocessor-face
              (preproc_arg) @font-lock-constant-face)

             (preproc_nullable) @font-lock-preprocessor-face
             (preproc_pragma) @font-lock-preprocessor-face
             (preproc_region
              "#region" @font-lock-preprocessor-face
              (preproc_arg) @font-lock-comment-face)
             (preproc_endregion) @font-lock-preprocessor-face)))))

(add-hook 'csharp-mode-hook
  (schmo/reapply-csharp-ts-mode-font-lock-settings))

