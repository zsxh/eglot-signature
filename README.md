# eglot-signature (WIP)

Signature help (parameter hints) for [Eglot](https://github.com/joaotavora/eglot).

## Description

`eglot-signature` implements the LSP Signature Help protocol to display function signatures with active parameter highlighting when typing function arguments in a child frame popup.

## Screenshot

https://github.com/user-attachments/assets/186159c9-b219-49e1-a724-e9ed0df587cf

## Features

- Automatic triggering on trigger characters (e.g., `(`, `,`)
- Active parameter highlighting
- Multi-signature overload navigation
- Function and parameter documentation display
- Child frame popup display
- Context support for retriggering signature help

## Requirements

- Emacs 30+
- eglot 1.17+

## Installation

### use-package

```elisp
(use-package eglot-signature
  :vc (:url "https://github.com/zsxh/eglot-signature"
       :rev :newest)
  :hook (eglot-managed-mode . eglot-signature-mode)
  :init
  (with-eval-after-load 'eglot
    (eglot-signature-setup)))
```

### Manual

Download `eglot-signature.el` and add it to your `load-path` or install it via `package-vc-install`:

```elisp
(unless (package-installed-p 'eglot-signature)
  (package-vc-install
   '(eglot-signature :url "https://github.com/zsxh/eglot-signature")))
(require 'eglot)
(require 'eglot-signature)
(eglot-signature-setup)
(add-hook 'eglot-managed-mode-hook 'eglot-signature-mode)
```

### Integration with cape (optional)

```elisp
(with-eval-after-load 'cape
  (advice-add 'eglot-signature--capf-wrapper :around #'cape-wrap-buster))
```

## Usage

Once enabled, signature help appears automatically when typing function arguments.

### Key Bindings

#### Global (when `eglot-signature-mode` is enabled)

| Key | Command | Action |
|-----|---------|--------|
| `C-c C-s` | `eglot-signature-show` | Manually invoke signature help |

#### Transient (when signature popup is displayed)

| Key | Command | Action |
|-----|---------|--------|
| `<up>` / `<down>` | `eglot-signature-prev` / `eglot-signature-next` | Navigate between signature overloads |
| `C-g` / `<escape>` | `eglot-signature-quit` | Hide signature help |

### Commands

- `eglot-signature-toggle` - Toggle signature help mode
- `eglot-signature-show` - Manually show signature help at point
- `eglot-signature-quit` - Quit signature help
- `eglot-signature-next` - Navigate to next signature overload
- `eglot-signature-prev` - Navigate to previous signature overload
- `eglot-signature-switch-to-doc-buffer` - Switch to documentation buffer

## Customization

| Option | Default | Description |
|--------|---------|-------------|
| `eglot-signature-max-height` | 10 | Maximum frame height in lines |
| `eglot-signature-max-width` | 60 | Maximum frame width in characters |
| `eglot-signature-show-doc` | t | Show documentation |
| `eglot-signature-show-param-doc` | t | Show parameter documentation |
| `eglot-signature-debounce-delay` | 0.2 | Request debounce delay in seconds |

## License

GPL-3.0-or-later

## See Also

- [LSP Signature Help Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
