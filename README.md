[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/line-reminder-badge.svg)](https://melpa.org/#/line-reminder)
[![MELPA Stable](https://stable.melpa.org/packages/line-reminder-badge.svg)](https://stable.melpa.org/#/line-reminder)

# line-reminder
> Line annotation for changed and saved lines.

[![CI](https://github.com/emacs-vs/line-reminder/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-vs/line-reminder/actions/workflows/test.yml)

| Line Reminder in Emacs Comparison      | Visual Studio Comparison            |
|:--------------------------------------:|:-----------------------------------:|
|<img src="./etc/emacs-comparison.png"/> | <img src="./etc/vs-comparison.png"/>|

## 🔨 Usage

Enable for all buffers.

```el
(global-line-reminder-mode t)
```

Or you can just enable in specific buffer you want.

```el
(line-reminder-mode t)
```

### ❗ Display Annotation

You can either set `line-reminder-show-option` to `linum` or `indicators` like
the following snippet below.

```el
(setq line-reminder-show-option 'linum)  ; Or set to 'indicators
```

### 💥 Using `linum`

`linum` uses text to display annotation, you can customize the two variables
below to change the display symbol. The default value for both variables is set
to `"▐"`.

* `line-reminder-modified-sign`
* `line-reminder-saved-sign`

Customize format for how line annotation is displayed.

```el
(setq line-reminder-linum-format "%s ")
```

### 💥 Using `indicators`

Customize the symbol of the fringe (bitmap)

```el
(setq line-indicators-fringe 'filled-rectangle)
```

If you change the fringe location by altering this variable.

```el
(setq line-indicators-fringe-placed 'left-fringe)
```

### 💨 Face

List of face you can customize.

* `line-reminder-modified-sign-face`
* `line-reminder-saved-sign-face`
* `line-reminder-modified-sign-thumb-face`
* `line-reminder-saved-sign-thumb-face`

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
