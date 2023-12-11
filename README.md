[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/line-reminder.svg)](https://jcs-emacs.github.io/jcs-elpa/#/line-reminder)
[![MELPA](https://melpa.org/packages/line-reminder-badge.svg)](https://melpa.org/#/line-reminder)
[![MELPA Stable](https://stable.melpa.org/packages/line-reminder-badge.svg)](https://stable.melpa.org/#/line-reminder)

# line-reminder
> Line annotation for changed and saved lines.

[![CI](https://github.com/emacs-vs/line-reminder/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-vs/line-reminder/actions/workflows/test.yml)

| Emacs (with `line-reminder`) | Visual Studio   |
|:----------------------------:|:---------------:|
| ![](etc/emacs.png)           | ![](etc/vs.png) |

## 🔨 Usage

Enable for all buffers.

```el
(global-line-reminder-mode t)
```

Or you can just enable in specific buffer you want.

```el
(line-reminder-mode t)
```

## ❗ Display Annotation

You can either set `line-reminder-show-option` to `linum` or `indicators` like
the following snippet below.

```el
(setq line-reminder-show-option 'linum)  ; Or set to 'indicators
```

Change display symbol: (Default to `"▐"`)

```el
(setq line-reminder-modified-sign "▐"
      line-reminder-saved-sign "▐")
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

Customize the symbol of the fringe: (Default to `'line-reminder--default-bitmap`)

```el
(setq line-reminder-bitmap 'filled-rectangle)
```

If you change the fringe location by altering this variable: (Default to `'left-fringe`)

```el
(setq line-reminder-fringe-placed 'left-fringe)
```

## 🖼️ Display thumbnail

You can display thumbnail by setting:

```el
(setq line-reminder-thumbnail t)
```

*P.S. fringe is oppsing from variable `line-reminder-fringe-placed`, hence it's
default to `right-fringe`*

You can change the thumbnail bitmap by: (Defaul to `line-reminder--default-thumbnail-bitmap`)

```el
(setq line-reminder-thumbnail-bitmap 'filled-rectangle)
```

Change thumbnail display symbol: (Default to `"▐"`)

```el
(setq line-reminder-thumb-modified-sign "▐"
      line-reminder-thumb-saved-sign "▐")
```

## 💨 Face

List of face you can customize.

> For regular indicators

* `line-reminder-modified-sign-face`
* `line-reminder-saved-sign-face`

> For thumbnail

* `line-reminder-modified-sign-thumb-face`
* `line-reminder-saved-sign-thumb-face`

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

### 🔬 Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
$ eask install-deps --dev
```

To test the package's installation:

```sh
$ eask package
$ eask install
```

To test compilation:

```sh
$ eask compile
```

**🪧 The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
$ eask lint checkdoc
```

The standard `package` linter:

```sh
$ eask lint package
```

*📝 P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE.txt) for details.
