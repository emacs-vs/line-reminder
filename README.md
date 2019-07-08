[![Build Status](https://travis-ci.com/jcs090218/line-reminder.svg?branch=master)](https://travis-ci.com/jcs090218/line-reminder)
[![MELPA](https://melpa.org/packages/line-reminder-badge.svg)](https://melpa.org/#/line-reminder)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# line-reminder
> Line annotation similar to Visual Studio.

| Line Reminder in Emacs Comparison             | Visual Studio Comparison                                 |
|:---------------------------------------------:|:--------------------------------------------------------:|
|<img src="./screenshot/emacs-comparison.png"/> | <img src="./screenshot/vs-comparison.png"/>|


## Configuration

List of face you can customize.
* `line-reminder-modified-sign-face`
* `line-reminder-saved-sign-face`

### Using linum

Customize the modified sign.
```el
(setq line-reminder-modified-sign "▐")
```

Customize the saved sign.
```el
(setq line-reminder-saved-sign "▐")
```

Customize string on the right/left side of the line number.
```el
(setq line-reminder-linum-left-string "")
(setq line-reminder-linum-right-string " ")
```

### Using indicators

Customize the symbol of the fringe
```el
(setq line-indicators-fringe 'filled-rectangle)
```

If you change the fringe location by altering this variable.
```el
(setq line-indicators-fringe-placed 'left-fringe)
```


Buffer Name List that you do not want this mode to take effect.
```el
(setq line-reminder-ignore-buffer-names '("*Backtrace*"
                                          "*Buffer List*"
                                          "*Checkdoc Status*"
                                          "*Echo Area"
                                          "*helm"
                                          "*Help*"
                                          "magit"
                                          "*Minibuf-"
                                          "*Packages*"
                                          "*run*"
                                          "*shell*"
                                          "*undo-tree*"))
```


## Usage

Enable for all buffers.
```el
(global-line-reminder-mode t)
```
Or you can just enable in specific buffer you want.
```el
(line-reminder-mode t)
```


## Contribution

If you would like to contribute to this project, you may either 
clone and make pull requests to this repository. Or you can 
clone the project and establish your own branch of this tool. 
Any methods are welcome!
