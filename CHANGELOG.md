# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.


## 0.6.0 (Unreleased)
> Released N/A

* perf: Render only display area (#28)
* pref: Only render in post command (1892142ea12af1bbcbd7360eff7df9948e786548)

## 0.5.2
> Released Oct 27, 2023

* Use windows size changed hook instead of configuration hook. (f34db57ae66fe95e502fcf8cd1da959f82bf96df)
* Remove delay to avoid `window-scroll-functions` being called repeatedly (9728edf1c0db9efcd2a92055db9bb9747c8b4bf1)
* Avoid `Scan error: "Unbalanced parentheses"` when custom save file (121f5533038db59fdaca1b0d8da56f9b9efb296b)
* Refactor to increase performace, for #5 (7561700e37543bc7622e41d81d5b6dd038a8efe0)
* Render thumbnail by window (#15)
* Never `advice-remove` to `save-buffer` (#17)
* Redesign indicators clean up logic (#21)

## 0.5.1
> Released Feb 4, 2022

* Limit feature undone changes to only valid buffer, unlike `*scratch*` buffer.
* Added thumbnail support on the opposing fringe.
* Use `hash-table` instead of list to improve performance.
* Fix issue when max line cache is `nil` on save. (cd60583a6d040a9874d273d2d66fe397664ed54d)

## 0.5.0
> Released May 28, 2021

* Add feature to for undone changes, [#8](https://github.com/jcs-elpa/line-reminder/issues/8).
