# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.


## 0.5.2 (Unreleased)
> Released N/A

* N/A

## 0.5.1
> Released Feb 4, 2022

* Limit feature undone changes to only valid buffer, unlike `*scratch*` buffer.
* Added thumbnail support on the opposing fringe.
* Use `hash-table` instead of list to improve performance.
* Fix issue when max line cache is `nil` on save. (cd60583a6d040a9874d273d2d66fe397664ed54d)

## 0.5.0
> Released May 28, 2021

* Add feature to for undone changes, [#8](https://github.com/jcs-elpa/line-reminder/issues/8).
