emacs
=====

Tracking emacs configuration using `Cask`. I got the idea for this
from http://tullo.ch/articles/modern-emacs-setup

# Installation

Clone this repository and run the `install.sh` script.

# Local customizations

Create `~/.emacs.local.el` to load any customizations along with this
configuration.

## Appearance

To load custom theme, add this to `~/.emacs.local.el`:

``` lisp
(setq av/theme 'zenburn)
```

## org-agenda

Custom org agenda files and diary file can be loaded from a predefined
location. If these files are in your dropbox folder, then you can do
this:

``` lisp
(setq av/org-base-dir "~/Dropbox/org")
```

It is assumed that the diary file is named `diary`.
