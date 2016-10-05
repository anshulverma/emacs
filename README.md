emacs
=====

Tracking emacs configuration using `Cask`. I got the idea for this
from http://tullo.ch/articles/modern-emacs-setup

This is a WIP.

# Local customizations

Create `~/.emacs.local.el` to load any customizations along with this configuration.

# Appearance

To load custom theme, add this to `~/.emacs.local.el`:

``` lisp
(setq av/theme 'zenburn)
```
