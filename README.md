emacs
=====

Tracking emacs configuration using `package-install`.

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

By default `leuven` theme is loaded.

## org-agenda

Custom org agenda files and diary file can be loaded from a predefined
location. If these files are in your dropbox folder, then you can do
this:

``` lisp
(setq av/org-base-dir "~/Dropbox/org")
```

It is assumed that the diary file is named `diary`.

## Face height

If the font size is not according to your liking (default 144), then
change it:

``` lisp
(setq av/face-height 180)
```

# License


## Library sources

I don't own any of the code under `lib` directory. I have tried to keep
the original authoring information for each source file. Other than some
minor modifications to original work these source files are exact copies
of the original work.

## Fonts

There are two font type currently in this repository:

- `Symbola` -- http://users.teilar.gr/~g1951d/
- `source-code-pro` - https://github.com/adobe-fonts/source-code-pro
