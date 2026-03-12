# Emacs on Ubuntu — Snap Installation

The simplest way to run a current Emacs on Ubuntu. The snap tracks the latest
stable release (Emacs 30.x as of 2025) and auto-updates.

## Install

```sh
sudo snap install emacs --classic
```

`--classic` is required because Emacs needs unrestricted filesystem access
(GPG, tramp, vterm, native compilation cache, etc.).

## Verify

```sh
emacs --version
# GNU Emacs 30.x
```

## Configuration

The snap picks up `~/.emacs.d/` automatically. No extra steps.

First launch will trigger package auto-installation from MELPA/ELPA.
This takes a few minutes; subsequent startups are fast.

## Machine-local overrides

```sh
cp ~/.emacs.d/local.el.example ~/.emacs.d/local.el
# Edit local.el as needed (Copilot enterprise URL, GitHub Enterprise, etc.)
```

## Native compilation cache

The snap writes native-compiled `.eln` files to `~/.emacs.d/eln-cache/`.
This happens on first use of each package and is normal.

## Fonts

See [readme.org](readme.org) for the required font installation step.

## Updating

Snaps update automatically. To update immediately:

```sh
sudo snap refresh emacs
```

## Removal

```sh
sudo snap remove emacs
```
