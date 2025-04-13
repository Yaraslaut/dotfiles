#!/bin/sh

packages="emacs firefox imagemagick lua lua-lgi zsh npm bluez bluez-utils"

sudo pacman -Syu $packages

REPO_DIR=$HOME/repo
git clone --depth 1 https://github.com/doomemacs/doomemacs $HOME/.config/emacs
$HOME/.config/emacs/bin/doom sync

git clone https://github.com/llvm/llvm-project.git $REPO_DIR/llvm-project
git clone https://github.com/contour-terminal/contour.git $REPO_DIR/contour
git clone https://github.com/LASTRADA-Software/Lightweight.git $REPO_DIR/Lightweight
git clone https://github.com/awesomeWM/awesome.git $REPO_DIR/awesome
