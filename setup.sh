#!/bin/bash

DOTFILES_DIR=`pwd`
DOT_FILES=( .clang-format .tmux.conf .vimrc .zshrc .zshrc.Darwin .emacs.d )

for file in ${DOT_FILES[@]}
do
    if [ -d $DOTFILES_DIR/$file ]; then
        ln -s $DOTFILES_DIR/$file $HOME
    else
        ln -s $DOTFILES_DIR/$file $HOME/$file
    fi
done
