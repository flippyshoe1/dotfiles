#!/bin/bash

## flippyshoe's dotfile repo
## because im tired of rewriting

# get the source directory of the dotfiles
# (i.e. the script directory)
SCRIPT_DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# function to create a symlink
symlinkFile() {
    filename="$SCRIPT_DIR/$1"
    destination="$HOME/$2/$1"

    if [ ! -L "$destination" ]; then
        if [ -e "$destination" ]; then
            echo "[ERROR] $destination exists but it's not a symlink. Please fix that manually" && exit 1
        else
            ln -s "$filename" "$destination"
            echo "[OK] $filename -> $destination"
        fi
    else
        echo "[WARNING] $filename already symlinked"
    fi
}

# link i3
symlinkFile i3 .config

# link alacritty
symlinkFile alacritty .config

# link emacs
symlinkFile emacs/.emacs 
