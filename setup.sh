#!/bin/bash

## flippyshoe's dotfile repo
## because im tired of rewriting

# get the source directory of the dotfiles
# (i.e. the script directory)
SCRIPT_DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# function to create a symlink
symlinkFile() {
    name="$(basename "$1")"
    filename="$SCRIPT_DIR/$1"
    destination="$HOME/$2/$name"

    if [ -z ${2+x} ]; then
	destination="$HOME/$name"
    fi
    
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

# link alacritty (defaults to post ver13 build config, modify for your liking
symlinkFile alacritty .config

# link emacs
symlinkFile .emacs

# link zsh
symlinkFile .zshrc
symlinkFile zsh .config

## now considering how the default zsh config includes a .local/bin directory to path
## just link the bin dir in the dotfiles folder to there :3
## (stupid solution but it works)
symlinkFile bin .local
