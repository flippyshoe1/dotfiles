#!/bin/bash

## the one big shell script to rule them all
## sorts out config files, fonts, themes, etc
## follow the prompts on screen and good things will happen

## important symbols:
## >>> = a stage in the script
## [E] = an error
## [!] = attempting to run a privileged command
## [=] = stage done successfuly
## [?] = prompt
## [-] = anything else

echo '>>> starting setup...'

## prerequisite: getting the distrobution, package manager and optionally flatpak
echo '>>> asserting prerequisites'

if [ -f /etc/*release ]; then
    OS=$(grep '^ID=' /etc/*release | sed "s/ID=//g")
    echo "[-] asserting distribution ID is: $OS"
else
    echo '[E] missing /etc/os-release - cant figure out which distribution you are in'
    return
fi

if [[ $OS == "debian" ]]; then
    PM="apt"
fi
echo "[-] asserting package manager is going to be: $PM"

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

echo "[=] prerequesites successfuly filled!"
## stage 1: updating the package manager repos and installing the necessary programs
echo '>>> stage 1: downloading the necessary packages'

# variadic arg: commands?
run_privileged_cmd () {
    cmd=$@
    echo "[!] running privileged command: $cmd"
    sudo $cmd
}

# variadic arg: package names
get_dependencies () {
    if [[ $PM == "apt" ]]; then
	run_privileged_cmd "apt update"
	run_privileged_cmd "apt install $@"
    fi
}

# 2 args: command, message
run_optional_cmd () {
    command=$1
    message=$2
    read -p "$message (y/N): " confirm && [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]] || return
    $command
}

get_dependencies emacs alacritty i3 zsh zsh-syntax-highlighting rofi feh picom

echo "[=] packages successfuly installed!"
## stage 2: symbolic links and configurations
echo ">>> stage 2: setting up symlinks and dotfiles"

# function to create a symlink
# 2 args: filepath, destination
# TODO: kinda overcomplicated and gimmicky, redo later
symlinkFile() {
    name="$(basename "$1")"
    filename="$SCRIPT_DIR/$1"
    destination="$HOME/$2/$name"

    if [ -z ${2+x} ]; then
	destination="$HOME/$name"
    fi
    
    if [ ! -L "$destination" ]; then
        if [ -e "$destination" ]; then
            echo "[E] $destination exists but it's not a symlink. Please fix that manually" && exit 1
        else
            ln -s "$filename" "$destination"
            echo "[-] $filename -> $destination"
        fi
    else
        echo "[E] $filename already symlinked"
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

# link .assets (images ill use)
symlinkFile assets .assets

echo "[=] configs have been managed!"
## stage 3: optional dependencies
echo ">>> stage 3: optional dependencies"

# fonts
download_fonts () {
    if [[ ! -f /usr/bin/unzip ]]; then
	get_dependencies unzip
    fi
    
    mkdir ~/.local/share/fonts && mkdir /tmp/setup-temp
    ## JetBrainsMon and Terminus fonts
    wget -P /tmp/setup-temp https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/JetBrainsMono.zip https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/Terminus.zip
    ## unpack them and shove them into ~/.local/share/fonts
    unzip /tmp/setup-temp/JetBrainsMono.zip -d ~/.local/share/fonts/JetBrainsMono/
    unzip /tmp/setup-temp/Terminus.zip -d ~/.local/share/fonts/Terminus/
}

msg="[?] install optional fonts? (verify that the following link is not compromised if you do: https://www.nerdfonts.com/font-downloads)"
run_optional_cmd "download_fonts" "$msg"

echo ">>> completed stage 3!"

echo ">>> SETUP IS COMPLETE"
