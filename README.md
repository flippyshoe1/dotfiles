all of the dotfiles i use between computers\
only works on linux, windows support in the future (but i dont promise anything)

requirements:\
a linux distribution (preferably arch/debian based)\
emacs\
alacritty\
i3

optional requirements:\
JetBrainsMono and Terminus nerd fonts (from https://www.nerdfonts.com/font-downloads)

dependencies (debian and derivatives)
```shell
## special note abt debian: current alacritty package in debian repos still uses the yml config format
##                          once it finally updates ill rewrite the config to toml
apt install emacs alacritty i3 zsh zsh-syntax-highlighting rofi
```

dependencies (arch and derivatives)
```shell
pacman -S emacs alacritty i3 zsh zsh-syntax-highlighting rofi
```

to automatically add the config files (but dont be surprised when you gotta intervene)
```shell
./setup.sh
```

only reason its public is that i wont have to bother logging in every time i wanna use it\
do whatever you want with it
