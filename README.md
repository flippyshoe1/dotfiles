all of the dotfiles i use between computers\
only works on linux, windows support in the future (but i dont promise anything)

requirements:\
a linux distribution (preferably arch/debian based)\
emacs\
alacritty (preferably over version 0.13 unless you want to use the yaml format)\
i3

dependencies (debian and derivatives)
```shell
## notice for debian: alacritty is behind version 0.13, thus using yaml instead of toml, the yaml version of the config is in the .old directory
apt install emacs alacritty i3 zsh zsh-syntax-highlighting
```

dependencies (arch and derivatives)
```shell
pacman -S emacs alacritty i3 zsh zsh-syntax-highlighting
```

to automatically add the config files (but dont be surprised when you gotta intervene)
```shell
./setup.sh
```

only reason its public is that i wont have to bother logging in every time i wanna use it\
do whatever you want with it
