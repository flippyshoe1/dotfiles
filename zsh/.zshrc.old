# if not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# colors!
autoload -U colors && colors
PROMPT="%M | %~ > "
RPROMPT="%T | %W"

# history
autoload -U colors && colors
if [ ! -d ~/.cache/zsh ]; then
    mkdir -p ~/.cache/zsh
fi

HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# tab completion:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# basic aliases
alias ls='ls --color=auto'
alias ll='ls -lah --color=auto'
alias grep='grep --color=auto'
alias ec="$EDITOR $HOME/.zshrc" # edit .zshrc
alias sc="source $HOME/.zshrc"  # reload zsh configuration

# load zsh-syntax-highlighting
# fyi, location may vary from distro to distro
# official git repo states to run the following:
# echo "source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" >> ${ZDOTDIR:-$HOME}/.zshrc
# this will add the source command with the correct location to the .zshrc (which is the soft link to here.)
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
