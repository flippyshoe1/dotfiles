## zshrc config for personal use
## written by flippyshoe

## loading functions, thing of it as c's #include<>
autoload -Uz compinit promptinit colors

## history
if [ ! -d ~/.cache/zsh ]; then
    mkdir -p ~/.cache/zsh
fi

HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.cache/zsh/history

## autocompletion configuration
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit

## default aliases (more can be added at zsh_aliases)
alias ls='ls --color=auto'
alias ll='ls -lah --color=auto'
alias hd='hexdump -C'
alias grep='grep --color=auto'
alias ec="$EDITOR $HOME/.zshrc" # edit .zshrc
alias sc="source $HOME/.zshrc"  # reload zsh configuration

## load aliases file (if it exists)
if [ -f ~/.config/zsh/zshaliases.zsh ]; then
    source ~/.config/zsh/zshaliases.zsh 
else
    print "[.zshrc] Missing aliases file!"
fi

## paths
typeset -U path PATH
path=(~/.local/bin $path)
export PATH

## prompt
promptinit && colors
#decide on user color
case "$USER" in
    root) # if i am root (though im not sure if i wish to use zsh on root)
	C_USER="red"
	;;
    *) # if im not root
	C_USER="blue"
	;;
esac
C_HOST="green"

PROMPT="%F{$C_USER}%n%f@%F{$C_HOST}%m%f:/%d $ "
RPROMPT="%T | %W"
    
## load zsh-syntax-highlighting
# fyi, location may vary from distro to distro
# official git repo states to run the following:
# echo "source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" >> ${ZDOTDIR:-$HOME}/.zshrc
# this will add the source command with the correct location to the .zshrc (which is the soft link to here.)
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
