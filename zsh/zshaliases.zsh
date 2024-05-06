# make a dir and cd into it
md() {
    mkdir -p "$@" ;
    cd "$@" ;
}

# cd back (.. <amount>)
..() {
    local dir
    local i
    local levels="${1:-1}"

    for ((i = 0; i < levels; i++)); do
        dir+="../"
    done

    cd "$dir"
}

# fix which
alias which="command -v"

# prefer content-disposition on wget
unalias wget &> /dev/null || true
if which wget &> /dev/null ; then
    alias wget="wget --content-disposition"
else
    # use curl if no wget
    if which curl &> /dev/null ; then
        wget () {
            echo "WARNING: Using curl as wget equivalent" ;
            curl -JL --remote-name-all "$@" ;
        }
    else
    # neither curl or wget, really?
        echo "WARNING: neither curl or wget in this session"
    fi
fi

# git stuff
alias gps="git push"
alias gpl="git pull"
alias gc="git commit"
alias gadd="git add"
alias gst="git status"
alias glog="git log --pretty=format:\"%ar - %an  - %s\" --graph"

# no nano :)
if which vim &> /dev/null ; then
    export EDITOR="vim"
    alias vi="vim"
elif which vi &> /dev/null ; then
    export EDITOR="vi"
fi

# others
alias cp="cp -v"
alias mv="mv -v"
alias rm="rm -vI"
alias lc="wc -l"
alias mkdir="mkdir -pv"
alias tmux="tmux new -A"
    
