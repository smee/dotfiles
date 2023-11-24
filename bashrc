EDITOR=/bin/vi
export EDITOR
##########################3
# aliases
alias ll='ls -al --color=auto'
alias ..='cd ..'
alias ...='cd .. ; cd ..'
alias webshare='python -c "import SimpleHTTPServer;SimpleHTTPServer.test()"'

#GIT
alias g='git'
alias gd='git diff --ignore-space-at-eol --word-diff'
alias gs='git status'
alias ga='git add'
alias pull='git pull'
alias push='git push'
#push to all remotes
alias pa='git remote | xargs -L1 git push --all'
alias gc='git commit'
alias gca='git commit --amend --no-edit'
alias gp='git push origin && git push github'
alias gap='git add --interactive'
alias gl='git lg'
alias gk='gitk --all'
# show simplified git commit history: all tags, last commit per branch and branch points
alias glo='git lg --simplify-by-decoration'

#find all commits that contains the given regex
find-commit () {
  git log -G "$@" --graph --oneline --stat
}
# GIT SVN
alias gitlogsmall='git log --graph --abbrev-commit --pretty=oneline --decorate'
# maven
alias l='lein'
alias leni='lein'
# top-like interface for running docker containers, see https://github.com/bcicen/ctop
alias ctop="docker run -ti --rm --name ctop-$(date +%Y-%m-%d-%H-%M-%S) -v /var/run/docker.sock:/var/run/docker.sock quay.io/vektorlab/ctop:latest -a"
pg() { # htop-style monitor for running queries in postgres databases running as dokku plugin
docker run -it --rm -e PGPASSWORD=`sudo cat /var/lib/dokku/services/postgres/$1/PASSWORD` -e PGUSER=postgres -e PGHOST=`docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' dokku.postgres.$1` rsmnarts/pg_activity --dbname=`cat /var/lib/dokku/services/postgres/$1/DATABASE_NAME`
}


# create a distinct color from the hostname of the current system
if [[ $TERM =~ "256color" ]]; then
   host_color="38;5;$((16 + $(hostname | cksum | cut -c1-3) % 256))";
else
   host_color="1;$((31 + $(hostname | cksum | cut -c1-3) % 6))";
fi
# show date, user, distinctly colored hostname and path as prompt
export PS1='\[\e[0;37m\](\d \t) \[\e[0;32m\]\u@\[\e[${host_color}m\]\h \[\e[1;36m\]\w \n\$ \[\e[00m\]'

export EDITOR=/bin/vim

export HISTCONTROL=ignoreboth # Ignores dupes in the history
shopt -s checkwinsize # After each command, checks the windows size and changes lines and columns

# bash completion settings (actually, these are readline settings)
if [ -t 1 ]
then
  bind "set completion-ignore-case on" # note: bind is used instead of setting these in .inputrc.  This ignores case in bash completion
  bind "set bell-style none" # No bell, because it's damn annoying
  bind "set show-all-if-ambiguous On" # this allows you to automatically show completion without double tab-ing
fi
# Turn on advanced bash completion if the file exists (get it here: http://www.caliban.org/bash/index.shtml#completion)
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
##########################################
###   Handy Extract Program

extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xvjf $1        ;;
            *.tar.gz)    tar xvzf $1     ;;
		    *.tar.xz)     tar xvf $1     ;;
            *.bz2)       bunzip2 $1       ;;
            *.rar)       unrar x $1     ;;
            *.gz)        gunzip $1     ;;
            *.tar)       tar xvf $1        ;;
            *.tbz2)      tar xvjf $1      ;;
            *.tgz)       tar xvzf $1       ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1    ;;
            *)           echo "'$1' cannot be extracted via >extract<" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}
#############################3
#Make Bash append rather than overwrite the history on disk:

shopt -s histappend
#Whenever displaying the prompt, write the previous line to disk: 
export PROMPT_COMMAND='history -a'
#ignore small typos when entering paths
shopt -s cdspell
###########################3
# CDPATH
CDPATH='.'
source ~/dotfiles/scripts/z.sh
export PATH=~/bin:~/dotfiles/scripts:~/.cargo/bin:$PATH
# ignore ls, bg, fg, exit and all commands that start with a space
export HISTIGNORE="&:ls:ll:[bf]g:exit:[ \t]*:j:z *"
export HISTFILESIZE=10000000
export HISTSIZE=10000000

################################
# use ssh-agent for caching key passwords
SSH_ENV="$HOME/.ssh/environment"

function start_agent {
  echo "Initializing new SSH agent..."
  /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
  echo succeeded
  chmod 600 "${SSH_ENV}"
  . "${SSH_ENV}" > /dev/null
  /usr/bin/ssh-add;
}

# Source SSH settings, if applicable
if [ -f "${SSH_ENV}" ]; then
  . "${SSH_ENV}" > /dev/null
  #ps ${SSH_AGENT_PID} doesn't work under cywgin
  ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
    start_agent;
  }
else
  start_agent;
fi

export DISPLAY=:0.0
export TERM=xterm-256color
# -rw-r--r--
# umask 0133
# diable XOFF, else Ctrl+s will completely block the terminal, needs a Ctrl+q to unlock
# only if the current shell is interactive (else we see the error "stty: 'standard input': Inappropriate ioctl for device")
[[ $- == *i* ]] && stty -ixon
# direnv for directory specific environment variables, see https://direnv.net/
command -v direnv >/dev/null 2>&1 && eval "$(direnv hook bash)"
