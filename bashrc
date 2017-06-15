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

# GIT SVN
alias svnpull='git svn rebase'
alias svnpush='git svn dcommit'
alias gitlogsmall='git log --graph --abbrev-commit --pretty=oneline --decorate'
# maven
alias mvnjr='mvn org.zeroturnaround:javarebel-maven-plugin:generate'
alias deploy='mvn clean org.zeroturnaround:javarebel-maven-plugin:generate install -PdeployTomcat -Dtomcat.path=d:/tomcat-5.5.27/'
alias l='lein'
alias leni='lein'
alias li='lein install'

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
export MAVEN_OPTS="-Xmx512m -XX:MaxPermSize=150m "
export JDK6_HOME=d:/Java/jdk1.6/
export PATH=~/bin:~/dotfiles/scripts:$PATH
# ignore ls, bg, fg, exit and all commands that start with a space
export HISTIGNORE="&:ls:ll:[bf]g:exit:[ \t]*:j:z"
export HISTFILESIZE=10000000
export HISTSIZE=10000000
#export http_proxy=localhost:3128

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
####################################
# maven autocomplete
#!/bin/bash
# Programmed bash completion for use with maven2
# Copyright 2007 Pawel Kierat (pawel.kierat@gmail.com)
#
# Usage:
# The script first completes phases and plugins. If the user entry ends with ':',
# goals for the specified plugin are searched.
#
# Development:
# Every entry in PLUGINS array matches the corresponding element of GOALS array.
# To add a new plugin with its set of goals, put its name in PLUGINS and
# a space-separated list of goals as a string in GOALS.
#

PHASES="validate generate-sources process-sources generate-resources process-resources compile process-classes generate-test-sources process-test-sources generate-test-resources process-test-resources test-compile test package integration-test verify install deploy"

PLUGINS=("clean" "compiler" "deploy" "install" "resources" "site" "surfire" "verifier" "ear" "ejb" "jar" "rar" "war" "changelog" "changes" "checkstyle" "clover" "doap" "docck" "javadoc" "jxr" "pmd" "project-info-reports" "surfire-report" "ant" "antrun" "archetype" "assembly" "dependency" "enforcer" "gpg" "help" "invoker" "one" "plugin" "release" "remote-resources" "repository" "scm" "source" "eclipse" "idea" "build-helper" "castor" "javacc" "jdepend" "native" "sql" "taglist")

GOALS=("clean" "compile testCompile" "deploy deploy-file" "install install-file" "resources testResources" "deploy attach-descriptor site run stage-deploy stage" "test" "verify" "ear generate-application-xml" "ejb" "jar test-jar sign sign-verify" "rar" "war exploded inplace" "changelog dev-activity file-activity" "announcement-mail announcement-generate changes-report jira-report" "checkstyle check" "aggregate check instrument log clover save-history" "generate" "check" "javadoc test-javadoc jar" "jxr test-jxr" "pmd cpd check cpd-check" "cim dependencies dependency-convergence issue-tracking license mailing-list index summary scm project-team" "report report-only" "ant clean" "run" "create create-from-project" "assembly attached directory-online directory directory-single single unpack" "copy copy-dependencies unpack unpack-dependencies resolve list sources resolve-plugins go-offline purge-local-repository build-classpath analyze analyze-only analyze-dep-mgt tree" "enforce enforce-once display-info" "sign sign-and-deploy-file" "active-profiles describe effective-pom effective-settings" "run" "convert deploy-maven-one-repository install-maven-one-repository maven-one-plugin" "descriptor report updateRegistry xdoc addPluginArtifactMetadata" "clean perform prepare rollback branch" "bundle process" "bundle-create bundle-pack" "add changelog checkin checkout diff edit status tag unedit update validate" "jar test-jar" "add-maven-repo clean eclipse make-artifacts install-plugins" "idea project module workspace clean" "add-source add-test-source attach-artifact" "generate" "jtb jjtree javacc" "generate" "initialize compile link javah ranlib resource-compile compile-messages" "execute" "taglist")

POM_OPTS="-DmodelVersion -DartifactId -DgroupId -Dpackaging -Dname -Ddescription -Dversion -Durl -Dmaven.test.skip"

MVN_OPTS="-q --quiet -C --strict-checksums -c --lax-checksums -P --activate-profiles -ff --fail-fast -fae --fail-at-end -B --batch-mode -fn --fail-never -up --update-plugins -N --non-recursive -npr --no-plugin-registry -U --update-snapshots -cpu --check-plugin-updates -npu --no-plugin-updates -D --define -X --debug -e --errors -f --file -h --help -o --offline -r --reactor -s --settings -v --version"

_mvn() {
    local cur opts prev plugin mojos
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    case "${cur}" in
    -*)
	opts="${POM_OPTS} ${MVN_OPTS}"
	;;
    *:*)
    	plugin=$(echo "${cur}" | sed 's/:.*//g')
    	cur=$(echo "${cur}" | sed 's/.*://g')
	for i in ${!PLUGINS[*]}; do
	    if [[ "${plugin}" == ${PLUGINS[${i}]} ]]; then
		mojos=${GOALS[${i}]}
    		break
	    fi
	done
	opts=${mojos}
	;;
    *)
	opts="${PHASES} ${PLUGINS[@]}"
	;;
    esac
    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
    return 0
}

complete -F _mvn mvn
export DISPLAY=:0.0
export TERM=xterm-256color
# -rw-r--r--
# umask 0133
