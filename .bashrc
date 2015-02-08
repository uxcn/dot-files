# path

for f in ~/bin ~/bin/*; do

  if [[ -d $f && \
        ! $PATH =~ $f:.* && \
        ! $PATH =~ .*:$f:.* && \
        ! $PATH =~ .*:$f ]]; then 
  
    PATH=$f:${PATH}

  fi

done

export MANPATH=${MANPATH%%:/home/${USER}/share/man}:/home/${USER}/share/man

# test if shell interactive

if [[ $- != *i* ]] ; then
    return
fi

# shell optional behavior

shopt -s histappend

# environment vars

export LC_ALL="en_US.UTF-8"

# general

export EDITOR=nvim

# gnupg

export GPG_TTY=$(/usr/bin/tty)

# compiler

export CC="clang"
export CXX="clang++"

# development

export hg=~/development/hg
export git=~/development/git
export svn=~/development/svn

# aliases

alias aga="ag -a"

alias cdh="cd $hg"
alias cdg="cd $git"
alias cds="cd $svn"
alias cdd="cd ~/development"


alias ls="ls -hF --color=tty"
alias la="ls -A"
alias ll="ls -l"

alias pm="pm -f"
alias cpm="cpm -f"
alias upm="upm -f"

alias diff="colordiff"
alias hostname="hostname --fqdn"

