# path

typeset -U path
typeset -U fpath
typeset -U manpath

host=`hostname --fqdn` 

path=( ~/bin ~/bin/* $path )
fpath=( ~/.zfuns/$host $fpath )
manpath=( ~/share/man $manpath )

# test if shell interactive

if [[ ! -o interactive ]] ; then
    return
fi

# init

autoload -U compinit; compinit

# aliases

if test -d ~/.zaliases/$host; then

  for a in ~/.zaliases/$host/*(@); do 
    . $a
  done

fi

# functions

if test -d ~/.zfuns/$host; then

  autoload -U ~/.zfuns/$host/*(:t)

fi

# key binds

bindkey  '^r'   history-incremental-search-backward

# prompt

prompt="%B%F{green}%n@%m%k %B%F{blue}%1~ %b%f%k"

# zsh options

DIRSTACKSIZE=16
DIRSTACKFILE=~/.zdirs

HISTSIZE=8192
SAVEHIST=8192
HISTFILE=~/.zhistory

setopt auto_cd

setopt null_glob
setopt auto_pushd
setopt pushd_to_home
setopt pushd_ignore_dups

setopt extended_glob

setopt share_history 
setopt append_history
setopt inc_append_history

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

hg=~/development/hg
git=~/development/git
svn=~/development/svn

uxcn=~git/uxcn
