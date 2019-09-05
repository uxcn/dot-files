# path

typeset -U path
typeset -U fpath
typeset -U manpath

zdotdir=~

host=`hostname --fqdn`

path=( ~/bin $path )
fpath=( ~/.zfuns/$host ~/.zfuns/$host/* $fpath )
manpath=( ~/share/man $manpath )

# test if shell interactive

if [[ ! -o interactive ]] ; then
    return
fi

# init

autoload -U compinit; compinit -u

# aliases

if test -d ~/.zaliases/$host; then

  for a in ~/.zaliases/$host/*(@); do 
    . $a
  done

fi

# functions

if test -d ~/.zfuns/$host; then

  autoload -U ~/.zfuns/$host/*(:t)
  #autoload -U ~/.zfuns/$host/*/*(:t)

fi

# key binds

bindkey  '^r'   history-incremental-search-backward

# prompt

prompt="%B%F{gray}%n@%m%k %B%F{gray}%B%F{black}%1~ %b%f%k"

setopt auto_cd

setopt null_glob
setopt auto_pushd
setopt pushd_to_home
setopt pushd_ignore_dups

setopt extended_glob

setopt share_history 
setopt append_history
setopt inc_append_history

# rtorrent (FIXME)

/bin/stty start undef
/bin/stty stop undef

# paths (named)

hg=~/dev/hg
cvs=~/dev/cvs
git=~/dev/git
svn=~/dev/svn

uxcn=~git/uxcn

