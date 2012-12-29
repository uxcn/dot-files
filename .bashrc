# path

PATH=~/bin:${PATH}

for f in ~/bin/*; do

    # add any sub-dirs

    if [[ -d $f ]]; then

        PATH=$f:${PATH}

    fi

done

export PATH

# test if shell interactive

if [[ $- != *i* ]] ; then
	return
fi

# shell optional behavior

shopt -s histappend

# environment vars

export LC_ALL="en_US.UTF-8"

# general

export EDITOR=vim

# ccache

if [[ -d /var/ccache/${USER} ]]; then

    export CCACHE_DIR="/var/ccache/${USER}"
    export CCACHE_SIZE="4G"
    export CCACHE_COMPRESS="1"

fi

# aliases

alias ls="ls -hF --color=tty"
alias la="ls -A"
alias ll="ls -l"

