# path

declare -A PHASH

PRE="${IFS}"; IFS=$\:; DS=( ${PATH} ); IFS="${PRE}";

for d in ${DS[@]}; do
    PHASH[$d]=1
done

if [[ -z ${PHASH[~/bin]} ]]; then
    PATH=~/bin:${PATH}
fi

for f in ~/bin/*; do

    # add any sub-dirs

    if [[ -z ${PHASH[$f]} && -d $f ]]; then

        PHASH[$f]=1
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

# compiler

export CC="clang"
export CXX="clang++"

# ccache

if [[ -d /dev/hdd ]]; then

    export CCACHE_DIR="/var/ccache/${USER}"
    export CCACHE_SIZE="4G"
    export CCACHE_CPP2="1"
    export CCACHE_COMPRESS="1"

fi

# aliases

alias ls="ls -hF --color=tty"
alias la="ls -A"
alias ll="ls -l"

