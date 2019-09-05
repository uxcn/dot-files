[[ -f ~/.zshrc ]] && . ~/.zshrc
[[ -f ~/.zhosts/$host ]] && . ~/.zhosts/$host

# environment

if test -d ~/.zenv/$host; then

  for a in ~/.zenv/$host/*(@); do
    . $a
  done

fi
