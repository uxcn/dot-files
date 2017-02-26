# Setup fzf
# ---------
if [[ ! "$PATH" == */home/jason/dev/git/uxcn/fzf/bin* ]]; then
  export PATH="$PATH:/home/jason/dev/git/uxcn/fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/jason/dev/git/uxcn/fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/jason/dev/git/uxcn/fzf/shell/key-bindings.zsh"

