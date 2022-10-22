# check if the antigen is installed, install it if it's not
ANTIGEN_CONF=~/.antigen.zsh

if [[ ! -f "$ANTIGEN_CONF" ]]; then
    echo "Failed to find antigen, installing it..."
    rm -rfv ~/.antigen/
    curl -L git.io/antigen > ~/.antigen.zsh
fi

source ~/.antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle fzf
antigen bundle fasd
antigen bundle pip
antigen bundle direnv
antigen bundle command-not-found
# antigen bundle nojhan/liquidprompt
antigen bundle blimmer/zsh-aws-vault@main
# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting
# Tell Antigen that you're done.
antigen apply

eval "$(starship init zsh)"
