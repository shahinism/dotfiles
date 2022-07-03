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
antigen bundle nojhan/liquidprompt
antigen bundle blimmer/zsh-aws-vault@main
# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Tell Antigen that you're done.
antigen apply

. /opt/asdf-vm/asdf.sh
source /usr/share/doc/git-extras/git-extras-completion.zsh

function ssm_upload_key {
    PUBLIC_KEY=$(ssh-keygen -y -f $1)
    PARAM="command=[\"bash -c 'echo ${PUBLIC_KEY} $USER | sudo tee -a /home/hadoop/.ssh/authorized_keys'\"]"
    aws ssm start-session --target ${@:2} --parameters="$PARAM" --document-name AWS-StartInteractiveCommand
}

# opam configuration
[[ ! -r /home/shahin/.opam/opam-init/init.zsh ]] || source /home/shahin/.opam/opam-init/init.zsh > /dev/null 2> /dev/null
