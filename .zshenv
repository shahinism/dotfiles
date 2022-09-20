. /opt/asdf-vm/asdf.sh
source /usr/share/doc/git-extras/git-extras-completion.zsh

function ssm_upload_key {
    PUBLIC_KEY=$(ssh-keygen -y -f $1)
    PARAM="command=[\"bash -c 'echo ${PUBLIC_KEY} $USER | sudo tee -a /home/hadoop/.ssh/authorized_keys'\"]"
    aws ssm start-session --target ${@:2} --parameters="$PARAM" --document-name AWS-StartInteractiveCommand
}

function download_aws_lambda {
    mkdir -p $1 && cd $1
    aws lambda get-function --function-name $1 | jq '.Code.Location' | xargs curl | bsdtar -xvf-
    cd -
}

# opam configuration
[[ ! -r /home/shahin/.opam/opam-init/init.zsh ]] || source /home/shahin/.opam/opam-init/init.zsh > /dev/null 2> /dev/null

# Editor Setup
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode
alias e='emacsclient -t'
