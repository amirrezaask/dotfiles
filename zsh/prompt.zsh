# TODO: Check if starship is set as prompt do these
if ! command -v starship &> /dev/null
then
    sh -c "$(curl -fsSL https://starship.rs/install.sh)"
fi

# eval "$(starship init zsh)"



get_branch(){
    branch=$(parse_git_branch)
    if [ -z "$branch" ]; then
        echo ""
    else
        echo " $branch"
    fi
}

setopt PROMPT_SUBST

PROMPT='%F{green}%~%f$(get_branch) > '
