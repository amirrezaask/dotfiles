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
        echo "%F{33}git:%f %F{9}$branch%f"
    fi
}

setopt PROMPT_SUBST
# PROMPT='%F{51}%2~%f $(get_branch) '
PROMPT='%2~ $(get_branch) %F{green}>%f '
