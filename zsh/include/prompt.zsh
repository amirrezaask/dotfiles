get_branch(){
    branch=$(parse_git_branch)
    if [ -z "$branch" ]; then
        echo ""
    else
        echo "%F{13}on%f %F{13} $branch%f"
    fi
}

setopt PROMPT_SUBST
PROMPT='%F{14}%2~%f $(get_branch) '
