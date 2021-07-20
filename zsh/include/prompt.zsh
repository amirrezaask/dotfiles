get_branch(){
    branch=$(parse_git_branch)
    if [ -z "$branch" ]; then
        echo ""
    else
        echo "%F{15}on%f %F{127} $branch%f"
    fi
}

setopt PROMPT_SUBST
PROMPT='%F{32}%2~%f $(get_branch) '
