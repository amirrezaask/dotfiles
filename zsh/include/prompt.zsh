get_branch(){
    branch=$(parse_git_branch)
    if [ -z "$branch" ]; then
        echo ""
    else
        echo "on  $branch"
    fi
}

PROMPT='%F{35}%~%f %F{127}$(get_branch)%f '
