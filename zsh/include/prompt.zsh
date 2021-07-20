get_branch(){
    branch=$(parse_git_branch)
    if [ -z "$branch" ]; then
        echo ""
    else
        echo "on  $branch"
    fi
}

PROMPT='%~ $(get_branch) '
