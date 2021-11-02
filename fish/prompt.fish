# install oh-my-posh
if [ $PROMPT_ENGINE = "starship" ]
    # install if not exists
    if not command -v starship &> /dev/null
        curl -fsSL 'https://starship.rs/install.sh'
    end

    starship init fish | source
end

if [ $PROMPT_ENGINE = "oh-my-posh" ]
    if not command -v oh-my-posh &> /dev/null
        sudo wget https://github.com/JanDeDobbeleer/oh-my-posh/releases/latest/download/posh-linux-amd64 -O /usr/local/bin/oh-my-posh
        sudo chmod +x /usr/local/bin/oh-my-posh
    end
    oh-my-posh --init --shell fish --config ~/.poshthemes/craver.omp.json | source
end

if [ $PROMPT_ENGINE = "none" ]
    # Fish git prompt
    set __fish_git_prompt_showuntrackedfiles 'yes'
    set __fish_git_prompt_showdirtystate 'yes'
    set __fish_git_prompt_showstashstate ''
    set __fish_git_prompt_showupstream 'none'
    set -g fish_prompt_pwd_dir_length 3

    function fish_prompt
        set_color brblack
        echo -n "["(date "+%H:%M")"] "
        set_color brblue
        echo -n (hostname)
        if [ $PWD != $HOME ]
            set_color brblack
            echo -n ':'
            set_color yellow
            echo -n (basename $PWD)
        end
        set_color green
        printf '%s ' (__fish_git_prompt)
        set_color red
        echo -n '| '
        set_color normal
    end
end
