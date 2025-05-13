export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/usr/local/bin"
export PATH="$PATH:/Applications/Emacs.app/Contents/MacOS"
export PATH="$PATH:/Applications/Emacs.app/Contents/MacOS/bin"
export PATH="$PATH:/opt/homebrew/bin"

if command -v brew &>/dev/null
   brew shellenv fish | source
end

if command -v fzf &>/dev/null
    fzf --fish | source
end

if command -v nvim &>/dev/null
    alias vim='nvim'
    export EDITOR='nvim'
    export GIT_EDITOR='nvim'
end

function oclogs -d "Read logs for given app name"
    oc logs --prefix -f --selector "app.kubernetes.io/instance=snappdoctor-$1-prod, app.kubernetes.io/name=$1"
end

function fish_greeting
end

function fish_prompt
    printf '%s' (set_color green) (prompt_pwd) (set_color white) (fish_git_prompt) ' > '
end

function fish_right_prompt
end


function gwip
    set -l git_branch (git branch 2>/dev/null | sed -n '/\* /s///p')
    git add . && git commit -m "Automated Commit: Work In Progress $(date +"%Y-%m-%d %H:%M:%S")" && git push origin $git_branch
end

function gd
    git diff
end

function reload
    source ~/.config/fish/config.fish
end

if command -v cursor &> /dev/null then
    alias code='cursor'
end

alias gs='git status'

# if command -v starship &> /dev/null then
#     starship init fish | source
# end


export HOMEBREW_NO_AUTO_UPDATE=1

function set_system_background
    set wallpaper_dir "$HOME/src/github/ricing-material/"  # Default directory for wallpapers
    set image_path $argv[1]

    # If no input is provided, use fzf to select an image
    if test -z "$image_path"
        if not type -q fzf
            echo "Error: 'fzf' is not installed. Please install it (e.g., brew install fzf or sudo apt install fzf)"
            return 1
        end
        if not test -d "$wallpaper_dir"
            echo "Error: Wallpaper directory '$wallpaper_dir' does not exist"
            return 1
        end
        set image_path (find "$wallpaper_dir" -type f \( -iname "*.jpg" -o -iname "*.png" -o -iname "*.jpeg" -o -iname "*.heic" \) | fzf --preview "catimg -w 80 {} 2>/dev/null || echo 'Preview not available'")
        if test -z "$image_path"
            echo "Error: No image selected"
            return 1
        end
    end

    # Validate the selected or provided image path
    if not test -f "$image_path"
        echo "Error: File '$image_path' does not exist or is not a file"
        return 1
    end

    # Check OS and set background
    set os (uname)
    if test "$os" = "Darwin"
        osascript -e "tell application \"System Events\" to tell every desktop to set picture to \"$image_path\""
        if test $status -eq 0
            echo "Background set to $image_path (macOS)"
        else
            echo "Error: Failed to set background (macOS)"
            return 1
        end
    else if test "$os" = "Linux"
        if not type -q feh
            echo "Error: 'feh' is not installed. Please install it (e.g., sudo apt install feh)"
            return 1
        end
        feh --bg-scale "$image_path"
        if test $status -eq 0
            echo "Background set to $image_path (Linux)"
        else
            echo "Error: Failed to set background (Linux)"
            return 1
        end
    else
        echo "Error: Unsupported operating system"
        return 1
    end
end

# Added by OrbStack: command-line tools and integration
# This won't be added again if you remove it.
source ~/.orbstack/shell/init2.fish 2>/dev/null || :
