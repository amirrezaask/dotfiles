if ! test -d $HOME/.oh-my-zsh 
then
    git clone https://github.com/ohmyzsh/ohmyzsh.git ~/.oh-my-zsh
fi

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh

reload() {
    source ~/.zshrc
}

if command -v nvim &> /dev/null
then
    alias vim='nvim'
    alias vi='nvim'
    alias v='nvim'
fi

if command -v cursor &> /dev/null
then
    alias code='cursor'
fi

if ! command -v starship &> /dev/null
then
    curl -sS https://starship.rs/install.sh | sh
fi

eval "$(starship init zsh)"


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias artisan='php artisan'
alias wip='git add .; git commit -m "$(date +"%Y-%m-%d %H:%M:%S")"; git push origin $(git_current_branch)'
alias gwip='git add .; git commit -m "$(date +"%Y-%m-%d %H:%M:%S")"; git push origin $(git_current_branch)'

export PATH="/Users/amirrezaask/.config/herd-lite/bin:$PATH"
export PHP_INI_SCAN_DIR="/Users/amirrezaask/.config/herd-lite/bin:$PHP_INI_SCAN_DIR"
export PATH="$HOME/go/bin:$PATH"

export EDITOR='nvim'

export GIT_EDITOR="$EDITOR"

alias z='zed .'

function set_system_background() {
  local wallpaper_dir="$HOME/src/github/ricing-material/"  # Default directory for wallpapers
  local image_path="$1"

  # If no input is provided, use fzf to select an image
  if [[ -z "$image_path" ]]; then
    if ! command -v fzf >/dev/null 2>&1; then
      echo "Error: 'fzf' is not installed. Please install it (e.g., brew install fzf or sudo apt install fzf)"
      return 1
    fi
    if [[ ! -d "$wallpaper_dir" ]]; then
      echo "Error: Wallpaper directory '$wallpaper_dir' does not exist"
      return 1
    fi
    image_path=$(find "$wallpaper_dir" -type f \( -iname "*.jpg" -o -iname "*.png" -o -iname "*.jpeg" -o -iname "*.heic" \) | fzf --preview "catimg -w 80 {} 2>/dev/null || echo 'Preview not available'")
    if [[ -z "$image_path" ]]; then
      echo "Error: No image selected"
      return 1
    fi
  fi

  # Validate the selected or provided image path
  if [[ ! -f "$image_path" ]]; then
    echo "Error: File '$image_path' does not exist or is not a file"
    return 1
  fi

  # Check if on macOS
  if [[ "$(uname)" == "Darwin" ]]; then
    osascript -e "tell application \"System Events\" to tell every desktop to set picture to \"$image_path\""
    if [[ $? -eq 0 ]]; then
      echo "Background set to $image_path (macOS)"
    else
      echo "Error: Failed to set background (macOS)"
      return 1
    fi

  # Check if on Linux
  elif [[ "$(uname)" == "Linux" ]]; then
    if ! command -v feh >/dev/null 2>&1; then
      echo "Error: 'feh' is not installed. Please install it (e.g., sudo apt install feh)"
      return 1
    fi
    feh --bg-scale "$image_path"
    if [[ $? -eq 0 ]]; then
      echo "Background set to $image_path (Linux)"
    else
      echo "Error: Failed to set background (Linux)"
      return 1
    fi

  else
    echo "Error: Unsupported operating system"
    return 1
  fi
}

[[ ! -r '/Users/amirrezaask/.opam/opam-init/init.zsh' ]] || source '/Users/amirrezaask/.opam/opam-init/init.zsh' > /dev/null 2> /dev/null
