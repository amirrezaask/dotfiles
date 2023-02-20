set fish_greeting ""

set -gx GO111MODULE 'on'
set -gx GOPATH "$HOME"
set -gx GOPRIVATE "gitlab.snapp.ir"
set -gx GOPROXY 'goproxy.io,direct'

set -gx PATH "/opt/homebrew/bin:$PATH"

set -gx PATH "$HOME/.local/bin:$PATH"

set -gx PATH "$GOROOT/bin:$PATH"

set -gx PATH "$HOME/.config/composer/vendor/bin:$PATH"

set -gx PATH "$GOPATH/bin:$PATH"

set -gx PATH "$HOME/.cargo/bin:$PATH"

eval $(brew shellenv)

set -gx EDITOR 'vim'
set -gx HOMEBREW_NO_AUTO_UPDATE '1'

set -gx FZF_DEFAULT_OPTS '--height 20%'
set -gx FZF_DEFAULT_COMMAND 'rg --files'

alias snappvpn='sudo openfortivpn -c ~/snappDC.conf'

function fish_prompt
	set_color red
	echo -n "["(date "+%H:%M")"] "
	set_color blue
	echo -n (whoami)
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
