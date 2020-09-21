{
    packageOverrides = pkgs: with pkgs; {
        amirrezaUserSpace = pkgs.buildEnv {
            name = "amirreza-user-space";
            paths = [
                fzf
                ripgrep
                go
                vlc
                neovim
                emacs                
            ];
        };
    };
}
