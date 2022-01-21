{
    allowBroken = true;
    packageOverrides = pkgs: with pkgs; {
        amirrezaUserSpace = pkgs.buildEnv {
            name = "amirreza-user-space";
            paths = [
                fzf
                ripgrep
                # neovim
                # alacritty
            ];
        };
     };
}
