{
    packageOverrides = pkgs: with pkgs; {
        amirrezaUserSpace = pkgs.buildEnv {
            name = "amirreza-user-space";
            paths = [
                ripgrep
            ];
        };
    };
}
