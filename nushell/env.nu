# Environment and PATH entries mirrored from fish/config.fish and fish/conf.d.

$env.EDITOR = "nvim"
$env.GIT_EDITOR = "nvim"
$env.PNPM_HOME = ($nu.home-dir | path join "Library" "pnpm")
$env.BUN_INSTALL = ($nu.home-dir | path join ".bun")
$env.VP_HOME = ($nu.home-dir | path join ".vite-plus")

# Keep the same effective order as fish_user_paths plus conf.d/vite-plus.fish.
# Cargo's generated setup only prepends its bin directory when it is absent.
let cargo_bin = ($nu.home-dir | path join ".cargo" "bin")
let fish_paths = [
    ($env.VP_HOME | path join "bin")
    ($env.BUN_INSTALL | path join "bin")
    ($nu.home-dir | path join ".local" "share" "nvim" "mason" "bin")
    ($nu.home-dir | path join ".gapcode" "bin")
    ($nu.home-dir | path join ".opencode" "bin")
    ($nu.home-dir | path join ".local" "bin")
    "/opt/homebrew/bin"
    "/usr/local/bin"
    ($env.PNPM_HOME | path join "bin")
]

$env.PATH = ($fish_paths | append ($env.PATH | where {|entry| $entry not-in $fish_paths }))
if $cargo_bin not-in $env.PATH {
    $env.PATH = ($env.PATH | prepend $cargo_bin)
}
