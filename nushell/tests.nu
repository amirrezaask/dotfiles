# Run from the repo root:
# nu --env-config nushell/env.nu --config nushell/config.nu --commands 'source nushell/tests.nu'
use std/assert

let entries = [
    {command: "git status", cwd: "/a"}
    {command: "legacy command"}
    {command: "cargo test", cwd: "/b"}
    {command: "git log", cwd: "/a"}
    {command: "git status", cwd: "/a"}
]
assert equal (nu-history-suggestions $entries "" --cwd /a) [
    {value: "git status"}
    {value: "git log"}
]
assert equal (nu-history-suggestions $entries "GIT" --cwd /a | length) 2
assert equal (nu-history-suggestions $entries "cargo" --cwd /a) []
assert equal (nu-history-suggestions $entries "cargo") [{value: "cargo test"}]
assert equal (nu-history-suggestions $entries "legacy") [{value: "legacy command"}]
assert equal (nu-history-suggestions [] "") []
let large_history = (
    [{command: "old matching command", cwd: "/a"}]
    | append (0..1100 | each {|i| {command: $"recent ($i)", cwd: "/a"}})
)
assert equal (nu-history-suggestions $large_history "" | length) 1000
assert equal (nu-history-suggestions $large_history "old matching") [
    {value: "old matching command"}
]
assert equal ($env.config.menus | where name == global_history_menu | length) 1
assert equal ($env.config.keybindings | where name == global_history | first | get modifier) control_alt
for menu in $env.config.menus {
    let results = (do $menu.source NU_TEST_MISSING_QUERY_7b3e 0)
    assert equal $results []
}
print "PASS: directory/global history, recency, deduplication, matching before limit"

assert equal $env.config.completions.algorithm fuzzy
assert equal (nu-external-completions []) null
assert equal (nu-external-completions [git]) null
assert equal (nu-external-completions [nu_missing_command_7b3e ""]) null
if not (which carapace | is-empty) {
    assert ("--help " in (nu-external-completions [git --he] | get value))
    assert ("--help " in (nu-external-completions [^git --he] | get value))
    assert ("--help " in (nu-external-completions [g --he] | get value))
    assert ("--track " in (nu-external-completions [gco --tr] | get value))
    assert equal ($env.config.completions.external.completer | describe) closure
    print "PASS: Carapace command flags, forced externals, Git aliases"
} else {
    print "SKIP: Carapace integration (optional executable is absent)"
}
do {
    $env.PATH = []
    assert equal (nu-external-completions [git --he]) null
    assert equal (nu-git-branch) ""
}
print "PASS: graceful fallback when Git or Carapace is unavailable"

let original_directory = $env.PWD
let scratch = (mktemp --directory)
try {
    # Real Git fixtures exercise unborn branches, subdirectories, and detached HEAD.
    let repository = ($scratch | path join repository)
    do {
        $env.GIT_CONFIG_GLOBAL = "/dev/null"
        $env.GIT_CONFIG_NOSYSTEM = "1"
        ^git -c init.defaultBranch=feature/prompt init --quiet $repository
        assert equal $env.LAST_EXIT_CODE 0
        cd $repository
        assert equal (nu-git-branch) feature/prompt
        $env.LAST_EXIT_CODE = 17
        let prompt = (do $env.PROMPT_COMMAND | ansi strip)
        assert ($prompt | str ends-with " (feature/prompt)")
        assert equal $env.LAST_EXIT_CODE 17
        mkdir nested
        cd nested
        assert equal (nu-git-branch) feature/prompt
        cd $repository
        ^git -c user.name="Nu Tests" -c user.email="nu-tests@example.invalid" -c commit.gpgsign=false commit --allow-empty --quiet -m fixture
        assert equal $env.LAST_EXIT_CODE 0
        ^git checkout --detach --quiet
        assert equal $env.LAST_EXIT_CODE 0
        assert equal (nu-git-branch) ""
        assert not (do $env.PROMPT_COMMAND | ansi strip | str contains "feature/prompt")
        cd $scratch
        assert equal (nu-git-branch) ""
        assert not (do $env.PROMPT_COMMAND | ansi strip | str contains " ()")
    }
    print "PASS: branch prompt, nested directories, detached HEAD, non-repository, exit status"

    # A fake executable records each step; no real commits or pushes occur here.
    let fake_bin = ($scratch | path join bin)
    mkdir $fake_bin
    let fake_git = ($fake_bin | path join git)
    '#!/bin/sh
printf "%s\n" "$*" >> "$NU_TEST_GIT_LOG"
if [ "$1" = "$NU_TEST_GIT_FAILURE" ]; then
    exit 23
fi
if [ "$1" = "symbolic-ref" ]; then
    printf "%s\n" "feature/test"
fi
' | save $fake_git
    ^chmod +x $fake_git
    do {
        $env.PATH = ($env.PATH | prepend $fake_bin)
        $env.NU_TEST_GIT_LOG = ($scratch | path join git.log)
        for scenario in [
            {failure: symbolic-ref, count: 1, last: "symbolic-ref --short HEAD"}
            {failure: add, count: 2, last: "add ."}
            {failure: commit, count: 3, last: "commit -m wip"}
            {failure: push, count: 4, last: "push origin feature/test"}
            {failure: "", count: 4, last: "push origin feature/test"}
        ] {
            "" | save --force $env.NU_TEST_GIT_LOG
            $env.NU_TEST_GIT_FAILURE = $scenario.failure
            let failed = (try { wip; false } catch { true })
            assert equal $failed ($scenario.failure != "")
            let calls = (open --raw $env.NU_TEST_GIT_LOG | lines)
            assert equal ($calls | length) $scenario.count
            assert equal ($calls | last) $scenario.last
        }
    }
    print "PASS: wip aborts on branch/staging/commit/push failures; successful sequence"
} catch {|err|
    cd $original_directory
    rm --recursive --permanent $scratch
    error make {msg: $err.msg}
}
cd $original_directory
rm --recursive --permanent $scratch
