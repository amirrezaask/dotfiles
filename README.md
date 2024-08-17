# AmirrezaAsk Configs

# OS Provision
## Ubuntu
```bash
sh -c "$(wget -qO- 'https://raw.githubusercontent.com/amirrezaask/dotfiles/master/provision.sh')"
```

## MacOS
TBA


# Installation
```bash
make
```


# Sublime Text Settings Installation
## MacOS
```bash
ln -s ./SublimeText "$HOME/Library/Application Support/Sublime Text 3/Packages/User"
```
## Windows
```cmd
cd "$env:appdata\Sublime Text 3\Packages\"
cmd /c mklink /D User $DOTFILES_DIR\SublimeText
```
