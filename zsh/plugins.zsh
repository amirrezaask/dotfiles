#!/usr/bin/env zsh
# Amirreza Plugin Manager for ZSH
PLUGINS_LIST=('ohmyzsh/ohmyzsh')
PLUGINS_PATH="${HOME}/.zsh/plugins"
THIS_PLUGIN=''
HAS_THIS_PLUGIN='no'
PLUGINS_ARE_DEFINED='no'

has_plugin() {
    echo "Checking if ${1} is present";
    if [ -d "${PLUGINS_PATH}/${1}" ]; then
        HAS_THIS_PLUGIN='yes'
    else
        HAS_THIS_PLUGIN='no'
    fi
}

install_plugin() {
    git clone "https://github.com/${THIS_PLUGIN}" "${PLUGINS_PATH}/${THIS_PLUGIN}"
    
}
install_plugins() {
    for plugin in $PLUGINS_LIST
    do
        has_plugin $plugin
        echo $HAS_THIS_PLUGIN
        if [ "$HAS_THIS_PLUGIN" = 'no' ]; then
            echo "Installing ${plugin}"
            THIS_PLUGIN="${plugin}"
            install_plugin
        else
            echo "No need to install ${plugin}"
        fi
    done
}

plugin_list_is_defined() {
    if test -z "${PLUGINS_LIST}"
    then
        echo 'No Plugin is defined'
        exit 1
    else
        PLUGINS_ARE_DEFINED='yes'
        echo 'Plugins are defined'
    fi
}

# check if any plugin has defined to load
plugin_list_is_defined

# Installing plugins
install_plugins
