#!/usr/bin/env zsh
# Amirreza Plugin Manager for ZSH
PLUGINS_LIST=('syntax')
PLUGINS_PATH="${HOME}/.zsh/plugins"
has_plugin_var='no'
PLUGINS_ARE_DEFINED='no'

has_plugin() {
    echo "Checking if ${1} is present";
    if [ -d "${PLUGINS_PATH}/${1}" ]; then
        has_plugin_var='yes'
    else
        has_plugin_var='no'
    fi
}

plugin() {
    has_plugin $1
    echo "${has_plugin_var}";
}

plugin_list_is_defined() {
    if test -z "${PLUGINS_LIST}"
    then
        echo 'No Plugin is defined'
    else
        PLUGINS_ARE_DEFINED='yes'
        echo 'Plugins are OK!'
    fi
}


