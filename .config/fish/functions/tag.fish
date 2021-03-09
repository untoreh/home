function tag
    set -x TAG_SEARCH_PROG rg  # replace with rg for ripgrep
    set -q TAG_ALIAS_FILE; or set -l TAG_ALIAS_FILE /tmp/tag_aliases
    set -x TAG_ALIAS_PREFIX :
    command tag $argv; and source $TAG_ALIAS_FILE ^/dev/null
    alias ag tag  # replace with rg for ripgrep
end
