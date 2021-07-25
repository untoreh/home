function zshlic
    zsh -lic ". ~/.cluster/crt/functions; $argv"
end
set -x REPO_USER
set -x REPO_EMAIL
set -x REPO_TOKEN
set -x REPO_EP
alias xdt "zshlic x_dashboard_tunnel"
alias ghi "zshlic ghi"
alias ghu "zshlic ghu"
alias ghr "zshlic ghr"
alias ghbs "zshlic ghbs"
alias bbbs "zshlic bbbs"
alias bbdel "zshlic bbdel"
alias ghdel "zshlic ghdel"
alias repodir "zshlic repodir"
alias repobs "zshlic repobs"
alias repodist "zshlic repodist"
alias cpmtx "zshlic cpmtx"
alias gcreds "zshlic gcreds"
alias gcap "zshlic gcap"
alias inar "zshlic inar"
alias whor "zshlic whor"
alias gcup "zshlic gcup"
alias lre "zshlic lre"
alias ggcm commit-msg
alias rpdel "zshlic rpdel"
alias glkey "zshlic glkey"
alias ggurl "zshlic ggurl"
alias gldel "zshlic gldel"
alias ghemail "zshlic ghemail"
alias ghtest "zshlic ghtest"
alias diffban "zshlic diffban"

function bbwh
    zshlic "bbwh '$argv[1]'"
end

function glwh
    zshlic "glwh '$argv[1]'"
end

function ghwh
    zshlic "ghwh '$argv[1]'"
end

function deobfs
    cat $argv[1] | sed 's/^eval "/echo "/' | sed 's#&>/dev/null##' | bash
end

function da
    zshlic "da '$argv[1]'"
end

function fac
    zshlic "fac '$argv[1]'"
end

for v in REPO_USER REPO_EMAIL REPO_TOKEN REPO_EP
    set -x $v
end

function set_git_user
    set usr $argv[1]
    set ep $argv[2]
    set vars (cat ~/.gitconfig | grep "$usr.*$ep" | sed 's/^#\s*//')
    [ -n "$vars" ] && echo found match\!
    set suf '([^ ]+) ?.*/\1/'
    for v in REPO_USER REPO_EMAIL REPO_TOKEN REPO_EP
        set $v (echo $vars | sed -r "s/.*$v=$suf")
    end
end

function src_repo_usr
    set REPO_NAME (basename (realpath "$PWD"))
    set REPO_USER (git config user.name)
    set REPO_EP (git config remote.origin.url | sed -r 's~.*://.*:.*@([^/]*)/.*~\1~')
    set REPO_TOKEN (git config remote.origin.url | sed -r 's~.*://.*:(.*)@.*~\1~')
    set REPO_EMAIL (cat ~/.gitconfig | grep "$REPO_USER.*$REPO_EP" | sed -r 's/.*REPO_EMAIL=([^ ]*) +.*/\1/')
end

function sync_pl
    cd ~/.tmp/scripts
    zip /tmp/stuff/payload.zip -x '.git*' -x 'README*' -r .
    if [ ! -e /tmp/stuff/payload.gif ]
        ln -sr /tmp/stuff/payload.zip /tmp/stuff/payload.gif
    end
    while ! scp /tmp/stuff/payload.gif vrmc5:/opt/pld/
        :
    end
    sudo docker exec --privileged -it -e SHELL=/bin/zsh dpl /bin/zshlic \
        "flarectl zone purge --zone=unto.re --files='http://pld.unto.re/payload.gif,https://pld.unto.re/payload.gif'"
end
