alias xdt "zsh -lc x_dashboard_tunnel"
alias ghi "zsh -lc ghi"
alias ghu "zsh -lc ghu"
alias ghr "zsh -lc ghr"
alias ghbs "zsh -lc ghbs"
alias bbbs "zsh -lc bbbs"
alias bbdel "zsh -lc bbdel"
alias ghdel "zsh -lc ghdel"
alias repodir "zsh -lc repodir"
alias repobs "zsh -lc repobs"
alias repodist "zsh -lc repodist"
alias cpmtx "zsh -lc cpmtx"
alias gcreds "zsh -lc gcreds"
alias gcap "zsh -lc gcap"
alias inar "zsh -lc inar"
alias whor "zsh -lc whor"
alias gcup "zsh -lc gcup"
alias lre "zsh -lc lre"
alias ggcm "commit-msg"
alias rpdel "zsh -lc rpdel"
alias glkey "zsh -lc glkey"
alias ggurl "zsh -lc ggurl"
alias gldel "zsh -lc gldel"

function bbwh
    zsh -lc "bbwh '$argv[1]'"
end

function glwh
    zsh -lc "glwh '$argv[1]'"
end

function ghwh
    zsh -lc "ghwh '$argv[1]'"
end

function deobfs
    cat $argv[1] | sed 's/^eval "/echo "/' | sed 's#&>/dev/null##' | bash
end

function da
    zsh -lc "da '$argv[1]'"
end

function fac
    zsh -lc "fac '$argv[1]'"
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
    sudo docker exec --privileged -it -e SHELL=/bin/zsh dpl /bin/zsh -lic \
        "flarectl zone purge --zone=unto.re --files='http://pld.unto.re/payload.gif,https://pld.unto.re/payload.gif'"
end
