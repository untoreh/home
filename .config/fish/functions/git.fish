alias gpfo "git push --force"
alias ggcm "commit-msg"
alias gcfo "git checkout HEAD^ "
alias grc "git rev-list --count HEAD"
alias gpuh "git pull --allow-unrelated-histories"
alias gc1 "git clone --depth=1"
alias galt "find \* -size -1M -type f -print0 | xargs -0 git add"

function gurl
    grep -oP '(?<=url = ).*' <.git/config | clc
end

function girt ## delete tag and retag
    git tag -d $1 ; git push --delete origin $1 ; git tag $1 ; git push origin $1
end
function gitchop
    git checkout --orphan latest
    git add -A git commit -am "."
    git branch -D master
    git branch -m master
    git push -f origin master
end
function git_rem
    set -q GIT_USER; or set GIT_USER user
    set -q GIT_TOKEN; or set GIT_TOKEN token
    echo (git remote show origin | grep -i "push.*url" | \
        sed -r 's~.*push.*?:[ \s]+(.*?://)(.*)$~\1'$GIT_USER:$GIT_TOKEN'@\2~i')
end
function gcg
	git config --global github.user
	git config --global github.password
end
## $1 glob to remove from history
function git_trim_history_target
    bfg --delete files $argv[1]
    #java -jar bfg.jar --delete-files $1
    git reflog expire --expire=now --all && \
        git gc --prune=now --aggressive && \
        git push --force
end
alias gth "git_trim_history_size"
function git_trim_history_size
    set -l repo (basename "$argv[1]").git
    git clone --mirror "$1" || { printf "provide a git repo"; return 1; }
    which bfg &>/dev/null || { printf "bfs executable not found"; return 1; }
    cd $repo
    set branches (git branch --list --format='%(refname:short)' | tr '\n' ',')
    cd -
    bfg --strip-blobs-bigger-than 10k --protect-blobs-from $branches $repo
    cd $repo
    git reflog expire --expire=now --all && git gc --prune=now --aggressive
    printf "force pushing to remote repo in 3..."; sleep 1;
    printf "2..."; sleep 1;
    printf "1..."; sleep 1;
    git push --force
end
function git_sync_tags
    git tag | xargs git tag -d
    git fetch --tags
end
