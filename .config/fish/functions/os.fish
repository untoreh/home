# arch
alias pai "pacaur --needed --noedit --silent "
alias par "pacaur --silent"
alias parc "rm -rf $HOME/.cache/pacaur/*"

# ubu
alias aptu "sudo apt update "
alias aptar "sudo apt autoremove -y --purge "
alias dpkgfr "sudo dpkg --remove --force-remove-reinstreq"
alias aptfb "sudo apt-get --fix-broken install"
alias aptc "sudo apt autoclean && sudo apt-get clean "
alias apts "sudo apt search "
alias apti "sudo apt install "
alias aptr "sudo add-apt-repository "
alias aptp "sudo apt-get purge "
alias aptpol "sudo apt-cache policy "

# ubuntu upgrade
function aptg
    sudo zfs destroy rpool/ROOT/ubuntu@(date +%m-%d-%y)
    sudo zfs snapshot rpool/ROOT/ubuntu@(date +%m-%d-%y)
    set DEBIAN_FRONTEND noninteractive
    $HOME/.bin/fix-apt-sources
    sudo apt update
    sudo apt full-upgrade -y -q
    aptc; aptar
    set -e DEBIAN_FRONTEND
end
