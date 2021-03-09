alias w6 "WINEARCH=win64 WINEPREFIX=~/.wine64"
alias w3 "WINEARCH=win32 WINEPREFIX=~/.wine32"
alias w6c  "w6 winecfg"
alias w3c "w3 winecfg"

function heidi
    wine /home/fra/Desktop/SPM/heidisql/heidisql.exe
end

function navicat
    WINEARCH=win64 \
        WINEPREFIX=$HOME/.navicat64 \
        wine $HOME/.navicat64/drive_c/Program\ Files/PremiumSoft/Navicat\ Premium/navicat.exe
end

function sqlyog
    WINEPREFIX=$HOME/.wine \
        wine $HOME/.wine/drive_c/Program\ Files\ \(x86\)/SQLyog/SQLyog.exe
end

function w6s
    w6 wine $HOME/.wine64/drive_c/Program\ Files\ \(x86\)/Steam/Steam.exe -no-cef-sandbox
end

function w3s
    w3 wine $HOME/.wine32/drive_c/Program\ Files/Steam/Steam.exe -no-cef-sandbox
end

function w6b
    kill -9 (pgrep -f \.exe) ; w6 wineboot
end

function w3b
    kill -9 (pgrep -f \.exe) ; w3 wineboot
end
