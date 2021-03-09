
function stm_runtime
    LD_PRELOAD="/usr/$LIB/libstdc++.so.6" \
        STEAM_FRAME_FORCE_CLOSE=1 \
        SDL_AUDIODRIVER=pulseaudio \
        steam
end

function stm
    set -l LD_LIBRARY_PATH "/lib/x86_64-linux-gnu:/usr/lib/x86_64-linux-gnu:" \
        "/lib/i386-linux-gnu:/usr/lib/i386-linux-gnu:" \
        "$HOME/.local/share/Steam/ubuntu12_32/steam-runtime/i386/lib/i386-linux-gnu:" \
        "/usr/lib/i386-linux-gnu:/usr/lib/x86_64-linux-gnu:" \
        "$HOME/.local/share/Steam/ubuntu12_32/steam-runtime/amd64/lib/x86_64-linux-gnu:"

    LD_LIBRARY_PATH=(echo -s $LD_LIBRARY_PATH) \
        STEAM_FRAME_FORCE_CLOSE=1 \
        SDL_AUDIODRIVER=pulseaudio \
        STEAM_RUNTIME=0 \
        steam
end
