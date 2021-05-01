sudo wsl --mount \\.\PHYSICALDRIVE2 --bare
wsl -e sudo mkdir /run/upper /run/work
wsl -e sudo mount -t overlay overlay -o lowerdir=/tmp,upperdir=/run/upper,workdir=/run/work /tmp
wsl -e sudo chmod 777 /tmp
wsl -e sudo mount -a

