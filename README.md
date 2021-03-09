# Layout

There are three types of files, configs, secrets and statefuls.
At `$HOME` path I setup a git repository. This get repository holds the configs. Most of the grunt work for the setup is making a proper `.gitignore` (one at the top and one under `.config/`).
We don't use any kind of templates, files that have secrets are stored under the `~/.secrets/files/` directory. `~/.secrets` is also a git repository, that stores all the secrets files with proper path, eg. `~/.secrets/files/.authinfo` -> `~/.authinfo`. After cloning the secrets repo, we build the linkfarm with gnu `stow`. like:

```bash
cd ~/.secrets
stow -t $HOME files/
```

Statefuls (`~/.config/statefuls.txt`) are all those directories that we would like to backup and that don't hold configurations, like media, browser cache, game files. For those we keep a list for a tool like `borg` or `duplicacy`.

`

# Encryption

I looked at `git-secret`, `git-remote-gcrypt` and `git-crypt`. I Chose `git-crypt`.

- `git-secret`: I didn't like the `show`/`reveal` enforcing and magit doesn't have an interface for it, it was too opaque.
- `git-remote-gcrypt`: was transparent enough but slow since it pushes the whole blobbed repository encrypted.
- `git-crypt`: encrypts the files singularly so doesn't take too much to push, it looks unmaintained, but it works for now.

# Motivation

All the dotfiles managers I looked at were too complex, requiring too many steps for deploying, and a I consider a templating system over engineering.
A file with secret tokens inside most likely doesn't have interesting useful to keep public, and usually credentials have their own separate files.
If I want to have different machine based configurations I use different branches. Programmatic devop-like config generation is not the goal of a dotfiles manager.
The benefit of this setup is that it is easier (through a gui like `magit`) to have a view of the upstanding changes in the home directory, and interactively discard, or commit changes as you please.
