# Setup

- Install stow

```bash
apt install -y stow
```

- Create and switch to the secrets directory

```bash
mkdir ~/.secrets && cd ~/.secrets
```

- Ensure you have access to the secrets repository, and password for decryption, (and possibly the deployment script sha256sum)
- Ensure you have the [`git-ssl` wrapper](git-ssl.sh) in your `$PATH`

```bash
[ -z "$REPO" ] && echo "Repository remote address \$REPO not set!"
[ -z "$PASSWORD" -o ! -f ./pass ] && echo "\$PASSWORD not set and './pass' file not found!"
```

- Fetch and execute the deployment script `secrets-setup.sh`
- If `$REPO` is provided, the setup assumes a remote exists, and fetches that, otherwise execution stops after the encryption filters are setup

```bash
SETUP_URL="https://unto.re/secrets-setup.sh"
SETUP="setup.sh"
SETUP_HASH=c3f93616b64e99bb00ec9f82f360a8b5e6b4a42a02958622adcd55b2ebbe2c48
wget $SETUP_URL -O $SETUP
FETCHED_HASH=$(sha256sum $SETUP | cut -d ' ' -f1)

[ "$FETCHED_HASH" -eq "$SETUP_HASH" ] &&
    eval "./$SETUP $REPO"
```

- Ensure no files in `files/` overlap with `~/`
- Generate the links from the `files` directory

```bash
stow -t ~/ files/
```

# Utilities

Use `cleanup.sh` to check broken links for secrets that no longer exists and `cleanup.sh -r` to remove them. Files that are not symlinks are not removed.

Use `decrypt.sh` to dump the tree under `files/` (or `$TARGET_PATH`) to `/tmp/.decrypt`. Decryption is attempted for all files.

# WARNING!

Take care that files in `~/.secrets` are **symlinked**, if you delete them from the repository _while still relying on their contents_, you will end up with a possibly **broken system** (..where did my keys go? :O)

# Quirks

`git status` shows un-staged changes for empty files ..this is annoying..
