Legend:
_folderLabel_ (Local folder) [Remote folder]

Send only folders, `phone (send) -> home (receive)`: 

- `$PHONE_recovery` (TWRP, clockworkmod) [Documents/books/toadd]
- `$PHONE_backups` (TitaniumBackup) [~/share/backups/$PHONE_apps]
- `$PHONE_camera` (DCIM) [~/Pictures/$PHONE_camera]

Receive only folders, `phone  (receive) <- home (send)`:
- SyncBooks (Books) [~/Documents/books/toadd]

Send and receive folders `phone <-> home` :
- `$PHONE_downloads` (Downloads) [~/Downloads/$PHONE]
- `$PHONE_andotp` (andotp) [~/.wallets/$PHONE_andotp]

Send and receive (p2p) folders `phone <-> phone <-> home`:
- `org` (org) [~/org]
- `locations` (locations) [~/share/locations]
- `keys` (keys)
P2P folders are also versioned.
