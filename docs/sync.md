Legend:
_folderLabel_ (Local folder) [Remote folder]

Send and receive folders `phone <-> home` :
- `$PHONE_downloads` (Downloads) [~/Downloads/$PHONE]
- `$PHONE_andotp` (andotp) [~/.wallets/$PHONE_andotp]
- `$PHONE_recovery` (TWRP/clockworkmod) [~/share/backups/$PHONE_recovery]
- `$PHONE_backups` (appmanager) [~/share/backups/$PHONE_apps]
- `$PHONE_camera` (DCIM) [~/Pictures/$PHONE_camera]

Send and receive (p2p) folders `phone <-> phone <-> home`:
- `org` (org) [~/org]
- `locations` (locations) [~/share/locations]
- `keys` (keys)
- `SyncBooks` (Books) [~/Documents/books/toadd]
P2P folders are versioned except `SyncBooks`.

### No send only, receive only folders
I found one way sync folders troublesome on the long run, since you have to keep track on who is the feeder and the consumer, and leads to inconsistencies. 
Recovery data is made such that once it is backed to the central server (i.e. home) it is moved to a local folder, such that it is cleared from the client (i.e. phone). 
A watchdog that automatically moves data (once there are no more syncthing `.tmp` files) could be added.
