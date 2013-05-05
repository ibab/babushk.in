---
title: OfflineIMAP with systemd
date: 2013-05-05
---

OfflineIMAP is a mail synchronization utility that allows you download mail over IMAP and save it locally as a [Maildir](https://en.wikipedia.org/wiki/Maildir). It's useful if you want to read your mail with a Mail User Agent (MUA) such as [Mutt](http://www.mutt.org/). Setting up OfflineIMAP has been explained nicely elsewhere:

* [The Homely Mutt (Steve Losh)](http://stevelosh.com/blog/2012/10/the-homely-mutt/)
* [Mutt + Gmail + Offlineimap (Patrick Brisbin)](http://pbrisbin.com/posts/mutt_gmail_offlineimap)
* [OfflineIMAP (Archwiki)](https://wiki.archlinux.org/index.php/OfflineIMAP)

This is just a quick post that explains how OfflineIMAP can be integrated into a user session controlled by [systemd](http://www.freedesktop.org/wiki/Software/systemd).

## Periodic synchronization with systemd

I've encountered problems trying to run OfflineIMAP continuously as a daemon.
It would often freeze and not synchronize mail for hours.
The most common solution to this is to start OfflineIMAP periodically with [cron](http://en.wikipedia.org/wiki/Cron) and only let it synchronize once on each run.

This is what I had been doing for over a year, until I've recently switched to the following systemd-based solution:
I run systemd with the `--user` switch inside my user session, allowing me to configure services (like OfflineIMAP) very conveniently.
Creating the following unit file in `~/.config/systemd/user` allows you to start a single synchronization.

```ini
# offlineimap.service

[Unit]
Description=OfflineIMAP Quicksync
After=network.target

[Service]
Type=oneshot
ExecStart=/usr/bin/offlineimap -o -q -u quiet
TimeoutStartSec=1min30s
```

Additionally, a second `.timer` unit is needed to run the service every 30 seconds:

```ini
# offlineimap.timer

[Unit]
Description=OfflineIMAP Quicksync timer

[Timer]
OnCalendar=*:*:0/30
Unit=offlineimap.service

[Install]
WantedBy=default.target
```

You can now enable mail synchronization by executing

```bash
systemctl --user enable offlineimap.timer
```

See if OfflineIMAP is working correctly by following the journal:

```bash
journalctl -f
```

