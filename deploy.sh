#!/bin/sh

make
./site clean
LANG=en_US.UTF-8 ./site build
rsync -avh --exclude '.git' _site/ /srv/http/www
chmod -R o+r /srv/http/www
