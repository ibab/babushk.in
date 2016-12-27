#!/bin/sh

stack setup
stack build
stack exec site clean
LANG=en_US.UTF-8 stack exec site build
rsync -avh --exclude '.git' _site/ /srv/http/www
chmod -R o+r /srv/http/www
