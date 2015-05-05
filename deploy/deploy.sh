#!/bin/sh

cp -f imaplet_* /usr/local/bin/
cp -f imaplet /usr/local/bin/
cp -f server.* /usr/local/share/imaplet/
cp -f imapletd /etc/init.d/imapletd

update-rc.d imapletd defaults

if [ -f "/usr/local/share/imaplet/imaplet.cf" ];
then
  echo "========== imaplet.cf exists"
  diff imaplet.cf /usr/local/share/imaplet/imaplet.cf
else
  cp -f imaplet.cf /usr/local/share/imaplet/imaplet.cf
fi

if [ -f "/usr/local/share/imaplet/users" ];
then
  echo "========== users exists"
  diff users /usr/local/share/imaplet/users
else
  cp -f users /usr/local/share/imaplet/users
fi

if [ $1 = "-a" ];
then
  mkdir -p /var/www/html
  mkdir -p /usr/lib/cgi-bin
  mkdir -p /usr/local/apache2/cgi-bin
  cp -f imaplet.html /var/www/html/imaplet.html
  cp -f crt.pl /usr/lib/cgi-bin/crt.pl
  rm -rf /usr/local/apache2/cgi-bin/imaplet_create_account
  ln -s /usr/local/share/imaplet_create_account /usr/local/apache2/cgi-bin/imaplet_create_account
fi
