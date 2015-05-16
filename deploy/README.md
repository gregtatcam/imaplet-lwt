Install instructions for Ubuntu

1. New user account creattion.
You can create a new user account via link to your server like
  htts://server/cgi-bin/imaplet.pl
In order to do this you need to install apache2 web server.
Alternativelly you can create a new user account by running from the command line
sudo imaplet_create_account
If you don't need apache2 installed then skip to step 2.
Instal apache2: sudo apt-fet install apache2
- configure ssl: https://www.digitalocean.com/community/tutorials/how-to-create-a-ssl-certificate-on-apache-for-ubuntu-14-04
- configure cgi: 
cp /etc/apache2/mods-available/cgi.load /etc/apache2/mods-enabled/cgi.load
sudo apach2ctl restart
You can use command line to configure the account
- update sudoers file
sudo chmod 640 /etc/sudoers
add this line at the end of sudoers: www-data ALL= NOPASSWD: /usr/local/bin/imaplet_create_account
sudo chmod 440 /etc/sudoers

2. install imaplet
Install imaplet via opam or manually on the target machine.
If the target machine doesn't have the development environment then
install imaplet on a development machine. Run imaplet_deploy - this creates
imaplet-deploy.tar.gz in the current directory.
Copy imaplet-deploy.tar.gz to the target machine, unzip and run deploy.sh [-a]
located in ./deploy folder. '-a' option installs cgi files for apache.

3. install imaplet as the service
If you installed imaplet directly via opam or manually on the target machine then
you need to install imapletd service. imapletd script is located in 
either /usr/local/share/imaplet or ~/.opam/compiler/share/imaplet folder.
- cp imapletd /etc/init.d/imapletd
- sudo update-rc.d imapletd defaults

4. By default the IMAP server is configured with ssl enabled on port 993 and SMTP configured with starttls enabled on port 587. User mailboxes are configured in /var/mail/accounts. You can edit the settings in imaplet.cf.

5. start/stop/restart imaplet as the service
- sudo service imapletd [start|stop|restart]
