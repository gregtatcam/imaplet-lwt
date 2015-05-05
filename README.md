imaplet
=======
This is a prototype of IMAP server.

To get started
- install imaplet via opam: opam install imaplet
- configure authentication in "users" file. 
  The file can be found in ~/.opam/xxxx/share/imaplet/users or /usr/local/share/imaplet/users if installed manually. There is one line per configured user, for instance
imaplet:{PLAIN}imaplet:501:::/Users/imaplet
Where 'imaplet' is the user name, {PLAIN} is the supported authentication, 'imaplet' is the password, 501 is the user id (you can get with 'id' command on MAC X), /Users/imaplet is user's home directory.
- configure run-time parameters in imaplet.cf (same dir as above). The important parameters to set:
- irmin_path: path for the git repository, default is /var/mail/accounts/%user/repo
- user_cert_path: path for the user's certificate, default is /var/mail/accounts/%user/cert
- addr: interface that IMAP server accepts connections on, default is 0.0.0.0
- port: port to listen on, normally 993 for SSL and 143 for plain, default is 993
- ssl: set to true to support ssl, default is true
- starttls: set to true to support STARTTLS, default is true
- smtp_addr: interface that SMTP server accepts connections on, default is 0.0.0.0
- smtp_port: port to listen on, normally it's either 25 or 587, default is 587
- smtp_ssl: this option is rarely suppported by email clients, default is false
- smtp_starttls: set to true to support STARTTLS, default is true
- create new user account - view README in the deploy folder
- import email archive
  there are two sample archives included in the data folder
  one is test.mbox with a few email messages in it
  run this command in the data folder to import email from test.mbox: sudo imaplet_irmin_build -u imaplet -m archive:test.mbox 
  another archive is maildir, which has a few messages and folder, you need first to unpack the archive with 'tar -xf maildir.tar.gz'
  then run this command in the data folder to import email from maildir: sudo imaplet_irmin_build -u imaplet -m maildir:./Maildir

Run the server as: sudo imaplet. You can set some configuration parameters from the command line, for instance: sudo imaplet -port 143 -ssl false -starttls false -net 0.0.0.0

After you start the IMAP server you can connect with your email client to the server. Since the server runs on your local machine, the email address is going to be 'imaplet@localhost' and the IMAP/SMTP address is 'localhost'.
