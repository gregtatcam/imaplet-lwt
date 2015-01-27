imaplet
=======
This is a prototype of IMAP server.

To get started
- install imaplet via opam: opam install imaplet
- configure authentication in "users" file. 
  The file can be found in ~/.opam/xxxx/share/imaplet/users or /usr/local/share/imaplet/users if installed manually. There is one line per configured user, for instance
dovecot:{PLAIN}dovecot1:501:::/Users/dovecot
Where 'dovecot' is the user name, {PLAINS} is the only supported authentication, 'dovecot1' is the password, 501 is the user id, /Users/dovecot is user's home directory.
- configure run-time parameters in imaplet.cf (same dir as above). The important parameters to set:
- irmin_path: path for the git repository
- port: port to listen on, normally 993 for SSL and 143 for plain
- ssl: set to true to support ssl
- starttls: set to true to support STARTTLS
- import email archive
  if you have one mbox archive file then run: imaplet_irmin_build -u username -m archive:archive-path, for instance: imaplet_irmin_build -u 'user' -m archive:/Users/'user'/gmail.mbox. 
  if you have maildir then run: imaplet_irmin_build -u user -m maildir:maildir-path, for instance: imaplet_irmin_build -u user -m maildir:/Users/'user'/Maildir.
  if you have mbox then run: imaplet_irmin_build -u user -m mbox:inbox-path:mailbox-path, for instance: imaplet_irmin_build -u user -m mbox:/var/mail/'user':/Users/'user'/mail.

Run the server as: sudo imaplet. You can set some configuration parameters from the command line: sudo imaplet -port 143 -ssl false -tls false -net 0.0.0.0
