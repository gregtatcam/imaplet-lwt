iimaplet
=======
This is a prototype of IMAP server.
Grammar for all base RFC3501rev4 commands is supported, but not all functionality.  
SSL and STARTTLS are supported. The plain authentication is supported via "users" file which is included with the source. 
Some configuraiton options are provided via imaplet.cf. Irminsule is the only supported storage type. There is conversion for Irminsule store from mbox - imaplet_irmin_build. Run it as:
imaplet_irmin_build.native -u user -i inbox-path -m mailboxes-path, where the user is the email account. imaplet_irmin_read is a simple interractive UI to access Irminsule store. 
Dependencies are:
core
batteries
lwt
sexplib
str
cstruct
irmin
tls
email_message.109.42.alpha1,

Use "opam install" to install dependencies.Tls and it's dependency libraries: nocrypto, asn1-combinators, x509 have to be installed manually off the github tip.
To build all components and utilities run "make configure/build/install". Depending on your computer security settings you may have to run executables as sudo: sudo imaplet.

