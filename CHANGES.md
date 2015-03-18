0.1.10 (2015-03-21):
* packaged the library as top level Imaplet with Server module to instantiate IMAP server and two submodules Parsemail (email message parsing) and Commands (IMAP related modules)
* changed 'imaplet' executable name to 'imaplet_server'
* don't overwrite imaplet.cf and users if exist, save new x.x.x configuration into imaplet.cf.x.x.x and user.x.x.x
* added utility to extract X:Y messages from mbox
* fixed postmark parsing
* added option to import cnt messages from mbox file
* added client utility to test IMAP commands
