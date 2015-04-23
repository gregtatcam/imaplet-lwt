0.1.10 (2015-03-21):
* packaged the library as top level Imaplet with Server module to instantiate IMAP server and two submodules Parsemail (email message parsing) and Commands (IMAP related modules)
* changed 'imaplet' executable name to 'imaplet_server'
* don't overwrite imaplet.cf and users if exist, save new x.x.x configuration into imaplet.cf.x.x.x and user.x.x.x
* added utility to extract X:Y messages from mbox
* fixed postmark parsing
* added option to import cnt messages from mbox file
* added client utility to test IMAP commands
* fixed parsed email map header and size/lines in MIME parts
* added timer to imaplet_irmin_build to measure time in Irmin functions
* added count/folder filter to imaplet_irmin_build
* fixed index and labels parsing
* added timer to search/fetch
* fixed import count filter
* added option to transform email mime parts
* fixed postmark to_string
* added parse_restore option to generate N unique messages from an archive
* update for ocaml-tls 0.4.0
* store message parts keyed by the message hash and part number 0-post;1-headers;2-content;3+-attachments. All messages are stored under imaplet:user:storage key
* the message is staged/commited under View keyed off imaplet:user
* added user account creation
* added per user configured key/certificate
