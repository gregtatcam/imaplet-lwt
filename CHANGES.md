### 0.1.17
* replaced camlp4 with ppx_sexp_conv
* linked with the latest packages

### 0.1.16

* git-like modules bundled into imaplet.commands library
* sync up with Irmin 0.10.0
* integrated gitl as another storage type
* handle net/disk io in async way in append

### 0.1.15

* add relay restrictions - required authenticaiton and list of users/domains
* fix opam lint errors
* add dedup to calculate duplicate attachments in mbox
* add replication and routing via 'master'
* add multiple recipient support
* reduce UID dependency - reference message metadata via message hash, hash to uid map is stored in the index
* add custom content merge
* move hash references to the immutable storage location
* added basic smtp client library and standalone
* add per/user store configuration to "users", i.e. ability to have different store for different users
* move message parsing from append to email parser, user String.concat for efficiency, return Lwt type, return string list in fetch for efficiency
* write fetch response as async thread
* added option of storing in irmin unparsed as/is message - basically maildir in irmin FS (irmin storage + single_store-false + maildir_parse-false)
* build the fetch message as vector to reduce string concat, write the vector if IMAP compression is not enabled

### 0.1.14

* improve network read error handling
* added new storage type - git working directory, mirrors git as is, all updates overwrite the content
* allow compression/encryption in any combination
* cache uidlist in maildir
* add message MIME parts parsing in maildir storage
* add attachments compression option
* add option to store irmin/workdir MIME parts as single store for dedup or in one blob for fast access
* support for tls 0.5.0
* add hybrid irmin/workdir storage - messages are stored in workdir on FS, metadata is stored in Git
* add multiple domain support (virtual user account folder for user@domain is domain/user)
* add COMPRESS=DEFLATE capability (rfc 4978)
* add name server(s)/domain(s) configuration for dns resolution
* supoort for tls 0.6.0

### 0.1.13

* added domain configuration
* smtp crash fix - Socket_utils.server was recursing on exception, should just return

### 0.1.12

* added inactivity timeout
* added configuration utility
* added SMTP server
* added DNS MX lookup for SMTP relay
* added authentication required to encrypt the private key with the user's password
* updated Ubuntu deployment scripts
* derive pub key from cert; priv key encrypted with user password, not available on smtp relay
* multiple ports connection option for SMTP server
* added simple library and utility for stun protocol to obtain mapped address of the server
* added Received: header to relayed email; includes private and mapped address; could be used for direct communication between peers

### 0.1.11

* added maintenance to send notifications to IDLE'ed clients
* fixed untagged search response - clients expect 'SEARCH' keyword before the list of matched sequence
* added connection id to logging
* fix to STATUS to support mobile devices
* improved client connections tracking, fixes for mobile devices

### 0.1.10 (2015-03-21):

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
* added smtp server (replaced lmtp with smtp)
* added option to iterate messages in reverse order
* added untagges response in NOOP
* added tracking of selected mailboxes for the same account from different clients for IDLE command.
* added deploy instructions for Ubuntu
