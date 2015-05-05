#!/usr/bin/perl -w
   use CGI;                             # load CGI routines
   $q = CGI->new(\*STDIN);                        # create new CGI object
   my $user = $q->param('user');
   my $pswd = $q->param('pass');
   my $retype = $q->param('retype');
   print $q->header;                    # create the HTTP header
   print $q->start_html('status'); # start the HTML
   if ($retype ne $pswd) {
      print $q->h4 ("passwords don't match");
   } else {
      my $cmd = "sudo /usr/local/bin/imaplet_create_account -u ".$user.":".$pswd;
      my $ret = `$cmd`;
      print $q->h4 ($ret."\n");
   }
   print $q->end_html;                  # end the HTML
