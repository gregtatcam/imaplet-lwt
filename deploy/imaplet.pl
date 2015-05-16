#!/usr/bin/perl -w
use CGI;
$q = CGI->new;
   
  print 
  $q->header,
  $q->start_html('IMAPlet account management'),
  $q->h4('Create IMAPlet account'),
  $q->start_form,
  $q->table({-border=>undefined},
    $q->Tr({-align=>'CENTER',-valign=>'TOP'},
    [
      $q->td(['User:', $q->textfield('name')]),
      $q->td(['Password', $q->password_field('pswd')]),
      $q->td(['Retype Password', $q->password_field('retype')])
    ]
    )
  ),$q->br,
  $q->submit,
  $q->end_form,
  $q->hr,"\n";

  if (scalar $q->param > 0) {
    if ($q->param('name') eq "" || $q->param('pswd') eq "" || $q->param('retype') eq "") {
      print $q->h4 ("enter user/password/retype password\n");
    } elsif ($q->param('pswd') ne $q->param('retype')) {
      print $q->h4 ("passwords don't match\n");
    } else {
      my $cmd = "sudo /usr/local/bin/imaplet_create_account -u ".$q->param('name').":".$q->param('pswd');
      print $q->h4 (`$cmd`."\n");
    }
  }
  print $q->end_html;
