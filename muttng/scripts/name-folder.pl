#!/usr/bin/perl -w

# $Id: name-folder.pl,v 1.1 2004/04/18 03:22:05 cmauch Exp $

# Justin Hawkins
# http://hawkins.dropbear.id.au/Software/Mail_Filtering/

# This script takes a single command line argument, which is a 'From'
# header of an email message (minus the actual 'From: '). Such an
# argument can be obtained via the formail program that comes with
# procmail.

# It outputs a folder name which is suitable for use by procmail in
# deciding a mailbox delivery destination.

# More specifically, it outputs a mailbox name that is suitable for
# Courier IMAP to use, with the the surname and christian names
# forming a 2 level hierarchy.

# Charles Mauch
# Specificly broke the hiearchy so it would work with my own scheme.
# If you want the original source, please visit the url listed above.

my ($pre) = "";

my $from = <>;
chomp ($from);

# no leading spaces thanks
$from =~ s/^\s*//;

# OK so lets first try the standard format, name then <email>

if ( $from =~ /(.*)\s+\<(.*)\>/ ) {

  # looks good so far, now unchunk the name
  $name = $1;
  $email = $2;

  # lower case only
  $name =~ tr/A-Z/a-z/;
  # only the letters thanks
  $name =~ tr/a-z,\ //cd;

} elsif ( $from =~ /(.*)\s+\[SMTP:\s*(.*)\]/ ) {

  # bloody microsoft
  $name = $1;
  $email = $2;

  # lower case only
  $name =~ tr/A-Z/a-z/;
  # only the letters thanks
  $name =~ tr/a-z,\ //cd;

} elsif ( $from =~ /(.*)\s+\((.*)\)/ ) {
  $name = $2;
  $email = $1;

  # lower case only
  $name =~ tr/A-Z/a-z/;
  # only the letters thanks
  $name =~ tr/a-z,\ //cd;

} elsif ( $from =~ /([A-Za-z\-_0-9\@\.]+)/ ) {

  # just a plain old email?
  $email = "anon.user";

} else {

  $email = "anon.user";
}


# now take the name string, and try to work out the firstname 
# and lastname of this person

if (! $name ) {
  # if we got no name at all, store it in the unknown folder
  $folder = $pre . "anon.user";
} else {
  # is it just firstname lastname?
  if ($name =~ /^([a-z]+)\s+([a-z]+)$/) {
    $firstname = $1;
    $lastname  = $2;
    # round the other way?
  } elsif ($name =~ /^([a-z]+)\s*,\s*([a-z]+)/ ) {
    $firstname = $2;
    $lastname  = $1;
    # more than one name?
  } elsif ($name =~ /^([a-z]+)\s+.*\s+([a-z]+)$/ ) {
    $firstname = $1;
    $lastname  = $2;
  }

  # now construct the folder name, based on the first and
  # last name we derived (or didn't)
  if (defined $firstname && defined $lastname) {
    $folder = $pre . hash($lastname) . "." . $firstname;
  } else {
    # no firstname and lastname
    $folder = $pre . hash($name);
  }
}

# we use Maildir, so add a trailing slash
#$folder .= "/";

# print the result out, for procmail to use
print "$folder\n";

# subroutine to hash a name like 'hawkins' into something like
# h.ha.hawkins
sub hash {
  my ($surname) = $_[0];

  $surname =~ /^((.).).*/;
  if (defined $1 && defined $2) {
    return ("$surname");
  } else {
    return ($surname);
  }
}
