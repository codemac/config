#!/usr/bin/perl -Tw

# message-flag
# copyright 2001 Donald B. Marti Jr. <dmarti@zgp.org>
# This script may be freely redistributed under
# Reverse Bastard Copyleft.

# to use this script from mutt, put this in .muttrc
# my_hdr X-Message-Flag: `message-flag`

use strict;

BEGIN: {
  my $random;
  open (RANDOM, "/dev/urandom") 
    or die "Can't open /dev/random for reading: $! ";
  read (RANDOM, $random, 4);
  close RANDOM;
  srand(unpack('L', $random));
}
 
print "Message text blocked: ", random_words(2, censorship()), "\n";

sub censorship {
  ('HATE SPEECH',
   'EXCRETORY SPEECH',
   'DISCLOSURE OF TRADE SECRET(S)',
   'VIOLATION OF US 1201(a)(3)',
   'CHILD PRIVACY VIOLATIONS',
   'HACKING',
   'ADULT LANGUAGE/SITUATIONS',
   'DRUGS/ALCOHOL',
   'HYPERLINK PATENT INFRINGEMENT',
   'TERRORISM/FIREARMS'
  ) }

sub random_words {
  my ($count, @words) = @_;
  my %rank = map { $_ => rand() } @words;
  return join(', ', (sort { $rank{$a} <=> $rank{$b} } @words)
    [0 .. rand $count ]);
}

