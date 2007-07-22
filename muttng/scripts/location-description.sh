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
 
print random_words(1, locationdesc()), "\n";

sub locationdesc {
  ('a Wasteland of Burning Tires',
   'hometown of Lee Boyd Malvo',
   'only 2,668 cars stolen last year',
   'we have more than 1000 dams',
   'we made the worlds first nukes',
   'the Ass End of the Galaxy',
   'home of the future Tacoma Spire',
   'now featuring Exploding Volcanos',
   'Earth, Sol, Milky Way, Verse'
  ) }

sub random_words {
  my ($count, @words) = @_;
  my %rank = map { $_ => rand() } @words;
  return join(', ', (sort { $rank{$a} <=> $rank{$b} } @words)
    [0 .. rand $count ]);
}

