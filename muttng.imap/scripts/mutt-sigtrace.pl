#!/usr/bin/perl
#
# mutt-sigtrace v0.4, (c) Darxus@ChaosReigns.com, released under the GPL.
#                     (c) 2005 Charles Mauch (cmauch@gmail.com) - see diffs
#
# Created: 11/26/00 ? by Darxus
# Last Modified: Sat 09 Jul 2005 12:53:25 AM PDT by cmauch
#                     
# http://www.ChaosReigns.com/code/mutt-sigtrace/
# http://www.taclug.org/~cmauch/crypto/sigtrace/
#
# -- [ meta information ] ----------------------------------------------------
#
# To use this script, modify your .muttrc so that the following two lines:
#
# set pgp_decode_command="gpg %?p? --passphrase-fd 0? --no-verbose --batch --output - %f"
# set pgp_verify_command="gpg --no-verbose  --batch --output - --verify %s %f"
#
# looks like this:
#
# set pgp_decode_command="~/.mutt/mutt-sigtrace.pl 3276D24897289852 gpg ....
# set pgp_verify_command="~/.mutt/mutt-sigtrace.pl 3276D24897289852 gpg ....
#
# where .... is the rest of the command listed above.
#
# You might also want to scroll down to the bottom of this script and modify
# the $path variable to fit your needs.
#
# Now when mutt sees a pgp signature, it will check the message validity and
# immediately following the signature output, you'll see sigtrace's output.
#
# Neat stuff.
#
# -- [ changelog ] -----------------------------------------------------------
# 
#  v0.3 Nov 21 21:29 EDT Initial Creation (Darxus)
#  v0.4 Jul 05 17:28 PST Updated to convert between short key id's which
#                        mutt outputs and long key id's which sigtrace uses.
# 

$linenum = 0;

$myid = shift @ARGV;

open (CMD, "@ARGV 2>&1 >/dev/null|");

while ($line=<CMD>)
{
  print STDERR "$line";
  if ($line =~ "key ID")
  {
    $shortid = (reverse(split(' ',$line)))[0];
    $convert=`gpg --list-keys --with-colons $shortid | grep -m1 "pub" 2>&1`;
    ($type,undef,undef,undef,$id,undef,undef,undef,undef,undef,undef,undef,undef,undef) = split(':',$convert);

  }
}

if ($id)
{
  $path = `~/.mutt/sigtrace.pl $myid $id`;
  if ($path)
  {
    print STDERR "$path\n"
  } else {
    print STDERR "No PGP Key Key path found, PGP Key Tracing Failed!.\n";
  }
} else {
  print STDERR "No destination KeyID found, PGP Key Tracing impossible\n";
}
