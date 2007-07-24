#!/usr/bin/env ruby
# Jeff Mickey's irbrc
# Lot's of fun stuff !

# get some gem action!
require 'rubygems'
# Class completion
require 'irb/completion'
# Save what I run
require 'irb/ext/save-history'
# Give me some pp love
require 'pp'
# colorize output!
require 'wirble'
Wirble.init
Wirble.colorize

# NOT WORKING.  Haven't figured out why yet.
# fix bad method names!
#require 'guessmethod'
#class Object
#  include GuessMethod
#end

IRB.conf[:USE_READLINE] = true							# readline doesn't suck
IRB.conf[:EVAL_HISTORY] = 1000							# Enable the _ variable
IRB.conf[:SAVE_HISTORY] = 1000							# I have diskspace for the history
IRB.conf[:HISTORY_FILE] = ENV['HOME'] + '/.irbhistory'  # We gotsta store it somewhere!

# Add 'ri whatever' to the irb command line

def ri(arg)
  puts `ri #{arg}`
end

class Module
  def ri(meth=nil)
    if meth
	  if instance_methods(false).include? meth.to_s
		puts `ri #{self}##{meth}`
	  else
		super
	  end
	else
	  puts `ri #{self}`
	end
  end
end

begin # ANSI codes
  ANSI_BLACK    = "\033[0;30m"
  ANSI_GRAY     = "\033[1;30m"
  ANSI_LGRAY    = "\033[0;37m"
  ANSI_WHITE    =  "\033[1;37m"
  ANSI_RED      ="\033[0;31m"
  ANSI_LRED     = "\033[1;31m"
  ANSI_GREEN    = "\033[0;32m"
  ANSI_LGREEN   = "\033[1;32m"
  ANSI_BROWN    = "\033[0;33m"
  ANSI_YELLOW   = "\033[1;33m"
  ANSI_BLUE     = "\033[0;34m"
  ANSI_LBLUE    = "\033[1;34m"
  ANSI_PURPLE   = "\033[0;35m"
  ANSI_LPURPLE  = "\033[1;35m"
  ANSI_CYAN     = "\033[0;36m"
  ANSI_LCYAN    = "\033[1;36m"
  
  ANSI_BACKBLACK  = "\033[40m"
  ANSI_BACKRED    = "\033[41m"
  ANSI_BACKGREEN  = "\033[42m"
  ANSI_BACKYELLOW = "\033[43m"
  ANSI_BACKBLUE   = "\033[44m"
  ANSI_BACKPURPLE = "\033[45m"
  ANSI_BACKCYAN   = "\033[46m"
  ANSI_BACKGRAY   = "\033[47m"
  
  ANSI_RESET      = "\033[0m"
  ANSI_BOLD       = "\033[1m"
  ANSI_UNDERSCORE = "\033[4m"
  ANSI_BLINK      = "\033[5m"
  ANSI_REVERSE    = "\033[7m"
  ANSI_CONCEALED  = "\033[8m"
  
  XTERM_SET_TITLE   = "\033]2;"
  XTERM_END         = "\007"
  ITERM_SET_TAB     = "\033]1;"
  ITERM_END         = "\007"
  SCREEN_SET_STATUS = "\033]0;"
  SCREEN_END        = "\007"
end

begin # My custom prompt!
  if ENV['RAILS_ENV']
	name = %{#{ENV['RAILS_ENV']} rails}
	colors = ANSI_BACKPURPLE + ANSI_YELLOW
  else
	name = 'ruby'
	colors = ANSI_BACKBLUE + ANSI_YELLOW
  end

  if IRB and IRB.conf[:PROMPT]
	IRB.conf[:PROMPT][:CODEMAC] = {
	  :PROMPT_I => "#{colors}#{name}: %m #{ANSI_RESET}\n" + ">> ", #normal
	  :PROMPT_S => "%l> ", #string concat
	  :PROMPT_C => " > ", # code completion
	  :PROMPT_N => " > ", # code continuation?
	  :RETURN   => "#{ANSI_BOLD}# => %s  #{ANSI_RESET}\n\n", # return value
	  :AUTO_INDENT => true
	}
	IRB.conf[:PROMPT_MODE] = :CODEMAC
  end
end
