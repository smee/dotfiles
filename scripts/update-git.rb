#!/usr/bin/env ruby

require 'rubygems'
require 'colored'
require 'fileutils'

old_pwd = `pwd`
default_remote = 'origin'
$highlight = '# '.magenta_on_black

def do_pull( repository = nil )
  if repository.nil?
    target = 'Pulling: '.cyan.bold + `git remote -v | grep fetch`.white_on_black.bold
    puts target.sub( "\t" , ' -> ' ).sub( '(fetch)' , '' ).strip
    puts $highlight + `git pull` + "\n"
  else
    target = 'Pulling: '.cyan.bold + `git remote -v | grep #{repository} | grep fetch`.white_on_black.bold
    puts target.sub( "\t" , ' -> ' ).sub( '(fetch)' , '' ).strip
    puts $highlight + `git pull #{repository}` + "\n"
  end
end

ARGV.each do|path|
  puts $highlight + "Checking given path.,,".white_on_black
  if File.directory?( path )
    puts $highlight + '[ SUCCESS ]'.green_on_black + ' ' + path.bold.white_on_black + "\n\n"
    FileUtils.cd path
    `find . -maxdepth 1 -mindepth 1 -type d`.each do |repository|
      repository = path.to_s.strip + repository.to_s.strip
      repository = repository.gsub( /.*\// , '' ).strip
      puts $highlight + "Checking: #{repository}".white_on_black
      FileUtils.cd repository
      remotes = `git remote`
      if remotes.include? default_remote
        do_pull default_remote
      else
        case remotes.to_a.count
        when 1
          do_pull
        when 0
          puts '[ FAIL ] '.red_on_black.bold + "Unable to locate any remote branches.\n".white_on_black.bold
        else
          puts "Default Remote \"#{default_remote}\" not detected. Too many alternative remotes to pull automatically.\n".white_on_black.bold
        end
      end
      FileUtils.cd old_pwd.strip
    end
    puts $highlight + "Do work son!\n".green.bold
  else
    puts '[ FAIL ] '.red_on_black.bold + "You must include a path containing repositories! #{path}".white_on_black.bold
  end
end