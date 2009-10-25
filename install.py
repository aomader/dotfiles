#!/usr/bin/env python
#-*- coding: utf-8 -*-


"""
install.py  --  a dotfiles related install script

AUTHORS
  Oliver Mader <dotb52@gmail.com>

Copyright (C) 2009 Oliver Mader

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.
"""


import optparse
import os
import shutil
import sys


def main():
	def exclude_list(option, opt_str, value, parser):
		parser.values.exclude = parser.values.exclude + value.split(',')
	
	parser = optparse.OptionParser(usage='Usage: %prog [OPTIONS]', description='Install all files and directories as dotfiles.')
	parser.add_option('-b', '--backup', action='store_true', dest='backup', default=False, help='Backup old files')
	parser.add_option('-c', '--copy', action='store_true', dest='copy', default=False, help='Copy files instead of hard-linking them')
	parser.add_option('-e', '--exclude', action='callback', callback=exclude_list, type='string', dest='exclude', default=['README', 'install.py', '.git'], help='Comma seperated list of files to exclude', metavar='FILES')
	parser.add_option('-f', '--force', action='store_true', dest='force', default=False, help='Overwrite without confirmation')
	parser.add_option('-p', '--prefix', action='store', dest='prefix', default=os.path.expanduser('~'), help='Install location [default: %default]', metavar='PATH')
	
	(options, args) = parser.parse_args()
	
	def traverse(path):
		for item in os.listdir(path):
			source = os.path.join(path, item)
			relative = os.path.relpath(source)
			
			if relative.startswith('.'):
				destination = os.path.join(options.prefix, relative)
			else:
				destination = os.path.join(options.prefix, '.' + relative)
			
			if relative in options.exclude:
				continue
			
			if os.path.isfile(source):
				install(source, destination)
			
			if os.path.isdir(source):
				if not os.path.exists(destination):
					os.mkdir(destination)
				
				traverse(source)
	
	def install(source, destination):
		if os.path.exists(destination):
			if options.backup:
				shutil.move(destination, destination + '.bkp')
			elif options.force:
				os.remove(destination)
			else:
				answer = raw_input(destination + ': File already exists, overwrite? [y/N]')
				
				if answer.lower() in ('y', 'yes'):
					os.remove(destination)
				else:
					return
		
		if options.copy:
			shutil.copy(source, destination)
		else:
			os.link(source, destination)
	
	traverse(os.getcwd())


if __name__ == '__main__':
	try:
		main()
	except (KeyboardInterrupt, SystemExit):
		sys.exit(1)

