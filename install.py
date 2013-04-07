#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
install.py  --  a dotfiles related install script

AUTHORS
  Oliver Mader <b52@reaktor42.de>

Copyright (C) 2009-2013 Oliver Mader

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
"""

import optparse
import os
import shutil
import subprocess
import sys


def main():
    def exclude_list(option, opt_str, value, parser):
        parser.values.exclude = parser.values.exclude + value.split(',')

    parser = optparse.OptionParser(
        usage='Usage: %prog [OPTIONS] [FILES/DIRECTORIES..]',
        description='A simple yet powerful dotfiles install script')
    parser.add_option('-b', '--backup', action='store_true', dest='backup',
                      default=False, help='Backup old files')
    parser.add_option('-c', '--copy', action='store_true', dest='copy',
                      default=False,
                      help='Copy files instead of hard-linking them')
    parser.add_option('-s', '--soft', action='store_true', dest='soft',
                      default=False,
                      help='Use soft-links instead of hard-links')
    parser.add_option('-f', '--force', action='store_true', dest='force',
                      default=False, help='Overwrite without confirmation')
    parser.add_option('-a', '--ask', action='store_true', dest='ask',
                      default=False, help='Ask for every file to install')
    parser.add_option('-e', '--exclude', action='callback',
                      callback=exclude_list, type='string', dest='exclude',
                      default=['README', 'install.py', '.git', '.svn', '.hg',
                               '_darcs'],
                      help='Comma seperated list of files/directories to exclude',
        metavar='FILES')
    parser.add_option('-S', '--submodules', action='store_true',
                      dest='submodules', default=False,
                      help='Firsst of all init and update git submodules')
    parser.add_option('-p', '--prefix', action='store', dest='prefix',
        default=os.path.expanduser('~'),
        help='Install location [default: %default]', metavar='PATH')

    (options, args) = parser.parse_args()

    to_install = [os.path.join(os.getcwd(), x) for x in args]
    excludes = [os.path.join(os.getcwd(), x) for x in options.exclude]

    if not to_install and not options.submodules:
        parser.print_help()
        sys.exit(1)

    def traverse(path):
        for item in os.listdir(path):
            source = os.path.join(path, item)
            relative = os.path.relpath(source)

            if relative.startswith('.'):
                destination = os.path.join(options.prefix, relative)
            else:
                destination = os.path.join(options.prefix, '.' + relative)

            if source in excludes:
                continue

            starts = tuple(x + '/' for x in to_install if os.path.isdir(x))
            if not source.startswith(starts) and not source in to_install:
                continue

            if os.path.isfile(source):
                install(source, destination)
            elif os.path.isdir(source):
                if not os.path.exists(destination):
                    os.mkdir(destination)
                    shutil.copymode(source, destination)
                traverse(source)
            else:
                print >> sys.stderr, '%s: Don\'t know how to handle that' % \
                                     relative

    def install(source, destination):
        if options.ask and not input('%s: Install this file? [y/N]' %
            destination).lower() in ('y', 'yes'):
            return

        if os.path.lexists(destination):
            if options.backup:
                shutil.move(destination, destination + '.bkp')
            elif options.force:
                os.remove(destination)
            else:
                if input('%s: File already exists, overwrite? [y/N]' %
                    destination).lower() in ('y', 'yes'):
                    os.remove(destination)
                else:
                    return

        if options.copy:
            shutil.copy(source, destination)
            typ = 'copy'
        elif options.soft:
            os.symlink(source, destination)
            typ = 'soft-link'
        else:
            os.link(source, destination)
            typ = 'hard-link'

        print('%s: Installed (%s)' % (destination, typ))

    if options.submodules:
        subprocess.call(('git submodule foreach \'git submodule init && '
                         'git submodule update && '
                         'git pull origin master\''),
                        shell=True)

    traverse(os.getcwd())


if __name__ == '__main__':
    try:
        main()
    except (KeyboardInterrupt, SystemExit):
        sys.exit(1)

