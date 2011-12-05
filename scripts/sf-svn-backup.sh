#! /bin/bash

# Create a backup (svn dump) for a project's subversion repository hosted with
# SourceForge

# Copyright (C) 2008 by Guy Rutenberg

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the
# Free Software Foundation, Inc.,
# 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


PROG=sf-svn-backup
SYNOPSIS="${PROG} [options] PROJECT"
VERSION=0.1

print_usage () {
    echo "Usage: ${SYNOPSIS}"
    cat << EOF
Backup a SourceForge hosted project's SVN repository. PROJECT should be the
project's unix name.
EOF
}
print_help ()  {
    print_usage
    cat << EOF

Options:
    -h, --help          show this help message and exit
    --version           show program's version number and exit

Example:
    sf-svn-backup openyahtzee > dumpfile
EOF
}

print_version () {
    echo "$PROG $VERSION"
    echo "Copyright (C) 2008 Guy Rutenberg <http://www.guyrutenberg.com>"
}

# sends output to $1
generate_hash () {
    find -type f -print0 | sort -z | xargs -0 "$CHECKSUM" > "$1"
}
# default values (where possible)
VERBOSE=0
CHECKSUM=md5sum
TEMP=`getopt -o ho:v --long help,version,verbose,output:,checksum: \
    -n '${PROG}' -- "$@"`
if [ $? != 0 ] ; then
    print_usage
    exit 1
fi

eval set -- "$TEMP"

while true ; do
    case "$1" in
        -h|--help) print_help; exit ;;
        --version) print_version; exit ;;
        --) shift; break;;
    esac
done

# no project name
if [ -z "$1" ]; then
    echo "$PROG: missing PROJECT"
    echo ""
    print_usage
    exit 1
fi

PROJECT="$1"

TEMPDIR=`mktemp -d`

rsync -aq $PROJECT.svn.sourceforge.net::svn/$PROJECT/* $TEMPDIR

if [ "$?" -ne "0" ]; then
	echo "rsync failed" > /dev/stderr
	echo "Cleaning up" > /dev/stderr
        rm -rf $TEMPDIR
	exit 1
fi

svnadmin dump $TEMPDIR

if [ "$?" -ne "0" ]; then
	echo "svnadmin dump failed" > /dev/stderr
	echo "Cleaning up" > /dev/stderr
        rm -rf $TEMPDIR
	exit 1
fi

rm -rf $TEMPDIR

# vim:sw=4:softtabstop=4:expandtab
