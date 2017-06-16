"""This is reckless and untested. UAYOR

Mike Burr <mburr@unintuitive.org>
"""


import sys

if sys.version_info.major != 3:
    raise Exception('Requires Python 3')

import os
import re
from urllib import request
import subprocess
import tarfile
import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

install_prefix = os.path.expanduser('~/.herp')
#import tempfile#temp_dir = tempfile.mkdtemp()
source_root = os.path.join(os.path.expanduser('~/.herp'), 'source')
archive_name = 'emacs.tar.xz'
emacs_url = 'https://mirrors.ocf.berkeley.edu/gnu/emacs/emacs-25.2.tar.xz'


def extract_tarball(tarball_path, dst):
    logger.info('Extracting')
    os.chdir(dst)
    with tarfile.open(tarball_path) as f:
        f.extractall('.')

def get_version_string(archive_path_fragment):
    m = re.match(r'.*(\d\d\.\d).*', archive_path_fragment)
    version_string, = m.groups()
    return version_string


def get_downloaded_emacs_archive_path(dest_path):
    logger.info('Downloading')
    dest_path = os.path.abspath(dest_path)
    if not os.path.exists(os.path.join(dest_path, archive_name)):
        opener = request.URLopener()
        opener.retrieve(emacs_url, os.path.join(dest_path, archive_name))
    return dest_path, archive_name


def system(command):
    #print(' '.join(command))
    p = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    pout, perr = p.communicate()
    if p.returncode:
        raise Exception(perr)
    return p.returncode, pout, perr


def configure(path):
    logger.info('Configuring')
    os.chdir(path)
    system(['./configure', '--prefix', install_prefix, '--without-x'])

def make(path, target=None):
    cmd = []
    cmd.append('make')
    if target is not None:
        cmd.append(target)
    logger.info('make {0}'.format('' if target is None else target))
    os.chdir(path)
    system(cmd)


if __name__ == '__main__':

    if not os.path.exists(source_root):
        os.makedirs(source_root)
    tarball_dir, tarball_name = get_downloaded_emacs_archive_path(source_root)
    tarball_path = os.path.join(tarball_dir, tarball_name)
    emacs_source_dir = os.path.join(source_root, 'emacs')

    extract_tarball(tarball_path, emacs_source_dir)
    this_version_source = os.path.join(emacs_source_dir, 'emacs-{0}'.format(get_version_string(emacs_url)))

    configure(this_version_source)
    make(this_version_source)
    make(this_version_source, 'install')
