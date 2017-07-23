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
import glob
import platform
import time

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

INSTALL_PREFIX = os.path.expanduser('~/.herp')
#import tempfile#temp_dir = tempfile.mkdtemp()
SOURCE_ROOT = os.path.join(os.path.expanduser('~/.herp'), 'source')
ARCHIVE_NAME = 'emacs.tar.xz'
EMACS_URL = 'https://mirrors.ocf.berkeley.edu/gnu/emacs/emacs-25.2.tar.xz'

PACKAGES = {
    'ipython': {'debian': 'ipython'},
    'pep8': {'debian': 'pep8'},
    'autopep8': {'debian': 'python-autopep8'},
    'virtualenv': {'debian': 'python-virtualenv'},
}


def extract_tarball(tarball_path, dest_dir):
    logger.info('Extracting')
    makedirs(dest_dir)
    os.chdir(dest_dir)
    with tarfile.open(tarball_path) as f:
        f.extractall('.')

def get_version_string(archive_path_fragment):
    m = re.match(r'.*(\d\d\.\d).*', archive_path_fragment)
    version_string, = m.groups()
    return version_string

def makedirs(path):
    if not os.path.exists(path):
        logger.debug('%s did not exist. Creating.' % path)
        os.makedirs(path)


def get_downloaded_emacs_archive_path(dest_dir, emacs_url):
    logger.info('Downloading emacs source archive')
    logger.debug('Destination: %s' % dest_dir)
    logger.debug('URL: %s' % emacs_url)
    dest_dir = os.path.abspath(dest_dir)
    makedirs(dest_dir)
    archive_name = os.path.split(emacs_url)[-1]  # good enough
    dest_filename = os.path.join(dest_dir, archive_name)
    if not os.path.exists(dest_filename):
        opener = request.URLopener()
        try:
            opener.retrieve(emacs_url, dest_filename)
        except Exception as e:
            logger.error('Download failed. Deleting archive %s' % dest_filename)
            os.remove(dest_filename)
            raise e
    else:
        logger.info('Emacs source archive there. Using that.')
    return dest_filename


def system(command):
    logger.debug('Executing command: %s' % ' '.join(command))
    p = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    pout, perr = p.communicate()
    if p.returncode:
        raise Exception(perr)
    return p.returncode, pout, perr


def configure(path):
    logger.info('Configuring')
    os.chdir(path)
    system(['./configure', '--prefix', INSTALL_PREFIX, '--without-x'])

def make(path, target=None):
    cmd = []
    cmd.append('make')
    if target is not None:
        cmd.append(target)
    logger.info('make %s' % ('' if target is None else target))
    os.chdir(path)
    system(cmd)

"""
~/git && cd ~/git/
git -c http.sslVerify=false clone https://github.com/stnbu/devenvbootstrap.git
cd devenvbootstrap/

python3 do_emacs.py
. ~/.bashrc
emacs --daemon
"""

def rotate_away_path(path):
    new_path = '%s.%s' % (path, time.time())
    logger.warn('%s already exists. moving to %s' % (path, new_path))
    os.rename(path, new_path)

def get_extracted_source_root(base_dir, tarball_path):
    base_dir = os.path.abspath(base_dir)
    makedirs(base_dir)
    emacs_source_dir, = glob.glob(os.path.join(base_dir, 'emacs', 'emacs*'))
    extract_tarball(tarball_path, emacs_source_dir)
    return emacs_source_dir



def link_dot_emacs(path):
    dot_emacs_path = os.path.expanduser('~/.emacs')
    if os.path.exists(dot_emacs_path) and os.path.islink(dot_emacs_path):
        rotate_away_path(dot_emacs_path)
    os.symlink('git/devenvbootstrap/dot.emacs', dot_emacs_path)

def update_dot_bashrc():
    dot_bashrc_path = os.path.expanduser('~/.bashrc')
    if not os.path.exists(dot_bashrc_path):
        with open(dot_bashrc_path, 'w') as f:
            f.close()
    if 'devenvbootstrap' not in open(dot_bashrc_path).read():
        with open(dot_bashrc_path, 'a') as f:
            f.write('\n. ~/git/devenvbootstrap/dot.bashrc\n')

def get_platform():
    name, _, _ = platform.dist()
    name = name.lower()
    if name == 'ubuntu':
        name = 'debian'
    return name


def install_system_packages(package_aliases):
    logger.info('Installing system packages.')
    logger.debug('Packages to install: %s' % package_aliases)
    platform_name = get_platform()
    to_install = [v[platform_name] for k, v in PACKAGES.items() if k in package_aliases]
    command = 'apt-get -y -q install'.split()
    command.extend(to_install)
    system(command)


def install_global_python_packages(package_names):
    logger.info('Installing python packages.')
    logger.debug('Packages to install: %s' % package_names)
    command = 'pip install'.split()
    command.extend(package_names)
    system(command)

def clone_repo(uri, dest_dir):
    logger.info('Cloning repository.')
    dest_dir = os.path.abspath(dest_dir)
    if os.path.exists(dest_dir):
        rotate_away_path(dest_dir)
    makedirs(dest_dir)
    command = 'git -c http.sslVerify=false clone'.split()
    command.append(uri)
    command.append(dest_dir)
    system(command)

def install_authorized_key(key):
    logger.info('Installing ssh key.')
    authorized_keys_path = os.path.expanduser('~/.ssh/authorized_keys')
    if key not in open(authorized_keys_path, 'r').read():
        with open(authorized_keys_path, 'a') as f:
            f.write('\n'+key)
    
if __name__ == '__main__':

    install_system_packages(['ipython', 'pep8', 'autopep8', 'virtualenv'])
    install_global_python_packages(['remote-pdb'])

    clone_repo('https://github.com/stnbu/devenvbootstrap.git', os.path.expanduser('~/git/devenvbootstrap'))
    
    key = open(os.path.expanduser('~/git/devenvbootstrap/id_rsa.pub'), 'r').read().strip()
    install_authorized_key(key)
    

    tarball_path = get_downloaded_emacs_archive_path(SOURCE_ROOT, EMACS_URL)
    emacs_source_dir = get_extracted_source_root(SOURCE_ROOT, tarball_path)

    configure(emacs_source_dir)
    make(emacs_source_dir)
    make(emacs_source_dir, 'install')

    link_dot_emacs('/root/git/devenvbootstrap/dot.emacs')
    update_dot_bashrc()

