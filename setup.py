import os
from pathlib import Path

from setuptools import setup
from setuptools.command.install import install

# The directory containing this file
HERE = Path(__file__).parent
README = (HERE / 'README.md').read_text()

with open('requirements.txt') as f:
    required = f.read().splitlines()

class InstallCommand(install):
    def run(self):
        install.run(self)
        os.system('mkdir -p ~/.local/share/koneko')
        os.system('cp -r ./pics/ ~/.local/share/koneko/')
        os.system('cp example_config.ini ~/.local/share/koneko/')
        os.system('cp pixiv-url.desktop ~/.local/share/koneko/')


setup(
    name='koneko',
    version='0.13.0',
    description='Browse pixiv in the terminal',
    long_description=README,
    long_description_content_type='text/markdown',
    url='https://github.com/akazukin5151/koneko',
    project_urls={
        'Source Code': 'https://github.com/akazukin5151/koneko',
        'Documentation': 'https://koneko.readthedocs.io/en/latest/',
    },
    author='akazukin5151',
    license='GPLv3',
    classifiers=[
        'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
        'Programming Language :: Python :: 3',
        'Operating System :: POSIX :: Linux',
        'Operating System :: MacOS',
        'Environment :: Console'
    ],
    packages=['koneko', 'koneko.url_login'],
    include_package_data=True,
    install_requires=required,
    tests_require=['pytest>=6.2,<8.0'],
    # Ueberzug's main repo no longer exists, use
    # https://github.com/WhiteBlackGoose/ueberzug-latest/
    # follow their installation instructions, or use your package manager
    #extras_require={'ueberzug': ['ueberzug~=18.1']},
    entry_points={
        'console_scripts': [
            'koneko=koneko.__main__:_main',
            'lscat=koneko.lscat_app:main',
            'koneko-url-login=koneko.url_login.main:main'
        ]
    },
    cmdclass={
        'install' : InstallCommand,
    },
)
