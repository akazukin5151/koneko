import os
from pathlib import Path

from setuptools import setup

# The directory containing this file
HERE = Path(__file__).parent
README = (HERE / 'README.md').read_text()

# Create the cache dir
# The pythonic way is so unnecessarily complicated for two lines of shell...
os.system('mkdir -p ~/.local/share/koneko')
os.system('cp -r ./pics/ ~/.local/share/koneko/')
os.system('cp example_config.ini ~/.local/share/koneko/')

setup(
    name='koneko',
    version='0.10.0',
    description='Browse pixiv in the terminal',
    long_description=README,
    long_description_content_type='text/markdown',
    url='https://github.com/twenty5151/koneko',
    author='twenty5151',
    license='GPLv3',
    classifiers=[
        'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
        'Programming Language :: Python :: 3',
        'Operating System :: POSIX :: Linux',
        'Operating System :: MacOS',
        'Environment :: Console'
    ],
    packages=['koneko'],
    include_package_data=True,
    install_requires=[
        'PixivPy~=3.5',
        'pixcat~=0.1',
        'docopt~=0.6',
        'blessed~=1.17',
        'pick~=0.6',
        'funcy~=1.14',
        'returns~=0.14',
        'placeholder~=1.1',
    ],
    tests_require=['pytest~=5.4'],
    entry_points={
        'console_scripts': [
            'koneko=koneko.main:main',
            'lscat=koneko.lscat_app:main'
        ]
    },
)
