"""Tries to run koneko in shell for github workflow.
To make sure the app doesn't crash on start
Not to be used with pytest.
For github workflows, the app will crash because it can't read stdin
You are presumably running this on a terminal that can read stdin, so it will
hang forever. So this is only for github actions, not for you
"""

import os


def test_launch():
    curdir = os.getcwd()
    os.system('cd ~')
    try:
        os.system('koneko')
    except EOFError:
        pass
    os.chdir(curdir)


cmds = (
    'pip install .',
    'python setup.py install',
    'python setup.py develop',
    'python -m koneko.__main__'
)

for cmd in cmds:
    print(f'========={cmd}=========')
    os.system(cmd)
    test_launch()
    print('=========pip uninstall koneko -y=========')
    os.system('pip uninstall koneko -y')
