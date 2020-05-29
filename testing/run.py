"""Tries to run koneko in shell for github workflow. Not to be used with pytest"""
import os

os.system("pip install .")
os.chdir('~')
try:
    os.system("koneko")
except EOFError:
    pass
