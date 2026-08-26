import os,sys

ROOT = os.path.dirname(os.path.realpath(__file__))
LIBS_PATH = os.path.join(ROOT, "libs")

sys.path.insert(0, ROOT)
sys.path.insert(0, LIBS_PATH)
sys.path.insert(0, os.getcwd())
