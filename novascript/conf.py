from pathlib import Path
import os
import sys

ROOT = Path(__file__).resolve().parent.parent
LIBS_PATH = ROOT / "libs"

sys.path.insert(0, str(ROOT))
sys.path.insert(0, str(LIBS_PATH))
sys.path.insert(0, os.getcwd())