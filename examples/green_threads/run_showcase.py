"""Green threads & serialized continuations -- pure Eval showcase.

The Eval program does everything: scheduling, freezing threads,
serializing continuations, and resuming them. This loader just
loads and executes the file.

Usage:
    python examples/green_threads/run_showcase.py
"""

import os
from chibi_eval import Eval

e = Eval()
e.load(os.path.join(os.path.dirname(os.path.abspath(__file__)), "showcase.eval"))
