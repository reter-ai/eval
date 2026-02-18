"""Thin runner that loads each .eval test file and asserts zero failures."""

import glob
import os

import pytest

from chibi_eval import Eval

eval_dir = os.path.join(os.path.dirname(__file__), "eval")

eval_files = sorted(glob.glob(os.path.join(eval_dir, "*.eval")))

SLOW_EVAL_FILES = {
    "test_pool.eval",
    "test_pool_continuations.eval",
    "test_pool_green_threads.eval",
    "test_async.eval",
    "test_http.eval",
    "test_nb.eval",
}


@pytest.fixture
def fresh_e():
    """Each .eval file needs its own context (shared state causes hangs)."""
    return Eval()


@pytest.mark.parametrize("eval_file", eval_files,
    ids=lambda p: os.path.basename(p))
def test_eval_file(fresh_e, eval_file, capsys):
    if os.path.basename(eval_file) in SLOW_EVAL_FILES:
        pytest.skip("slow test (use --runslow)")
    result = fresh_e.load(eval_file)
    captured = capsys.readouterr().out
    assert result == 0, f"Test failures in {os.path.basename(eval_file)}:\n{captured}"


@pytest.mark.slow
@pytest.mark.parametrize("eval_file",
    [f for f in eval_files if os.path.basename(f) in SLOW_EVAL_FILES],
    ids=lambda p: os.path.basename(p))
def test_eval_file_slow(fresh_e, eval_file, capsys):
    result = fresh_e.load(eval_file)
    captured = capsys.readouterr().out
    assert result == 0, f"Test failures in {os.path.basename(eval_file)}:\n{captured}"
