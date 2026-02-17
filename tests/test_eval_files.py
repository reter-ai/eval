"""Thin runner that loads each .eval test file and asserts zero failures."""

import glob
import os

import pytest

from chibi_eval import Eval

eval_dir = os.path.join(os.path.dirname(__file__), "eval")

eval_files = sorted(glob.glob(os.path.join(eval_dir, "*.eval")))


@pytest.fixture
def e():
    return Eval()


@pytest.mark.parametrize("eval_file", eval_files,
    ids=lambda p: os.path.basename(p))
def test_eval_file(e, eval_file, capsys):
    result = e.load(eval_file)
    captured = capsys.readouterr().out
    assert result == 0, f"Test failures in {os.path.basename(eval_file)}:\n{captured}"
