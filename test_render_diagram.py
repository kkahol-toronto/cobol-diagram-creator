import os
import tempfile
import shutil
import pytest

from render_diagram import convert_svg_to_png

SVG_PATH = 'Kanav_diagram/EXWWB909/EXWWB909.svg'

@pytest.fixture(scope="module")
def tmp_output_dir():
    tmpdir = tempfile.mkdtemp()
    yield tmpdir
    shutil.rmtree(tmpdir)

def test_png_is_generated(tmp_output_dir):
    output_png = os.path.join(tmp_output_dir, "EXWWB909.png")
    result_path = convert_svg_to_png(SVG_PATH, output_png)
    assert os.path.isfile(result_path), "PNG file was not created."

def test_png_is_not_empty(tmp_output_dir):
    output_png = os.path.join(tmp_output_dir, "EXWWB909.png")
    convert_svg_to_png(SVG_PATH, output_png)
    assert os.path.getsize(output_png) > 0, "PNG file is empty."

def test_invalid_svg_path():
    with pytest.raises(FileNotFoundError):
        convert_svg_to_png("nonexistent.svg")