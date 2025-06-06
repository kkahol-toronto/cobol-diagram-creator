import os
import tempfile
import shutil

import pytest

from render_diagram import convert_svg_to_png

SVG_SAMPLE_PATH = "Kanav_diagram/EXWWB909/EXWWB909.svg"

@pytest.fixture
def temp_png_file():
    temp_dir = tempfile.mkdtemp()
    png_path = os.path.join(temp_dir, "test_output.png")
    yield png_path
    shutil.rmtree(temp_dir)

def test_convert_svg_to_png_creates_png(temp_png_file):
    convert_svg_to_png(SVG_SAMPLE_PATH, temp_png_file)
    assert os.path.exists(temp_png_file), "PNG file was not created"
    assert os.path.getsize(temp_png_file) > 0, "PNG file is empty"

def test_convert_svg_to_png_overwrites_existing_file(temp_png_file):
    # Create an empty file first
    with open(temp_png_file, "wb") as f:
        f.write(b"")
    convert_svg_to_png(SVG_SAMPLE_PATH, temp_png_file)
    assert os.path.getsize(temp_png_file) > 0, "PNG file was not properly overwritten"