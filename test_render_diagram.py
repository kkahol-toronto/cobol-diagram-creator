import os
import tempfile
import shutil
from render_diagram import svg_to_png
from PIL import Image

import pytest

SVG_SAMPLE_PATH = "Kanav_diagram/EXWWB909/EXWWB909.svg"

@pytest.fixture(scope="module")
def temp_png_file():
    tmpdir = tempfile.mkdtemp()
    png_path = os.path.join(tmpdir, "test_output.png")
    yield png_path
    shutil.rmtree(tmpdir)

def test_svg_to_png_creates_png(temp_png_file):
    svg_to_png(SVG_SAMPLE_PATH, temp_png_file)
    assert os.path.isfile(temp_png_file), "PNG file was not created."

def test_png_is_not_empty(temp_png_file):
    svg_to_png(SVG_SAMPLE_PATH, temp_png_file)
    assert os.path.getsize(temp_png_file) > 0, "PNG file is empty."

def test_png_is_valid_image(temp_png_file):
    svg_to_png(SVG_SAMPLE_PATH, temp_png_file)
    with Image.open(temp_png_file) as img:
        assert img.format == "PNG", "Output file is not a PNG image."
        assert img.width > 0 and img.height > 0, "PNG image has invalid dimensions."

def test_svg_file_not_found():
    with pytest.raises(FileNotFoundError):
        svg_to_png("non_existent_file.svg", "output.png")