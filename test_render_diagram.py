import os
import tempfile
import shutil
from PIL import Image
import pytest

from render_diagram import convert_svg_to_png

SVG_PATH = "Kanav_diagram/EXWWB909/EXWWB909.svg"

@pytest.fixture
def temp_png_file():
    temp_dir = tempfile.mkdtemp()
    png_path = os.path.join(temp_dir, "test_output.png")
    yield png_path
    shutil.rmtree(temp_dir)

def test_png_is_created(temp_png_file):
    convert_svg_to_png(SVG_PATH, temp_png_file)
    assert os.path.exists(temp_png_file), "PNG file was not created."

def test_png_is_not_empty(temp_png_file):
    convert_svg_to_png(SVG_PATH, temp_png_file)
    assert os.path.getsize(temp_png_file) > 0, "PNG file is empty."

def test_png_is_valid_image(temp_png_file):
    convert_svg_to_png(SVG_PATH, temp_png_file)
    with Image.open(temp_png_file) as img:
        assert img.format == "PNG", "Output file is not a valid PNG image."
        width, height = img.size
        assert width > 0 and height > 0, "PNG image has invalid dimensions."