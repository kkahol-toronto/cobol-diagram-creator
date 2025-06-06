import os
import tempfile
import shutil
from render_diagram import convert_svg_to_png

import pytest
from PIL import Image

SVG_SAMPLE_PATH = "Kanav_diagram/EXWWB909/EXWWB909.svg"

@pytest.fixture
def temp_png_file():
    temp_dir = tempfile.mkdtemp()
    png_path = os.path.join(temp_dir, "test_output.png")
    yield png_path
    shutil.rmtree(temp_dir)

def test_convert_svg_to_png_creates_png(temp_png_file):
    png_path = convert_svg_to_png(SVG_SAMPLE_PATH, temp_png_file)
    assert os.path.isfile(png_path), "PNG file was not created"

def test_convert_svg_to_png_file_is_not_empty(temp_png_file):
    png_path = convert_svg_to_png(SVG_SAMPLE_PATH, temp_png_file)
    assert os.path.getsize(png_path) > 0, "PNG file is empty"

def test_convert_svg_to_png_file_is_valid_png(temp_png_file):
    png_path = convert_svg_to_png(SVG_SAMPLE_PATH, temp_png_file)
    with Image.open(png_path) as img:
        assert img.format == "PNG"
        # Optionally, check dimensions are non-zero
        assert img.width > 0 and img.height > 0

def test_convert_svg_to_png_raises_for_missing_file():
    with pytest.raises(FileNotFoundError):
        convert_svg_to_png("nonexistent_file.svg")