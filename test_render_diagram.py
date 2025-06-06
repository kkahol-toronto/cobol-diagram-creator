import os
import pytest
from PIL import Image

from render_diagram import convert_svg_to_png

SVG_PATH = "Kanav_diagram/EXWWB909/EXWWB909.svg"
PNG_PATH = "Kanav_diagram/EXWWB909/EXWWB909.png"

@pytest.fixture(autouse=True)
def cleanup_png():
    # Remove PNG before and after test to avoid false positives
    if os.path.exists(PNG_PATH):
        os.remove(PNG_PATH)
    yield
    if os.path.exists(PNG_PATH):
        os.remove(PNG_PATH)

def test_svg_to_png_conversion_creates_file():
    convert_svg_to_png(SVG_PATH, PNG_PATH)
    assert os.path.exists(PNG_PATH), "PNG file was not created."

def test_svg_to_png_conversion_file_not_empty():
    convert_svg_to_png(SVG_PATH, PNG_PATH)
    assert os.path.getsize(PNG_PATH) > 0, "PNG file is empty."

def test_svg_to_png_conversion_valid_image():
    convert_svg_to_png(SVG_PATH, PNG_PATH)
    with Image.open(PNG_PATH) as img:
        assert img.format == "PNG", "Output file is not a PNG image."
        # Optionally, check dimensions if you know expected width/height
        width, height = img.size
        assert width > 0 and height > 0, "PNG image has invalid dimensions."