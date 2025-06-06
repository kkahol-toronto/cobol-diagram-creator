import os
import tempfile
from PIL import Image
import pytest

from render_diagram import convert_svg_to_png

SVG_PATH = "Kanav_diagram/EXWWB909/EXWWB909.svg"

@pytest.mark.skipif(not os.path.exists(SVG_PATH), reason="SVG test file not found")
def test_convert_svg_to_png_creates_png():
    with tempfile.TemporaryDirectory() as tmpdir:
        png_path = os.path.join(tmpdir, "output.png")
        convert_svg_to_png(SVG_PATH, png_path)
        assert os.path.exists(png_path), "PNG file was not created."

        # Check file is not empty
        assert os.path.getsize(png_path) > 0, "PNG file is empty."

        # Check that it is a valid PNG image
        with Image.open(png_path) as img:
            assert img.format == "PNG", "Output file is not a valid PNG image."
            # Optionally, check dimensions are non-zero
            width, height = img.size
            assert width > 0 and height > 0, "PNG image has invalid dimensions."