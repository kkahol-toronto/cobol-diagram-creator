import os
import tempfile
import shutil
from convert_svg import convert_svg_to_png
from PIL import Image

SAMPLE_SVG = """<?xml version="1.0" encoding="UTF-8"?>
<svg width="100" height="100" xmlns="http://www.w3.org/2000/svg">
  <circle cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" />
</svg>
"""

def test_convert_svg_to_png_creates_png():
    with tempfile.TemporaryDirectory() as tmpdir:
        svg_path = os.path.join(tmpdir, "test.svg")
        png_path = os.path.join(tmpdir, "test.png")
        # Write sample SVG
        with open(svg_path, "w") as f:
            f.write(SAMPLE_SVG)
        # Convert to PNG
        out_png = convert_svg_to_png(svg_path, png_path)
        assert os.path.isfile(out_png)
        # Check PNG is not empty
        assert os.path.getsize(out_png) > 0
        # Check PNG can be opened and has expected dimensions
        with Image.open(out_png) as img:
            assert img.format == "PNG"
            assert img.size == (100, 100)

def test_convert_svg_to_png_default_output_path():
    with tempfile.TemporaryDirectory() as tmpdir:
        svg_path = os.path.join(tmpdir, "sample.svg")
        # Write sample SVG
        with open(svg_path, "w") as f:
            f.write(SAMPLE_SVG)
        # Convert without specifying png_path
        out_png = convert_svg_to_png(svg_path)
        assert out_png.endswith(".png")
        assert os.path.isfile(out_png)
        # Check PNG can be opened
        with Image.open(out_png) as img:
            assert img.format == "PNG"

def test_convert_svg_to_png_file_not_found():
    import pytest
    with pytest.raises(FileNotFoundError):
        convert_svg_to_png("nonexistent_file.svg")