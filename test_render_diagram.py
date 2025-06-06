import os
import tempfile
from PIL import Image

from render_diagram import convert_svg_to_png

SVG_PATH = "Kanav_diagram/EXWWB909/EXWWB909.svg"

def test_png_creation():
    with tempfile.TemporaryDirectory() as tmpdir:
        png_path = os.path.join(tmpdir, "output.png")
        convert_svg_to_png(SVG_PATH, png_path)
        assert os.path.isfile(png_path), "PNG file was not created."

def test_png_not_empty():
    with tempfile.TemporaryDirectory() as tmpdir:
        png_path = os.path.join(tmpdir, "output.png")
        convert_svg_to_png(SVG_PATH, png_path)
        assert os.path.getsize(png_path) > 1024, "PNG file is unexpectedly small."

def test_png_openable():
    with tempfile.TemporaryDirectory() as tmpdir:
        png_path = os.path.join(tmpdir, "output.png")
        convert_svg_to_png(SVG_PATH, png_path)
        with Image.open(png_path) as img:
            assert img.format == "PNG"
            width, height = img.size
            assert width > 0 and height > 0, "PNG image has invalid dimensions."