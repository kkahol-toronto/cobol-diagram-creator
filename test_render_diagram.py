```python
import os
import tempfile
import shutil
from render_diagram import convert_svg_to_png
from PIL import Image

def test_convert_svg_to_png_creates_file():
    svg_path = "Kanav_diagram/EXWWB909/EXWWB909.svg"
    with tempfile.TemporaryDirectory() as tmpdir:
        png_path = os.path.join(tmpdir, "output.png")
        result_path = convert_svg_to_png(svg_path, png_path)
        assert os.path.isfile(result_path), "PNG file was not created"

def test_convert_svg_to_png_file_not_empty():
    svg_path = "Kanav_diagram/EXWWB909/EXWWB909.svg"
    with tempfile.TemporaryDirectory() as tmpdir:
        png_path = os.path.join(tmpdir, "output.png")
        convert_svg_to_png(svg_path, png_path)
        assert os.path.getsize(png_path) > 0, "PNG file is empty"

def test_convert_svg_to_png_valid_image():
    svg_path = "Kanav_diagram/EXWWB909/EXWWB909.svg"
    with tempfile.TemporaryDirectory() as tmpdir:
        png_path = os.path.join(tmpdir, "output.png")
        convert_svg_to_png(svg_path, png_path)
        # Try to open with PIL to validate it's a real PNG
        with Image.open(png_path) as im:
            assert im.format == "PNG"
            assert im.width > 0 and im.height > 0

def test_convert_svg_to_png_raises_on_missing_file():
    fake_svg_path = "nonexistent.svg"
    with tempfile.TemporaryDirectory() as tmpdir:
        png_path = os.path.join(tmpdir, "output.png")
        try:
            convert_svg_to_png(fake_svg_path, png_path)
            assert False, "Expected FileNotFoundError"
        except FileNotFoundError:
            pass  # Expected

```