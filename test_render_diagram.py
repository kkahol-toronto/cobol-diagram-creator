```python
import os
import tempfile
import shutil
from render_diagram import convert_svg_to_png

import pytest

SAMPLE_SVG = """<?xml version="1.0" encoding="UTF-8"?>
<svg width="100" height="100"
     xmlns="http://www.w3.org/2000/svg">
  <circle cx="50" cy="50" r="40" stroke="green"
          stroke-width="4" fill="yellow" />
</svg>
"""

def test_convert_svg_to_png_creates_png(tmp_path):
    # Create a temporary SVG file
    svg_path = tmp_path / "test.svg"
    png_path = tmp_path / "test.png"
    svg_path.write_text(SAMPLE_SVG)

    convert_svg_to_png(str(svg_path), str(png_path))

    assert png_path.exists(), "PNG file was not created."
    assert png_path.stat().st_size > 0, "PNG file is empty."

def test_convert_svg_to_png_invalid_path():
    with pytest.raises(FileNotFoundError):
        convert_svg_to_png("nonexistent.svg", "output.png")

def test_convert_svg_to_png_real_file():
    # Test with the actual Kanav_diagram/EXWWB909/EXWWB909.svg if it exists
    svg_path = "Kanav_diagram/EXWWB909/EXWWB909.svg"
    if not os.path.exists(svg_path):
        pytest.skip(f"{svg_path} does not exist, skipping real file test.")
    with tempfile.TemporaryDirectory() as tmpdir:
        png_path = os.path.join(tmpdir, "output.png")
        convert_svg_to_png(svg_path, png_path)
        assert os.path.exists(png_path)
        assert os.path.getsize(png_path) > 0
```