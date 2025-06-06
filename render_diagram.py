```python
import os
from cairosvg import svg2png

def convert_svg_to_png(svg_path, png_path=None, scale=1.0):
    """
    Convert an SVG file to a PNG file at full resolution.

    Args:
        svg_path (str): Path to the input SVG file.
        png_path (str, optional): Path to output PNG file. If None, uses same name as SVG with .png extension.
        scale (float, optional): Scale factor for output PNG. Default is 1.0 (original size).
    Returns:
        str: Path to the generated PNG file.
    """
    if not os.path.isfile(svg_path):
        raise FileNotFoundError(f"SVG file not found: {svg_path}")
    if png_path is None:
        png_path = os.path.splitext(svg_path)[0] + ".png"
    with open(svg_path, "rb") as svg_file:
        svg_content = svg_file.read()
        svg2png(bytestring=svg_content, write_to=png_path, scale=scale)
    return png_path

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Convert SVG to PNG at full resolution.")
    parser.add_argument("svg_path", help="Path to input SVG file")
    parser.add_argument("--png_path", help="Path to output PNG file (optional)")
    parser.add_argument("--scale", type=float, default=1.0, help="Scale factor for output PNG (default: 1.0)")
    args = parser.parse_args()
    out_path = convert_svg_to_png(args.svg_path, args.png_path, args.scale)
    print(f"PNG saved to: {out_path}")
```