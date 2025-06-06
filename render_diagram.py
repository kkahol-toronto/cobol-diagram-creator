```python
import os
from cairosvg import svg2png

def convert_svg_to_png(input_svg_path, output_png_path, scale=1.0):
    """
    Converts an SVG file to a PNG file at full resolution.

    Args:
        input_svg_path (str): Path to the input SVG file.
        output_png_path (str): Path to save the output PNG file.
        scale (float): Scale factor for the output PNG. Default is 1.0.
    """
    if not os.path.exists(input_svg_path):
        raise FileNotFoundError(f"SVG file not found: {input_svg_path}")

    with open(input_svg_path, 'rb') as svg_file:
        svg_data = svg_file.read()
        svg2png(bytestring=svg_data, write_to=output_png_path, scale=scale)

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Convert SVG to PNG.")
    parser.add_argument("input_svg", help="Path to input SVG file")
    parser.add_argument("output_png", help="Path to output PNG file")
    parser.add_argument("--scale", type=float, default=1.0, help="Scale factor for PNG (default=1.0)")
    args = parser.parse_args()

    convert_svg_to_png(args.input_svg, args.output_png, scale=args.scale)
    print(f"Converted {args.input_svg} to {args.output_png}")
```