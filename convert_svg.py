import sys
import os
from cairosvg import svg2png

def convert_svg_to_png(svg_path, png_path=None):
    """
    Convert an SVG file to PNG format at full resolution.

    Args:
        svg_path (str): Path to the SVG file.
        png_path (str, optional): Path to save the PNG file. If None, saves next to the SVG.
    Returns:
        str: Path to the created PNG file.
    """
    if not os.path.isfile(svg_path):
        raise FileNotFoundError(f"SVG file not found: {svg_path}")

    if png_path is None:
        base, _ = os.path.splitext(svg_path)
        png_path = base + ".png"

    with open(svg_path, "rb") as svg_file:
        svg_data = svg_file.read()

    # Convert SVG to PNG using cairosvg
    svg2png(bytestring=svg_data, write_to=png_path)
    return png_path

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python convert_svg.py <path_to_svg> [output_png_path]")
        sys.exit(1)
    svg_path = sys.argv[1]
    png_path = sys.argv[2] if len(sys.argv) > 2 else None
    try:
        out_path = convert_svg_to_png(svg_path, png_path)
        print(f"PNG created at: {out_path}")
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(2)