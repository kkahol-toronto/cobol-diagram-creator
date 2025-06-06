import os
from cairosvg import svg2png

def convert_svg_to_png(svg_path, png_path):
    """
    Convert an SVG file to a PNG file at full resolution.

    Args:
        svg_path (str): Path to the input SVG file.
        png_path (str): Path where the output PNG will be saved.
    """
    if not os.path.exists(svg_path):
        raise FileNotFoundError(f"SVG file does not exist: {svg_path}")

    with open(svg_path, "rb") as svg_file:
        svg_data = svg_file.read()
        svg2png(bytestring=svg_data, write_to=png_path)