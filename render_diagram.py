import os
from cairosvg import svg2png

def convert_svg_to_png(svg_path, png_path):
    """
    Converts an SVG file to a PNG file at full resolution.

    Args:
        svg_path (str): Path to the SVG file.
        png_path (str): Output path for the PNG file.
    """
    if not os.path.isfile(svg_path):
        raise FileNotFoundError(f"SVG file not found: {svg_path}")
    with open(svg_path, "rb") as svg_file:
        svg_data = svg_file.read()
    svg2png(bytestring=svg_data, write_to=png_path)