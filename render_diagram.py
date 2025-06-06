import os
from cairosvg import svg2png

def convert_svg_to_png(svg_path, png_path=None, scale=1.0):
    """
    Convert an SVG file to a PNG file at full resolution.

    :param svg_path: Path to the SVG file.
    :param png_path: Path to output PNG file. If None, replaces .svg with .png.
    :param scale: Scaling factor for the image (1.0 = original size).
    :return: Path to the generated PNG file.
    """
    if not os.path.isfile(svg_path):
        raise FileNotFoundError(f"SVG file not found: {svg_path}")

    if png_path is None:
        png_path = os.path.splitext(svg_path)[0] + '.png'

    with open(svg_path, 'rb') as svg_file:
        svg_data = svg_file.read()

    svg2png(bytestring=svg_data, write_to=png_path, scale=scale)
    return png_path