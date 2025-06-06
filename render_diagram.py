import os
from cairosvg import svg2png

def svg_to_png(svg_path, png_path):
    """
    Converts an SVG file to a PNG file at full resolution.
    :param svg_path: Path to the SVG file.
    :param png_path: Path where the PNG file will be saved.
    """
    if not os.path.isfile(svg_path):
        raise FileNotFoundError(f"SVG file not found: {svg_path}")
    with open(svg_path, "rb") as svg_file:
        svg_content = svg_file.read()
        svg2png(bytestring=svg_content, write_to=png_path)

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 3:
        print("Usage: python render_diagram.py <input_svg_path> <output_png_path>")
        sys.exit(1)
    svg_to_png(sys.argv[1], sys.argv[2])