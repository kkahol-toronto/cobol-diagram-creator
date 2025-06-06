import os
from cairosvg import svg2png

def convert_svg_to_png(svg_path, png_path):
    """
    Converts an SVG file to a PNG file at the SVG's original resolution.

    Args:
        svg_path (str): Path to the source SVG file.
        png_path (str): Path to the output PNG file.
    """
    if not os.path.isfile(svg_path):
        raise FileNotFoundError(f"SVG file not found: {svg_path}")

    with open(svg_path, "rb") as svg_file:
        svg_content = svg_file.read()
        svg2png(bytestring=svg_content, write_to=png_path)

if __name__ == "__main__":
    svg_file = "Kanav_diagram/EXWWB909/EXWWB909.svg"
    png_file = "Kanav_diagram/EXWWB909/EXWWB909.png"
    convert_svg_to_png(svg_file, png_file)
    print(f"Converted {svg_file} to {png_file}")