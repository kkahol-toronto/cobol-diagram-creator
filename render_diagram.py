import os
from cairosvg import svg2png

def convert_svg_to_png(svg_path, png_path):
    """
    Converts an SVG file to a PNG file at full resolution.

    Args:
        svg_path (str): Path to the input SVG file.
        png_path (str): Path to the output PNG file.
    """
    with open(svg_path, 'rb') as svg_file:
        svg_bytes = svg_file.read()
        svg2png(bytestring=svg_bytes, write_to=png_path)

if __name__ == "__main__":
    # Example usage
    svg_input = "Kanav_diagram/EXWWB909/EXWWB909.svg"
    png_output = "Kanav_diagram/EXWWB909/EXWWB909.png"
    convert_svg_to_png(svg_input, png_output)
    print(f"Converted {svg_input} to {png_output}")