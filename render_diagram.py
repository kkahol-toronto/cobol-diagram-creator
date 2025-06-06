import os
from cairosvg import svg2png

def convert_svg_to_png(svg_path, png_path=None, scale=1.0):
    """
    Converts an SVG file to a PNG file at full resolution.

    Args:
        svg_path (str): Path to the input SVG file.
        png_path (str, optional): Path to the output PNG file. 
            If None, replaces .svg with .png in svg_path.
        scale (float, optional): Scaling factor for output PNG. Default is 1.0 (original size).

    Returns:
        str: The path to the generated PNG file.
    """
    if not os.path.isfile(svg_path):
        raise FileNotFoundError(f"SVG file not found: {svg_path}")

    if png_path is None:
        png_path = os.path.splitext(svg_path)[0] + ".png"

    with open(svg_path, "rb") as svg_file:
        svg_data = svg_file.read()
        svg2png(bytestring=svg_data, write_to=png_path, scale=scale)

    return png_path

if __name__ == "__main__":
    import sys
    if len(sys.argv) < 2:
        print("Usage: python render_diagram.py input_svg [output_png]")
        sys.exit(1)
    svg_path = sys.argv[1]
    png_path = sys.argv[2] if len(sys.argv) > 2 else None
    out_path = convert_svg_to_png(svg_path, png_path)
    print(f"PNG file saved at: {out_path}")