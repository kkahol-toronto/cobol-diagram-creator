import os
from cairosvg import svg2png

def convert_svg_to_png(svg_path: str, png_path: str, scale: float = 1.0):
    """
    Converts an SVG file to a PNG file at full resolution.
    
    Args:
        svg_path (str): Path to the input SVG file.
        png_path (str): Path to save the output PNG file.
        scale (float): Scaling factor for the output PNG (default is 1.0).
    """
    if not os.path.isfile(svg_path):
        raise FileNotFoundError(f"Input SVG file {svg_path} does not exist.")
    with open(svg_path, "rb") as svg_file:
        svg_data = svg_file.read()
        svg2png(bytestring=svg_data, write_to=png_path, scale=scale)

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 3:
        print("Usage: python render_diagram.py <input_svg> <output_png>")
        sys.exit(1)
    input_svg = sys.argv[1]
    output_png = sys.argv[2]
    convert_svg_to_png(input_svg, output_png)
    print(f"Converted {input_svg} to {output_png}")