A small set of Python utilities that turn COBOL assets and Markdown
documentation into richly-formatted diagrams:

| Script | What it does |
|--------|--------------|
| `comprehension_with_diagram_generator.py` | • Scans every `*.md` in **Kanav_diagram/** (skipping any `_control_flow.md`)<br>• Renders each Mermaid block → **SVG** + **PNG** (via `@mermaid-js/mermaid-cli`)<br>• Swaps the code block for an image tag that points to the PNG<br>• Converts the updated Markdown into a **Word (.docx)** file with the diagrams embedded |
| `generate_flow_diagram.py` | Parses every COBOL sub-folder, reverse-engineers procedure flow, and writes a `{program}_control_flow.md` with an embedded Mermaid flow-chart |
| `render_diagram.py` | Batch-renders all `*_control_flow.md` files:<br>  • static **SVG**<br>  • interactive **HTML** (vis.js + Pyvis, collapsible & scrollable) |

---

## Directory layout

```
Kanav_diagram/
│   └─ EXWWB915/
│       ├─ EXWWB915.md          ← Markdown with Mermaid blocks
│       └─ …                    ← COBOL .txt / .json / .md triplets
scripts/
│   ├─ comprehension_with_diagram_generator.py
│   ├─ generate_flow_diagram.py
│   └─ render_diagram.py
README.md
requirements.txt
```

---

## Prerequisites

| Kind | Tool | Why |
|------|------|-----|
| **Python 3.9+** | `pip install -r requirements.txt` | Network & HTML diagrams |
| **Node.js 18+** | `npm install -g @mermaid-js/mermaid-cli`<br>`npx playwright install chromium` | Renders Mermaid → SVG/PNG |
| **Pandoc 3.x** | `brew install pandoc` / `apt install pandoc` | Markdown → DOCX |
| *(optional)* | Inkscape ≥ 1.2 | If you ever want local SVG → PNG without Mermaid-CLI |

---

## Installation

### 1. Clone the repository
```sh
git clone <your-repo-url>
cd <your-repo-directory>
```

### 2. Set up Python environment
```sh
python3 -m venv venv
source venv/bin/activate
```

### 3. Install Python dependencies
```sh
pip install -r requirements.txt
```

### 4. Install Node.js and Mermaid CLI
- **Node.js:** [Download & install](https://nodejs.org/)
- **Mermaid CLI:**
```sh
npm install -g @mermaid-js/mermaid-cli
```
- **Playwright (for Mermaid CLI rendering):**
```sh
npx playwright install chromium
```

### 5. Install System Dependencies for Diagram Export
- **Inkscape** (required for PNG export from SVG/mermaid-cli):
  - **macOS:**
    ```sh
    brew install inkscape
    ```
  - **Ubuntu/Debian:**
    ```sh
    sudo apt-get install inkscape
    ```
  - **Windows:**
    [Download Inkscape](https://inkscape.org/release/)

- **Pandoc** (for DOCX/PDF generation):
  - **macOS:**
    ```sh
    brew install pandoc
    ```
  - **Ubuntu/Debian:**
    ```sh
    sudo apt-get install pandoc
    ```
  - **Windows:**
    [Download Pandoc](https://pandoc.org/installing.html)

- **TeX (for PDF export, optional):**
  - **macOS:**
    ```sh
    brew install --cask mactex
    ```
  - **Ubuntu/Debian:**
    ```sh
    sudo apt-get install texlive-xetex
    ```
  - **Windows:**
    [Install MiKTeX](https://miktex.org/download)

---

## Quick start

```bash
git clone <repo-url>
cd <repo-dir>

# 1) Python deps
python3 -m venv venv && source venv/bin/activate
pip install -r requirements.txt

# 2) Node + Mermaid CLI
npm i -g @mermaid-js/mermaid-cli
npx playwright install chromium   # one-time browser download

# 3) Batch-generate DOCX with diagrams
python scripts/comprehension_with_diagram_generator.py

# 4) Build interactive HTML / SVG for control-flow files
python scripts/render_diagram.py
```

Open `Kanav_diagram/EXWWB915/EXWWB915.docx` – the diagrams are embedded and
their labels are crisp because the PNG versions are used.

---

## Typical workflow

1. **Drop** a new COBOL folder under `Kanav_diagram/`  
   (must contain `<name>.txt`, `<name>.json`, `<name>.md`).
2. Run `python scripts/generate_flow_diagram.py` – you'll get
   `<name>_control_flow.md` in that same folder.
3. Run `python scripts/render_diagram.py` – now you have:
   * `…svg` (static)  
   * `…_interactive.html` (zoomable, collapsible)
4. Commit the generated assets or ignore them in *.gitignore* as you prefer.

---

## Troubleshooting

| Problem | Fix |
|---------|-----|
| `mmdc not found` | `npm i -g @mermaid-js/mermaid-cli` |
| `Playwright … browser not installed` | `npx playwright install chromium` |
| Word shows red "image missing" text | Ensure the **PNG** files are created – check the console for "PNG export failed". |
| Pandoc missing | `brew install pandoc` or `