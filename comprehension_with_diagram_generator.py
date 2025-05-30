#!/usr/bin/env python3
"""
comprehension_with_diagram_generator.py
───────────────────────────────────────
• Convert Mermaid blocks ➜ SVG **and** PNG (via mermaid-cli)
• Replace code blocks with <img> links → PNG path
• Convert Markdown ➜ DOCX (via Pandoc)
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
from pathlib import Path
from typing import List, Tuple

ROOT = Path("Kanav_diagram")  # folder with *.md files


# ─────────── Mermaid helpers ──────────────────────────────────────────
def escape_labels(src: str) -> str:
    def repl(m):
        node_id, label = (m.group(1) or ""), m.group(2)
        if label.startswith('"') and label.endswith('"'):
            return m.group(0)
        if any(c in label for c in "()/:,|"):
            return f'{node_id}["{label}"]' if node_id else f'["{label}"]'
        return m.group(0)
    return re.sub(r"(?:([A-Za-z0-9_]+)\s*)?\[([^\]]+)\]", repl, src)


def fix_edges(txt: str) -> str:
    return re.sub(r"([A-Za-z0-9_])-->\|\s*(\||$)", r"\1-->\2", txt)


def render_diagram(code: str, base_path: Path, mmdc: str | None) -> Path:
    """
    Render Mermaid ➜ SVG and PNG.
    Returns the PNG Path (for embedding in DOCX).
    """
    svg_path = base_path.with_suffix(".svg")
    png_path = base_path.with_suffix(".png")
    mmd_path = base_path.with_suffix(".mmd")
    mmd_path.write_text(escape_labels(code), encoding="utf-8")

    if not mmdc:
        print("  ↳  mmdc not found – diagrams skipped.")
        return png_path  # non-existent; Word will show placeholder

    def _run(out: Path, extra_env: dict | None = None) -> bool:
        env = {**os.environ, **(extra_env or {})}
        try:
            subprocess.run(
                [mmdc, "-i", str(mmd_path), "-o", str(out), "-b", "transparent"],
                check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env
            )
            return True
        except subprocess.CalledProcessError as e:
            msg = e.stderr.decode().splitlines()[-1] if e.stderr else ""
            print(f"  ⚠️  mmdc error ({out.name}): {msg}")
            return False

    # SVG first
    if not _run(svg_path):
        _run(svg_path, {"NODE_OPTIONS": "--max_old_space_size=4096"})

    # PNG export (mmdc uses the output file extension to decide format)
    if not _run(png_path):
        print("  ↳  PNG export failed – Word may miss this diagram.")

    mmd_path.unlink(missing_ok=True)
    return png_path


# ─────────── Markdown utils ──────────────────────────────────────────
def find_md_files() -> List[Path]:
    return [
        p for p in ROOT.glob("*/*.md")
        if not p.name.endswith(("_control_flow.md", ".tmp.md"))
    ]


def extract_mermaid_blocks(text: str) -> List[Tuple[str, str, int, int]]:
    lines, blocks, title = text.splitlines(), [], None
    i = 0
    while i < len(lines):
        if lines[i].lstrip().startswith("## "):
            title = lines[i].lstrip("#").strip()
        if lines[i].lstrip().startswith("```mermaid"):
            start, body = i, []
            i += 1
            while i < len(lines) and not lines[i].lstrip().startswith("```"):
                body.append(lines[i]); i += 1
            end = i
            if title and body:
                blocks.append((title, "\n".join(body), start, end))
        i += 1
    return blocks


# ─────────── DOCX helper ─────────────────────────────────────────────
class DocxMaker:
    def __init__(self):
        self.pandoc = shutil.which("pandoc")

    def make(self, md: str, docx_path: Path, res_dir: Path):
        if not self.pandoc:
            print("  ↳  Pandoc not found – skipping DOCX.")
            return
        tmp = docx_path.with_suffix(".tmp.md")
        tmp.write_text(md, encoding="utf-8")
        try:
            subprocess.run(
                [
                    self.pandoc, str(tmp),
                    "-o", str(docx_path),
                    "--from", "markdown",
                    "--resource-path", str(res_dir),
                ],
                check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE
            )
            print(f"  ✔  DOCX created: {docx_path.name}")
        except subprocess.CalledProcessError as e:
            tail = e.stderr.decode().splitlines()[-1] if e.stderr else "(no log)"
            print(f"     ↳ pandoc/docx error: {tail}")
        finally:
            tmp.unlink(missing_ok=True)


# ─────────── Main orchestration ──────────────────────────────────────
def process_one(md_path: Path, mmdc: str | None, maker: DocxMaker):
    print(f"Processing {md_path}")
    md_text = md_path.read_text(encoding="utf-8")
    blocks = extract_mermaid_blocks(md_text)
    lines, offset = md_text.splitlines(), 0

    for title, code, start, end in blocks:
        base_name = f"{md_path.stem}_{re.sub(r'[^0-9A-Za-z]+', '_', title)}"
        png = render_diagram(fix_edges(code), md_path.parent / base_name, mmdc)
        lines = (
            lines[: start - offset]
            + [f"### {title}", f"![{title}]({png.name})"]
            + lines[end + 1 - offset :]
        )
        offset += end - start

    maker.make("\n".join(lines), md_path.with_suffix(".docx"), md_path.parent)


def main():
    mmdc = shutil.which("mmdc")
    if not mmdc:
        print("Mermaid-CLI not found – diagrams will be skipped.")

    maker = DocxMaker()
    for md in find_md_files():
        process_one(md, mmdc, maker)


if __name__ == "__main__":
    main()
