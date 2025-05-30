#!/usr/bin/env python
"""
render_exwwb910.py
• exwwb910.svg  – static Mermaid rendering
• exwwb910_interactive.html – collapsible, colour-coded, scrollable vis.js view
"""

import html, json, re, subprocess, textwrap, glob
from pathlib import Path

import networkx as nx
from pyvis.network import Network

# ──────────────────────────────────────────────────────────────────────────────
# Mermaid text
# ──────────────────────────────────────────────────────────────────────────────
MERMAID = textwrap.dedent(r"""
flowchart TD
    %% Main Program Flow
    Start([Program Start]) --> Init[0000P-MAINLINE]
    Init --> InitPhase[0100I-INITIALIZATION]

    %% Main Processing Loop
    InitPhase --> MainLoop[1000P-PROCESS]
    MainLoop --> ProcessProducer{Process Producer}
    ProcessProducer --> OpenMEXW001[Open MEXW001_CSR]
    OpenMEXW001 --> FetchMEXW001[Fetch MEXW001 Records]
    FetchMEXW001 --> ProcessVehicle[2000C-PROCESS-GEVIS-VEHICLE]
    ProcessVehicle --> MoreMEXW001{More Records?}
    MoreMEXW001 -->|Yes| FetchMEXW001
    MoreMEXW001 -->|No| CloseMEXW001[Close MEXW001_CSR]

    %% Vehicle Processing Details
    ProcessVehicle --> GetWERS[Get WERS Data]
    GetWERS --> GetDealerData[Get Dealer Data]
    GetDealerData --> GetRetailData[Get Retail Data]
    GetRetailData --> GetWholesaleData[Get Wholesale Data]
    GetWholesaleData --> GetWERSDetails[Get WERS Details]
    GetWERSDetails --> GetStatusDates[Get Status Dates]
    GetStatusDates --> WriteRecord[Write VINCENT Record]
    WriteRecord --> Checkpoint{Checkpoint Needed?}
    Checkpoint -->|Yes| DoCheckpoint[Perform Checkpoint]
    Checkpoint -->|No| NextVehicle[Next Vehicle]

    %% Conclusion Phase
    MainLoop --> Conclusion[0200I-CONCLUSION]
    Conclusion --> UpdateTimestamp[Update Run Timestamp]
    UpdateTimestamp --> UpdateBatch[Update Batch Number]
    UpdateBatch --> WriteTrailers[Write Trailers]
    WriteTrailers --> WriteAudit[Write Audit Statistics]
    WriteAudit --> SetReturnCode[Set Return Code]
    SetReturnCode --> End([Program End])

    %% Error Handling
    InitPhase -->|Error| Abend[9999I-ABEND]
    MainLoop -->|Error| Abend
    ProcessVehicle -->|Error| Abend
    Conclusion -->|Error| Abend
    Abend --> WriteAbendMsg[Write Abend Message]
    WriteAbendMsg --> Rollback[Perform Rollback]
    Rollback --> CoreDump[Call COREDUMP]
    CoreDump --> End

    %% Styling
    classDef process fill:#f9f,stroke:#333,stroke-width:2px
    classDef decision fill:#bbf,stroke:#333,stroke-width:2px
    classDef error fill:#fbb,stroke:#333,stroke-width:2px
    classDef start fill:#bfb,stroke:#333,stroke-width:2px

    class Start,End start
    class ProcessProducer,MoreMEXW001,Checkpoint decision
    class Abend,WriteAbendMsg,Rollback,CoreDump error
    class Init,InitPhase,MainLoop,ProcessVehicle,Conclusion process
""").strip()

# ──────────────────────────────────────────────────────────────────────────────
# 1. Static SVG via Mermaid-CLI (unchanged)
# ──────────────────────────────────────────────────────────────────────────────
def build_svg():
    Path("exwwb910.mmd").write_text(MERMAID, encoding="utf-8")
    subprocess.run(
        ["mmdc", "-i", "exwwb910.mmd", "-o", "exwwb910.svg", "-b", "transparent"],
        check=True,
    )
    print("✔  exwwb910.svg created")

# ──────────────────────────────────────────────────────────────────────────────
# 2. Interactive, scrollable, colour-coded HTML
# ──────────────────────────────────────────────────────────────────────────────
EDGE_RE  = re.compile(r'^\s*([\w\d_]+)\s*-->\s*([\w\d_]+)')
NODE_RE  = re.compile(r'^\s*([\w\d_]+)\s*(\(\[|\[\(|\(\(|\[\[|\[|\{|\()([^\]\)\}]+)')
CLASSDEF_RE = re.compile(r'^\s*classDef\s+(\w+).*?fill:\s*(#[0-9a-fA-F]{3,6})')
CLASS_RE    = re.compile(r'^\s*class\s+([^ ]+)\s+(\w+)')

SHAPE_MAP = {"(": "ellipse", "((": "circle", "([": "ellipse", "[(": "ellipse",
             "[": "box", "{": "diamond"}

def parse_mermaid(text: str):
    edges, labels, shapes = [], {}, {}
    class_defs, node_classes = {}, {}

    for ln in text.splitlines():
        if (m := EDGE_RE.match(ln)):     # A --> B
            edges.append(m.groups())
        if (m := NODE_RE.match(ln)):     # Node[label]
            node, opener, label = m.groups()
            labels[node]  = label.strip()
            shapes[node]  = SHAPE_MAP.get(opener[:2], "box")
        if (m := CLASSDEF_RE.match(ln)): # classDef name fill:#abc
            class_defs[m.group(1)] = m.group(2)
        if (m := CLASS_RE.match(ln)):    # class node1,node2 someClass
            for n in m.group(1).split(","):
                node_classes[n.strip()] = m.group(2)

    return edges, labels, shapes, class_defs, node_classes

def build_interactive():
    edges, labels, shapes, class_defs, node_classes = parse_mermaid(MERMAID)

    # roots = nodes with in-degree 0
    indeg = {n: 0 for n in {x for e in edges for x in e}}
    for _, t in edges: indeg[t] += 1
    roots = sorted([n for n, d in indeg.items() if d == 0]) or [edges[0][0]]

    # Pyvis network
    net = Network(height="1200px", width="100%", directed=True, bgcolor="#ffffff")
    net.set_options(json.dumps({
        "layout": {"hierarchical": {
            "enabled": True, "direction": "UD",
            "nodeSpacing": 170, "levelSeparation": 140}},
        "physics": {"hierarchicalRepulsion": {"nodeDistance": 160}},
        "edges": {"arrows": "to",
                  "smooth": {"type": "cubicBezier", "forceDirection": "vertical"}}
    }))

    # add nodes (hidden except root chain) with class colours
    for node in {x for e in edges for x in e}:
        cls   = node_classes.get(node, "")
        color = class_defs.get(cls, "#E6D6FF")
        net.add_node(
            node,
            label = html.escape(labels.get(node, node)),
            shape = shapes.get(node, "box"),
            color = color,
            hidden= node not in roots,
        )

    # edges
    for s, t in edges:
        net.add_edge(s, t, hidden=(t not in roots))

    html_path = Path("exwwb910_interactive.html")
    net.save_graph(str(html_path))

    # ── inject scrollbars + legend + collapse script ──────────────────────────
    legend_items = [
        (class_defs[c], {"start":"Start / End",
                         "process":"Process",
                         "decision":"Decision",
                         "error":"Error"}.get(c, c.capitalize()))
        for c in class_defs
    ]
    legend_html = "<div id='legend' style='font:14px sans-serif;margin:8px;'>"
    legend_html += "<b>Legend</b><br>"
    for col, txt in legend_items:
        legend_html += (
            f"<span style='display:inline-flex;align-items:center;margin:4px 10px 4px 0'>"
            f"<span style='display:inline-block;width:16px;height:16px;background:{col};"
            f"border:1px solid #333;margin-right:6px'></span>{txt}</span>")
    legend_html += "</div>"

    adjacency = {}
    for s, t in edges: adjacency.setdefault(s, []).append(t)

    script = f"""
    <script>
      const adjacency = {json.dumps(adjacency)};
      const shown = new Set({json.dumps(roots)});
      network.on("click", p => {{
         if(p.nodes.length!==1) return;
         const n = p.nodes[0], kids = adjacency[n] || [];
         const show = kids.length && nodes.get(kids[0]).hidden;
         kids.forEach(c=>{{
             nodes.update({{id:c,hidden:!show}});
             edges.update({{from:n,to:c,hidden:!show}});
         }});
      }});
    </script>"""

    css = "<style>#mynetwork{overflow:auto;}</style>"

    # patch the file
    html_txt = html_path.read_text(encoding="utf-8")
    html_txt = html_txt.replace("<head>", "<head>" + css, 1)
    html_txt = html_txt.replace("<body>", "<body>" + legend_html, 1)
    html_txt += script
    html_path.write_text(html_txt, encoding="utf-8")

    print("✔  exwwb910_interactive.html created (scrollable, colour-coded)")

# ──────────────────────────────────────────────────────────────────────────────
class MermaidDiagramRenderer:
    def __init__(self, mermaid_path):
        self.mermaid_path = Path(mermaid_path)
        self.base_name = self.mermaid_path.stem.replace('_control_flow', '')
        self.svg_path = self.mermaid_path.with_name(f"{self.base_name}.svg")
        self.html_path = self.mermaid_path.with_name(f"{self.base_name}_interactive.html")
        self.mermaid_text = self.extract_mermaid_code()

    def extract_mermaid_code(self):
        """
        Extracts the first Mermaid code block from a Markdown file.
        Returns the Mermaid code as a string, or raises ValueError if not found.
        """
        text = self.mermaid_path.read_text(encoding="utf-8")
        in_block = False
        lines = []
        for line in text.splitlines():
            if line.strip().startswith('```mermaid'):
                in_block = True
                continue
            if in_block:
                if line.strip().startswith('```'):
                    break
                lines.append(line)
        if not lines:
            raise ValueError(f"No Mermaid code block found in {self.mermaid_path}")
        return '\n'.join(lines)

    def build_svg(self):
        Path(f"{self.base_name}.mmd").write_text(self.mermaid_text, encoding="utf-8")
        subprocess.run([
            "mmdc", "-i", f"{self.base_name}.mmd", "-o", str(self.svg_path), "-b", "transparent"
        ], check=True)
        print(f"✔  {self.svg_path} created")

    def build_interactive(self):
        import html, json, re
        from pyvis.network import Network
        import networkx as nx
        # --- parsing logic (copied from previous parse_mermaid) ---
        EDGE_RE  = re.compile(r'^\s*([\w\d_]+)\s*-->\s*([\w\d_]+)')
        NODE_RE  = re.compile(r'^\s*([\w\d_]+)\s*(\(\[|\[\(|\(\(|\[\[|\[|\{|\()([^\]\)\}]+)')
        CLASSDEF_RE = re.compile(r'^\s*classDef\s+(\w+).*?fill:\s*(#[0-9a-fA-F]{3,6})')
        CLASS_RE    = re.compile(r'^\s*class\s+([^ ]+)\s+(\w+)')
        SHAPE_MAP = {"(": "ellipse", "((": "circle", "([": "ellipse", "[": "box", "{": "diamond"}
        edges, labels, shapes = [], {}, {}
        class_defs, node_classes = {}, {}
        for ln in self.mermaid_text.splitlines():
            if (m := EDGE_RE.match(ln)):
                edges.append(m.groups())
            if (m := NODE_RE.match(ln)):
                node, opener, label = m.groups()
                labels[node]  = label.strip()
                shapes[node]  = SHAPE_MAP.get(opener[:2], "box")
            if (m := CLASSDEF_RE.match(ln)):
                class_defs[m.group(1)] = m.group(2)
            if (m := CLASS_RE.match(ln)):
                for n in m.group(1).split(","):
                    node_classes[n.strip()] = m.group(2)
        indeg = {n: 0 for n in {x for e in edges for x in e}}
        for _, t in edges: indeg[t] += 1
        roots = sorted([n for n, d in indeg.items() if d == 0]) or [edges[0][0]]
        net = Network(height="1200px", width="100%", directed=True, bgcolor="#ffffff")
        net.set_options(json.dumps({
            "layout": {"hierarchical": {
                "enabled": True, "direction": "UD",
                "nodeSpacing": 170, "levelSeparation": 140}},
            "physics": {"hierarchicalRepulsion": {"nodeDistance": 160}},
            "edges": {"arrows": "to",
                      "smooth": {"type": "cubicBezier", "forceDirection": "vertical"}}
        }))
        for node in {x for e in edges for x in e}:
            cls   = node_classes.get(node, "")
            color = class_defs.get(cls, "#E6D6FF")
            net.add_node(
                node,
                label = html.escape(labels.get(node, node)),
                shape = shapes.get(node, "box"),
                color = color,
                hidden= node not in roots,
            )
        for s, t in edges:
            net.add_edge(s, t, hidden=(t not in roots))
        net.save_graph(str(self.html_path))
        # --- inject scrollbars + legend + collapse script ---
        legend_items = [
            (class_defs[c], {"start":"Start / End",
                             "process":"Process",
                             "decision":"Decision",
                             "error":"Error"}.get(c, c.capitalize()))
            for c in class_defs
        ]
        legend_html = "<div id='legend' style='font:14px sans-serif;margin:8px;'>"
        legend_html += "<b>Legend</b><br>"
        for col, txt in legend_items:
            legend_html += (
                f"<span style='display:inline-flex;align-items:center;margin:4px 10px 4px 0'>"
                f"<span style='display:inline-block;width:16px;height:16px;background:{col};"
                f"border:1px solid #333;margin-right:6px'></span>{txt}</span>")
        legend_html += "</div>"
        adjacency = {}
        for s, t in edges: adjacency.setdefault(s, []).append(t)
        script = f"""
        <script>
          const adjacency = {json.dumps(adjacency)};
          const shown = new Set({json.dumps(roots)});
          network.on("click", p => {{
             if(p.nodes.length!==1) return;
             const n = p.nodes[0], kids = adjacency[n] || [];
             const show = kids.length && nodes.get(kids[0]).hidden;
             kids.forEach(c=>{{
                 nodes.update({{id:c,hidden:!show}});
                 edges.update({{from:n,to:c,hidden:!show}});
             }});
          }});
        </script>"""
        css = "<style>#mynetwork{overflow:auto;}</style>"
        html_txt = self.html_path.read_text(encoding="utf-8")
        html_txt = html_txt.replace("<head>", "<head>" + css, 1)
        html_txt = html_txt.replace("<body>", "<body>" + legend_html, 1)
        html_txt += script
        self.html_path.write_text(html_txt, encoding="utf-8")
        print(f"✔  {self.html_path} created (scrollable, colour-coded)")

if __name__ == "__main__":
    # Find all _control_flow.md files in Kanav_diagram subfolders
    for md_path in glob.glob("Kanav_diagram/*/*_control_flow.md"):
        print(f"Processing {md_path}")
        renderer = MermaidDiagramRenderer(md_path)
        renderer.build_svg()
        renderer.build_interactive()
