#!/usr/bin/env python
"""
render_exwwb910.py
▪ Creates a static SVG with Mermaid CLI   → exwwb910.svg
▪ Builds an interactive, click-to-expand HTML (vis.js) → exwwb910_interactive.html
"""

import html
import json
import re
import subprocess
import textwrap
from pathlib import Path

import networkx as nx
from pyvis.network import Network

# ──────────────────────────────────────────────────────────────────────────────
# Mermaid diagram text -- drop your own flow-chart here
# ──────────────────────────────────────────────────────────────────────────────
MERMAID = textwrap.dedent(r"""
flowchart TD
    %% Main Program Flow
    Start([Program Start]) --> Init[0000P-MAINLINE]
    Init --> InitPhase[0100I-INITIALIZATION]

    %% Initialization Phase

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
# 1.  Build a STATIC SVG via Mermaid-CLI
# ──────────────────────────────────────────────────────────────────────────────
def build_svg():
    mmd_file = Path("exwwb910.mmd")
    svg_file = Path("exwwb910.svg")

    mmd_file.write_text(MERMAID, encoding="utf-8")
    subprocess.run(
        ["mmdc", "-i", str(mmd_file), "-o", str(svg_file), "-b", "transparent"],
        check=True,
    )
    print(f"✔  Static diagram → {svg_file.resolve()}")


# ──────────────────────────────────────────────────────────────────────────────
# 2.  Build an INTERACTIVE, COLLAPSIBLE HTML (vis.js via Pyvis)
# ──────────────────────────────────────────────────────────────────────────────
EDGE_RE = re.compile(r'^\s*([\w\d_]+)\s*-->\s*([\w\d_]+)')
NODE_RE = re.compile(
    r"""^\s*([\w\d_]+)\s*            # id
         (\(\[|\[\(|\(\(|\[\[|\[|\{|\()  # opener token
         ([^\]\)\}]+?)                # label text
         (?:\]\)|\)\)|\]\]|\]|\}|\))   # closer
    """,
    re.X,
)

SHAPE_MAP = {  # pick a vis.js shape based on the Mermaid opener
    "(": "ellipse",
    "((": "circle",
    "([": "ellipse",
    "[(": "ellipse",
    "[": "box",
    "{": "diamond",
}


def parse_mermaid(text: str):
    """Return (edges, node_labels, node_shapes)."""
    edges, labels, shapes = [], {}, {}
    for ln in text.splitlines():
        if (m := EDGE_RE.match(ln)):
            edges.append(m.groups())
        if (n := NODE_RE.match(ln)):
            node_id, opener, label = n.group(1), n.group(2), n.group(3).strip()
            labels[node_id] = label
            shapes[node_id] = SHAPE_MAP.get(opener[:2], "box")
    return edges, labels, shapes


def build_interactive():
    edges, labels, shapes = parse_mermaid(MERMAID)

    # roots = nodes whose in-degree is 0
    indeg = {n: 0 for n in {x for e in edges for x in e}}
    for _, tgt in edges:
        indeg[tgt] += 1
    roots = sorted([n for n, d in indeg.items() if d == 0]) or [edges[0][0]]

    G = nx.DiGraph(edges)

    net = Network(height="850px", width="100%", directed=True, bgcolor="#ffffff")

    # -----------  FIX: give set_options real JSON  -----------
    options = {
        "layout": {
            "hierarchical": {
                "enabled": True,
                "direction": "UD",
                "nodeSpacing": 170,
                "levelSeparation": 140,
            }
        },
        "physics": {
            "hierarchicalRepulsion": {"nodeDistance": 160}
        },
        "edges": {
            "arrows": "to",
            "smooth": {"type": "cubicBezier", "forceDirection": "vertical"},
        },
    }
    net.set_options(json.dumps(options, separators=(",", ":")))
    # ----------------------------------------------------------

    # add nodes (hidden except root chain)
    for n in G.nodes():
        net.add_node(
            n,
            label=html.escape(labels.get(n, n)),
            shape=shapes.get(n, "box"),
            color="#BBF" if shapes.get(n) == "diamond"
            else "#BFB" if n in roots
            else "#E6D6FF",
            hidden=n not in roots,
        )

    # add edges (hidden if target is hidden)
    for s, t in G.edges():
        net.add_edge(s, t, hidden=(t not in roots))

    html_path = Path("exwwb910_interactive.html")
    net.save_graph(str(html_path))

    # JS for click-to-expand / collapse
    adjacency = {}
    for s, t in edges:
        adjacency.setdefault(s, []).append(t)

    toggle_js = f"""
    <script>
      const adjacency = {json.dumps(adjacency)};
      const shown = new Set({json.dumps(roots)});
      network.on("click", p => {{
        if(p.nodes.length!==1) return;
        const n=p.nodes[0], kids=adjacency[n]||[];
        const show = kids.length && nodes.get(kids[0]).hidden;
        kids.forEach(c=>{{
           nodes.update({{id:c,hidden:!show}});
           edges.update({{from:n,to:c,hidden:!show}});
        }});
      }});
    </script>"""
    with html_path.open("a", encoding="utf-8") as fp:
        fp.write(toggle_js)

    print(f"✔  Interactive diagram → {html_path.resolve()}")


# ──────────────────────────────────────────────────────────────────────────────
# MAIN
# ──────────────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    build_svg()
    build_interactive()
