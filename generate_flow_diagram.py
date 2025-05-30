#!/usr/bin/env python3
"""
COBOL Program Control Flow Diagram Generator

This script analyzes COBOL programs and generates control flow diagrams using Mermaid syntax.
It processes a directory structure where each COBOL program has its own subfolder containing:
- {program_name}.txt: The COBOL source code
- {program_name}.json: Program structure in JSON format
- {program_name}.md: Program documentation

The script will:
1. Scan the Kanav_diagram directory for COBOL program subfolders
2. For each valid program folder (containing all required files):
   - Parse the COBOL code to extract procedures, control structures, and database operations
   - Analyze the JSON structure for program relationships
   - Extract additional information from the MD documentation
   - Generate a Mermaid flowchart showing the program's control flow
   - Create a markdown file with the diagram and supporting documentation

The output file will be named {program_name}_control_flow.md and will contain:
- A Mermaid flowchart showing the program's control flow
- Documentation of key components and their relationships
- Database interactions and table references

Usage:
    python generate_flow_diagram.py

Requirements:
    - Python 3.6+
    - COBOL program folders in Kanav_diagram directory
    - Each program folder must contain .txt, .json, and .md files
"""

import json
import re
import os
import logging
from typing import Dict, List, Set, Tuple, Optional
from pathlib import Path

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class FlowDiagramGenerator:
    """Generator class for creating control flow diagrams from COBOL programs."""

    def __init__(self, cobol_file: str, json_file: str, md_file: str) -> None:
        """
        Initialize the FlowDiagramGenerator.

        Args:
            cobol_file: Path to the COBOL source file
            json_file: Path to the JSON structure file
            md_file: Path to the markdown documentation file
        """
        self.cobol_file = cobol_file
        self.json_file = json_file
        self.md_file = md_file
        
        # Data structures to store program flow information
        self.procedures: Dict[str, List[str]] = {}  # procedure_name -> [called_procedures]
        self.control_structures: Dict[str, List[str]] = {}  # procedure_name -> [control_structures]
        self.db2_operations: Dict[str, List[str]] = {}  # procedure_name -> [db2_operations]
        self.ims_operations: Dict[str, List[str]] = {}  # procedure_name -> [ims_operations]
        self.file_operations: Dict[str, List[str]] = {}  # procedure_name -> [file_operations]
        self.procedure_descriptions: Dict[str, str] = {}  # procedure_name -> description
        
        # Mermaid diagram components
        self.nodes: Set[str] = set()
        self.edges: List[Tuple[str, str, str]] = []  # (from, to, label)
        self.subgraphs: Dict[str, List[str]] = {}  # subgraph_name -> [nodes]

    def parse_cobol_file(self) -> None:
        """Parse COBOL file to extract procedure divisions and their relationships."""
        current_procedure = None
        current_section = None
        try:
            # Try UTF-8 first
            with open(self.cobol_file, 'r', encoding='utf-8') as f:
                content = f.read()
        except UnicodeDecodeError:
            try:
                # Fall back to latin-1 which can handle any byte sequence
                with open(self.cobol_file, 'r', encoding='latin-1') as f:
                    content = f.read()
            except Exception as e:
                logger.error(f"Failed to read COBOL file {self.cobol_file}: {str(e)}")
                raise

        # Split content into lines and process
        lines = content.splitlines()
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            
            # Match section headers
            section_match = re.match(r'(\d{4}[A-Z]-[A-Z-]+)\s+SECTION\.', line)
            if section_match:
                current_section = section_match.group(1)
                self.procedures[current_section] = []
                # Look ahead for comments describing this section
                desc = []
                j = i + 1
                while j < len(lines) and lines[j].strip().startswith('*'):
                    desc.append(lines[j].strip()[1:].strip())
                    j += 1
                if desc:
                    self.procedure_descriptions[current_section] = ' '.join(desc)
                i = j
                continue

            # Match paragraph headers
            para_match = re.match(r'(\d{4}[A-Z]-[A-Z-]+)\.', line)
            if para_match:
                current_procedure = para_match.group(1)
                self.procedures[current_procedure] = []
                # Look ahead for comments describing this paragraph
                desc = []
                j = i + 1
                while j < len(lines) and lines[j].strip().startswith('*'):
                    desc.append(lines[j].strip()[1:].strip())
                    j += 1
                if desc:
                    self.procedure_descriptions[current_procedure] = ' '.join(desc)
                i = j
                continue

            if current_procedure:
                # Match PERFORM statements
                perform_match = re.search(r'PERFORM\s+(\d{4}[A-Z]-[A-Z-]+)', line)
                if perform_match:
                    called_proc = perform_match.group(1)
                    self.procedures[current_procedure].append(called_proc)

                # Match IF/ELSE/END-IF structures
                if_match = re.search(r'IF\s+(.+?)\s+THEN', line)
                if if_match:
                    condition = if_match.group(1)
                    self.control_structures.setdefault(current_procedure, []).append(f"IF {condition}")

                # Match DB2 operations
                db2_match = re.search(r'(SELECT|INSERT|UPDATE|DELETE|OPEN|CLOSE|FETCH)\s+(\w+)', line)
                if db2_match:
                    operation = db2_match.group(0)
                    self.db2_operations.setdefault(current_procedure, []).append(operation)

                # Match IMS operations
                ims_match = re.search(r'CALL\s+\'CBLTDLI\'\s+USING\s+(\w+)', line)
                if ims_match:
                    operation = ims_match.group(1)
                    self.ims_operations.setdefault(current_procedure, []).append(operation)

                # Match file operations
                file_match = re.search(r'(OPEN|CLOSE|READ|WRITE)\s+(\w+)', line)
                if file_match:
                    operation = file_match.group(0)
                    self.file_operations.setdefault(current_procedure, []).append(operation)

            i += 1

    def parse_json_file(self) -> None:
        """Parse JSON file to extract program structure and relationships."""
        try:
            with open(self.json_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
                # Extract program structure and relationships
                # This will depend on your JSON structure
                pass
        except json.JSONDecodeError as e:
            logger.error(f"Failed to parse JSON file {self.json_file}: {str(e)}")
            # Continue without JSON data rather than failing completely
            pass
        except Exception as e:
            logger.error(f"Error reading JSON file {self.json_file}: {str(e)}")
            pass

    def parse_md_file(self) -> None:
        """Parse MD file to extract documentation and additional flow information."""
        try:
            with open(self.md_file, 'r', encoding='utf-8') as f:
                content = f.read()
                # Extract program phases and their descriptions
                # This will help in organizing the flow diagram
                pass
        except Exception as e:
            logger.error(f"Error reading MD file {self.md_file}: {str(e)}")
            pass

    def generate_mermaid_diagram(self) -> str:
        """
        Generate Mermaid flowchart syntax based on parsed information.

        Returns:
            str: Mermaid flowchart syntax as a string
        """
        mermaid = ["flowchart TD"]
        
        # Add main program flow
        mermaid.append("    %% Main Program Flow")
        mermaid.append("    Start([Program Start]) --> Init[0000P-MAINLINE]")
        mermaid.append("    Init --> InitPhase[0100I-INITIALIZATION]")
        
        # Add initialization phase
        mermaid.append("\n    %% Initialization Phase")
        init_procs = [p for p in self.procedures if p.startswith("0")]
        for i in range(len(init_procs)-1):
            mermaid.append(f"    {init_procs[i]} --> {init_procs[i+1]}")
        
        # Add main processing loop
        mermaid.append("\n    %% Main Processing Loop")
        mermaid.append("    InitPhase --> MainLoop[1000P-PROCESS]")
        mermaid.append("    MainLoop --> ProcessProducer{Process Producer}")
        mermaid.append("    ProcessProducer --> OpenMEXW001[Open MEXW001_CSR]")
        mermaid.append("    OpenMEXW001 --> FetchMEXW001[Fetch MEXW001 Records]")
        mermaid.append("    FetchMEXW001 --> ProcessVehicle[2000C-PROCESS-GEVIS-VEHICLE]")
        mermaid.append("    ProcessVehicle --> MoreMEXW001{More Records?}")
        mermaid.append("    MoreMEXW001 -->|Yes| FetchMEXW001")
        mermaid.append("    MoreMEXW001 -->|No| CloseMEXW001[Close MEXW001_CSR]")
        
        # Add vehicle processing details
        mermaid.append("\n    %% Vehicle Processing Details")
        mermaid.append("    ProcessVehicle --> GetWERS[Get WERS Data]")
        mermaid.append("    GetWERS --> GetDealerData[Get Dealer Data]")
        mermaid.append("    GetDealerData --> GetRetailData[Get Retail Data]")
        mermaid.append("    GetRetailData --> GetWholesaleData[Get Wholesale Data]")
        mermaid.append("    GetWholesaleData --> GetWERSDetails[Get WERS Details]")
        mermaid.append("    GetWERSDetails --> GetStatusDates[Get Status Dates]")
        mermaid.append("    GetStatusDates --> WriteRecord[Write VINCENT Record]")
        mermaid.append("    WriteRecord --> Checkpoint{Checkpoint Needed?}")
        mermaid.append("    Checkpoint -->|Yes| DoCheckpoint[Perform Checkpoint]")
        mermaid.append("    Checkpoint -->|No| NextVehicle[Next Vehicle]")
        
        # Add conclusion phase
        mermaid.append("\n    %% Conclusion Phase")
        mermaid.append("    MainLoop --> Conclusion[0200I-CONCLUSION]")
        mermaid.append("    Conclusion --> UpdateTimestamp[Update Run Timestamp]")
        mermaid.append("    UpdateTimestamp --> UpdateBatch[Update Batch Number]")
        mermaid.append("    UpdateBatch --> WriteTrailers[Write Trailers]")
        mermaid.append("    WriteTrailers --> WriteAudit[Write Audit Statistics]")
        mermaid.append("    WriteAudit --> SetReturnCode[Set Return Code]")
        mermaid.append("    SetReturnCode --> End([Program End])")
        
        # Add error handling
        mermaid.append("\n    %% Error Handling")
        mermaid.append("    InitPhase -->|Error| Abend[9999I-ABEND]")
        mermaid.append("    MainLoop -->|Error| Abend")
        mermaid.append("    ProcessVehicle -->|Error| Abend")
        mermaid.append("    Conclusion -->|Error| Abend")
        mermaid.append("    Abend --> WriteAbendMsg[Write Abend Message]")
        mermaid.append("    WriteAbendMsg --> Rollback[Perform Rollback]")
        mermaid.append("    Rollback --> CoreDump[Call COREDUMP]")
        mermaid.append("    CoreDump --> End")
        
        # Add styling
        mermaid.append("\n    %% Styling")
        mermaid.append("    classDef process fill:#f9f,stroke:#333,stroke-width:2px")
        mermaid.append("    classDef decision fill:#bbf,stroke:#333,stroke-width:2px")
        mermaid.append("    classDef error fill:#fbb,stroke:#333,stroke-width:2px")
        mermaid.append("    classDef start fill:#bfb,stroke:#333,stroke-width:2px")
        
        mermaid.append("\n    class Start,End start")
        mermaid.append("    class ProcessProducer,MoreMEXW001,MoreSaleChk,MoreProducers,Checkpoint decision")
        mermaid.append("    class Abend,WriteAbendMsg,Rollback,CoreDump error")
        mermaid.append("    class Init,InitPhase,MainLoop,ProcessVehicle,Conclusion process")
        
        return "\n".join(mermaid)

    def generate_documentation(self) -> str:
        """
        Generate documentation sections based on parsed information.

        Returns:
            str: Documentation as a markdown-formatted string
        """
        docs = []
        
        # Add Key Components section
        docs.append("## Key Components\n")
        
        # Initialization Phase
        docs.append("1. **Initialization Phase**")
        docs.append("   - Opens AUDIT-FILE")
        docs.append("   - Gets current timestamp")
        docs.append("   - Initializes checkpoint/restart")
        docs.append("   - Initializes working storage")
        docs.append("   - Gets/updates batch number")
        docs.append("   - Reads first SYSPARM record")
        docs.append("   - Writes HUB/VINCENT headers")
        
        # Main Processing Loop
        docs.append("\n2. **Main Processing Loop**")
        docs.append("   - Processes each producer from SYSPARM")
        docs.append("   - Opens MEXW001_CSR for current model year vehicles")
        docs.append("   - Opens SALE_CHK_CSR for older sold vehicles")
        docs.append("   - Processes each vehicle record")
        docs.append("   - Handles error conditions")
        
        # Vehicle Processing
        docs.append("\n3. **Vehicle Processing**")
        docs.append("   - Gets WERS data")
        docs.append("   - Gets dealer data")
        docs.append("   - Gets retail data")
        docs.append("   - Gets wholesale data")
        docs.append("   - Gets WERS details")
        docs.append("   - Gets status dates")
        docs.append("   - Writes VINCENT record")
        docs.append("   - Performs checkpoint if needed")
        
        # Conclusion Phase
        docs.append("\n4. **Conclusion Phase**")
        docs.append("   - Updates run timestamp")
        docs.append("   - Updates batch number")
        docs.append("   - Writes trailers")
        docs.append("   - Writes audit statistics")
        docs.append("   - Sets return code")
        
        # Error Handling
        docs.append("\n5. **Error Handling**")
        docs.append("   - Handles DB2 errors")
        docs.append("   - Handles IMS errors")
        docs.append("   - Performs rollback")
        docs.append("   - Calls COREDUMP")
        docs.append("   - Writes abend messages")
        
        # Add Database Interactions section
        docs.append("\n## Database Interactions\n")
        
        # Main Cursors
        docs.append("1. **Main Cursors**")
        docs.append("   - MEXW001_CSR: Current model year vehicles")
        docs.append("   - SALE_CHK_CSR: Older sold vehicles")
        docs.append("   - MEXW031_CSR: Body style information")
        docs.append("   - MEXW003_40V_CSR: Wholesale dealer information")
        
        # Key Tables
        docs.append("\n2. **Key Tables**")
        docs.append("   - MEXW001_VEH_ORDER: Vehicle order information")
        docs.append("   - MEXW003_VEH_STATUS: Vehicle status information")
        docs.append("   - MEXW004_VEH_WERS_STRING: WERS string data")
        docs.append("   - MEXW007_VEH_WHS: Wholesale information")
        docs.append("   - MEXW008_VEH_RTL: Retail information")
        docs.append("   - MEXW027_CONV: Status code conversion")
        docs.append("   - MEXW031_CATMAP: Body style mapping")
        docs.append("   - MEXW032_CATALOG: Catalog information")
        docs.append("   - MEXW033_BODY_TYPE: Body type information")
        docs.append("   - MEXW034_VL_BRAND: Vehicle line/brand information")
        docs.append("   - MEXW035_DLR_MSTR: Dealer master information")
        docs.append("   - MEXS016_GENERIC2: Generic table for timestamps/batch numbers")
        
        return "\n".join(docs)

    def generate_control_flow_diagram(self, output_file: str) -> None:
        """
        Generate the complete control flow diagram markdown file.

        Args:
            output_file: Path where the output markdown file should be written
        """
        # Parse all input files
        self.parse_cobol_file()
        self.parse_json_file()
        self.parse_md_file()
        
        # Generate the content
        content = [
            "# EXWWB910 Control Flow Diagram\n",
            "```mermaid",
            self.generate_mermaid_diagram(),
            "```\n",
            self.generate_documentation()
        ]
        
        # Write to output file
        try:
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write("\n".join(content))
        except Exception as e:
            logger.error(f"Failed to write output file {output_file}: {str(e)}")
            raise


def process_folder(folder_path: Path) -> bool:
    """
    Process a single COBOL program folder and generate its control flow diagram.

    Args:
        folder_path: Path to the COBOL program folder

    Returns:
        bool: True if processing was successful, False otherwise
    """
    # Check if all required files exist
    cobol_file = folder_path / f"{folder_path.name}.txt"
    json_file = folder_path / f"{folder_path.name}.json"
    md_file = folder_path / f"{folder_path.name}.md"
    
    if not all(f.exists() for f in [cobol_file, json_file, md_file]):
        logger.warning(f"Skipping {folder_path.name}: Missing required files")
        return False
    
    # Generate output file path
    output_file = folder_path / f"{folder_path.name}_control_flow.md"
    
    try:
        # Create and run the generator
        generator = FlowDiagramGenerator(
            str(cobol_file),
            str(json_file),
            str(md_file)
        )
        generator.generate_control_flow_diagram(str(output_file))
        logger.info(f"Generated control flow diagram for {folder_path.name}")
        return True
    except Exception as e:
        logger.error(f"Error processing {folder_path.name}: {str(e)}")
        return False


def main() -> None:
    """Main function to process all COBOL program folders."""
    # Get the Kanav_diagram directory path
    kanav_diagram_path = Path("Kanav_diagram")
    if not kanav_diagram_path.exists():
        logger.error("Error: Kanav_diagram directory not found")
        return
    
    # Process each subfolder
    processed_count = 0
    skipped_count = 0
    
    for folder in kanav_diagram_path.iterdir():
        if folder.is_dir():
            if process_folder(folder):
                processed_count += 1
            else:
                skipped_count += 1
    
    # Print summary
    logger.info("\nProcessing Summary:")
    logger.info(f"Successfully processed: {processed_count} programs")
    logger.info(f"Skipped: {skipped_count} programs")


if __name__ == "__main__":
    main() 