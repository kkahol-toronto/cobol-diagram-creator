# EXWWB910 Control Flow Diagram

```mermaid
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
    class ProcessProducer,MoreMEXW001,MoreSaleChk,MoreProducers,Checkpoint decision
    class Abend,WriteAbendMsg,Rollback,CoreDump error
    class Init,InitPhase,MainLoop,ProcessVehicle,Conclusion process
```

## Key Components

1. **Initialization Phase**
   - Opens AUDIT-FILE
   - Gets current timestamp
   - Initializes checkpoint/restart
   - Initializes working storage
   - Gets/updates batch number
   - Reads first SYSPARM record
   - Writes HUB/VINCENT headers

2. **Main Processing Loop**
   - Processes each producer from SYSPARM
   - Opens MEXW001_CSR for current model year vehicles
   - Opens SALE_CHK_CSR for older sold vehicles
   - Processes each vehicle record
   - Handles error conditions

3. **Vehicle Processing**
   - Gets WERS data
   - Gets dealer data
   - Gets retail data
   - Gets wholesale data
   - Gets WERS details
   - Gets status dates
   - Writes VINCENT record
   - Performs checkpoint if needed

4. **Conclusion Phase**
   - Updates run timestamp
   - Updates batch number
   - Writes trailers
   - Writes audit statistics
   - Sets return code

5. **Error Handling**
   - Handles DB2 errors
   - Handles IMS errors
   - Performs rollback
   - Calls COREDUMP
   - Writes abend messages

## Database Interactions

1. **Main Cursors**
   - MEXW001_CSR: Current model year vehicles
   - SALE_CHK_CSR: Older sold vehicles
   - MEXW031_CSR: Body style information
   - MEXW003_40V_CSR: Wholesale dealer information

2. **Key Tables**
   - MEXW001_VEH_ORDER: Vehicle order information
   - MEXW003_VEH_STATUS: Vehicle status information
   - MEXW004_VEH_WERS_STRING: WERS string data
   - MEXW007_VEH_WHS: Wholesale information
   - MEXW008_VEH_RTL: Retail information
   - MEXW027_CONV: Status code conversion
   - MEXW031_CATMAP: Body style mapping
   - MEXW032_CATALOG: Catalog information
   - MEXW033_BODY_TYPE: Body type information
   - MEXW034_VL_BRAND: Vehicle line/brand information
   - MEXW035_DLR_MSTR: Dealer master information
   - MEXS016_GENERIC2: Generic table for timestamps/batch numbers