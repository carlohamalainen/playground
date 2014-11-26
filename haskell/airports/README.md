## Building

Use a sandbox:

    ./build_in_sandbox.sh

## Running:

Say you have 16 cores:

    .cabal-sandbox/bin/airports +RTS -N16 > output.txt
    cat output.txt | sort | uniq          > output_sorted_uniq.txt

Grids are output one per line, e.g.

    AACDENLPB

corresponds to the grid

    AAC
    DEN
    LPB

Sample grids (as at 2014-11-26-1153):

    AAC
    DEN
    LPB

    AKL
    MBE
    MVD

    AMM
    KBV
    LED

    ATL
    MSE
    MVD

    ATL
    MUC
    MKY

    BCN
    WNR
    NST

    BHN
    COR
    NRT

    BLT
    MEX
    ADL

    BMA
    LED
    TXL

    BWN
    CNS
    NRT

    CNC
    NRT
    STL

    FRA
    ROK
    ATL

    HBA
    BLK
    ATL

    LAS
    PMO
    BMI

    SCL
    JOE
    ORD

    SCL
    UDG
    LGW

    SLC
    CPH
    LBC

    SLC
    JED
    UDG

    SLC
    LED
    UDG

    STL
    CHG
    LGW

    TSV
    WIC
    BNE

    TWB
    SIN
    VCE

    TWB
    XNN
    LRE

    TXL
    WNR
    BNE
