       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROGRAM2.
       AUTHOR.        MATTHEW-WILLIS.
      ***************************************************************
      * This program reads data from an an external file called
      * inventory-totals and outputs a summary report showing the quanit
      * of medications and the cost of all the medications and it is
      * sorted by product name.
      *
      *
      * INPUT: Inventory-totals "PR2FA17.TXT". Which contains:
      * Customer-ID,CUSTOMER-NAME, PROD-ID, PROD-NAME, Qty-SOLD,
      * COST-PER-ITEM.
      *
      *
      * OUTPUT: Report-file "INVENTORY-OUT.TXT" which has:
      * customer name product-id, product-name,qty-sold, sales-vale,
      *  total: ,total amount sold, and total value of sales.
      *
      *
      * Calculations: Total QTY sold, total sales value,
      * Total qty sold overall, total value of sales overall
      *
      *
      *
      *
      *
      ***************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT INVENTORY-TOTALS
           ASSIGN TO 'PR2FA17.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REPORT-FILE
           ASSIGN TO 'INVENTORY-OUT.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD  INVENTORY-TOTALS.
       01  INVENTORY-REC.
           05 CUS-ID-IN        PIC 9(5).
           05 CUS-NAME-IN      PIC X(25).
           05 PROD-ID-IN       PIC X(3).
           05 FILLER           PIC X(5).
           05 PROD-NAME-IN     PIC X(14).
           05 QTY-SOLD-IN      PIC 9(3).
           05 COST-PER-ITEM-IN PIC 9(3)V9(2).


       FD  REPORT-FILE.
       01  REPORT-REC.
           05 PIC X(73).


       WORKING-STORAGE SECTION.
       01  WS-WORK-AREAS.
           05 EOF-FLAG         PIC X(3) VALUE 'YES'.
           05 PROPER-SPACING   PIC 99 VALUE 0.
           05 PROD-NAME-SAME PIC X(3) VALUE 'NEW'.
       01  WS-CURRENT-DATE-DATA.
             05  WS-CURRENT-DATE.
                   10 WS-CURRENT-YEAR      PIC 9(4).
                   10 WS-CURRENT-MONTH     PIC 9(2).
                   10 WS-CURRENT-DAY       PIC 9(2).

       01  WS-TEMP-VARIABLES.
           05 WS-SALES-VALUE           PIC 9(6)V9(2).
           05 WS-GRP-QTY               PIC 9(6).
           05 WS-QTY-HOLDER            PIC 9(7).
           05 WS-SV-HOLDER             PIC 9(10)V9(2).
           05 WS-GRP-SV                PIC 9(7)V9(2).
           05 WS-GRAND-QTY             PIC 9(7).
           05 WS-GRAND-SV              PIC 9(10)V9(2).

       01  HEADING-LINE1.
           05 FILLER   PIC X(33).
           05 CLIENT-NAME  PIC X(8) VALUE "DR.CHEEB".
           05 FILLER   PIC X(34).

       01  HEADING-LINE2.
           05 FILLER               PIC X(9).
           05 HL2-CURRENT-MONTH    PIC X(2).
           05 FILLER               PIC X(1) VALUE '/'.
           05 HL2-CURRENT-DAY      PIC X(2).
           05 FILLER               PIC X(1) VALUE '/'.
           05 HL2-CURRENT-YEAR     PIC X(4).
           05 FILLER               PIC X(7).
           05 HL2-SALES            PIC X(6) VALUE 'SALES '.
           05 HL2-SPEC             PIC X(12) VALUE 'SPECULATION '.
           05 HL2-REP              PIC X(6) VALUE 'REPORT'.
           05 FILLER               PIC X(18).
           05 HL2-Y3I              PIC X(3) VALUE 'MAW'.

       01  HEADING-LINE3.
           05 FILLER       PIC X(73) VALUE SPACES.

       01  HEADING-LINE4.
           05 FILLER PIC X(73) VALUE SPACES.



       01  HEADING-LINE5.
           05 FILLER           PIC X(17).
           05 HL5-PROD         PIC X(4) VALUE 'PROD'.
           05 FILLER           PIC X(11).
           05 HL5-CUS          PIC X(8) VALUE 'CUSTOMER'.
           05 FILLER           PIC X(13).
           05 HL5-QTY          PIC X(3) VALUE 'QTY'.
           05 FILLER           PIC X(9).
           05 HL5-SALES        PIC X(5) VALUE 'SALES'.
           05 FILLER           PIC X(3).

       01  HEADING-LINE6.
           05 FILLER           PIC X(2).
           05 HL6-PRD-NAME     PIC X(12) VALUE 'PRODUCT NAME'.
           05 FILLER           PIC X(4).
           05 HL6-ID           PIC X(2) VALUE 'ID'.
           05 FILLER           PIC X(14).
           05 HL6-NAME         PIC X(4) VALUE 'NAME'.
           05 FILLER           PIC X(14).
           05 HL6-SOLD         PIC X(4) VALUE 'SOLD'.
           05 FILLER           PIC X(9).
           05 HL6-VALUE        PIC X(5) VALUE 'VALUE'.

       01  HEADING-LINE7.
           05 FILLER PIC X(73) VALUE SPACES.

       01  OUTPUT-LINE.
           05 FILLER           PIC X(1).
           05 PROD-NAME-OUT    PIC X(14).
           05 FILLER           PIC X(2).
           05 PROD-ID-OUT      PIC X(3).
           05 FILLER           PIC X(3).
           05 CUS-NAME-OUT     PIC X(25).
           05 FILLER           PIC X(4).
           05 QTY-SOLD-OUT     PIC Z(1)9(3).
           05 FILLER           PIC X(5).
           05 SALES-VALUE-OUT  PIC Z(3),Z(3).99.

       01  HEADING-LINE8.
           05 FILLER  PIC X(73) VALUE SPACES.

       01  TOTAL-HEADER.
           05 FILLER               PIC X(31).
           05 TOTAL                PIC X(6) VALUE 'TOTAL:'.
           05 FILLER               PIC X(13).
           05 QTY-SOLD-TOTAL       PIC Z(3)9(3).
           05 FILLER               PIC X(2).
           05 MONEY                PIC X(1) VALUE '$'.
           05 SALES-VALUE-TOTAL-OUT   PIC Z(1),Z(3),Z(3).99.

       01  HEADING-LINE9.
           05 FILLER   PIC X(73) VALUE SPACES.

       01  HEADING-LINE10.
           05  FILLER  PIC X(73) VALUE SPACES.

       01  TOTAL-AMUNT-LINE.
           05  FILLER          PIC X(30).
           05  TOTAL           PIC X(6) VALUE 'TOTAL '.
           05  AMOUNT           PIC X(7) VALUE 'AMOUNT '.
           05  SOLD             PIC X(5) VALUE 'SOLD:'.
           05  FILLER           PIC X(14).
           05  TOTAL-AMUNT-SOLD PIC Z(1),Z(3),Z(3).
           05 FILLER           PIC X(2).

       01  TOTAL-VAL-LINE.
           05  FILLER           PIC X(27).
           05  TOTAL            PIC X(6) VALUE 'TOTAL '.
           05  VAL              PIC X(6) VALUE 'VALUE '.
           05  OFD              PIC X(3) VALUE 'OF '.
           05  SALES            PIC X(6) VALUE 'SALES:'.
           05 MONEY             PIC X(1) VALUE '$'.
           05  TOT-VAL-SALES    PIC Z(1),Z(3),Z(3),Z(3).99.
           05  FILLER           PIC X(2).










       PROCEDURE DIVISION.

       100-MAIN-ROUTINE.
           PERFORM 120-HOUSE-KEEPING
           PERFORM 130-READ-FILES
           PERFORM 300-END-ROUTINE
           .

       120-HOUSE-KEEPING.
           OPEN INPUT INVENTORY-TOTALS
               OUTPUT REPORT-FILE

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           MOVE WS-CURRENT-YEAR TO HL2-CURRENT-YEAR
           MOVE WS-CURRENT-MONTH TO HL2-CURRENT-MONTH
           MOVE WS-CURRENT-DAY TO HL2-CURRENT-DAY

           PERFORM 140-HEADER-WRITER


           .
       130-READ-FILES.
              PERFORM UNTIL EOF-FLAG = 'NO'
                      READ INVENTORY-TOTALS
                          AT END
                              MOVE 'NO' TO EOF-FLAG
                              MOVE SPACES TO REPORT-REC
                          NOT AT END
                              PERFORM 200-PROCESS-RTN
                      END-READ
                  END-PERFORM
               .
       140-HEADER-WRITER.
           MOVE HEADING-LINE1 TO REPORT-REC
           WRITE REPORT-REC
           MOVE HEADING-LINE2 TO REPORT-REC
           WRITE REPORT-REC
           MOVE HEADING-LINE3 TO REPORT-REC
           WRITE REPORT-REC
           MOVE HEADING-LINE4 TO REPORT-REC
           WRITE REPORT-REC
           MOVE HEADING-LINE5 TO REPORT-REC
           WRITE REPORT-REC
           MOVE HEADING-LINE6 TO REPORT-REC
           WRITE REPORT-REC
           MOVE HEADING-LINE7 TO REPORT-REC
           WRITE REPORT-REC
           MOVE HEADING-LINE8 TO REPORT-REC
           WRITE REPORT-REC

           .
       150-MOVE-WITHNO-CALC.
           MOVE CUS-NAME-IN TO CUS-NAME-OUT
           MOVE QTY-SOLD-IN TO QTY-SOLD-OUT
           MOVE PROD-ID-IN TO PROD-ID-OUT

           .
       175-PARAGRAPH-LAYOUT.

           COMPUTE WS-SALES-VALUE =
                   COST-PER-ITEM-IN * QTY-SOLD-IN
           MOVE WS-SALES-VALUE TO SALES-VALUE-OUT
           COMPUTE WS-SV-HOLDER=
                   WS-SALES-VALUE + WS-SV-HOLDER
           COMPUTE WS-QTY-HOLDER =
                   WS-QTY-HOLDER + QTY-SOLD-IN
           MOVE WS-SV-HOLDER TO SALES-VALUE-TOTAL-OUT

           COMPUTE WS-GRP-QTY =
                   QTY-SOLD-IN + WS-GRP-QTY
           MOVE WS-GRP-QTY TO QTY-SOLD-TOTAL
           COMPUTE WS-GRP-SV =
                   WS-SALES-VALUE + WS-GRP-SV




           PERFORM 150-MOVE-WITHNO-CALC
           MOVE OUTPUT-LINE TO REPORT-REC
           MOVE 1 TO PROPER-SPACING
           WRITE REPORT-REC
           AFTER ADVANCING PROPER-SPACING
           MOVE 2 TO PROPER-SPACING



           .


       200-PROCESS-RTN.

           IF PROD-NAME-SAME IS EQUAL TO 'NEW'
                   MOVE PROD-ID-IN TO PROD-NAME-SAME
                   MOVE PROD-NAME-IN TO PROD-NAME-OUT
                   PERFORM 175-PARAGRAPH-LAYOUT

           ELSE IF PROD-ID-IN IS EQUAL TO PROD-NAME-SAME
               MOVE '' TO PROD-NAME-OUT

               PERFORM 175-PARAGRAPH-LAYOUT



               ELSE

                    MOVE PROD-NAME-IN TO PROD-NAME-OUT
                    MOVE WS-GRP-QTY TO QTY-SOLD-TOTAL
                    MOVE WS-GRP-SV TO SALES-VALUE-TOTAL-OUT
                    MOVE TOTAL-HEADER TO REPORT-REC
                    WRITE REPORT-REC
                    AFTER ADVANCING PROPER-SPACING
                    MOVE PROD-ID-IN TO PROD-NAME-SAME
                    MOVE 0 TO WS-GRP-SV
                    MOVE 0 TO WS-GRP-QTY
                   PERFORM 175-PARAGRAPH-LAYOUT


             END-IF
            END-IF





           .

       300-END-ROUTINE.

           COMPUTE WS-GRAND-QTY =
                   WS-QTY-HOLDER + WS-GRAND-QTY

           COMPUTE WS-GRAND-SV =
                       WS-SV-HOLDER + WS-GRAND-SV
           MOVE WS-GRP-SV TO SALES-VALUE-TOTAL-OUT
           MOVE WS-GRAND-SV TO TOT-VAL-SALES
           MOVE WS-GRAND-QTY TO TOTAL-AMUNT-SOLD
           MOVE TOTAL-HEADER TO REPORT-REC
           MOVE 2 TO PROPER-SPACING
           WRITE REPORT-REC
           AFTER ADVANCING PROPER-SPACING
           MOVE 2 TO PROPER-SPACING

           MOVE TOTAL-AMUNT-LINE TO REPORT-REC
           WRITE REPORT-REC
           AFTER ADVANCING PROPER-SPACING
           MOVE 2 TO PROPER-SPACING
           MOVE TOTAL-VAL-LINE TO REPORT-REC
           WRITE REPORT-REC
           AFTER ADVANCING PROPER-SPACING
           CLOSE INVENTORY-TOTALS
                 REPORT-FILE
           STOP RUN
           .
