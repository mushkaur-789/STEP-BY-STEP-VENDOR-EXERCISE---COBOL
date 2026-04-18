       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPTREC02.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "SLVND02.cbl".

           SELECT PRINTER-FILE
               ASSIGN TO "vnd_print_file.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           COPY "FDVND02.cbl".
       FD  PRINTER-FILE.
       01  PRINT-RECORD                PIC X(70).

       WORKING-STORAGE SECTION.
       77  END-OF-FILE                 PIC X.

       01  TITLE-LINE.
           05  FILLER                  PIC X(10) VALUE SPACE.
           05  FILLER                  PIC X(11) VALUE "VENDOR LIST".
           05  FILLER                  PIC X(20) VALUE SPACE.
           05  FILLER                  PIC X(6)  VALUE "PAGE: ".
           05  WS-PAGE-NUMBER          PIC Z(5).

        01 COLUMN-HEADING.
           05  FILLER                  PIC X(2)  VALUE "NO".
           05  FILLER                  PIC X(4)  VALUE SPACE.
           05  FILLER                  PIC X(12) VALUE "NAME-ADDRESS".
           05  FILLER                  PIC X(19) VALUE SPACE.
           05  FILLER                  PIC X(17)
                                       VALUE "CONTACT-PHONE-ZIP".
           05  FILLER                  PIC X(18) VALUE SPACE.

       01  DISPLAY-DETAIL-LINE.
           05  WS-DISPLAY-BOX1         PIC 9(5).
           05  FILLER                  PIC X     VALUE SPACE.
           05  WS-DISPLAY-BOX2         PIC X(30).
           05  FILLER                  PIC X     VALUE SPACE.
           05  WS-DISPLAY-BOX3         PIC X(30).

       01  DISPLAY-CITY-LINE.
           05  WS-DISPLAY-CITY         PIC X(20).
           05  FILLER                  PIC X     VALUE SPACE.
           05  WS-DISPLAY-STATE        PIC X(2).

       01  WS-DISPLAY-RECORD           PIC X(68).
       01  WS-REC-COUNTER              PIC 99 VALUE ZERO.
       01  WS-MAX-REC                  PIC 9(3) VALUE 5.
       01  A-DUMMY                     PIC X.
       01  WS-PAG-NUM                  PIC 9(5).

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           DISPLAY "------------CREATE REPORT------------".
           PERFORM OPEN-PROCEDURE.
           MOVE ZERO TO WS-REC-COUNTER
                        WS-PAGE-NUMBER.

           PERFORM START-NEW-PAGE.
           MOVE "N" TO END-OF-FILE.
           PERFORM READ-NEXT-RECORD.

           IF END-OF-FILE = "Y"
               MOVE "NO RECORDS FOUND" TO PRINT-RECORD
               PERFORM WRITE-PRINT-RECORD
               PERFORM END-PAGE
           ELSE
               PERFORM DISPLAY-VENDOR-FILE-02
                   UNTIL END-OF-FILE = "Y".
           PERFORM CLOSE-PROCEDURE.

       PROGRAM-END.
           GOBACK.
      *-----------------------------------------------------------------
       OPEN-PROCEDURE.
           OPEN I-O VENDOR-FILE-02.
           OPEN OUTPUT PRINTER-FILE.

       CLOSE-PROCEDURE.
           DISPLAY "-------------------------".
           DISPLAY "|      REPORT DONE      |".
           DISPLAY "-------------------------".
           CLOSE VENDOR-FILE-02.
           CLOSE PRINTER-FILE.
      *-----------------------------------------------------------------
       DISPLAY-VENDOR-FILE-02.
           PERFORM CHECK-NUM-REC.
           PERFORM READ-AND-DISPLAY.
           PERFORM READ-NEXT-RECORD.

       READ-AND-DISPLAY.
           PERFORM DISPLAY-LINE-1.
           PERFORM DISPLAY-LINE-2.
           PERFORM DISPLAY-LINE-3.
           PERFORM DISPLAY-LINE-4.
           PERFORM LINE-FEED.
           ADD 1 TO WS-REC-COUNTER.

       CHECK-NUM-REC.
           IF WS-REC-COUNTER NOT < 5
               PERFORM END-PAGE
               PERFORM START-NEW-PAGE.

       READ-NEXT-RECORD.
           READ VENDOR-FILE-02 NEXT RECORD
              AT END MOVE "Y" TO END-OF-FILE.

       DISPLAY-LINE-1.
           MOVE SPACE TO DISPLAY-DETAIL-LINE.
           MOVE FS-VENDOR-NUMBER TO WS-DISPLAY-BOX1.
           MOVE FS-VENDOR-NAME TO WS-DISPLAY-BOX2.
           MOVE FS-VENDOR-CONTACT TO WS-DISPLAY-BOX3.
           MOVE DISPLAY-DETAIL-LINE TO PRINT-RECORD.
           PERFORM WRITE-PRINT-RECORD.

       DISPLAY-LINE-2.
           MOVE SPACE TO DISPLAY-DETAIL-LINE.
           MOVE FS-VENDOR-ADDRESS1 TO WS-DISPLAY-BOX2.
           MOVE FS-VENDOR-PHONE TO WS-DISPLAY-BOX3.
           MOVE DISPLAY-DETAIL-LINE TO PRINT-RECORD.
           PERFORM WRITE-PRINT-RECORD.

       DISPLAY-LINE-3.
           MOVE SPACE TO DISPLAY-DETAIL-LINE.
           IF FS-VENDOR-ADDRESS2 NOT = SPACE
               MOVE FS-VENDOR-ADDRESS2 TO WS-DISPLAY-BOX2
               MOVE DISPLAY-DETAIL-LINE TO PRINT-RECORD
               PERFORM WRITE-PRINT-RECORD.

       DISPLAY-LINE-4.
           MOVE SPACE TO DISPLAY-DETAIL-LINE.
           MOVE FS-VENDOR-CITY TO WS-DISPLAY-CITY.
           MOVE FS-VENDOR-STATE TO WS-DISPLAY-STATE.
           MOVE FS-VENDOR-ZIP TO WS-DISPLAY-BOX3.
           MOVE DISPLAY-CITY-LINE TO WS-DISPLAY-BOX2.
           MOVE DISPLAY-DETAIL-LINE TO PRINT-RECORD
           PERFORM WRITE-PRINT-RECORD.

       START-NEW-PAGE.
           ADD 1 TO WS-PAG-NUM.
           MOVE WS-PAG-NUM TO WS-PAGE-NUMBER.
           MOVE TITLE-LINE TO PRINT-RECORD.
           PERFORM WRITE-PRINT-RECORD.
           PERFORM LINE-FEED.
           MOVE COLUMN-HEADING TO PRINT-RECORD.
           PERFORM WRITE-PRINT-RECORD.

       END-PAGE.
           MOVE ZERO TO WS-REC-COUNTER.
           MOVE "------------------------------------------------------"
               TO PRINT-RECORD.
           PERFORM WRITE-PRINT-RECORD.
           PERFORM PRINT-NEW-PAGE.

       LINE-FEED.
           MOVE SPACE TO PRINT-RECORD.
           PERFORM WRITE-PRINT-RECORD.

       WRITE-PRINT-RECORD.
           WRITE PRINT-RECORD BEFORE ADVANCING 1.

       PRINT-NEW-PAGE.
           MOVE SPACES TO PRINT-RECORD.
           WRITE PRINT-RECORD BEFORE ADVANCING PAGE.
