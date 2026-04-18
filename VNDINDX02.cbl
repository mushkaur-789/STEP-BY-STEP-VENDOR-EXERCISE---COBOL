       IDENTIFICATION DIVISION.
       PROGRAM-ID. VNDINDX02.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "SLVND02.cbl".
       DATA DIVISION.
       FILE SECTION.
           COPY "FDVND02.cbl".

       WORKING-STORAGE SECTION.
       77  WS-RAW-INPUT                PIC X(10).
       77  WS-MENU-PICK                PIC 9.

       77  WS-COUNT-SPACE              PIC 99.
       77  WS-COUNT-NUM                PIC 99.
       77  WS-NUM-VALID                PIC X.

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           PERFORM MAIN-PROCESS.
           PERFORM CALL-OPERATION
               UNTIL WS-MENU-PICK = ZERO.

       PROGRAM-END.
            STOP RUN.
      *-----------------------------------------------------------------
       MAIN-PROCESS.
           PERFORM INIT-VALUES.
           PERFORM DISPLAY-MENU.
           PERFORM USER-CHOICE.
           PERFORM VALIDATE-PICK.
           PERFORM TRY-AGAIN
               UNTIL WS-NUM-VALID = "Y".

       INIT-VALUES.
           MOVE "N" TO WS-NUM-VALID.
           MOVE ZERO TO WS-COUNT-SPACE.
           MOVE ZERO TO WS-COUNT-NUM.

       DISPLAY-MENU.
           DISPLAY "  ".
           DISPLAY "           VENDOR FILE ".
           DISPLAY "  ".
           DISPLAY "       MENU".
           DISPLAY "       1. ADD RECORD".
           DISPLAY "       2. CHANGE RECORD".
           DISPLAY "       3. DELETE RECORD".
           DISPLAY "       4. INQUIRY".
           DISPLAY "       5. PRINT REPORT".
           DISPLAY "       6. DISPLAY ALL RECORDS".
           DISPLAY "  ".
           DISPLAY "       0. END PROGRAM".
           DISPLAY "  ".

       USER-CHOICE.
           DISPLAY "MAKE YOUR CHOICE (0-6)".
           ACCEPT WS-RAW-INPUT.

       VALIDATE-PICK.
           INSPECT WS-RAW-INPUT
               TALLYING WS-COUNT-SPACE FOR ALL SPACE.

           INSPECT WS-RAW-INPUT
               TALLYING WS-COUNT-NUM FOR ALL
                   "0" "1" "2" "3" "4" "5" "6" "7" "8" "9".

           IF WS-COUNT-SPACE = 9 AND WS-COUNT-NUM = 1
               IF WS-RAW-INPUT >= 0 AND WS-RAW-INPUT <= 6
                   MOVE WS-RAW-INPUT TO WS-MENU-PICK
                   MOVE "Y" TO WS-NUM-VALID.

       TRY-AGAIN.
           DISPLAY "-------------------------------".
           DISPLAY "| INPUT NOT VALID - TRY AGAIN |".
           DISPLAY "-------------------------------".
           PERFORM INIT-VALUES.
           PERFORM USER-CHOICE.
           PERFORM VALIDATE-PICK.

       CALL-OPERATION.
           EVALUATE WS-MENU-PICK
               WHEN 1
                   CALL "ADDREC02"
               WHEN 2
                   CALL "CHGREC02"
               WHEN 3
                   CALL "DLTREC02"
               WHEN 4
                   CALL "INQREC02"
               WHEN 5
                   CALL "RPTREC02"
               WHEN 6
                   CALL "DSPREC02"
           END-EVALUATE.
           PERFORM MAIN-PROCESS.
