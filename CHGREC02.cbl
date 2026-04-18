       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHGREC02.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "SLVND02.cbl".
       DATA DIVISION.
       FILE SECTION.
           COPY "FDVND02.cbl".

       WORKING-STORAGE SECTION.
       77  WS-VENDOR-NUM-FIELD         PIC Z(5).
       77  WS-RECORD-FOUND             PIC X.
       77  WS-WHICH-FIELD              PIC 9.
       77  WS-KEEP-GOING               PIC X.
       77  WS-EXIT-FLAG                PIC X.

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           DISPLAY "------------CHANGE RECORD------------".
           PERFORM OPENING-PROCEDURE.
           PERFORM GET-NEW-VENDOR-NUMBER.
           PERFORM FIND-RECORD
               UNTIL FS-VENDOR-NUMBER = ZEROS.
           PERFORM CLOSING-PROCEDURE.
       PROGRAM-END.
           GOBACK.
      *-----------------------------------------------------------------
       OPENING-PROCEDURE.
           OPEN I-O VENDOR-FILE-02.

       CLOSING-PROCEDURE.
           CLOSE VENDOR-FILE-02.

       GET-NEW-VENDOR-NUMBER.
           PERFORM INIT-VENDOR-RECORD.
           PERFORM ENTER-VENDOR-NUMBER.

       INIT-VENDOR-RECORD.
           MOVE SPACE TO VENDOR-RECORD.
           MOVE ZEROES TO FS-VENDOR-NUMBER.
           MOVE SPACES TO WS-KEEP-GOING.
           MOVE "F" TO WS-EXIT-FLAG.

       ENTER-VENDOR-NUMBER.
           DISPLAY "ENTER VENDOR NUMBER (1 - 99999)".
           DISPLAY "ENTER 0 TO STOP CHANGING".
           ACCEPT WS-VENDOR-NUM-FIELD.
           MOVE WS-VENDOR-NUM-FIELD TO FS-VENDOR-NUMBER.

       FIND-RECORD.
           PERFORM READ-AND-FIND.
           IF WS-RECORD-FOUND = "Y"
               PERFORM CHANGE-RECORD
           ELSE
               DISPLAY "RECORD NOT FOUND"
               PERFORM ENTER-VENDOR-NUMBER.
           PERFORM GET-NEW-VENDOR-NUMBER.

       CHANGE-RECORD.
           DISPLAY "-------------------------------------------".
           DISPLAY "RECORD FOUND".
           DISPLAY "-------------------------------------------".
           PERFORM DISPLAY-RECORD-FOUND.
           IF WS-EXIT-FLAG = "F"
               PERFORM ADD-NEW-FIELD
                   UNTIL WS-KEEP-GOING = "N".

       READ-AND-FIND.
           MOVE "Y" TO WS-RECORD-FOUND.
           READ VENDOR-FILE-02 RECORD
               INVALID KEY
                   MOVE "N" TO WS-RECORD-FOUND.

       ADD-NEW-FIELD.
           MOVE "T" TO WS-EXIT-FLAG.
           PERFORM ASK-WICH-FIELD-CHANGE.
           PERFORM CHANGE-FIELD.
           PERFORM REWRITE-RECORD.
           PERFORM DISPLAY-RECORD-FOUND.
           PERFORM ASK-IF-CONTINUE.

       ASK-IF-CONTINUE.
           DISPLAY "------------------------------------------------".
           DISPLAY "CONTINUE TO CHANGE SAME RECORD? (Y = yes/N=no)".
           ACCEPT WS-KEEP-GOING.
           IF WS-KEEP-GOING = "Y" OR  WS-KEEP-GOING = "y"
               MOVE "Y" TO WS-KEEP-GOING
           ELSE MOVE "N" TO WS-KEEP-GOING.

       DISPLAY-RECORD-FOUND.
           DISPLAY "VENDOR NUMBER: " FS-VENDOR-NUMBER.
           DISPLAY "1) VENDOR NAME:" FS-VENDOR-NAME.
           DISPLAY "2) VENDOR ADDRESS 1:" FS-VENDOR-ADDRESS1.
           DISPLAY "3) VENDOR ADDRESS 2:" FS-VENDOR-ADDRESS2.
           DISPLAY "4) VENDOR CITY:" FS-VENDOR-CITY.
           DISPLAY "5) VENDOR STATE:" FS-VENDOR-STATE.
           DISPLAY "6) VENDOR ZIP:" FS-VENDOR-ZIP.
           DISPLAY "7) VENDOR CONTACT:" FS-VENDOR-CONTACT.
           DISPLAY "8) VENDOR PHONE:" FS-VENDOR-PHONE.
           DISPLAY " ".

       ASK-WICH-FIELD-CHANGE.
           DISPLAY "WHICH FIELD DO YOU WANT TO CHANGE (1-8)".
           DISPLAY "ENTER 9 TO CHANGE ALL FIELDS".
           ACCEPT WS-WHICH-FIELD.

       CHANGE-FIELD.
           EVALUATE WS-WHICH-FIELD
               WHEN 1
                   PERFORM ENTER-VENDOR-NAME
               WHEN 2
                   PERFORM ENTER-VENDOR-ADDRESS1
               WHEN 3
                   PERFORM ENTER-VENDOR-ADDRESS2
               WHEN 4
                   PERFORM ENTER-VENDOR-CITY
               WHEN 5
                   PERFORM ENTER-VENDOR-STATE
               WHEN 6
                   PERFORM ENTER-VENDOR-ZIP
               WHEN 7
                   PERFORM ENTER-VENDOR-CONTACT
               WHEN 8
                   PERFORM ENTER-VENDOR-PHONE
               WHEN 9
                   PERFORM ENTER-VENDOR-FIELD
           END-EVALUATE.

       REWRITE-RECORD.
           REWRITE VENDOR-RECORD
               INVALID KEY
               DISPLAY "----------------------------"
               DISPLAY "ERROR DURING REWRITE"
               DISPLAY "----------------------------".

       ENTER-VENDOR-FIELD.
           PERFORM ENTER-VENDOR-NAME.
           PERFORM ENTER-VENDOR-ADDRESS1.
           PERFORM ENTER-VENDOR-ADDRESS2.
           PERFORM ENTER-VENDOR-CITY.
           PERFORM ENTER-VENDOR-STATE.
           PERFORM ENTER-VENDOR-ZIP.
           PERFORM ENTER-VENDOR-CONTACT.
           PERFORM ENTER-VENDOR-PHONE.

       ENTER-VENDOR-NAME.
           DISPLAY "ENTER VENDOR NAME".
           ACCEPT FS-VENDOR-NAME.

       ENTER-VENDOR-ADDRESS1.
           DISPLAY "ENTER VENDOR ADDRESS 1".
           ACCEPT FS-VENDOR-ADDRESS1.

       ENTER-VENDOR-ADDRESS2.
           DISPLAY "ENTER VENDOR ADDRESS 2".
           ACCEPT FS-VENDOR-ADDRESS2.

       ENTER-VENDOR-CITY.
           DISPLAY "ENTER VENDOR CITY".
           ACCEPT FS-VENDOR-CITY.

       ENTER-VENDOR-STATE.
           DISPLAY "ENTER VENDOR STATE".
           ACCEPT FS-VENDOR-STATE.

       ENTER-VENDOR-ZIP.
           DISPLAY "ENTER VENDOR ZIP".
           ACCEPT FS-VENDOR-ZIP.

       ENTER-VENDOR-CONTACT.
           DISPLAY "ENTER VENDOR CONTACT".
           ACCEPT FS-VENDOR-CONTACT.

       ENTER-VENDOR-PHONE.
           DISPLAY "ENTER VENDOR PHONE".
           ACCEPT FS-VENDOR-PHONE.
