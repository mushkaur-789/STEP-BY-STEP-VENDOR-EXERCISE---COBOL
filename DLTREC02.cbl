       IDENTIFICATION DIVISION.
       PROGRAM-ID. DLTREC02.
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
       77  WS-CONFIRM-DELETE           PIC X.

       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           DISPLAY "------------DELETE RECORD------------".
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
           MOVE "N" TO WS-CONFIRM-DELETE.

       ENTER-VENDOR-NUMBER.
           DISPLAY "ENTER VENDOR NUMBER (1 - 99999)".
           DISPLAY "ENTER 0 TO STOP DELETE".
           ACCEPT WS-VENDOR-NUM-FIELD.
           MOVE WS-VENDOR-NUM-FIELD TO FS-VENDOR-NUMBER.

       FIND-RECORD.
           PERFORM READ-AND-FIND.
           IF WS-RECORD-FOUND = "Y"
               PERFORM ASK-DELETE-RECORD
           ELSE
               DISPLAY "RECORD NOT FOUND".
           PERFORM GET-NEW-VENDOR-NUMBER.

       ASK-DELETE-RECORD.
           DISPLAY "-------------------------------------------".
           DISPLAY "| RECORD FOUND                            |".
           DISPLAY "-------------------------------------------".
           PERFORM DISPLAY-RECORD-FOUND.
           PERFORM ASK-CONFIRM.
           IF WS-CONFIRM-DELETE = "Y"
               PERFORM DELETE-RECORD
               PERFORM CHECK-RECORD.

       CHECK-RECORD.
           PERFORM READ-AND-FIND.
           IF WS-RECORD-FOUND = "N"
               DISPLAY "----------------------------------".
               DISPLAY "|RECORD DELETED SUCCESSFULLY      |".
               DISPLAY "----------------------------------".

       ASK-CONFIRM.
           DISPLAY "---------------------------------------"
           DISPLAY "| DO YOU WANT TO DELETE THIS RECORD?  |".
           DISPLAY "| (Y = YES / N = NO)".
           DISPLAY "---------------------------------------"
           ACCEPT WS-CONFIRM-DELETE.
           IF WS-CONFIRM-DELETE = "y" OR "Y"
               MOVE "Y" TO WS-CONFIRM-DELETE
           ELSE IF WS-CONFIRM-DELETE = "N" OR "n"
               MOVE "N" TO WS-CONFIRM-DELETE
           ELSE
               DISPLAY "PLEASE ENTER Y (YES) OR N(NO)."
               PERFORM ASK-CONFIRM.

       DELETE-RECORD.
           DELETE VENDOR-FILE-02 RECORD
               INVALID KEY
                   DISPLAY "ERROR DELETING VENDOR RECORD".

       READ-AND-FIND.
           MOVE "Y" TO WS-RECORD-FOUND.
           READ VENDOR-FILE-02 RECORD
               INVALID KEY
                   MOVE "N" TO WS-RECORD-FOUND.

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
