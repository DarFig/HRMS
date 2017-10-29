       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINHRMS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 CHOICE PIC 9.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "*************************" LINE 3 COL 15.
           DISPLAY "HUMAN RESOURCE MANAGEMENT SYSTEM"LINE 5 COL 15.
           DISPLAY "*************************" LINE 7 COL 15.
           DISPLAY "1. HRMS WRITE" LINE 10 COL 25.
           DISPLAY "2. HRMS READ" LINE 12 COL 25.
           DISPLAY "3. LIST BRANCH FILE" LINE 14 COL 25.
           DISPLAY "4. EXIT" LINE 16 COL 25.
           DISPLAY "ENTER YOUR CHOICE :" LINE 18 COL 25.
           ACCEPT CHOICE LINE 18 COL 46.
           IF CHOICE = 1
              CALL "EMPWRITE"
              CANCEL "EMPWRITE"
              GO TO MAIN-PARA
           ELSE
             IF CHOICE = 2
                CALL "EMPREAD"
                CANCEL "EMPREAD"
                GO TO MAIN-PARA
             ELSE
               IF CHOICE = 3
                   CALL "EMPBRANCHLIST"
                   CANCEL "EMPBRANCHLIST"
                   GO TO MAIN-PARA
               ELSE
                 STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPREAD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EEMPID
           FILE STATUS IS FSO.

           SELECT LEAVEFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS LEMPID
           FILE STATUS IS FSL.

           SELECT BRANCHFILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS BBRID
           FILE STATUS IS FSB.

           SELECT DESIGNATIONFILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FSDES.

           SELECT DEPARTMENTFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS DEPCODE
           FILE STATUS IS FSDEP.

           SELECT REVISIONFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS RREVID
           ALTERNATE RECORD KEY IS REMPID
           FILE STATUS IS FSR.

           SELECT PAYMENTFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS PEMPID
           FILE STATUS IS FSP.

           SELECT CONFIRMATIONFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CCONID
           ALTERNATE RECORD KEY IS CEMPID
           FILE STATUS IS FSC.

           SELECT GRADEFILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FSG.

           SELECT TRANSFERFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TTRFID
           FILE STATUS IS FST.

           SELECT EMPPERSONALFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EPEMPID
           FILE STATUS IS FSEP.

       DATA DIVISION.
       FILE SECTION.
       FD EMPFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "EMP.DAT".
       01 EMPREC.
           02 EEMPID    PIC X(6).
           02 EEMPNAME  PIC X(25).
           02 EEMPADDR  PIC X(30).
           02 EPHONE    PIC X(10).
           02 EDOJ      PIC X(10).
           02 EDIP      PIC X(10).
           02 EUG       PIC X(4).
           02 EPG       PIC X(4).
           02 EPROFQ    PIC X(4).
           02 ESKILL    PIC X(10).
           02 EGRDNO    PIC 99.
           02 EBRNID    PIC X(6).
           02 EDESID    PIC X(6).

       FD LEAVEFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "LEAVE.DAT".
       01 LEAVEREC.
           02 LEMPID    PIC X(6).
           02 LFMDATE   PIC X(10).
           02 LTODATE   PIC X(10).
           02 LLEVCAT   PIC X(3).

       FD BRANCHFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "BRANCH.DAT".
       01 BRANCHREC.
           02 BBRID    PIC X(6).
           02 BBRNAME  PIC X(15).
           02 BBRADD   PIC X(30).
           02 BBRPH    PIC X(10).
           02 BEMAIL   PIC X(20).
           02 BMGRNAME PIC X(25).

       FD DESIGNATIONFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "DESIG.DAT".
       01 DESIGNATIONREC.
           02 DESID    PIC X(6).
           02 DESIGN   PIC X(15).
           02 DESHRT   PIC X(4).

       FD DEPARTMENTFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "DEPART.DAT".
       01 DEPARTMENTREC.
           02 DEPCODE  PIC X(6).
           02 DEPNAME  PIC X(20).

       FD REVISIONFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "REVISION.DAT".
       01 REVISIONREC.
           02 RREVID   PIC X(6).
           02 REMPID   PIC X(6).
           02 RDESCODE PIC X(6).
           02 RBASIC   PIC 9(6)V99.
           02 RHRA     PIC 9(6)V99.
           02 RDPA     PIC 9(6)V99.
           02 RPPA     PIC 9(6)V99.
           02 REDUA    PIC 9(6)V99.
           02 RTECHJR  PIC 9(6)V99.
           02 RLUNCHA  PIC 9(6)V99.
           02 RCONVEY  PIC 9(6)V99.
           02 RBUSATR  PIC 9(6)V99.
           02 RLTA     PIC 9(6)V99.
           02 RPF      PIC 9(6)V99.
           02 RESI     PIC 9(6)V99.
           02 RREVDATE PIC X(10).

       FD PAYMENTFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "PAYMENT.DAT".
       01 PAYMENTREC.
           02 PEMPID   PIC X(6).
           02 PBASIC   PIC 9(6)V99.
           02 PDA      PIC 9(6)V99.
           02 PCCA     PIC 9(6)V99.
           02 PHRA     PIC 9(6)V99.
           02 PDPA     PIC 9(6)V99.
           02 PPPA     PIC 9(6)V99.
           02 PEDUA    PIC 9(6)V99.
           02 PTECHJR  PIC 9(6)V99.
           02 PLUNCHA  PIC 9(6)V99.
           02 PCONVEY  PIC 9(6)V99.
           02 PBUSATR  PIC 9(6)V99.
           02 PLTA     PIC 9(6)V99.
           02 PPF      PIC 9(6)V99.
           02 PESI     PIC 9(6)V99.
           02 PGRTY    PIC 9(6)V99.
           02 PPTAX    PIC 9(6)V99.
           02 PITAX    PIC 9(6)V99.
           02 PLOAN    PIC 9(8)V99.
           02 PLOANDA  PIC 9(8)V99.
           02 POTHERD  PIC 9(6)V99.
           02 PPERINC  PIC 9(6)V99.
           02 PMEDI    PIC 9(6)V99.
           02 PBOOK    PIC 9(6)V99.
           02 PENTER   PIC 9(6)V99.
           02 PTPH     PIC 9(6)V99.
           02 PHOUSE   PIC 9(6)V99.
           02 PVEHMAN  PIC 9(6)V99.
           02 PCREDIT  PIC 9(6)V99.
           02 PCLUB    PIC 9(6)V99.
           02 PCL      PIC 99.
           02 PSL      PIC 99.
           02 PPL      PIC 99.
           02 PLLOP    PIC 999.
           02 POTHERL  PIC 999.

       FD CONFIRMATIONFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "CONFIRM.DAT".
       01 CONFIRMATIONREC.
           02 CCONID   PIC X(6).
           02 CEMPID   PIC X(6).
           02 CCDATE   PIC X(6).

       FD GRADEFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "GRADE.DAT".
       01 GRADEREC.
           02 GGRADE   PIC 99.
           02 GDESIGN  PIC X(25).

       FD TRANSFERFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "TRANSFER.DAT".
       01 TRANSFERREC.
           02 TTRFID   PIC X(6).
           02 TEMPID   PIC X(6).
           02 TOBRID   PIC X(6).
           02 TTRFDT   PIC X(10).

       FD EMPPERSONALFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "EMPPER.DAT".
       01 EMPPERSONALREC.
           02 EPEMPID  PIC X(6).
           02 EPTADD   PIC X(30).
           02 EPTPH    PIC X(10).
           02 EPDOB    PIC X(10).
           02 EPPOB    PIC X(10).
           02 EPLANG   PIC X(15).
           02 EPBLOOD  PIC X(4).
           02 EPWEIGHT PIC 999.
           02 EPHEIGHT PIC 999.
           02 EPVISION PIC X(15).
           02 EPFATHER PIC X(25).
           02 EPDOBF   PIC X(10).
           02 EPMOTHER PIC X(25).
           02 EPDOBM   PIC X(10).
           02 EPSPOUSE PIC X(25).
           02 EPCHILD  PIC X(25).
           02 EPDOBC   PIC X(10).

       WORKING-STORAGE SECTION.
       77 FSO   PIC XX.
       77 FSL   PIC XX.
       77 FSB   PIC XX.
       77 FSDES PIC XX.
       77 FSDEP PIC XX.
       77 FSR   PIC XX.
       77 FSP   PIC XX.
       77 FSC   PIC XX.
       77 FSG   PIC XX.
       77 FST   PIC XX.
       77 FSEP  PIC XX.
       77 DES   PIC X(6).
       77 GR    PIC 99.
       77 CHOICE PIC 99.
       77 IID   PIC X(6).
       77 ENTER PIC X.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "******************************" LINE 3 COL 10.
           DISPLAY " HUMAN RESOURCE MANAGEMENT SYSTEM " LINE 5 COL 10.
           DISPLAY "****************************" LINE 7 COL 10.
           DISPLAY "01. EMPLOYEE FILE" LINE 11 COL 5.
           DISPLAY "02. LEAVE FILE" LINE 12 COL 5.
           DISPLAY "03. BRANCH FILE" LINE 13 COL 5.
           DISPLAY "04. DESIGNATION FILE" LINE 14 COL 5.
           DISPLAY "05. DEPARTMENT FILE" LINE 15 COL 5.
           DISPLAY "06. REVISION FILE" LINE 16 COL 5.
           DISPLAY "07. PAYMENT FILE" LINE 17 COL 5.
           DISPLAY "08. CONFIRMATION FILE" LINE 18 COL 5.
           DISPLAY "09. GRADE FILE" LINE 19 COL 5.
           DISPLAY "10. TRANSFER FILE" LINE 20 COL 5.
           DISPLAY "11. EMPLOYEE PERSONAL FILE" LINE 21 COL 5.
           DISPLAY "12. EXIT" LINE 22 COL 5.
           DISPLAY "ENTER U R CHOICE :" LINE 23 COL 25.
           ACCEPT CHOICE LINE 23 COL 45.
           IF CHOICE = 1
              GO TO EMP-PARA
           ELSE
             IF CHOICE = 2
                GO TO LEAVE-PARA
             ELSE
               IF CHOICE = 3
                  GO TO BRANCH-PARA
               ELSE
                 IF CHOICE = 4
                    GO TO DESIGNATION-PARA
                 ELSE
                   IF CHOICE = 5
                      GO TO DEPARTMENT-PARA
                   ELSE
                     IF CHOICE = 6
                        GO TO REVISION-PARA
                     ELSE
                       IF CHOICE = 7
                          GO TO PAYMENT-PARA
                       ELSE
                          IF CHOICE = 8
                             GO TO CONFIRMATION-PARA
                          ELSE
                            IF CHOICE = 9
                               GO TO GRADE-PARA
                            ELSE
                              IF CHOICE = 10
                                 GO TO TRANSFER-PARA
                              ELSE
                                IF CHOICE = 11
                                   GO TO EMPPERSONAL-PARA
                                 ELSE
                                   EXIT PROGRAM.

       EMP-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN INPUT EMPFILE.
           DISPLAY "ENTER CODE :".
           ACCEPT EEMPID.
           DISPLAY " " WITH BLANK SCREEN.
           READ EMPFILE INVALID KEY GO TO ERROR-EMP-PARA.
           DISPLAY " CODE                 :" LINE 1 COL 1.
           DISPLAY EEMPID LINE 1 COL 25.
           DISPLAY " NAME                 :" LINE 2 COL 1.
           DISPLAY EEMPNAME LINE 2 COL 25.
           DISPLAY " ADDRESS              :" LINE 3 COL 1.
           DISPLAY EEMPADDR LINE 3 COL 25.
           DISPLAY " PHONE                :" LINE 4 COL 1.
           DISPLAY EPHONE LINE 4 COL 25.
           DISPLAY " DATE OF JOIN         :" LINE 5 COL 1.
           DISPLAY EDOJ LINE 5 COL 25.
           DISPLAY " DIPLOMA              :" LINE 6 COL 1.
           DISPLAY EDIP LINE 6 COL 25.
           DISPLAY " UG                   :" LINE 7 COL 1.
           DISPLAY EUG LINE 7 COL 25.
           DISPLAY " PG                   :" LINE 8 COL 1.
           DISPLAY EPG LINE 8 COL 25.
           DISPLAY " PROFESSIONAL QUALITY :" LINE 9 COL 1.
           DISPLAY EPROFQ LINE 9 COL 25.
           DISPLAY " SKILL SET            :" LINE 10 COL 1.
           DISPLAY ESKILL LINE 10 COL 25.
           DISPLAY " GRADE NUMBER         :"  LINE 11 COL 1.
           DISPLAY EGRDNO LINE 11 COL 25.
           DISPLAY " BRANCH CODE          :"  LINE 12 COL 1.
           DISPLAY EBRNID LINE 12 COL 25.
           DISPLAY " DESIGNATION CODE     :"  LINE 13 COL 1.
           DISPLAY EDESID LINE 13 COL 25.
           CLOSE EMPFILE.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       LEAVE-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN INPUT LEAVEFILE.
           DISPLAY "ENTER CODE :".
           ACCEPT LEMPID.
           DISPLAY " " WITH BLANK SCREEN.
           READ LEAVEFILE INVALID KEY GO TO ERROR-LEAVE-PARA.
           DISPLAY " CODE           :" LINE 1 COL 1.
           DISPLAY LEMPID LINE 1 COL 20.
           DISPLAY " DATE           :" LINE 2 COL 1.
           DISPLAY LFMDATE LINE 2 COL 20.
           DISPLAY " DATE           :" LINE 3 COL 1.
           DISPLAY LTODATE LINE 3 COL 20.
           DISPLAY " LEAVE CATEGORY :" LINE 4 COL 1.
           DISPLAY LLEVCAT LINE 4 COL 20.
           CLOSE LEAVEFILE.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       BRANCH-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN INPUT BRANCHFILE.
           DISPLAY " BRANCH CODE :".
           ACCEPT IID.
           DISPLAY " " WITH BLANK SCREEN.
           PERFORM BRANCH-READ-IT UNTIL FSB = 10.
       BRANCH-READ-IT.
           READ BRANCHFILE RECORD.
           IF BBRID = IID
               DISPLAY " BRANCH CODE    :" LINE 1 COL 1.
               DISPLAY " BRANCH CODE    :" LINE 1 COL 1.
               DISPLAY IID LINE 1 COL 20.
               DISPLAY " BRANCH NAME    :" LINE 2 COL 1.
               DISPLAY BBRNAME LINE 2 COL 20.
               DISPLAY " BRANCH ADDRESS :" LINE 3 COL 1.
               DISPLAY BBRADD LINE 3 COL 20.
               DISPLAY " PHONE          :" LINE 4 COL 1.
               DISPLAY BBRPH LINE 4 COL 20.
               DISPLAY " E-MAIL         :" LINE 5 COL 1.
               DISPLAY BEMAIL LINE 5 COL 20.
               DISPLAY " MANAGER NAME   :" LINE 6 COL 1.
               DISPLAY BMGRNAME LINE 6 COL 20.
       BRANCH-EXIT-P.
           CLOSE BRANCHFILE.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       DESIGNATION-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN INPUT DESIGNATIONFILE.
           DISPLAY "ENTER THE DESIGNATION CODE :".
           ACCEPT DES.
           DISPLAY " " WITH BLANK SCREEN.
           PERFORM DES-READ-PARA UNTIL FSDES = 10.
       DES-READ-PARA.
           READ DESIGNATIONFILE AT END GO TO DES-EXIT-PARA.
           IF DESID = DES
           DISPLAY " DESIGNATION CODE     :" LINE 1 COL 1.
           DISPLAY DESID LINE 1 COL 25.
           DISPLAY " DESIGNATION          :" LINE 2 COL 1.
           DISPLAY DESIGN LINE 2 COL 25.
           DISPLAY " DESIGNATION IN SHORT :" LINE 3 COL 1.
           DISPLAY DESHRT LINE 3 COL 25.
       DES-EXIT-PARA.
           CLOSE DESIGNATIONFILE.
           DISPLAY ' '.
           DISPLAY ' '.
           DISPLAY "PRESS ENTER TO RETURN TO HRMS READ MENU".
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       DEPARTMENT-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN INPUT DEPARTMENTFILE.
           DISPLAY "ENTER DEP CODE :".
           ACCEPT DEPCODE.
           DISPLAY " " WITH BLANK SCREEN.
           READ DEPARTMENTFILE INVALID KEY
                     GO TO ERROR-DEPARTMENT-PARA.
           DISPLAY " DEPARTMENT CODE :" LINE 1 COL 1.
           DISPLAY DEPCODE LINE 1 COL 20.
           DISPLAY " DEPARTMENT NAME :" LINE 2 COL 1.
           DISPLAY DEPNAME LINE 2 COL 20.
           CLOSE DEPARTMENTFILE.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       REVISION-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN INPUT REVISIONFILE.
           DISPLAY "ENTER REVISION CODE:".
           ACCEPT RREVID.
           DISPLAY " " WITH BLANK SCREEN.
           READ REVISIONFILE INVALID KEY
                    GO TO ERROR-REVISION-PARA.
           DISPLAY " REVISION CODE           :" LINE 1 COL 1.
           DISPLAY RREVID LINE 1 COL 27.
           DISPLAY " EMPLOYEE CODE           :" LINE 2 COL 1.
           DISPLAY REMPID LINE 2 COL 27.
           DISPLAY " DESIGNATION CODE        :" LINE 3 COL 1.
           DISPLAY RDESCODE LINE 3 COL 27.
           DISPLAY " BASIC                   :" LINE 4 COL 1.
           DISPLAY RBASIC LINE 4 COL 27.
           DISPLAY " HRA                     :" LINE 5 COL 1.
           DISPLAY RHRA LINE 5 COL 27.
           DISPLAY " DPA                     :" LINE 6 COL 1.
           DISPLAY RDPA LINE 6 COL 27.
           DISPLAY " PPA                     :" LINE 7 COL 1.
           DISPLAY RPPA LINE 7 COL 27.
           DISPLAY " EDUCATIONAL ALLOWANCE   :" LINE 8 COL 1.
           DISPLAY REDUA LINE 8 COL 27.
           DISPLAY " TECHNICAL JOURNAL       :" LINE 9 COL 1.
           DISPLAY RTECHJR LINE 9 COL 27.
           DISPLAY " LUNCH ALLOWANCE        :" LINE 10 COL 1.
           DISPLAY RLUNCHA LINE 10 COL 27.
           DISPLAY " CONVEYANCE             :" LINE 11 COL 1.
           DISPLAY RCONVEY LINE 11 COL 27.
           DISPLAY " BUSINESS ATTIREMENT    :" LINE 12 COL 1.
           DISPLAY RBUSATR LINE 12 COL 27.
           DISPLAY " LEAVE TRAVEL ALLOWANCE :" LINE 13 COL 1.
           DISPLAY RLTA LINE 13 COL 27.
           DISPLAY " PF                     :" LINE 14 COL 1.
           DISPLAY RPF LINE 14 COL 27.
           DISPLAY " ESI                    :" LINE 15 COL 1.
           DISPLAY RESI LINE 15 COL 27.
           DISPLAY " REVISED DATE           :" LINE 16 COL 1.
           DISPLAY RREVDATE LINE 16 COL 27.
           CLOSE REVISIONFILE.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       PAYMENT-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN INPUT PAYMENTFILE.
           DISPLAY "ENTER EMP CODE :".
           ACCEPT PEMPID.
           DISPLAY " " WITH BLANK SCREEN.
           READ PAYMENTFILE INVALID KEY GO TO ERROR-PAYMENT-PARA.
           DISPLAY " EMPLOYEE CODE                  :" LINE 1 COL 1.
           DISPLAY PEMPID LINE 1 COL 35.
           DISPLAY " BASIC                          :" LINE 2 COL 1.
           DISPLAY PBASIC LINE 2 COL 35.
           DISPLAY " DEARNESS ALLOWANCE             :" LINE 3 COL 1.
           DISPLAY PDA LINE 3 COL 35.
           DISPLAY " CITY COMPENSATORY ALLOWANCE    :" LINE 4 COL 1.
           DISPLAY PCCA LINE 4 COL 35.
           DISPLAY " HRA                            :" LINE 5 COL 1.
           DISPLAY PHRA LINE 5 COL 35.
           DISPLAY " DPA                            :" LINE 6 COL 1.
           DISPLAY PDPA LINE 6 COL 35.
           DISPLAY " PPA                            :" LINE 7 COL 1.
           DISPLAY PPPA LINE 7 COL 35.
           DISPLAY " EDUCATIONAL ALLOWANCE          :" LINE 8 COL 1.
           DISPLAY PEDUA LINE 8 COL 35.
           DISPLAY " TECHNICAL JOURNAL              :" LINE 9 COL 1.
           DISPLAY PTECHJR LINE 9 COL 35.
           DISPLAY " LUNCH ALLOWANCE               :" LINE 10 COL 1.
           DISPLAY PLUNCHA LINE 10 COL 35.
           DISPLAY " CONVEYANCE                    :" LINE 11 COL 1.
           DISPLAY PCONVEY LINE 11 COL 35.
           DISPLAY " BUSINESS ATTIREMENT           :" LINE 12 COL 1.
           DISPLAY PBUSATR LINE 12 COL 35.
           DISPLAY " LEAVE TRAVEL ALLOWANCE        :" LINE 13 COL 1.
           DISPLAY PLTA LINE 13 COL 35.
           DISPLAY " PF                            :" LINE 14 COL 1.
           DISPLAY PPF LINE 14 COL 35.
           DISPLAY " ESI                           :" LINE 15 COL 1.
           DISPLAY PESI LINE 15 COL 35.
           DISPLAY " GRATUITY                      :" LINE 16 COL 1.
           DISPLAY PGRTY LINE 16 COL 35.
           DISPLAY " PROFESSIONAL TAX              :" LINE 17 COL 1.
           DISPLAY PPTAX LINE 17 COL 35.
           DISPLAY " INCOME TAX                    :" LINE 18 COL 1.
           DISPLAY PITAX LINE 18 COL 35.
           DISPLAY " LOAN                          :" LINE 19 COL 1.
           DISPLAY PLOAN LINE 19 COL 35.
           DISPLAY " LOAN DEDUCTION AMOUNT         :" LINE 20 COL 1.
           DISPLAY PLOANDA LINE 20 COL 35.
           DISPLAY " OTHER DEDUCTION               :" LINE 21 COL 1.
           DISPLAY POTHERD LINE 21 COL 35.
           DISPLAY " PERFORMANCE INCENTIVE         :" LINE 22 COL 1.
           DISPLAY PPERINC LINE 22 COL 35.
           DISPLAY " MEDICAL REIMBURSEMENT         :" LINE 23 COL 1.
           DISPLAY PMEDI LINE 23 COL 35.
           DISPLAY " BOOK REIMBURSEMENT            :" LINE 24 COL 1.
           DISPLAY PBOOK LINE 24 COL 35.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY " ENTERTAINMENT                  :" LINE 1 COL 1.
           DISPLAY PENTER LINE 1 COL 35.
           DISPLAY " PHONE                          :" LINE 2 COL 1.
           DISPLAY PTPH LINE 2 COL 35.
           DISPLAY " HOUSE RELATED                  :" LINE 3 COL 1.
           DISPLAY PHOUSE LINE 3 COL 35.
           DISPLAY " VEHICLE MAINTENANCE            :" LINE 4 COL 1.
           DISPLAY PVEHMAN LINE 4 COL 35.
           DISPLAY  " CREDIT CARD                    :" LINE 5 COL 1.
           DISPLAY PCREDIT LINE 5 COL 35.
           DISPLAY " CLUB                           :" LINE 6 COL 1.
           DISPLAY PCLUB LINE 6 COL 35.
           DISPLAY " CASUAL LEAVE                   :" LINE 7 COL 1.
           DISPLAY PCL LINE 7 COL 35.
           DISPLAY " SICK LEAVE                     :" LINE 8 COL 1.
           DISPLAY PSL LINE 8 COL 35.
           DISPLAY " PAID LEAVE                     :" LINE 9 COL 1.
           DISPLAY PPL LINE 9 COL 35.
           DISPLAY " LEAVE LOSS OF PAY             :" LINE 10 COL 1.
           DISPLAY PLLOP LINE 10 COL 35.
           DISPLAY " OTHER LEAVES                  :" LINE 11 COL 1.
           DISPLAY POTHERL LINE 11 COL 35.
           CLOSE PAYMENTFILE.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       CONFIRMATION-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN INPUT CONFIRMATIONFILE.
           DISPLAY "ENTER CODE :".
           ACCEPT CCONID.
           DISPLAY " " WITH BLANK SCREEN.
           READ CONFIRMATIONFILE INVALID KEY
                   GO TO ERROR-CONFIRMATION-PARA.
           DISPLAY " CONFIRMATION CODE :" LINE 1 COL 1.
           DISPLAY CCONID LINE 1 COL 25.
           DISPLAY " EMPLOYEE CODE     :" LINE 2 COL 1.
           DISPLAY CEMPID LINE 2 COL 25.
           DISPLAY " CONFIRMATION DATE :" LINE 3 COL 1.
           DISPLAY CCDATE LINE 3 COL 25.
           CLOSE CONFIRMATIONFILE.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       GRADE-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN INPUT GRADEFILE.
           DISPLAY "ENTER GRADE NO. :".
           ACCEPT GR.
           DISPLAY " " WITH BLANK SCREEN.
           PERFORM GR-READ-PARA UNTIL FSG = 10.
       GR-READ-PARA.
           READ GRADEFILE AT END GO TO GR-EXIT-PARA.
           IF GGRADE = GR
           DISPLAY " GRADE NO.   :" LINE 1 COL 1.
           DISPLAY GGRADE LINE 1 COL 18.
           DISPLAY " DESIGNATION :" LINE 2 COL 1.
           DISPLAY GDESIGN LINE 2 COL 18.
       GR-EXIT-PARA.
           CLOSE GRADEFILE.
           DISPLAY ' '.
           DISPLAY ' '.
           DISPLAY "PRESS ENTER TO RETURN TO HRMS READ MENU".
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       TRANSFER-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN INPUT TRANSFERFILE.
           DISPLAY "ENTER TRANSFER CODE :".
           ACCEPT TTRFID.
           DISPLAY " " WITH BLANK SCREEN.
           READ TRANSFERFILE INVALID KEY GO TO ERROR-TRANSFER-PARA.
           DISPLAY " TRANSFER CODE     :" LINE 1 COL 1.
           DISPLAY TTRFID LINE 1 COL 25.
           DISPLAY " EMP CODE          :" LINE 2 COL 1.
           DISPLAY TEMPID LINE 2 COL 25.
           DISPLAY " OLD BRANCH CODE   :" LINE 3 COL 1.
           DISPLAY TOBRID LINE 3 COL 25.
           DISPLAY " TRANSFER DATE     :" LINE 4 COL 1.
           DISPLAY TTRFDT LINE 4 COL 25.
           CLOSE TRANSFERFILE.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       EMPPERSONAL-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN INPUT EMPPERSONALFILE.
           DISPLAY "ENTER EMP CODE :".
           ACCEPT EPEMPID.
           DISPLAY " " WITH BLANK SCREEN.
           READ EMPPERSONALFILE INVALID KEY
                           GO TO ERROR-EMPPERSONAL-PARA.
           DISPLAY " EMPLOYEE CODE     :" LINE 1 COL 1.
           DISPLAY EPEMPID LINE 1 COL 30.
           DISPLAY " TEMPORARY ADDRESS :" LINE 2 COL 1.
           DISPLAY EPTADD LINE 2 COL 30.
           DISPLAY " PHONE             :" LINE 3 COL 1.
           DISPLAY EPTPH LINE 3 COL 30.
           DISPLAY " DOB               :" LINE 4 COL 1.
           DISPLAY EPDOB LINE 4 COL 30.
           DISPLAY " POB               :" LINE 5 COL 1.
           DISPLAY EPPOB LINE 5 COL 30.
           DISPLAY " LANGUAGE KNOWN    :" LINE 6 COL 1.
           DISPLAY EPLANG LINE 6 COL 30.
           DISPLAY " BLOOD GROUP       :" LINE 7 COL 1.
           DISPLAY EPBLOOD LINE 7 COL 30.
           DISPLAY " WEIGHT            :" LINE 8 COL 1.
           DISPLAY EPWEIGHT LINE 8 COL 30.
           DISPLAY " HEIGHT            :" LINE 9 COL 1.
           DISPLAY EPHEIGHT LINE 9 COL 30.
           DISPLAY " VISION           :" LINE 10 COL 1.
           DISPLAY EPVISION LINE 10 COL 30.
           DISPLAY " FATHER'S NAME    :" LINE 11 COL 1.
           DISPLAY EPFATHER LINE 11 COL 30.
           DISPLAY " DOB OF FATHER    :" LINE 12 COL 1.
           DISPLAY EPDOBF LINE 12 COL 30.
           DISPLAY " MOTHER'S NAME    :" LINE 13 COL 1.
           DISPLAY EPMOTHER LINE 13 COL 30.
           DISPLAY " DOB OF MOTHER    :" LINE 14 COL 1.
           DISPLAY EPDOBM LINE 14 COL 30.
           DISPLAY " SPOUSE NAME      :" LINE 15 COL 1.
           DISPLAY EPSPOUSE LINE 15 COL 30.
           DISPLAY " CHILD NAME       :" LINE 16 COL 1.
           DISPLAY EPCHILD LINE 16 COL 30.
           DISPLAY " DOB OF CHILD     :" LINE 17 COL 1.
           DISPLAY EPDOBC LINE 17 COL 30.
           CLOSE EMPPERSONALFILE.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-EMP-PARA.
           CLOSE EMPFILE.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "INVALID CODE" LINE 12 COL 30.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-LEAVE-PARA.
           CLOSE LEAVEFILE.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "INVALID CODE" LINE 12 COL 30.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-BRANCH-PARA.
           CLOSE BRANCHFILE.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "INVALID CODE" LINE 12 COL 30.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-DEPARTMENT-PARA.
           CLOSE DEPARTMENTFILE.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "INVALID CODE" LINE 12 COL 30.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-REVISION-PARA.
           CLOSE REVISIONFILE.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "INVALID CODE" LINE 12 COL 30.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-PAYMENT-PARA.
           CLOSE PAYMENTFILE.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "INVALID CODE" LINE 12 COL 30.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-CONFIRMATION-PARA.
           CLOSE CONFIRMATIONFILE.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "INVALID CODE" LINE 12 COL 30.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-TRANSFER-PARA.
           CLOSE TRANSFERFILE.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "INVALID CODE" LINE 12 COL 30.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-EMPPERSONAL-PARA.
           CLOSE EMPPERSONALFILE.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "INVALID CODE" LINE 12 COL 30.
           DISPLAY
             "PRESS ENTER TO RETURN TO HRMS READ MENU" LINE 20 COL 10.
           ACCEPT ENTER LINE 20 COL 50.
           STOP ' '.
           GO TO MAIN-PARA.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPWRITE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EEMPID
           FILE STATUS IS FSO.

           SELECT LEAVEFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS LEMPID
           FILE STATUS IS FSL.

           SELECT BRANCHFILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FSB.

           SELECT DESIGNATIONFILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FSDES.

           SELECT DEPARTMENTFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS DEPCODE
           FILE STATUS IS FSDEP.

           SELECT REVISIONFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS RREVID
           ALTERNATE RECORD KEY IS REMPID
           FILE STATUS IS FSR.

           SELECT PAYMENTFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS PEMPID
           FILE STATUS IS FSP.

           SELECT CONFIRMATIONFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CCONID
           ALTERNATE RECORD KEY IS CEMPID
           FILE STATUS IS FSC.

           SELECT GRADEFILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FSG.

           SELECT TRANSFERFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TTRFID
           FILE STATUS IS FST.

           SELECT EMPPERSONALFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EPEMPID
           FILE STATUS IS FSEP.

       DATA DIVISION.
       FILE SECTION.
       FD EMPFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "EMP.DAT".
       01 EMPREC.
           02 EEMPID    PIC X(6).
           02 EEMPNAME  PIC X(25).
           02 EEMPADDR  PIC X(30).
           02 EPHONE    PIC X(10).
           02 EDOJ      PIC X(10).
           02 EDIP      PIC X(10).
           02 EUG       PIC X(4).
           02 EPG       PIC X(4).
           02 EPROFQ    PIC X(4).
           02 ESKILL    PIC X(10).
           02 EGRDNO    PIC 99.
           02 EBRNID    PIC X(6).
           02 EDESID    PIC X(6).

       FD LEAVEFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "LEAVE.DAT".
       01 LEAVEREC.
           02 LEMPID    PIC X(6).
           02 LFMDATE   PIC X(10).
           02 LTODATE   PIC X(10).
           02 LLEVCAT   PIC X(3).

       FD BRANCHFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "BRANCH.DAT".
       01 BRANCHREC.
           02 BBRID    PIC X(6).
           02 BBRNAME  PIC X(15).
           02 BBRADD   PIC X(30).
           02 BBRPH    PIC X(10).
           02 BEMAIL   PIC X(20).
           02 BMGRNAME PIC X(25).

       FD DESIGNATIONFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "DESIG.DAT".
       01 DESIGNATIONREC.
           02 DESID    PIC X(6).
           02 DESIGN   PIC X(15).
           02 DESHRT   PIC X(4).

       FD DEPARTMENTFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "DEPART.DAT".
       01 DEPARTMENTREC.
           02 DEPCODE  PIC X(6).
           02 DEPNAME  PIC X(20).

       FD REVISIONFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "REVISION.DAT".
       01 REVISIONREC.
           02 RREVID   PIC X(6).
           02 REMPID   PIC X(6).
           02 RDESCODE PIC X(6).
           02 RBASIC   PIC 9(6)V99.
           02 RHRA     PIC 9(6)V99.
           02 RDPA     PIC 9(6)V99.
           02 RPPA     PIC 9(6)V99.
           02 REDUA    PIC 9(6)V99.
           02 RTECHJR  PIC 9(6)V99.
           02 RLUNCHA  PIC 9(6)V99.
           02 RCONVEY  PIC 9(6)V99.
           02 RBUSATR  PIC 9(6)V99.
           02 RLTA     PIC 9(6)V99.
           02 RPF      PIC 9(6)V99.
           02 RESI     PIC 9(6)V99.
           02 RREVDATE PIC X(10).

       FD PAYMENTFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "PAYMENT.DAT".
       01 PAYMENTREC.
           02 PEMPID   PIC X(6).
           02 PBASIC   PIC 9(6)V99.
           02 PDA      PIC 9(6)V99.
           02 PCCA     PIC 9(6)V99.
           02 PHRA     PIC 9(6)V99.
           02 PDPA     PIC 9(6)V99.
           02 PPPA     PIC 9(6)V99.
           02 PEDUA    PIC 9(6)V99.
           02 PTECHJR  PIC 9(6)V99.
           02 PLUNCHA  PIC 9(6)V99.
           02 PCONVEY  PIC 9(6)V99.
           02 PBUSATR  PIC 9(6)V99.
           02 PLTA     PIC 9(6)V99.
           02 PPF      PIC 9(6)V99.
           02 PESI     PIC 9(6)V99.
           02 PGRTY    PIC 9(6)V99.
           02 PPTAX    PIC 9(6)V99.
           02 PITAX    PIC 9(6)V99.
           02 PLOAN    PIC 9(8)V99.
           02 PLOANDA  PIC 9(8)V99.
           02 POTHERD  PIC 9(6)V99.
           02 PPERINC  PIC 9(6)V99.
           02 PMEDI    PIC 9(6)V99.
           02 PBOOK    PIC 9(6)V99.
           02 PENTER   PIC 9(6)V99.
           02 PTPH     PIC 9(6)V99.
           02 PHOUSE   PIC 9(6)V99.
           02 PVEHMAN  PIC 9(6)V99.
           02 PCREDIT  PIC 9(6)V99.
           02 PCLUB    PIC 9(6)V99.
           02 PCL      PIC 99.
           02 PSL      PIC 99.
           02 PPL      PIC 99.
           02 PLLOP    PIC 999.
           02 POTHERL  PIC 999.

       FD CONFIRMATIONFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "CONFIRM.DAT".
       01 CONFIRMATIONREC.
           02 CCONID   PIC X(6).
           02 CEMPID   PIC X(6).
           02 CCDATE   PIC X(6).

       FD GRADEFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "GRADE.DAT".
       01 GRADEREC.
           02 GGRADE   PIC 99.
           02 GDESIGN  PIC X(25).

       FD TRANSFERFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "TRANSFER.DAT".
       01 TRANSFERREC.
           02 TTRFID   PIC X(6).
           02 TEMPID   PIC X(6).
           02 TOBRID   PIC X(6).
           02 TTRFDT   PIC X(10).

       FD EMPPERSONALFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "EMPPER.DAT".
       01 EMPPERSONALREC.
           02 EPEMPID  PIC X(6).
           02 EPTADD   PIC X(30).
           02 EPTPH    PIC X(10).
           02 EPDOB    PIC X(10).
           02 EPPOB    PIC X(10).
           02 EPLANG   PIC X(15).
           02 EPBLOOD  PIC X(4).
           02 EPWEIGHT PIC 999.
           02 EPHEIGHT PIC 999.
           02 EPVISION PIC X(15).
           02 EPFATHER PIC X(25).
           02 EPDOBF   PIC X(10).
           02 EPMOTHER PIC X(25).
           02 EPDOBM   PIC X(10).
           02 EPSPOUSE PIC X(25).
           02 EPCHILD  PIC X(25).
           02 EPDOBC   PIC X(10).

       WORKING-STORAGE SECTION.
       77 FSO   PIC XX.
       77 FSL   PIC XX.
       77 FSB   PIC XX.
       77 FSDES PIC XX.
       77 FSDEP PIC XX.
       77 FSR   PIC XX.
       77 FSP   PIC XX.
       77 FSC   PIC XX.
       77 FSG   PIC XX.
       77 FST   PIC XX.
       77 FSEP  PIC XX.
       77 CHOICE PIC 99.
       77  ENTER PIC X.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "*********************************" LINE 3 COL 10.
           DISPLAY "  HUMAN RESOURCE MANAGEMENT SYSTEM " LINE 5 COL 10.
           DISPLAY "********************************" LINE 7 COL 10.
           DISPLAY "01. EMPLOYEE FILE" LINE 11 COL 5.
           DISPLAY "02. LEAVE FILE" LINE 12 COL 5.
           DISPLAY "03. BRANCH FILE" LINE 13 COL 5.
           DISPLAY "04. DESIGNATION FILE" LINE 14 COL 5.
           DISPLAY "05. DEPARTMENT FILE" LINE 15 COL 5.
           DISPLAY "06. REVISION FILE" LINE 16 COL 5.
           DISPLAY "07. PAYMENT FILE" LINE 17 COL 5.
           DISPLAY "08. CONFIRMATION FILE" LINE 18 COL 5.
           DISPLAY "09. GRADE FILE" LINE 19 COL 5.
           DISPLAY "10. TRANSFER FILE" LINE 20 COL 5.
           DISPLAY "11. EMPLOYEE PERSONAL FILE" LINE 21 COL 5.
           DISPLAY "12. EXIT" LINE 22 COL 5.
           DISPLAY "ENTER U R CHOICE :" LINE 23 COL 25.
           ACCEPT CHOICE LINE 23 COL 45.
           IF CHOICE = 1
              GO TO EMP-PARA
           ELSE
             IF CHOICE = 2
                GO TO LEAVE-PARA
             ELSE
               IF CHOICE = 3
                  GO TO BRANCH-PARA
               ELSE
                 IF CHOICE = 4
                    GO TO DESIGNATION-PARA
                 ELSE
                   IF CHOICE = 5
                      GO TO DEPARTMENT-PARA
                   ELSE
                     IF CHOICE = 6
                        GO TO REVISION-PARA
                     ELSE
                       IF CHOICE = 7
                          GO TO PAYMENT-PARA
                       ELSE
                          IF CHOICE = 8
                             GO TO CONFIRMATION-PARA
                          ELSE
                            IF CHOICE = 9
                               GO TO GRADE-PARA
                            ELSE
                              IF CHOICE = 10
                                 GO TO TRANSFER-PARA
                              ELSE
                                IF CHOICE = 11
                                   GO TO EMPPERSONAL-PARA
                                 ELSE
                                   EXIT PROGRAM.

       EMP-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN I-O EMPFILE.
           IF FSO = 35
              OPEN OUTPUT EMPFILE.
           DISPLAY "ENTER CODE :" LINE 1 COL 1.
           ACCEPT EEMPID LINE 1 COL 35.
           DISPLAY "ENTER NAME :" LINE 2 COL 1.
           ACCEPT EEMPNAME LINE 2 COL 35.
           DISPLAY "ENTER ADDRESS :" LINE 3 COL 1.
           ACCEPT EEMPADDR LINE 3 COL 35.
           DISPLAY "ENTER PHONE :" LINE 4 COL 1.
           ACCEPT EPHONE LINE 4 COL 35.
           DISPLAY "ENTER DATE OF JOIN :" LINE 5 COL 1.
           ACCEPT EDOJ LINE 5 COL 35.
           DISPLAY "ENTER DIPLOMA :" LINE 6 COL 1.
           ACCEPT EDIP LINE 6 COL 35.
           DISPLAY "ENTER UG :" LINE 7 COL 1.
           ACCEPT EUG LINE 7 COL 35.
           DISPLAY "ENTER PG :" LINE 8 COL 1.
           ACCEPT EPG LINE 8 COL 35.
           DISPLAY "ENTER PROFESSIONAL QUALITY :" LINE 9 COL 1.
           ACCEPT EPROFQ  LINE 9 COL 35.
           DISPLAY "ENTER SKILL SET :" LINE 10 COL 1.
           ACCEPT ESKILL LINE 10 COL 35.
           DISPLAY "ENTER GRADE NUMBER :" LINE 11 COL 1.
           ACCEPT EGRDNO LINE 11 COL 35.
           DISPLAY "ENTER BRANCH CODE :" LINE 12 COL 1.
           ACCEPT EBRNID LINE 12 COL 35.
           DISPLAY "ENTER DESIGNATION CODE :" LINE 13 COL 1.
           ACCEPT EDESID LINE 13 COL 35.
           WRITE EMPREC.
           CLOSE EMPFILE.
           GO TO MAIN-PARA.

       LEAVE-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN I-O LEAVEFILE.
           IF FSL = 35
              OPEN OUTPUT LEAVEFILE.
           DISPLAY "ENTER CODE :" LINE 1 COL 1.
           ACCEPT LEMPID LINE 1 COL 35.
           DISPLAY "ENTER FROM DATE :" LINE 2 COL 1.
           ACCEPT LFMDATE LINE 2 COL 35.
           DISPLAY "ENTER TO DATE :" LINE 3 COL 1.
           ACCEPT LTODATE LINE 3 COL 35.
           DISPLAY "ENTER LEAVE CATEGORY :" LINE 4 COL 1.
           ACCEPT LLEVCAT LINE 4 COL 35.
           WRITE LEAVEREC.
           CLOSE LEAVEFILE.
           GO TO MAIN-PARA.

       BRANCH-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN EXTEND BRANCHFILE.
           IF FSB = 35
              OPEN OUTPUT BRANCHFILE.
           DISPLAY "ENTER BRANCH CODE :" LINE 1 COL 1.
           DISPLAY "ENTER BRANCH NAME :" LINE 2 COL 1.
           DISPLAY "ENTER BRANCH ADDRESS :" LINE 3 COL 1.
           DISPLAY "ENTER PHONE :" LINE 4 COL 1.
           DISPLAY "ENTER E-MAIL :" LINE 5 COL 1.
           DISPLAY "ENTER MANAGER NAME :" LINE 6 COL 1.
           ACCEPT BBRID LINE 1 COL 30 WITH UNDERLINE.
           ACCEPT BBRNAME LINE 2 COL 30 WITH UNDERLINE.
           ACCEPT BBRADD LINE 3 COL 30 WITH UNDERLINE.
           ACCEPT BBRPH LINE 4 COL 30 WITH UNDERLINE.
           ACCEPT BEMAIL LINE 5 COL 30 WITH UNDERLINE.
           ACCEPT BMGRNAME LINE 6 COL 30 WITH UNDERLINE.
           WRITE BRANCHREC.
           CLOSE BRANCHFILE.
           GO TO MAIN-PARA.

       DESIGNATION-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN EXTEND DESIGNATIONFILE.
           DISPLAY "ENTER DESIGNATION CODE :" LINE 1 COL 1.
           ACCEPT DESID LINE 1 COL 35.
           DISPLAY "ENTER DESIGNATION :" LINE 2 COL 1.
           ACCEPT DESIGN LINE 2 COL 35.
           DISPLAY "ENTER DES IN SHORT :" LINE 3 COL 1.
           ACCEPT DESHRT LINE 3 COL 35.
           WRITE DESIGNATIONREC.
           CLOSE DESIGNATIONFILE.
           GO TO MAIN-PARA.

       DEPARTMENT-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN I-O DEPARTMENTFILE.
           IF FSDEP = 35
           OPEN OUTPUT DEPARTMENTFILE.
           DISPLAY "ENTER DEPARTMENT CODE :" LINE 1 COL 1.
           ACCEPT DEPCODE LINE 1 COL 35.
           DISPLAY "ENTER DEPARTMENT NAME :" LINE 2 COL 1.
           ACCEPT DEPNAME LINE 2 COL 35.
           WRITE DEPARTMENTREC.
           CLOSE DEPARTMENTFILE.
           GO TO MAIN-PARA.

       REVISION-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN I-O REVISIONFILE.
           IF FSR = 35
              OPEN OUTPUT REVISIONFILE.
           DISPLAY "ENTER REVISION CODE :"  LINE 1 COL 1.
           ACCEPT RREVID LINE 1 COL 35.
           DISPLAY "ENTER EMPLOYEE CODE :" LINE 2 COL 1.
           ACCEPT REMPID LINE 2 COL 35.
           DISPLAY "ENTER DESIGNATION CODE :" LINE 3 COL 1.
           ACCEPT RDESCODE LINE 3 COL 35.
           DISPLAY "ENTER BASIC :" LINE 4 COL 1.
           ACCEPT RBASIC LINE 4 COL 35.
           DISPLAY "ENTER HRA :" LINE 5 COL 1.
           ACCEPT RHRA LINE 5 COL 35.
           DISPLAY "ENTER DPA :" LINE 6 COL 1.
           ACCEPT RDPA LINE 6 COL 35.
           DISPLAY "ENTER PPA :" LINE 7 COL 1.
           ACCEPT RPPA LINE 7 COL 35.
           DISPLAY "ENTER EDUCATIONAL ALLOWANCE :" LINE 8 COL 1.
           ACCEPT REDUA LINE 8 COL 35.
           DISPLAY "ENTER TECH. JOURNAL :" LINE 9 COL 1.
           ACCEPT RTECHJR LINE 9 COL 35.
           DISPLAY "ENTER LUNCH ALLOWANCE :" LINE 10 COL 1.
           ACCEPT RLUNCHA LINE 10 COL 35.
           DISPLAY "ENTER CONVEYANCE :" LINE 11 COL 1.
           ACCEPT RCONVEY LINE 11 COL 35.
           DISPLAY "ENTER BUSINESS ATTIREMENT :" LINE 12 COL 1.
           ACCEPT RBUSATR LINE 12 COL 35.
           DISPLAY "ENTER LEAVE TRAVEL ALLOWANCE :" LINE 13 COL 1.
           ACCEPT RLTA LINE 13 COL 35.
           DISPLAY "ENTER PF :" LINE 14 COL 1.
           ACCEPT RPF LINE 14 COL 35.
           DISPLAY "ENTER ESI :" LINE 15 COL 1.
           ACCEPT RESI LINE 15 COL 35.
           DISPLAY "ENTER REVISED DATE :" LINE 16 COL 1.
           ACCEPT RREVDATE LINE 16 COL 35.
           WRITE REVISIONREC.
           CLOSE REVISIONFILE.
           GO TO MAIN-PARA.

       PAYMENT-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN I-O PAYMENTFILE.
           IF FSP = 35
              OPEN OUTPUT PAYMENTFILE.
           DISPLAY "ENTER EMPLOYEE CODE :" LINE 1 COL 1.
           ACCEPT PEMPID LINE 1 COL 35.
           DISPLAY "ENTER BASIC :" LINE 2 COL 1.
           ACCEPT PBASIC LINE 2 COL 35.
           DISPLAY "ENTER DA :" LINE 3 COL 1.
           ACCEPT PDA LINE 3 COL 35.
           DISPLAY "ENTER CCA :" LINE 4 COL 1.
           ACCEPT PCCA LINE 4 COL 35.
           DISPLAY "ENTER HRA :" LINE 5 COL 1.
           ACCEPT PHRA LINE 5 COL 35.
           DISPLAY "ENTER DPA :" LINE 6 COL 1.
           ACCEPT PDPA LINE 6 COL 35.
           DISPLAY "ENTER PPA :" LINE 7 COL 1.
           ACCEPT PPPA LINE 7 COL 35.
           DISPLAY "ENTER EDUCATIONAL ALLOWANCE :" LINE 8 COL 1.
           ACCEPT PEDUA LINE 8 COL 35.
           DISPLAY "ENTER TECH. JOURNAL :" LINE 9 COL 1.
           ACCEPT PTECHJR  LINE 9 COL 35.
           DISPLAY "ENTER LUNCH ALLOWANCE :" LINE 10 COL 1.
           ACCEPT PLUNCHA  LINE 10 COL 35.
           DISPLAY "ENTER CONVEYANCE :" LINE 11 COL 1.
           ACCEPT PCONVEY  LINE 11 COL 35.
           DISPLAY "ENTER BUSINESS ATTIREMENT :" LINE 12 COL 1.
           ACCEPT PBUSATR LINE 12 COL 35.
           DISPLAY "ENTER LEAVE TRAVEL ALLOWANCE :" LINE 13 COL 1.
           ACCEPT PLTA LINE 13 COL 35.
           DISPLAY "ENTER PF :" LINE 14 COL 1.
           ACCEPT PPF LINE 14 COL 35.
           DISPLAY "ENTER ESI :" LINE 15 COL 1.
           ACCEPT PESI  LINE 15 COL 35.
           DISPLAY "ENTER GRATUITY :" LINE 16 COL 1.
           ACCEPT PGRTY  LINE 16 COL 35.
           DISPLAY "ENTER PROFESSIONAL TAX :" LINE 17 COL 1.
           ACCEPT PPTAX  LINE 17 COL 35.
           DISPLAY "ENTER INCOME TAX :" LINE 18 COL 1.
           ACCEPT PITAX LINE 18 COL 35.
           DISPLAY "ENTER LOAN :" LINE 19 COL 1.
           ACCEPT PLOAN LINE 19 COL 35.
           DISPLAY "ENTER LOAN DEDUCTION AMOUNT :" LINE 20 COL 1.
           ACCEPT PLOANDA LINE 20 COL 35.
           DISPLAY "ENTER OTHER DEDUCTION :" LINE 21 COL 1.
           ACCEPT POTHERD LINE 21 COL 35.
           DISPLAY "ENTER PERFORMANCE INCENTIVE :" LINE 22 COL 1.
           ACCEPT PPERINC LINE 22 COL 35.
           DISPLAY "ENTER MEDICAL REIMBURSEMENT :" LINE 23 COL 1.
           ACCEPT PMEDI LINE 23 COL 35.
           DISPLAY "ENTER BOOK REIMBURSEMENT :" LINE 24 COL 1.
           ACCEPT PBOOK LINE 24 COL 35.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "ENTER ENTERTAINMENT :" LINE 1 COL 1.
           ACCEPT PENTER  LINE 1 COL 35.
           DISPLAY "ENTER PHONE :" LINE 2 COL 1.
           ACCEPT PTPH  LINE 2 COL 35.
           DISPLAY "ENTER HOUSE RELATED :" LINE 3 COL 1.
           ACCEPT PHOUSE  LINE 3 COL 35.
           DISPLAY "ENTER VEHICLE MAINTENANCE :" LINE 4 COL 1.
           ACCEPT PVEHMAN  LINE 4 COL 35.
           DISPLAY "ENTER CREDIT CARD :" LINE 5 COL 1.
           ACCEPT PCREDIT LINE 5 COL 35.
           DISPLAY "ENTER CLUB :" LINE 6 COL 1.
           ACCEPT PCLUB LINE 6 COL 35.
           DISPLAY "ENTER CLUB :" LINE 7 COL 1.
           ACCEPT PCLUB LINE 7 COL 35.
           DISPLAY "ENTER CLUB :" LINE 8 COL 1.
           ACCEPT PCLUB LINE 8 COL 35.
           DISPLAY "ENTER CASUAL LEAVE :" LINE 9 COL 1.
           ACCEPT PCL LINE 9 COL 35.
           DISPLAY "ENTER SICK LEAVE :" LINE 10 COL 1.
           ACCEPT PSL LINE 10 COL 35.
           DISPLAY "ENTER PAID LEAVE :" LINE 11 COL 1.
           ACCEPT PPL LINE 11 COL 35.
           DISPLAY "ENTER LEAVE LOSS OF PAY :" LINE 12 COL 1.
           ACCEPT PLLOP LINE 12 COL 35.
           DISPLAY "ENTER OTHER LEAVES :" LINE 13 COL 1.
           ACCEPT POTHERL LINE 13 COL 35.
           WRITE PAYMENTREC.
           CLOSE PAYMENTFILE.
           GO TO MAIN-PARA.

       CONFIRMATION-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN I-O CONFIRMATIONFILE.
           IF FSC = 35
              OPEN OUTPUT CONFIRMATIONFILE.
           DISPLAY "ENTER CONFIRMATION CODE :" LINE 1 COL 1.
           ACCEPT CCONID LINE 1 COL 35.
           DISPLAY "ENTER EMP CODE :" LINE 2 COL 1.
           ACCEPT CEMPID LINE 2 COL 35.
           DISPLAY "ENTER CONFIRMATION DATE :" LINE 3 COL 1.
           ACCEPT CCDATE LINE 3 COL 35.
           WRITE CONFIRMATIONREC.
           CLOSE CONFIRMATIONFILE.
           GO TO MAIN-PARA.

       GRADE-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN EXTEND GRADEFILE.
           DISPLAY "ENTER GRADE NO. :" LINE 1 COL 1.
           ACCEPT GGRADE LINE 1 COL 35.
           DISPLAY "ENTER DESIGNATION :" LINE 2 COL 1.
           ACCEPT GDESIGN LINE 2 COL 35.
           WRITE GRADEREC.
           CLOSE GRADEFILE.
           GO TO MAIN-PARA.

       TRANSFER-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN I-O TRANSFERFILE.
           IF FST = 35
              OPEN OUTPUT TRANSFERFILE.
           DISPLAY "ENTER TRANSFER CODE :" LINE 1 COL 1.
           ACCEPT TTRFID LINE 1 COL 35.
           DISPLAY "ENTER EMP CODE :" LINE 2 COL 1.
           ACCEPT TEMPID LINE 2 COL 35.
           DISPLAY "ENTER OLD BRANCH CODE :" LINE 3 COL 1.
           ACCEPT TOBRID LINE 3 COL 35.
           DISPLAY "ENTER TRANSFER DATE :" LINE 4 COL 1.
           ACCEPT TTRFDT LINE 4 COL 35.
           WRITE TRANSFERREC.
           CLOSE TRANSFERFILE.
           GO TO MAIN-PARA.

       EMPPERSONAL-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN I-O EMPPERSONALFILE.
           IF FSEP = 35
              OPEN OUTPUT EMPPERSONALFILE.
           DISPLAY "ENTER EMP CODE :" LINE 1 COL 1.
           ACCEPT EPEMPID LINE 1 COL 35.
           DISPLAY "ENTER TEMP ADDRESS :" LINE 2 COL 1.
           ACCEPT EPTADD LINE 2 COL 35.
           DISPLAY "ENTER PHONE :" LINE 3 COL 1.
           ACCEPT EPTPH LINE 3 COL 35.
           DISPLAY "ENTER DOB :" LINE 4 COL 1.
           ACCEPT EPDOB LINE 4 COL 35.
           DISPLAY "ENTER POB :" LINE 5 COL 1.
           ACCEPT EPPOB LINE 5 COL 35.
           DISPLAY "ENTER LANGUAGE KNOWN :" LINE 6 COL 1.
           ACCEPT EPLANG LINE 6 COL 35.
           DISPLAY "ENTER BLOOD GROUP :" LINE 7 COL 1.
           ACCEPT EPBLOOD LINE 7 COL 35.
           DISPLAY "ENTER WEIGHT :" LINE 8 COL 1.
           ACCEPT EPWEIGHT LINE 8 COL 35.
           DISPLAY "ENTER HEIGHT :"LINE 9 COL 1.
           ACCEPT EPHEIGHT LINE 9 COL 35.
           DISPLAY "ENTER VISION :" LINE 10 COL 1.
           ACCEPT EPVISION LINE 10 COL 35.
           DISPLAY "ENTER FATHER'S NAME :" LINE 11 COL 1.
           ACCEPT EPFATHER LINE 11 COL 35.
           DISPLAY "ENTER DOB OF FATHER :" LINE 12 COL 1.
           ACCEPT EPDOBF LINE 12 COL 35.
           DISPLAY "ENTER MOTHER'S NAME :" LINE 13 COL 1.
           ACCEPT EPMOTHER LINE 13 COL 35.
           DISPLAY "ENTER DOB OF MOTHER :" LINE 14 COL 1.
           ACCEPT EPDOBM LINE 14 COL 35.
           DISPLAY "ENTER SPOUSE NAME :" LINE 15 COL 1.
           ACCEPT EPSPOUSE LINE 15 COL 35.
           DISPLAY "ENTER CHILD NAME :" LINE 16 COL 1.
           ACCEPT EPCHILD LINE 16 COL 35.
           DISPLAY "ENTER DOB OF CHILD :" LINE 17 COL 1.
           ACCEPT EPDOBC LINE 17 COL 35.
           WRITE EMPPERSONALREC.
           CLOSE EMPPERSONALFILE.
           GO TO MAIN-PARA.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPBRANCHLIST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BRANCHFILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FSB.

       DATA DIVISION.
       FILE SECTION.

       FD BRANCHFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "BRANCH.DAT".
       01 BRANCHREC.
           02 BBRID    PIC X(6).
           02 BBRNAME  PIC X(15).
           02 BBRADD   PIC X(30).
           02 BBRPH    PIC X(10).
           02 BEMAIL   PIC X(20).
           02 BMGRNAME PIC X(25).

       WORKING-STORAGE SECTION.
       77 FSB PIC XX.
       77 CITY PIC X(20).
       77 CTT PIC X(10).
       77 BBR PIC X(6).
       77 ACTS PIC 9999 VALUE 0101.
       77 ACT PIC 9999 VALUE 0135.
       77 SHOW PIC 9 VALUE 0.
       77 ENTER PIC X.
       77 LONG PIC 9(2).
       77 CONT PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           GO TO BRANCH-PARA.

       BRANCH-PARA.
           DISPLAY " " WITH BLANK SCREEN.
           OPEN INPUT BRANCHFILE.
           DISPLAY "CITY NAME: ".
           ACCEPT CITY.
           DISPLAY " " WITH BLANK SCREEN.
           PERFORM BRANCH-READ-IN UNTIL FSB = 10.
       BRANCH-READ-IN.
           PERFORM BRANCH-READ-PARA UNTIL FSB = 10 OR SHOW = 3.
           DISPLAY "PREESS TO SEE MORE" AT ACTS.
           ADD 45 TO ACTS.
           ACCEPT ENTER AT ACTS.
           DISPLAY " " WITH BLANK SCREEN.
           MOVE 0 TO SHOW.
           MOVE 0101 TO ACTS.
           MOVE 0135 TO ACT.
       BRANCH-READ-PARA.
           READ BRANCHFILE RECORD AT END GO TO BRANCH-EXIT.
           *>no hay ciudad hay que cambiarlo o algo
           IF CITY = ""
               ADD 1 TO SHOW
               DISPLAY " BRANCH CODE    :" AT ACTS
               DISPLAY BBRID AT ACT
               ADD 100 TO ACT
               ADD 100 TO ACTS
               DISPLAY " BRANCH NAME    :" AT ACTS
               DISPLAY BBRNAME AT ACT
               ADD 100 TO ACT
               ADD 100 TO ACTS
               DISPLAY " BRANCH ADDRESS :" AT ACTS
               DISPLAY BBRADD AT ACT
               ADD 100 TO ACT
               ADD 100 TO ACTS
               DISPLAY " PHONE          :" AT ACTS
               DISPLAY BBRPH AT ACT
               ADD 100 TO ACT
               ADD 100 TO ACTS
               DISPLAY " E-MAIL         :" AT ACTS
               DISPLAY BEMAIL AT ACT
               ADD 100 TO ACT
               ADD 100 TO ACTS
               DISPLAY " MANAGER NAME   :" AT ACTS
               DISPLAY BMGRNAME AT ACT
               ADD 100 TO ACT
               ADD 100 TO ACTS
               DISPLAY "------" AT ACTS
               ADD 100 TO ACT
               ADD 100 TO ACTS
           ELSE
               INSPECT CITY CONVERTING "abcdefghijklmnñopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ"
               INSPECT BBRADD CONVERTING "abcdefghijklmnñopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ"
               INSPECT CITY TALLYING LONG FOR CHARACTERS BEFORE "  "
               INSPECT BBRADD TALLYING CONT FOR ALL CITY(1:LONG).
               IF CONT > 0
                   ADD 1 TO SHOW
                   DISPLAY " BRANCH CODE    :" AT ACTS
                   DISPLAY BBRID AT ACT
                   ADD 100 TO ACT
                   ADD 100 TO ACTS
                   DISPLAY " BRANCH NAME    :" AT ACTS
                   DISPLAY BBRNAME AT ACT
                   ADD 100 TO ACT
                   ADD 100 TO ACTS
                   DISPLAY " BRANCH ADDRESS :" AT ACTS
                   DISPLAY BBRADD AT ACT
                   ADD 100 TO ACT
                   ADD 100 TO ACTS
                   DISPLAY " PHONE          :" AT ACTS
                   DISPLAY BBRPH AT ACT
                   ADD 100 TO ACT
                   ADD 100 TO ACTS
                   DISPLAY " E-MAIL         :" AT ACTS
                   DISPLAY BEMAIL AT ACT
                   ADD 100 TO ACT
                   ADD 100 TO ACTS
                   DISPLAY " MANAGER NAME   :" AT ACTS
                   DISPLAY BMGRNAME AT ACT
                   ADD 100 TO ACT
                   ADD 100 TO ACTS
                   DISPLAY "------" AT ACTS
                   ADD 100 TO ACT
                   ADD 100 TO ACTS
           END-IF.
               MOVE 0 TO CONT.
               MOVE 00 TO LONG.
       BRANCH-EXIT.
           CLOSE BRANCHFILE.
           DISPLAY " ".
           DISPLAY "PRESS ENTER TO RETURN TO HRMS MENU" AT ACTS.
           ADD 45 TO ACTS.
           ACCEPT ENTER AT ACTS.
           STOP ' '.
           EXIT PROGRAM.

       END PROGRAM EMPBRANCHLIST.
       END PROGRAM EMPWRITE.
       END PROGRAM EMPREAD.
       END PROGRAM MAINHRMS.
