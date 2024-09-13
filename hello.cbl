000100 ID DIVISION.                                                     00000100
000200 PROGRAM-ID. HELLO.                                               00000200
000300*    THIS IS SAMPLE FOR GIT/DBB TRAINING NEW12345  - EPSDEMOS 2008    00000300
000400*     test123                                                            00000400
000500*    THIS PROGRAM WILL RECEIVE A DATE AND COVERT THE DATE TO      00000500
000600*    AN INTEGER IN A CALLED PROGRAM TO DETERMINE DAYS FROM        00000600
000700*    CURRENT DATE.                                                00000700
000800*                                                                 00000800
000900*    (C) 2017 IBM - JIM HILDNER RESERVED.                         00000900
001000 ENVIRONMENT DIVISION.                                            00001000
001100 CONFIGURATION SECTION.                                           00001100
001200 SOURCE-COMPUTER. IBM-FLEX-ES.                                    00001200
001300 OBJECT-COMPUTER. IBM-FLEX-ES.                                    00001300
001400*                                                                 00001400
001500 DATA DIVISION.                                                   00001500
001600 WORKING-STORAGE SECTION.                                         00001600
001700*                                                                 00001700
001800 01 W-FLAGS.                                                      00001800
001900    10 W-SEND-FLAG             PIC X.                             00001900
002000       88 SEND-ERASE                            VALUE '1'.        00002000
002100       88 SEND-DATAONLY                         VALUE '2'.        00002100
002200       88 SEND-MAPONLY                          VALUE '3'.        00002200
002300       88 SEND-DATAONLY-ALARM                   VALUE '4'.        00002300
002400       88 SEND-ALL                              VALUE '5'.        00002400
002500                                                                  00002500
002600 01 W-CONVERSIONS.                                                00002600
002700    05 W-PMT-CNVRT             PIC X(12).                         00002700
002800    05 W-PMT-NUMBER                                               00002800
002900          REDEFINES W-PMT-CNVRT                                   00002900
003000                               PIC 9(10)V99.                      00003000
003100    05 WS-FORMAT-NUMBER        PIC Z,ZZZ,ZZ9.99.                  00003100
003200    05 W-PRINC-CNVRT           PIC X(12).                         00003200
003300    05 W-PRINC-NUMBER                                             00003300
003400          REDEFINES W-PRINC-CNVRT                                 00003400
003500                               PIC 9(10)V99.                      00003500
003600                                                                  00003600
003700 01 W-CALL-PROGRAM             PIC X(8).                          00003700
003800*                                                                 00003800
003900 01 W-RETIREMENT-WA            PIC 9(4).                          00003900
004000 01 W-COMAREA-LENGTH           PIC 9(4) COMP.                     00004000
004100                                                                  00004100
004200 01 SQL-ERROR-MSG.                                                00004200
004300    03 FILLER                  PIC X(11)        VALUE             00004300
004400                                                    'SQL ERROR: '.00004400
004500    03 SQL-ERROR-CODE          PIC 9(5) DISPLAY.                  00004500
004600*                                                                 00004600
004700     EXEC SQL                                                     00004700
004800         INCLUDE SQLCA                                            00004800
004900     END-EXEC.                                                    00004900
005000*                                                                 00005000
005100     EXEC SQL DECLARE SYSIBM.SYSDUMMY1 TABLE                      00005100
005200     ( IBMREQD CHAR(1) NOT NULL                                   00005200
005300     ) END-EXEC.                                                  00005300
005400*                                                                 00005400
005500 01 IBMREQD                    PIC X(1).                          00005500
005600*                                                                 00005600
005700 01 END-OF-TRANS-MSG           PIC X(30)                          00005700
005800                                                VALUE             00005800
005900                                 'END OF TRANSACTION - THANK YOU'.00005900
006000 01 BLANK-MSG                  PIC X(1)         VALUE ' '.        00006000
006100     COPY DFHAID.                                                 00006100
006200*    COPY DFHEIBLK.                                               00006200
006300     COPY EPSMORT.                                                00006300
006400                                                                  00006400
006500 01 W-COMMUNICATION-AREA.                                         00006500
006600     COPY EPSMTCOM.                                               00006600
006700                                                                  00006700
006800 COPY EPSNBRPM.                                                   00006800
006900                                                                  00006900
007000 LINKAGE SECTION.                                                 00007000
007100                                                                  00007100
007200 01 DFHCOMMAREA.                                                  00007200
007300 COPY EPSMTCOM.                                                   00007300
007400                                                                  00007400
007500 PROCEDURE DIVISION USING DFHCOMMAREA.                            00007500
007600                                                                  00007600
007700 EPSCMORT-MAINLINE.                                               00007700
007800* Call procedure to do SQL call                                   00007800
007900     PERFORM A805-DUMMY-SQL-CALL                                  00007900
008000     MOVE LENGTH OF DFHCOMMAREA TO W-COMAREA-LENGTH.              00008000
008100     MOVE DFHCOMMAREA TO W-COMMUNICATION-AREA.                    00008100
008200     EVALUATE TRUE                                                00008200
008300     WHEN EIBCALEN = ZERO                                         00008300
008400* First time in - Show Screen                                     00008400
008500          MOVE LOW-VALUES TO EPMENUO                              00008500
008600          SET SEND-ERASE TO TRUE                                  00008600
008700          PERFORM A300-SEND-MAP                                   00008700
008800          MOVE '3' TO                                             00008800
008900             PROCESS-INDICATOR OF W-COMMUNICATION-AREA            00008900
009000     WHEN EIBAID = DFHCLEAR                                       00009000
009100* Process CLEAR key                                               00009100
009200          MOVE LOW-VALUES TO EPMENUO                              00009200
009300          SET SEND-ERASE TO TRUE                                  00009300
009400          PERFORM A300-SEND-MAP                                   00009400
009500     WHEN EIBAID = DFHPF3 OR DFHPF12                              00009500
009600* Process END/RETURN keys                                         00009600
009700          IF PROCESS-INDICATOR OF W-COMMUNICATION-AREA = '3'      00009700
009800             EXEC CICS                                            00009800
009900                  SEND TEXT FROM (END-OF-TRANS-MSG)               00009900
010000                  ERASE                                           00010000
010100                  FREEKB                                          00010100
010200                  END-EXEC                                        00010200
010300             EXEC CICS                                            00010300
010400                  RETURN                                          00010400
010500                  END-EXEC                                        00010500
010600          ELSE                                                    00010600
010700             SET SEND-ALL TO TRUE                                 00010700
010800             EXEC CICS                                            00010800
010900                  SEND TEXT FROM (BLANK-MSG)                      00010900
011000                  ERASE                                           00011000
011100                  FREEKB                                          00011100
011200                  END-EXEC                                        00011200
011300             PERFORM A300-SEND-MAP                                00011300
011400             MOVE '3' TO                                          00011400
011500                PROCESS-INDICATOR OF W-COMMUNICATION-AREA         00011500
011600          END-IF                                                  00011600
011700     WHEN EIBAID = DFHPF9                                         00011700
011800          MOVE '9' TO                                             00011800
011900             PROCESS-INDICATOR OF W-COMMUNICATION-AREA            00011900
012000          EXEC CICS LINK PROGRAM('EPSMLIST')                      00012000
012100               COMMAREA(W-COMMUNICATION-AREA)                     00012100
012200               END-EXEC                                           00012200
012300     WHEN EIBAID = DFHENTER                                       00012300
012400* Process ENTER Key                                               00012400
012500          IF PROCESS-INDICATOR OF W-COMMUNICATION-AREA = '3'      00012500
012600             PERFORM A100-PROCESS-MAP                             00012600
012700          ELSE                                                    00012700
012800             EXEC CICS LINK PROGRAM('EPSMLIST')                   00012800
012900                  COMMAREA(W-COMMUNICATION-AREA)                  00012900
013000                  END-EXEC                                        00013000
013100          END-IF                                                  00013100
013200     WHEN OTHER                                                   00013200
013300* Process Data                                                    00013300
013400          IF PROCESS-INDICATOR OF W-COMMUNICATION-AREA = '3'      00013400
013500             PERFORM A600-CALCULATE-MORTGAGE                      00013500
013600             EXEC CICS RETURN                                     00013600
013700                  END-EXEC                                        00013700
013800*             ELSE                                                00013800
013900*                MOVE X'E8' TO MSGERRA                            00013900
014000*                MOVE LOW-VALUES TO EPMENUO                       00014000
014100*                SET SEND-DATAONLY-ALARM TO TRUE                  00014100
014200*                PERFORM A300-SEND-MAP                            00014200
014300          END-IF                                                  00014300
014400     END-EVALUATE                                                 00014400
014500     EXEC CICS                                                    00014500
014600          RETURN TRANSID('EPSP')                                  00014600
014700          COMMAREA(W-COMMUNICATION-AREA)                          00014700
014800          LENGTH(W-COMAREA-LENGTH)                                00014800
014900          END-EXEC.                                               00014900
015000                                                                  00015000
015100 A100-PROCESS-MAP.                                                00015100
015200     PERFORM A400-RECEIVE-MAP.                                    00015200
015300     PERFORM A600-CALCULATE-MORTGAGE                              00015300
015400     SET SEND-DATAONLY TO TRUE                                    00015400
015500     PERFORM A300-SEND-MAP                                        00015500
015600     .                                                            00015600
015700                                                                  00015700
015800 A300-SEND-MAP.                                                   00015800
015900     EVALUATE TRUE                                                00015900
016000     WHEN SEND-MAPONLY                                            00016000
016100          EXEC CICS                                               00016100
016200               SEND MAP('EPMENU')                                 00016200
016300               MAPSET('EPSMORT')                                  00016300
016400               MAPONLY                                            00016400
016500               CURSOR                                             00016500
016600               END-EXEC                                           00016600
016700     WHEN SEND-ERASE                                              00016700
016800          EXEC CICS                                               00016800
016900               SEND MAP('EPMENU')                                 00016900
017000               MAPSET('EPSMORT')                                  00017000
017100               FROM (EPMENUO)                                     00017100
017200               ERASE                                              00017200
017300               CURSOR                                             00017300
017400               END-EXEC                                           00017400
017500     WHEN SEND-DATAONLY                                           00017500
017600          EXEC CICS                                               00017600
017700               SEND MAP('EPMENU')                                 00017700
017800               MAPSET('EPSMORT')                                  00017800
017900               FROM (EPMENUO)                                     00017900
018000               DATAONLY                                           00018000
018100               CURSOR                                             00018100
018200               END-EXEC                                           00018200
018300     WHEN SEND-ALL                                                00018300
018400          EXEC CICS                                               00018400
018500               SEND MAP('EPMENU')                                 00018500
018600               MAPSET('EPSMORT')                                  00018600
018700               FROM (EPMENUO)                                     00018700
018800               END-EXEC.                                          00018800
018900                                                                  00018900
019000 A400-RECEIVE-MAP.                                                00019000
019100     EXEC CICS                                                    00019100
019200          RECEIVE MAP('EPMENU')                                   00019200
019300          MAPSET('EPSMORT')                                       00019300
019400          INTO (EPMENUI)                                          00019400
019500          END-EXEC.                                               00019500
019600                                                                  00019600
019700     MOVE EPLOANI TO EPSPARM-VALIDATE-DATA.                       00019700
019800     MOVE LENGTH OF EPLOANI                                       00019800
019900        TO EPSPARM-MAX-LENGTH.                                    00019900
020000     CALL 'EPSNBRVL' USING EPS-NUMBER-VALIDATION.                 00020000
020100     COMPUTE EPSPCOM-PRINCIPLE-DATA                               00020100
020200        OF W-COMMUNICATION-AREA                                   00020200
020300        = EPSPARM-NUMBER + EPSPARM-DECIMAL.                       00020300
020400                                                                  00020400
020500     MOVE EPYEARSI TO EPSPARM-VALIDATE-DATA.                      00020500
020600     MOVE LENGTH OF EPYEARSI TO EPSPARM-MAX-LENGTH.               00020600
020700     CALL 'EPSNBRVL' USING EPS-NUMBER-VALIDATION.                 00020700
020800     COMPUTE EPSPCOM-NUMBER-OF-YEARS                              00020800
020900        OF W-COMMUNICATION-AREA                                   00020900
021000        = EPSPARM-NUMBER + EPSPARM-DECIMAL.                       00021000
021100                                                                  00021100
021200     MOVE EPRATEI TO EPSPARM-VALIDATE-DATA.                       00021200
021300     MOVE LENGTH OF EPRATEI TO EPSPARM-MAX-LENGTH.                00021300
021400     CALL 'EPSNBRVL' USING EPS-NUMBER-VALIDATION.                 00021400
021500     COMPUTE EPSPCOM-QUOTED-INTEREST-RATE                         00021500
021600        OF W-COMMUNICATION-AREA                                   00021600
021700        = EPSPARM-NUMBER + EPSPARM-DECIMAL.                       00021700
021800                                                                  00021800
021900                                                                  00021900
022000 A600-CALCULATE-MORTGAGE.                                         00022000
022100     MOVE 'Y' TO EPSPCOM-YEAR-MONTH-IND                           00022100
022200        OF W-COMMUNICATION-AREA.                                  00022200
022300     MOVE 'EPSCSMRT' TO W-CALL-PROGRAM                            00022300
022400     EXEC CICS LINK PROGRAM(W-CALL-PROGRAM)                       00022400
022500          COMMAREA(W-COMMUNICATION-AREA)                          00022500
022600          END-EXEC                                                00022600
022700     .                                                            00022700
022800     MOVE EPSPCOM-RETURN-MONTH-PAYMENT                            00022800
022900        OF W-COMMUNICATION-AREA                                   00022900
023000        TO WS-FORMAT-NUMBER.                                      00023000
023100                                                                  00023100
023200     MOVE WS-FORMAT-NUMBER                                        00023200
023300        TO EPPAYMNTO.                                             00023300
023400     MOVE EPSPCOM-ERRMSG                                          00023400
023500        OF W-COMMUNICATION-AREA                                   00023500
023600        TO MSGERRO.                                               00023600
023700                                                                  00023700
023800 A805-DUMMY-SQL-CALL.                                             00023800
023900     EXEC SQL                                                     00023900
024000          SELECT IBMREQD                                          00024000
024100          INTO :IBMREQD                                           00024100
024200          FROM SYSIBM.SYSDUMMY1                                   00024200
024300          END-EXEC.                                               00024300
024400*                                                                 00024400
024500     IF SQLCODE = 100                                             00024500
024600        MOVE 'No rows found on SYSDUMM1.' TO MSGERRO              00024600
024700     ELSE                                                         00024700
024800        IF SQLCODE NOT = 0                                        00024800
024900           MOVE SQLCODE TO SQL-ERROR-CODE                         00024900
025000           MOVE SQL-ERROR-MSG TO MSGERRO                          00025000
025100        END-IF                                                    00025100
025200     END-IF.                                                      00025200
025300*                                                                 00025300
