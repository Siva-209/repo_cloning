000100 IDENTIFICATION DIVISION.                                         
000200 PROGRAM-ID.    MXBPA045.                                         
000300*AUTHOR.        TEST.                                             
000400*DATE-WRITTEN.  DECEMBER 1995.                                    
000500*REMARKS.                                                         
000600*================================================================*
000700* DESCRIPTION:                                                   |
000800*                                                                |
000900* MXBPA045 - STATEMENT OF CREDITS EXTRACT PROGRAM.               |
001000* THIS PROGRAM EXTRACTS DATA FOR STATEMENT OF CREDITS PROCESSING.|
001100*================================================================*
001200*  TABLE   |   VIEW   |     DESCRIPTION                          |
001300*----------+-----------------------------------------------------*
001400* TBMCTUPD | VWMCTUPD |  DATE TABLE                              |
001500*          | VWMJ280  |  APPLIED PAYMENTS                        |
001600* TBMTRLI  | VWMTRLI  |  TRUST LINE ITEM                         |
001700* TBMCU00  | VWMCU00  |  CUSTOMER MASTER                         |
001800* TBMCN00  | VWMCN00  |  CONTROL ENTITY                          |
001900* TBMRP00  | VWMRP00  |  REP MASTER                              |
002000*================================================================*
002100*   PLAN   | BIND INCLUDE MEMBERS                                |
002200*----------+-----------------------------------------------------*
002300* PBMPA045 | PBMPA045                                            |
002400*================================================================*
009900*-----------------------------------------------------------------
010000                                                                  
010100 ENVIRONMENT DIVISION.                                            
010200                                                                  
010300 INPUT-OUTPUT SECTION.                                            
010400 FILE-CONTROL.                                                    
010500     SELECT PROCESSING-FREQUENCY-PARM                             
010600         ASSIGN TO MXPA045I.                                      
010700                                                                  
010800     SELECT STATEMENT-OF-CREDIT-EXTRACT                           
010900         ASSIGN TO MXPA045T.                                      
011000                                                                  
011100 DATA DIVISION.                                                   
011200 FILE SECTION.                                                    
011300 FD  PROCESSING-FREQUENCY-PARM                                    
011400     RECORDING MODE IS F.                                         
011500 01  PROCESSING-FREQUENCY-PARM-REC       PIC X(80).               
011600                                                                  
011700 FD  STATEMENT-OF-CREDIT-EXTRACT                                  
011800     RECORDING MODE IS F.                                         
011900****  P0516718 S                                                  
012000*01  STATEMENT-OF-CREDIT-EXTRACT-RC      PIC X(140).              
012100 01  STATEMENT-OF-CREDIT-EXTRACT-RC      PIC X(162).              
012200****  P0516718 E                                                  
012300                                                                  
012400 WORKING-STORAGE SECTION.                                         
012500 01  WS-CONSTANTS.                                                
012600     05  WS-ADDR-PHONE-PGM       PIC X(08) VALUE 'MXBPW020'.      
012700 01  WS-SWITHCES.                                                 
012800     05  WS-NO-MORE-ROWS-SW              PIC X(01) VALUE 'N'.     
012900         88 NO-MORE-ROWS                           VALUE 'Y'.     
013000     05  WS-CRED-PRT-FREQ-CODE           PIC X(01).               
013100                                                                  
013200 01  WS-COUNTERS COMP-3.                                          
013300     05  WS-VWMJ280-ROWS-FETCHED         PIC S9(09).              
013400     05  WS-EXTRACT-RECS-WRITTEN         PIC S9(09).              
013500                                                                  
013600 01  WS-ACCUMULATORS COMP-3.                                      
013700     05  WS-NBR-CREDIT-DETAIL            PIC S9(04).              
013800     05  WS-UNIDENTIFIED-CASH            PIC S9(09)V9(02).        
013900     05  WS-PAYABLE-TRANSFER             PIC S9(09)V9(02).        
014000     05  WS-MISCELLANEOUS                PIC S9(09)V9(02).        
014100                                                                  
014200 01  WS-CURRENT-FIELDS.                                           
014300     05  WS-CURRENT-CUST-NO              PIC S9(09) VALUE 0 COMP. 
014400     05  WS-CURRENT-PAYEE-NO             PIC S9(09) VALUE 0 COMP. 
014500     05  WS-CURRENT-PBL-DTL-NO           PIC X(11).               
014600     05  WS-CURRENT-APPLIED-DATE         PIC X(10).               
014700     05  WS-CURRENT-PAY-TYPE-CODE        PIC X(04).               
014800         88  TRUST-RELATED       VALUES '0001', '0002', '0003',   
014900                                        '0005', '0013', '0015',   
015000                                        '0810', '0820', '0840',   
015100                                        'R001', 'R003', 'R015'.   
015200         88  CHARGE-TYPE         VALUES '0010', '0020', '0030',   
015300                                        '0040', '0050', '0060',   
015400                                        '0070', '0090', '0110',   
015500                                        'R010', 'R020', 'R030',   
015600                                        'R040', 'R050', 'R060',   
015700                                        'R070', 'R110'.           
015800         88  PAYABLE-TRANSFER    VALUES '0330'.                   
015900         88  UNIDENTIFIED-CASH   VALUES '0999', 'R999'.           
016000         88  CURTAILMENT         VALUES '0010', 'R010'.           
016100         88  FLAT                VALUES '0020', '0040', '0070',   
016200                                        'R020', 'R040', 'R070'.   
016300         88  ADB                 VALUES '0030', 'R030'.           
016400         88  SCHED-LIQ           VALUES '0050', 'R050'.           
016500         88  INSURANCE           VALUES '0060', 'R060'.           
016600         88  ADMIN               VALUES '0110', 'R110'.           
016700         88  NSF                 VALUES '0090'.                   
016800                                                                  
016900 01  WS-DATE-FIELDS.                                              
017000     05  WS-PROCESSING-DATE              PIC X(10).               
017100     05  WS-FROM-DATE                    PIC X(10).               
017200     05  WS-TO-DATE                      PIC X(10).               
017300                                                                  
017400 01  WS-INDICATOR-VARIABLES.                                      
017500     05  WS-CUST-NO-NN                   PIC S9(04) VALUE 0 COMP. 
017600     05  WS-DLR-NO-NN                    PIC S9(04) VALUE 0 COMP. 
017700     05  WS-TRUST-NO-NN                  PIC S9(04) VALUE 0 COMP. 
017800     05  WS-BILL-DATE-NN                 PIC S9(04) VALUE 0 COMP. 
017900     05  WS-RECV-BRANCH-NO-NN            PIC S9(04) VALUE 0 COMP. 
018000     05  WS-DLR-REP-CODE-NN              PIC S9(04) VALUE 0 COMP. 
018100     05  WS-PROC-DATE-NN                 PIC S9(04) VALUE 0 COMP. 
018200     05  WS-FROM-DATE-NN                 PIC S9(04) VALUE 0 COMP. 
018300     05  WS-TO-DATE-NN                   PIC S9(04) VALUE 0 COMP. 
018400                                                                  
018500 01  WS-DISPLAY-FIELDS.                                           
018600     05  WS-DISPLAY-COUNTER              PIC ZZZ,ZZZ,ZZ9.         
018700     05  WS-DISPLAY-DLR-NO               PIC ZZZZZZZZ9.           
018800     05  WS-DISPLAY-TRUST-LINE-NO        PIC ZZZ9.                
018900                                                                  
019000 01  WS-PROCESSING-FREQUENCY-PARM.                                
019100     05  WS-PROCESSING-FREQUENCY         PIC X(01).               
019200         88  DAILY-PROCESSING                      VALUE 'D'.     
019300         88  WEEKLY-PROCESSING                     VALUE 'W'.     
019400     05  FILLER                          PIC X(79).               
019500                                                                  
019600     EXEC SQL                                                     
019700          INCLUDE MXWW03                                          
019800     END-EXEC.                                                    
019900                                                                  
020000     EXEC SQL                                                     
020100         INCLUDE MX0W07                                           
020200     END-EXEC.                                                    
020300                                                                  
020400     EXEC SQL                                                     
020500         INCLUDE MXAW21                                           
020600     END-EXEC.                                                    
020700                                                                  
020800     EXEC SQL                                                     
020900         INCLUDE MXCW020                                          
021000     END-EXEC.                                                    
021100                                                                  
021200     EXEC SQL                                                     
021300         INCLUDE MXLTA047                                         
021400     END-EXEC.                                                    
021500                                                                  
021600******************************************************************
021700*  DB2 table includes                                             
021800******************************************************************
021900                                                                  
022000     EXEC SQL                                                     
022100         INCLUDE VWMJ280                                          
022200     END-EXEC.                                                    
022300                                                                  
022400     EXEC SQL                                                     
022500         INCLUDE VWMTRLI                                          
022600     END-EXEC.                                                    
022700                                                                  
022800     EXEC SQL                                                     
022900         INCLUDE VWMCU00                                          
023000     END-EXEC.                                                    
023100                                                                  
023200     EXEC SQL                                                     
023300         INCLUDE VWMCN00                                          
023400     END-EXEC.                                                    
023500                                                                  
023600     EXEC SQL                                                     
023700         INCLUDE VWMRP00                                          
023800     END-EXEC.                                                    
023900                                                                  
024000     EXEC SQL                                                     
024100         INCLUDE VWMCTUPD                                         
024200     END-EXEC.                                                    
024300                                                                  
024400     EXEC SQL                                                     
024500         INCLUDE VWMCUCP                                          
024600     END-EXEC.                                                    
024700****  P0516718 S                                                  
024800     EXEC SQL                                                     
024900         INCLUDE VWMTR00                                          
025000     END-EXEC.                                                    
025100                                                                  
025200     EXEC SQL                                                     
025300         INCLUDE VWMPBTR                                          
025400     END-EXEC.                                                    
025500****  P0516718 E                                                  
025600                                                                  
025700     EXEC SQL                                                     
025800         INCLUDE SQLCA                                            
025900     END-EXEC.                                                    
026000                                                                  
026100***************************************************************** 
026200*  CURSOR DEFINITION FOR APPLIED PAYMENT INFORMATION            * 
026300***************************************************************** 
026400                                                                  
026500     EXEC SQL                                                     
026600        DECLARE VWMJ280 CURSOR FOR                                
026700         SELECT ISSUING_BRANCH_NO                                 
026800               ,ISSUE_DATE                                        
026900               ,PAYEE_TYPE                                        
027000               ,PAYEE_NO                                          
027100               ,PAYEE_SUFF_NO                                     
027200               ,SEQ_NO                                            
027300               ,PBL_DTL_NO                                        
027400               ,DTL_SEQ_NO                                        
027500               ,PBL_DTL_TYPE_CODE                                 
027600               ,NET_AMT                                           
027700               ,SERVICE_CHRG_AMT                                  
027800               ,BRANCH_NO                                         
027900               ,CUST_NO                                           
028000               ,PAY_POST_DATE                                     
028100               ,PAY_APPLIED_DATE                                  
028200               ,PAY_NO                                            
028300               ,PAY_STAT_CODE                                     
028400               ,DLR_NO                                            
028500               ,TRUST_NO                                          
028600               ,TRUST_LINE_NO                                     
028700               ,CHRG_SEQ_NO                                       
028800               ,BILL_LOC_NO                                       
028900               ,BILL_DATE                                         
029000               ,RECV_BRANCH_NO                                    
029100               ,PAY_APPLIED_AMT                                   
029200               ,PAY_TYPE_CODE                                     
029300* ECR02987633 - S                                                 
029400               ,CUST_ORG_CM_NO                                    
029500* ECR02987633 - S                                                 
029600           FROM VWMJ280 A                                         
029700          WHERE A.PAY_POST_DATE  <= :WS-TO-DATE                   
029800            AND A.PAY_NO LIKE 'CM%'                               
029900            AND A.PAY_APPLIED_DATE                                
030000                BETWEEN :WS-FROM-DATE AND :WS-TO-DATE             
030100            AND A.PBL_DTL_TYPE_CODE = '6'                         
030200*** TRACKER 2078 BEGIN                                            
030300            AND A.PAY_TYPE_CODE <> '0019'                         
030400*** TRACKER 2078 END                                              
030500************ test customers ********************************      
030600****        AND A.CUST_NO IN (11045, 13580, 2606, 2689, 39221,    
030700****                          4008, 5446, 91230, 91726, 67346,    
030800****                          14165, 29418, 31245, 31486,         
030900****                          42347, 32928, 2699, 30879,          
031000****                          31733, 31477, 13636, 11148,         
031100****                          13054)                              
031200************************************************************      
031300            AND EXISTS                                            
031400            (SELECT  *                                            
031500               FROM VWMCU00 B                                     
031600              WHERE A.CUST_NO  =  B.CUST_NO                       
031700                AND B.CRED_PRT_FLAG  =  'Y'                       
031800                AND B.CRED_PRT_FREQ_CODE = :WS-CRED-PRT-FREQ-CODE)
031900          ORDER BY A.CUST_NO, A.PAYEE_NO, A.PBL_DTL_NO,           
032000                   A.PAY_APPLIED_DATE                             
032100     END-EXEC.                                                    
032200                                                                  
032300                                                                  
032400 PROCEDURE DIVISION.                                              
032500                                                                  
032600***************************************************************** 
032700*                     0000-MAINLINE                             * 
032800***************************************************************** 
032900                                                                  
033000 0000-MAINLINE.                                                   
033100     PERFORM 1000-INITIALIZATION THRU 1000-EXIT.                  
033200     PERFORM 2000-PROCESS-APPLIED-PAYMENTS THRU 2000-EXIT         
033300         UNTIL NO-MORE-ROWS.                                      
033400     PERFORM 9900-TERMINATION THRU 9900-EXIT.                     
033500     GOBACK.                                                      
033600 0000-EXIT.                                                       
033700     EXIT.                                                        
033800                                                                  
033900***************************************************************** 
034000*                      1000-INITIALIZATION                      * 
034100***************************************************************** 
034200                                                                  
034300 1000-INITIALIZATION.                                             
034400     DISPLAY ' '.                                                 
034500     DISPLAY 'PROGRAM MXBPA045 BEGINNING EXECUTION'.              
034600     DISPLAY ' '.                                                 
034700     MOVE 'MXBPA045' TO ABT-PGM-NAME.                             
034800****  P0516718 S                                                  
034900*    MOVE SPACES TO SUBSYSTEM-ID-IND                              
035000*                   SUBFUNCTION-CODE.                             
035100     MOVE SPACES TO SUBSYSTEM-ID-IND OF DCLVWMCTUPD               
035200                    SUBFUNCTION-CODE OF DCLVWMCTUPD.              
035300****  P0516718 E                                                  
035400                                                                  
035500     PERFORM 7000-SELECT-VWMCTUPD THRU 7000-EXIT.                 
035600     IF DA-OK                                                     
035700         MOVE PROC-DATE TO WS-TO-DATE                             
035800     ELSE                                                         
035900         SET ABT-DO-ABEND     TO TRUE                             
036000         SET ABT-ERROR-IS-DB2 TO TRUE                             
036100         MOVE 'SELECT  '      TO ABT-DA-FUNCTION                  
036200         MOVE '1000-INIT'     TO ABT-ERROR-SECTION                
036300         MOVE 3601            TO ABT-ERROR-ABEND-CODE             
036400         MOVE 'VWMCTUPD'      TO ABT-DA-ACCESS-NAME               
036500         PERFORM Z-980-ABNORMAL-TERM THRU                         
036600                 Z-980-ABNORMAL-TERM-RETURN                       
036700     END-IF.                                                      
036800                                                                  
036900     OPEN INPUT PROCESSING-FREQUENCY-PARM.                        
037000                                                                  
037100     READ PROCESSING-FREQUENCY-PARM INTO                          
037200          WS-PROCESSING-FREQUENCY-PARM.                           
037300                                                                  
037400     IF DAILY-PROCESSING                                          
037500         MOVE 'D' TO WS-CRED-PRT-FREQ-CODE                        
037600         MOVE 'A045DALY' TO SUBFUNCTION-CODE                      
037700         DISPLAY 'DAILY STATEMENT OF CREDIT RUN'                  
037800     ELSE                                                         
037900         IF WEEKLY-PROCESSING                                     
038000             MOVE 'W' TO WS-CRED-PRT-FREQ-CODE                    
038100             MOVE 'A045WKLY' TO SUBFUNCTION-CODE                  
038200             DISPLAY 'WEEKLY STATEMENT OF CREDIT RUN'             
038300         ELSE                                                     
038400             SET ABT-DO-ABEND     TO TRUE                         
038500             SET ABT-ERROR-IS-SEQ TO TRUE                         
038600             MOVE 'SEQ     '      TO ABT-DA-FUNCTION              
038700             MOVE '1000-INIT'     TO ABT-ERROR-SECTION            
038800             MOVE 3602            TO ABT-ERROR-ABEND-CODE         
038900             MOVE 'MXPA045I'      TO ABT-DA-ACCESS-NAME           
039000             PERFORM Z-980-ABNORMAL-TERM THRU                     
039100                     Z-980-ABNORMAL-TERM-RETURN                   
039200         END-IF                                                   
039300     END-IF.                                                      
039400                                                                  
039500     CLOSE PROCESSING-FREQUENCY-PARM.                             
039600                                                                  
039700****  P0516718 S                                                  
039800*    MOVE 'A' TO SUBSYSTEM-ID-IND.                                
039900     MOVE 'A' TO SUBSYSTEM-ID-IND OF DCLVWMCTUPD.                 
040000****  P0516718 E                                                  
040100     PERFORM 7000-SELECT-VWMCTUPD THRU 7000-EXIT.                 
040200     IF DA-OK                                                     
040300         NEXT SENTENCE                                            
040400     ELSE                                                         
040500         SET ABT-DO-ABEND     TO TRUE                             
040600         SET ABT-ERROR-IS-DB2 TO TRUE                             
040700         MOVE 'SELECT  '      TO ABT-DA-FUNCTION                  
040800         MOVE '1000-INIT'     TO ABT-ERROR-SECTION                
040900         MOVE 3603            TO ABT-ERROR-ABEND-CODE             
041000         MOVE 'VWMCTUPD'      TO ABT-DA-ACCESS-NAME               
041100         PERFORM Z-980-ABNORMAL-TERM THRU                         
041200                 Z-980-ABNORMAL-TERM-RETURN                       
041300     END-IF.                                                      
041400                                                                  
041500     IF WS-TO-DATE >= WS-FROM-DATE                                
041600         DISPLAY 'PROCESSING DATE RANGE ' WS-FROM-DATE ' TO '     
041700                                          WS-TO-DATE              
041800     ELSE                                                         
041900         DISPLAY 'PROCESSING DATE ' WS-TO-DATE                    
042000                 ' IS LESS THAN FROM DATE ' WS-FROM-DATE          
042100         DISPLAY 'UPDATE VWMCTUPD PROC_DATE FOR SUBSYSTEM_ID_IND '
042200****  P0516718 S                                                  
042300*                 SUBSYSTEM-ID-IND ' AND SUBFUNCTION_CODE '       
042400*                                        SUBFUNCTION-CODE         
042500                  SUBSYSTEM-ID-IND OF DCLVWMCTUPD                 
042600                  ' AND SUBFUNCTION_CODE '                        
042700                  SUBFUNCTION-CODE OF DCLVWMCTUPD                 
042800****  P0516718 E                                                  
042900         SET ABT-DO-ABEND TO TRUE                                 
043000         MOVE '1000-INIT' TO ABT-ERROR-SECTION                    
043100         MOVE 3604        TO ABT-ERROR-ABEND-CODE                 
043200         MOVE 'VWMCTUPD'  TO ABT-DA-ACCESS-NAME                   
043300         PERFORM Z-980-ABNORMAL-TERM THRU                         
043400                 Z-980-ABNORMAL-TERM-RETURN                       
043500     END-IF.                                                      
043600                                                                  
043700     OPEN OUTPUT STATEMENT-OF-CREDIT-EXTRACT.                     
043800                                                                  
043900     MOVE LOW-VALUES TO MXAW21-CREDIT-GENERAL-RECORD.             
044000     INITIALIZE WS-COUNTERS                                       
044100                WS-ACCUMULATORS                                   
044200                WS-CURRENT-FIELDS.                                
044300                                                                  
044400     EXEC SQL                                                     
044500         OPEN VWMJ280                                             
044600     END-EXEC.                                                    
044700                                                                  
044800     PERFORM Z-970-SET-DA-STATUS-DB2 THRU                         
044900             Z-970-SET-DA-STATUS-DB2-EXIT.                        
045000                                                                  
045100     IF DA-OK                                                     
045200         PERFORM 5000-FETCH-VWMJ280 THRU 5000-EXIT                
045300     ELSE                                                         
045400         SET ABT-DO-ABEND     TO TRUE                             
045500         SET ABT-ERROR-IS-DB2 TO TRUE                             
045600         MOVE 'OPEN    '      TO ABT-DA-FUNCTION                  
045700         MOVE '1000-INIT'     TO ABT-ERROR-SECTION                
045800         MOVE 3605            TO ABT-ERROR-ABEND-CODE             
045900         MOVE 'VWMJ280 '      TO ABT-DA-ACCESS-NAME               
046000         PERFORM Z-980-ABNORMAL-TERM THRU                         
046100                 Z-980-ABNORMAL-TERM-RETURN                       
046200     END-IF.                                                      
046300 1000-EXIT.                                                       
046400     EXIT.                                                        
046500                                                                  
046600***************************************************************** 
046700*                2000-PROCESS-APPLIED-PAYMENTS                  * 
046800***************************************************************** 
046900                                                                  
047000 2000-PROCESS-APPLIED-PAYMENTS.                                   
047100                                                                  
047200     INITIALIZE MXAW21-CGR-SORT-KEY                               
047300                WS-CURRENT-FIELDS.                                
047400                                                                  
047500     MOVE CUST-NO OF DCLVWMJ280 TO WS-CURRENT-CUST-NO             
047600                                    MXAW21-SK-DLR-NBR.            
047700                                                                  
047800     MOVE CUST-NO OF DCLVWMJ280      TO CUST-NO OF DCLVWMCU00     
047900     PERFORM 6000-SELECT-VWMCU00        THRU 6000-EXIT.           
048000     MOVE COUNTRY-CODE OF DCLVWMCU00 TO MXAW21-SK-COUNTRY-CODE.   
048100     MOVE CNTL-ENT-NO OF DCLVWMCU00  TO CNTL-ENT-NO OF DCLVWMCN00.
048200     MOVE LANGUAGE-CODE OF DCLVWMCU00 TO MXAW21-SK-LANG-IND.      
048300     INITIALIZE MXAW21-CGR-DATA.                                  
048400     MOVE CUST-NO OF DCLVWMCU00      TO MXAW21-0001-DLR-NBR.      
048500     MOVE CNTL-ENT-NO OF DCLVWMCU00  TO MXAW21-0001-DLR-CNTL-ENT. 
048600     MOVE '00'                       TO MXAW21-SK-RECORD-TYPE.    
048700     MOVE 1                          TO MXAW21-SK-RECORD-TYPE-SEQ.
048800     PERFORM 8000-WRITE-EXTRACT-RECORD  THRU 8000-EXIT.           
048900     INITIALIZE MXAW21-CGR-DATA.                                  
049000     MOVE CUST-NO OF DCLVWMJ280      TO MX0W07-DLR-NO.            
049100     MOVE CNTL-ENT-NO OF DCLVWMCU00  TO MX0W07-CNTL-ENT-NO.       
049200     PERFORM MX0P07-RETRIEVE-ADDL-CUST  THRU MX0P07-EXIT.         
049300                                                                  
049400     IF MX0W07-STAT-CODE = 'E' AND SQLCODE NOT = 0                
049500         DISPLAY 'ABEND IN ADDL-CUST ROUTINE'                     
049600         DISPLAY 'CUST NUMBER: '  CUST-NO OF DCLVWMJ280           
049700         PERFORM Z-980-ABNORMAL-TERM                              
049800     END-IF.                                                      
049900                                                                  
050000     IF MX0W07-ADDL-CUST-FLAG = 'Y'                               
050100         MOVE 5                 TO TABLE-ENTRY-WANTED             
050200         MOVE LANGUAGE-CODE     OF DCLVWMCU00                     
050300                                TO MXCW021-LANG-CODE              
050400         PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT         
050500         MOVE MXCW021-LANG-TEXT TO MXAW21-0002-ADDL-CUST-LIT      
050600         MOVE MX0W07-CPU-DLR-NO TO MXAW21-0002-ADDL-CUST-NO       
050700     ELSE                                                         
050800         MOVE SPACES            TO MXAW21-0002-ADDL-CUST-LIT      
050900         MOVE SPACES            TO MXAW21-0002-ADDL-CUST-NO       
051000     END-IF.                                                      
051100                                                                  
051200     MOVE '00'                      TO MXAW21-SK-RECORD-TYPE.     
051300     MOVE 2                         TO MXAW21-SK-RECORD-TYPE-SEQ. 
051400     PERFORM 8000-WRITE-EXTRACT-RECORD THRU 8000-EXIT.            
051500                                                                  
051600     PERFORM 2100-FORMAT-ADDRESS       THRU 2100-EXIT.            
051700                                                                  
051800     INITIALIZE MXAW21-CGR-DATA.                                  
051900     MOVE MXCW020-LINE1             TO MXAW21-0004-DLR-NAME-ADDR. 
052000     MOVE 4                         TO MXAW21-SK-RECORD-TYPE-SEQ. 
052100     PERFORM 8000-WRITE-EXTRACT-RECORD THRU 8000-EXIT.            
052200     INITIALIZE MXAW21-CGR-DATA.                                  
052300                                                                  
052400     MOVE MXCW020-LINE2             TO MXAW21-0004-DLR-NAME-ADDR. 
052500     MOVE 5                         TO MXAW21-SK-RECORD-TYPE-SEQ. 
052600     PERFORM 8000-WRITE-EXTRACT-RECORD THRU 8000-EXIT.            
052700     INITIALIZE MXAW21-CGR-DATA.                                  
052800                                                                  
052900     MOVE MXCW020-LINE3             TO MXAW21-0004-DLR-NAME-ADDR. 
053000     MOVE 6                         TO MXAW21-SK-RECORD-TYPE-SEQ. 
053100     PERFORM 8000-WRITE-EXTRACT-RECORD     THRU 8000-EXIT.        
053200     INITIALIZE MXAW21-CGR-DATA.                                  
053300                                                                  
053400     MOVE MXCW020-LINE4             TO MXAW21-0004-DLR-NAME-ADDR. 
053500     MOVE 7                         TO MXAW21-SK-RECORD-TYPE-SEQ. 
053600     PERFORM 8000-WRITE-EXTRACT-RECORD     THRU 8000-EXIT.        
053700     INITIALIZE MXAW21-CGR-DATA.                                  
053800                                                                  
053900     MOVE MXCW020-LINE5             TO MXAW21-0004-DLR-NAME-ADDR. 
054000     MOVE 8                         TO MXAW21-SK-RECORD-TYPE-SEQ. 
054100     PERFORM 8000-WRITE-EXTRACT-RECORD     THRU 8000-EXIT.        
054200     INITIALIZE MXAW21-CGR-DATA.                                  
054300                                                                  
054400     MOVE MXCW020-LINE6             TO MXAW21-0004-DLR-NAME-ADDR. 
054500     MOVE 9                         TO MXAW21-SK-RECORD-TYPE-SEQ. 
054600     PERFORM 8000-WRITE-EXTRACT-RECORD     THRU 8000-EXIT.        
054700     INITIALIZE MXAW21-CGR-DATA.                                  
054800                                                                  
054900     IF WS-DLR-REP-CODE-NN  =  -1                                 
055000         MOVE SPACES TO MXAW21-0003-DLR-REP-NAME                  
055100                        MXAW21-0003-DLR-REP-PHONE                 
055200     ELSE                                                         
055300         MOVE DLR-REP-CODE OF DCLVWMCU00 TO                       
055400              DLR-REP-CODE OF DCLVWMRP00                          
055500         PERFORM 6500-SELECT-VWMRP00       THRU 6500-EXIT         
055600         MOVE DLR-REP-NAME OF DCLVWMRP00 TO                       
055700              MXAW21-0003-DLR-REP-NAME                            
055800         PERFORM 2200-FORMAT-PHONE         THRU 2200-EXIT         
055900         MOVE MXCW020-PHONE-OUT      TO MXAW21-0003-DLR-REP-PHONE 
056000     END-IF.                                                      
056100     MOVE 3            TO MXAW21-SK-RECORD-TYPE-SEQ.              
056200     PERFORM 8000-WRITE-EXTRACT-RECORD THRU 8000-EXIT.            
056300                                                                  
056400     PERFORM 3000-PROCESS-CUSTOMER THRU 3000-EXIT                 
056500         UNTIL CUST-NO OF DCLVWMJ280 NOT = WS-CURRENT-CUST-NO OR  
056600               NO-MORE-ROWS.                                      
056700 2000-EXIT. EXIT.                                                 
056800***************************************************************** 
056900*                  2100-FORMAT-ADDRESS                          * 
057000***************************************************************** 
057100 2100-FORMAT-ADDRESS.                                             
057200                                                                  
057300     INITIALIZE MXCW020-ADDR-PHONE-FORMAT.                        
057400     SET  MXCW020-EDIT-FORMAT-ADDR  TO TRUE.                      
057500     MOVE COUNTRY-CODE              OF DCLVWMCU00                 
057600                                    TO MXCW020-COUNTRY-CODE.      
057700                                                                  
057800     MOVE LEGAL-NAME-TEXT           OF DCLVWMCU00                 
057900                                    TO MXCW020-NAME-INPUT.        
058000* TRACKER 9651 S                                                  
058100     IF COUNTRY-CODE OF DCLVWMCU00 = 'CAN'                        
058200         MOVE DBA-NAME-TEXT         OF DCLVWMCU00                 
058300                                    TO MXCW020-NAME2-INPUT        
058400     END-IF.                                                      
058500* TRACKER 9651 E                                                  
058600     MOVE ADDR1-NAME-TEXT           OF DCLVWMCU00                 
058700                                    TO MXCW020-ADDRESS1-INPUT.    
058800     MOVE ADDR2-NAME-TEXT           OF DCLVWMCU00                 
058900                                    TO MXCW020-ADDRESS2-INPUT.    
059000     MOVE CITY-NAME-TEXT            OF DCLVWMCU00                 
059100                                    TO MXCW020-CITY-INPUT.        
059200     MOVE ST-PROV-CODE              OF DCLVWMCU00                 
059300                                    TO MXCW020-STATE-INPUT.       
059400     MOVE ZIP-POSTAL-CODE           OF DCLVWMCU00                 
059500                                    TO MXCW020-ZIP-INPUT.         
059600                                                                  
059700     CALL WS-ADDR-PHONE-PGM   USING MXCW020-ADDR-PHONE-FORMAT.    
059800                                                                  
059900     IF  MXCW020-OK                                               
060000         CONTINUE                                                 
060100     ELSE                                                         
060200        DISPLAY '2100- PROBLEM WITH ADDRESS/PHONE MODULE'         
060300        DISPLAY 'MXCW020-RESULTS = ' MXCW020-RESULT-FLAG          
060400        DISPLAY 'COUNTRY CODE = '    MXCW020-COUNTRY-CODE         
060500        DISPLAY MXCW020-ADDR-PHONE-FORMAT                         
060600        SET ABT-DO-ABEND     TO TRUE                              
060700        SET ABT-ERROR-IS-DB2 TO TRUE                              
060800        MOVE 'SUBRTN  '      TO ABT-DA-FUNCTION                   
060900        MOVE '2100-    '     TO ABT-ERROR-SECTION                 
061000        MOVE 3605            TO ABT-ERROR-ABEND-CODE              
061100        MOVE 'ADDRPHON'      TO ABT-DA-ACCESS-NAME                
061200        PERFORM Z-980-ABNORMAL-TERM THRU                          
061300                Z-980-ABNORMAL-TERM-RETURN                        
061400     END-IF.                                                      
061500                                                                  
061600 2100-EXIT. EXIT.                                                 
061700***************************************************************** 
061800*                  2200-FORMAT-PHONE                            * 
061900***************************************************************** 
062000 2200-FORMAT-PHONE.                                               
062100                                                                  
062200     INITIALIZE MXCW020-ADDR-PHONE-FORMAT.                        
062300     SET  MXCW020-EDIT-PHONE        TO TRUE.                      
062400     MOVE COUNTRY-CODE              OF DCLVWMCU00                 
062500                                    TO MXCW020-COUNTRY-CODE.      
062600                                                                  
062700     MOVE PHONE-NO                  OF DCLVWMRP00                 
062800                                    TO MXCW020-PHONE-INPUT.       
062900                                                                  
063000     CALL WS-ADDR-PHONE-PGM   USING MXCW020-ADDR-PHONE-FORMAT.    
063100                                                                  
063200     IF  MXCW020-OK                                               
063300         CONTINUE                                                 
063400     ELSE                                                         
063500        DISPLAY '2200- PROBLEM WITH ADDRESS/PHONE MODULE'         
063600        DISPLAY 'MXCW020-RESULTS = ' MXCW020-RESULT-FLAG          
063700        DISPLAY 'COUNTRY CODE = '    MXCW020-COUNTRY-CODE         
063800        DISPLAY MXCW020-ADDR-PHONE-FORMAT                         
063900        SET ABT-DO-ABEND     TO TRUE                              
064000        SET ABT-ERROR-IS-DB2 TO TRUE                              
064100        MOVE 'SUBRTN  '      TO ABT-DA-FUNCTION                   
064200        MOVE '2200-    '     TO ABT-ERROR-SECTION                 
064300        MOVE 3605            TO ABT-ERROR-ABEND-CODE              
064400        MOVE 'ADDRPHON'      TO ABT-DA-ACCESS-NAME                
064500        PERFORM Z-980-ABNORMAL-TERM THRU                          
064600                Z-980-ABNORMAL-TERM-RETURN                        
064700     END-IF.                                                      
064800                                                                  
064900 2200-EXIT. EXIT.                                                 
065000                                                                  
065100     EXEC SQL                                                     
065200         INCLUDE MX0P07                                           
065300     END-EXEC.                                                    
065400                                                                  
065500***************************************************************** 
065600*                  3000-PROCESS-CUSTOMER                        * 
065700***************************************************************** 
065800                                                                  
065900 3000-PROCESS-CUSTOMER.                                           
066000****  P0516718 S                                                  
066100*    MOVE PBL-DTL-NO TO WS-CURRENT-PBL-DTL-NO                     
066200*                       MXAW21-SK-CREDIT-MEMO-NBR.                
066300     MOVE PBL-DTL-NO OF DCLVWMJ280 TO WS-CURRENT-PBL-DTL-NO       
066400                                      MXAW21-SK-CREDIT-MEMO-NBR.  
066500****  P0516718 E                                                  
066600                                                                  
066700     MOVE PAY-APPLIED-DATE TO WS-CURRENT-APPLIED-DATE.            
066800****  P0516718 S                                                  
066900*    IF PAYEE-NO = WS-CURRENT-PAYEE-NO                            
067000*        CONTINUE                                                 
067100*    ELSE                                                         
067200*        MOVE PAYEE-NO TO WS-CURRENT-PAYEE-NO                     
067300*                         CUST-NO OF DCLVWMCU00                   
067400*        PERFORM 6000-SELECT-VWMCU00 THRU 6000-EXIT               
067500*        MOVE LEGAL-NAME-TEXT TO MXAW21-SK-DIST-NAME              
067600*    END-IF.                                                      
067700     IF PAYEE-NO OF DCLVWMJ280 = WS-CURRENT-PAYEE-NO              
067800         CONTINUE                                                 
067900     ELSE                                                         
068000        MOVE PAYEE-NO OF DCLVWMJ280 TO WS-CURRENT-PAYEE-NO        
068100                                       CUST-NO OF DCLVWMCU00      
068200        PERFORM 6000-SELECT-VWMCU00 THRU 6000-EXIT                
068300        MOVE LEGAL-NAME-TEXT OF DCLVWMCU00                        
068400                                    TO MXAW21-SK-DIST-NAME        
068500     END-IF.                                                      
068600****  P0516718 E                                                  
068700                                                                  
068800     INITIALIZE MXAW21-CGR-DATA.                                  
068900****  P0516718 S                                                  
069000*    MOVE PBL-DTL-NO TO MXAW21-0103-CREDIT-NBR.                   
069100     MOVE PBL-DTL-NO OF DCLVWMJ280 TO MXAW21-0103-CREDIT-NBR.     
069200****  P0516718 E                                                  
069300     MOVE PAY-APPLIED-DATE TO MXAW21-0103-APPLIED-DATE.           
069400     MOVE '01' TO MXAW21-SK-RECORD-TYPE.                          
069500     MOVE 3 TO MXAW21-SK-RECORD-TYPE-SEQ.                         
069600****  P0516718 S                                                  
069700     MOVE WS-CURRENT-PBL-DTL-NO  TO PBL-DTL-NO OF DCLVWMPBTR.     
069800     MOVE DLR-NO OF DCLVWMJ280   TO DLR-NO OF DCLVWMPBTR.         
069900     MOVE DTL-SEQ-NO  OF DCLVWMJ280                               
070000                                 TO DTL-SEQ-NO  OF DCLVWMPBTR.    
070100     MOVE PBL-DTL-TYPE-CODE OF DCLVWMJ280 TO                      
070200                                PBL-DTL-TYPE-CODE OF DCLVWMPBTR.  
070300* ECR02987633 - S                                                 
070400*    PERFORM 3100-SELECT-VWMPBTR THRU 3100-EXIT.                  
070500*    IF CUST-ORG-CM-NO OF DCLVWMPBTR NOT EQUAL SPACE              
070600*       MOVE CUST-ORG-CM-NO OF DCLVWMPBTR TO                      
070700*                      MXAW21-0103-ORG-CREDIT-MEMO                
070800     IF CUST-ORG-CM-NO OF DCLVWMJ280 NOT EQUAL SPACE              
070900        MOVE CUST-ORG-CM-NO OF DCLVWMJ280 TO                      
071000                       MXAW21-0103-ORG-CREDIT-MEMO                
071100* ECR02987633 - E                                                 
071200     ELSE                                                         
071300        MOVE SPACES TO MXAW21-0103-ORG-CREDIT-MEMO                
071400     END-IF.                                                      
071500****  P0516718 E                                                  
071600     PERFORM 8000-WRITE-EXTRACT-RECORD THRU 8000-EXIT.            
071700     PERFORM 4000-PROCESS-CREDIT-MEMO THRU 4000-EXIT              
071800************** PAY-APPLIED-DATE NOT = WS-CURRENT-APPLIED-DATE OR  
071900****  P0516718 S                                                  
072000*        UNTIL PBL-DTL-NO NOT = WS-CURRENT-PBL-DTL-NO         OR  
072100*              PAYEE-NO NOT = WS-CURRENT-PAYEE-NO             OR  
072200*              CUST-NO OF DCLCWMJ280 NOT = WS-CURRENT-CUST-NO OR  
072300*              NO-MORE-ROWS.                                      
072400         UNTIL PBL-DTL-NO OF DCLVWMJ280 NOT =                     
072500                                    WS-CURRENT-PBL-DTL-NO  OR     
072600               PAYEE-NO OF DCLVWMJ280 NOT =                       
072700                                    WS-CURRENT-PAYEE-NO    OR     
072800               CUST-NO OF DCLVWMJ280 NOT =                        
072900                                    WS-CURRENT-CUST-NO     OR     
073000               NO-MORE-ROWS.                                      
073100****  P0516718 E                                                  
073200                                                                  
073300     INITIALIZE MXAW21-CGR-DATA.                                  
073400     IF WS-UNIDENTIFIED-CASH NOT = 0                              
073500         MOVE '04' TO MXAW21-SK-RECORD-TYPE                       
073600         MOVE 1 TO MXAW21-SK-RECORD-TYPE-SEQ                      
073700         MOVE WS-UNIDENTIFIED-CASH TO MXAW21-0400-APPLIED-AMT     
073800         ADD +1 TO WS-NBR-CREDIT-DETAIL                           
073900         PERFORM 8000-WRITE-EXTRACT-RECORD THRU 8000-EXIT         
074000     END-IF.                                                      
074100                                                                  
074200     IF WS-PAYABLE-TRANSFER NOT = 0                               
074300         MOVE '04' TO MXAW21-SK-RECORD-TYPE                       
074400         MOVE 2 TO MXAW21-SK-RECORD-TYPE-SEQ                      
074500         MOVE WS-PAYABLE-TRANSFER TO MXAW21-0400-APPLIED-AMT      
074600         ADD +1 TO WS-NBR-CREDIT-DETAIL                           
074700         PERFORM 8000-WRITE-EXTRACT-RECORD THRU 8000-EXIT         
074800     END-IF.                                                      
074900                                                                  
075000     IF WS-MISCELLANEOUS NOT = 0                                  
075100         MOVE '04' TO MXAW21-SK-RECORD-TYPE                       
075200         MOVE 3 TO MXAW21-SK-RECORD-TYPE-SEQ                      
075300         MOVE WS-MISCELLANEOUS TO MXAW21-0400-APPLIED-AMT         
075400         ADD +1 TO WS-NBR-CREDIT-DETAIL                           
075500         PERFORM 8000-WRITE-EXTRACT-RECORD THRU 8000-EXIT         
075600     END-IF.                                                      
075700                                                                  
075800     INITIALIZE MXAW21-CGR-DATA.                                  
075900     MOVE '01' TO MXAW21-SK-RECORD-TYPE.                          
076000     MOVE 1 TO MXAW21-SK-RECORD-TYPE-SEQ.                         
076100     MOVE MXAW21-SK-DIST-NAME TO MXAW21-0101-DIST-NAME.           
076200     MOVE WS-NBR-CREDIT-DETAIL TO MXAW21-0101-NBR-CREDIT-DETAIL.  
076300     PERFORM 8000-WRITE-EXTRACT-RECORD THRU 8000-EXIT.            
076400     INITIALIZE WS-ACCUMULATORS.                                  
076500 3000-EXIT.                                                       
076600     EXIT.                                                        
076700                                                                  
076800****  P0516718 S                                                  
076900* * ECR02987633 - S                                               
077000**********************************************************        
077100*                3100-SELECT-VWMPBTR                              
077200**********************************************************        
077300*                                                                 
077400*3100-SELECT-VWMPBTR.                                             
077500*                                                                 
077600*    EXEC SQL                                                     
077700*      SELECT CUST_ORG_CM_NO                                      
077800*       INTO :CUST-ORG-CM-NO                                      
077900*        FROM VWMPBTR                                             
078000*       WHERE DLR_NO       = :DCLVWMPBTR.DLR-NO                   
078100*         AND PBL_DTL_NO   = :DCLVWMPBTR.PBL-DTL-NO               
078200*         AND DTL_SEQ_NO   = :DCLVWMPBTR.DTL-SEQ-NO               
078300*         AND PBL_DTL_TYPE_CODE = :DCLVWMPBTR.PBL-DTL-TYPE-CODE   
078400*    END-EXEC.                                                    
078500*                                                                 
078600*    PERFORM Z-970-SET-DA-STATUS-DB2 THRU                         
078700*            Z-970-SET-DA-STATUS-DB2-EXIT.                        
078800*    IF DA-OK                                                     
078900*       CONTINUE                                                  
079000*    ELSE                                                         
079100*       IF DA-NOTFOUND                                            
079200*          MOVE SPACES TO CUST-ORG-CM-NO OF DCLVWMPBTR            
079300*       ELSE                                                      
079400*          MOVE DLR-NO OF DCLVWMPBTR TO WS-DISPLAY-DLR-NO         
079500*          DISPLAY ' '                                            
079600*          DISPLAY 'FATAL ERROR ON SELECT VWMPBTR'                
079700*          DISPLAY 'DEALER ' WS-DISPLAY-DLR-NO                    
079800*          DISPLAY 'CREDIT MEMO NO ' PBL-DTL-NO OF DCLVWMPBTR     
079900*          DISPLAY ' '                                            
080000*          SET ABT-DO-ABEND     TO TRUE                           
080100*          SET ABT-ERROR-IS-DB2 TO TRUE                           
080200*          MOVE 'SELECT  '      TO ABT-DA-FUNCTION                
080300*          MOVE '3100-SELE'     TO ABT-ERROR-SECTION              
080400*          MOVE 3610            TO ABT-ERROR-ABEND-CODE           
080500*          MOVE 'VWMPBTR '      TO ABT-DA-ACCESS-NAME             
080600*          PERFORM Z-980-ABNORMAL-TERM THRU                       
080700*                  Z-980-ABNORMAL-TERM-RETURN                     
080800*       END-IF                                                    
080900*    END-IF.                                                      
081000*3100-EXIT.                                                       
081100*    EXIT.                                                        
081200*                                                                 
081300* ECR02987633 - E                                                 
081400****  P0516718 E                                                  
081500***************************************************************** 
081600*              4000-PROCESS-CREDIT-MEMO                         * 
081700***************************************************************** 
081800                                                                  
081900 4000-PROCESS-CREDIT-MEMO.                                        
082000     MOVE PAY-TYPE-CODE TO WS-CURRENT-PAY-TYPE-CODE.              
082100     EVALUATE TRUE                                                
082200         WHEN TRUST-RELATED                                       
082300             PERFORM 4300-PROCESS-TRUST-RELATED THRU 4300-EXIT    
082400                                                                  
082500         WHEN CHARGE-TYPE                                         
082600             PERFORM 4500-DETERMINE-CHARGE-TYPE THRU 4500-EXIT    
082700                                                                  
082800         WHEN PAYABLE-TRANSFER                                    
082900             ADD PAY-APPLIED-AMT TO WS-PAYABLE-TRANSFER           
083000                                                                  
083100         WHEN UNIDENTIFIED-CASH                                   
083200             ADD PAY-APPLIED-AMT TO WS-UNIDENTIFIED-CASH          
083300                                                                  
083400         WHEN OTHER                                               
083500             ADD PAY-APPLIED-AMT TO WS-MISCELLANEOUS              
083600     END-EVALUATE.                                                
083700                                                                  
083800     PERFORM 5000-FETCH-VWMJ280 THRU 5000-EXIT.                   
083900 4000-EXIT.                                                       
084000     EXIT.                                                        
084100                                                                  
084200***************************************************************** 
084300*                4300-PROCESS-TRUST-RELATED                     * 
084400***************************************************************** 
084500                                                                  
084600 4300-PROCESS-TRUST-RELATED.                                      
084700     INITIALIZE MXAW21-CGR-DATA.                                  
084800     MOVE TRUST-NO OF DCLVWMJ280 TO MXAW21-0200-INVOICE-NBR       
084900                                     MXAW21-SK-INVOICE-NBR.       
085000                                                                  
085100     MOVE TRUST-LINE-NO OF DCLVWMJ280 TO MXAW21-0200-LINE-NBR.    
085200     IF TRUST-LINE-NO OF DCLVWMJ280 = 0                           
085300         CONTINUE                                                 
085400     ELSE                                                         
085500         MOVE DLR-NO OF DCLVWMJ280 TO DLR-NO OF DCLVWMTRLI        
085600         MOVE TRUST-NO OF DCLVWMJ280 TO TRUST-NO OF DCLVWMTRLI    
085700         MOVE TRUST-LINE-NO OF DCLVWMJ280 TO                      
085800              TRUST-LINE-NO OF DCLVWMTRLI                         
085900         PERFORM 5500-SELECT-VWMTRLI THRU 5500-EXIT               
086000         MOVE MODEL-NO TO MXAW21-0200-MODEL-NBR                   
086100         MOVE SERIAL-NO TO MXAW21-0200-SERIAL-NBR                 
086200     END-IF.                                                      
086300                                                                  
086400     MOVE PAY-APPLIED-AMT TO MXAW21-0200-APPLIED-AMT.             
086500****  P0516718 S                                                  
086600     MOVE DLR-NO OF DCLVWMJ280   TO DLR-NO OF DCLVWMTR00.         
086700     MOVE TRUST-NO OF DCLVWMJ280 TO TRUST-NO OF DCLVWMTR00.       
086800     PERFORM 4310-SELECT-VWMTR00 THRU 4310-EXIT.                  
086900     MOVE CUST-ORG-INV-NO OF DCLVWMTR00 TO                        
087000                                MXAW21-0200-ORG-INV-NO.           
087100****  P0516718 E                                                  
087200     MOVE '02' TO MXAW21-SK-RECORD-TYPE.                          
087300     MOVE 0 TO MXAW21-SK-RECORD-TYPE-SEQ.                         
087400     ADD +1 TO WS-NBR-CREDIT-DETAIL.                              
087500     PERFORM 8000-WRITE-EXTRACT-RECORD THRU 8000-EXIT.            
087600 4300-EXIT.                                                       
087700     EXIT.                                                        
087800                                                                  
087900****  P0516718 S                                                  
088000*************************************************************     
088100*                4310-SELECT-VWMTR00                              
088200*************************************************************     
088300                                                                  
088400 4310-SELECT-VWMTR00.                                             
088500     EXEC SQL                                                     
088600       SELECT CUST_ORG_INV_NO                                     
088700        INTO : CUST-ORG-INV-NO                                    
088800         FROM VWMTR00                                             
088900        WHERE DLR_NO         = :DCLVWMTR00.DLR-NO                 
089000          AND TRUST_NO       = :DCLVWMTR00.TRUST-NO               
089100     END-EXEC.                                                    
089200                                                                  
089300     PERFORM Z-970-SET-DA-STATUS-DB2 THRU                         
089400             Z-970-SET-DA-STATUS-DB2-EXIT.                        
089500     IF DA-OK                                                     
089600        CONTINUE                                                  
089700     ELSE                                                         
089800        IF DA-NOTFOUND                                            
089900           MOVE SPACES TO CUST-ORG-INV-NO OF DCLVWMTR00           
090000        ELSE                                                      
090100           MOVE DLR-NO OF DCLVWMTR00 TO WS-DISPLAY-DLR-NO         
090200           DISPLAY ' '                                            
090300           DISPLAY 'FATAL ERROR ON SELECT VWMTR00'                
090400           DISPLAY 'DEALER ' WS-DISPLAY-DLR-NO                    
090500           DISPLAY 'TRUST NO ' TRUST-NO OF DCLVWMTR00             
090600           DISPLAY ' '                                            
090700           SET ABT-DO-ABEND     TO TRUE                           
090800           SET ABT-ERROR-IS-DB2 TO TRUE                           
090900           MOVE 'SELECT  '      TO ABT-DA-FUNCTION                
091000           MOVE '4310-SELE'     TO ABT-ERROR-SECTION              
091100           MOVE 3609            TO ABT-ERROR-ABEND-CODE           
091200           MOVE 'VWMTR00 '      TO ABT-DA-ACCESS-NAME             
091300           PERFORM Z-980-ABNORMAL-TERM THRU                       
091400                   Z-980-ABNORMAL-TERM-RETURN                     
091500        END-IF                                                    
091600     END-IF.                                                      
091700 4310-EXIT.                                                       
091800     EXIT.                                                        
091900                                                                  
092000****  P0516718 E                                                  
092100***************************************************************** 
092200*                4500-DETERMINE-CHARGE-TYPE                     * 
092300***************************************************************** 
092400                                                                  
092500 4500-DETERMINE-CHARGE-TYPE.                                      
092600     INITIALIZE MXAW21-CGR-DATA.                                  
092700     EVALUATE TRUE                                                
092800       WHEN CURTAILMENT                                           
092900           MOVE 25                TO TABLE-ENTRY-WANTED           
093000           MOVE LANGUAGE-CODE     OF DCLVWMCU00                   
093100                                  TO MXCW021-LANG-CODE            
093200           PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT       
093300           MOVE MXCW021-LANG-TEXT TO MXAW21-0300-CHARGE-TYPE      
093400                                     MXAW21-SK-CHARGE-TYPE        
093500       WHEN FLAT                                                  
093600           MOVE 26                TO TABLE-ENTRY-WANTED           
093700           MOVE LANGUAGE-CODE     OF DCLVWMCU00                   
093800                                  TO MXCW021-LANG-CODE            
093900           PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT       
094000           MOVE MXCW021-LANG-TEXT TO MXAW21-0300-CHARGE-TYPE      
094100                                     MXAW21-SK-CHARGE-TYPE        
094200       WHEN ADB                                                   
094300           MOVE 27                TO TABLE-ENTRY-WANTED           
094400           MOVE LANGUAGE-CODE     OF DCLVWMCU00                   
094500                                  TO MXCW021-LANG-CODE            
094600           PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT       
094700           MOVE MXCW021-LANG-TEXT TO MXAW21-0300-CHARGE-TYPE      
094800                                     MXAW21-SK-CHARGE-TYPE        
094900       WHEN SCHED-LIQ                                             
095000           MOVE 28                TO TABLE-ENTRY-WANTED           
095100           MOVE LANGUAGE-CODE     OF DCLVWMCU00                   
095200                                  TO MXCW021-LANG-CODE            
095300           PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT       
095400           MOVE MXCW021-LANG-TEXT TO MXAW21-0300-CHARGE-TYPE      
095500                                     MXAW21-SK-CHARGE-TYPE        
095600       WHEN INSURANCE                                             
095700           MOVE 29                TO TABLE-ENTRY-WANTED           
095800           MOVE LANGUAGE-CODE     OF DCLVWMCU00                   
095900                                  TO MXCW021-LANG-CODE            
096000           PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT       
096100           MOVE MXCW021-LANG-TEXT TO MXAW21-0300-CHARGE-TYPE      
096200                                     MXAW21-SK-CHARGE-TYPE        
096300       WHEN ADMIN                                                 
096400           MOVE 30                TO TABLE-ENTRY-WANTED           
096500           MOVE LANGUAGE-CODE     OF DCLVWMCU00                   
096600                                  TO MXCW021-LANG-CODE            
096700           PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT       
096800           MOVE MXCW021-LANG-TEXT TO MXAW21-0300-CHARGE-TYPE      
096900                                     MXAW21-SK-CHARGE-TYPE        
097000       WHEN NSF                                                   
097100           MOVE 31                TO TABLE-ENTRY-WANTED           
097200           MOVE LANGUAGE-CODE     OF DCLVWMCU00                   
097300                                  TO MXCW021-LANG-CODE            
097400           PERFORM 9000-LANGUAGE-TRANSLATION THRU 9000-EXIT       
097500           MOVE MXCW021-LANG-TEXT TO MXAW21-0300-CHARGE-TYPE      
097600                                     MXAW21-SK-CHARGE-TYPE        
097700       WHEN OTHER                                                 
097800           MOVE '     '           TO MXAW21-0300-CHARGE-TYPE      
097900                                     MXAW21-SK-CHARGE-TYPE        
098000     END-EVALUATE.                                                
098100                                                                  
098200     MOVE PAY-APPLIED-AMT TO MXAW21-0300-APPLIED-AMT.             
098300     MOVE BILL-DATE       TO MXAW21-0300-BILL-DATE                
098400                             MXAW21-SK-BILL-DATE.                 
098500     MOVE '03'            TO MXAW21-SK-RECORD-TYPE.               
098600     MOVE 0               TO MXAW21-SK-RECORD-TYPE-SEQ.           
098700     ADD +1               TO WS-NBR-CREDIT-DETAIL.                
098800     PERFORM 8000-WRITE-EXTRACT-RECORD THRU 8000-EXIT.            
098900                                                                  
099000     MOVE SPACES TO MXAW21-SK-BILL-DATE                           
099100                    MXAW21-SK-CHARGE-TYPE.                        
099200 4500-EXIT.                                                       
099300     EXIT.                                                        
099400                                                                  
099500***************************************************************** 
099600*                5000-FETCH-VWMJ280                             * 
099700***************************************************************** 
099800                                                                  
099900 5000-FETCH-VWMJ280.                                              
100000     EXEC SQL                                                     
100100       FETCH VWMJ280                                              
100200         INTO :DCLVWMJ280.ISSUING-BRANCH-NO,                      
100300              :DCLVWMJ280.ISSUE-DATE,                             
100400              :DCLVWMJ280.PAYEE-TYPE,                             
100500              :DCLVWMJ280.PAYEE-NO,                               
100600              :DCLVWMJ280.PAYEE-SUFF-NO,                          
100700              :DCLVWMJ280.SEQ-NO,                                 
100800              :DCLVWMJ280.PBL-DTL-NO,                             
100900              :DCLVWMJ280.DTL-SEQ-NO,                             
101000              :DCLVWMJ280.PBL-DTL-TYPE-CODE,                      
101100              :DCLVWMJ280.NET-AMT,                                
101200              :DCLVWMJ280.SERVICE-CHRG-AMT,                       
101300              :DCLVWMJ280.BRANCH-NO,                              
101400              :DCLVWMJ280.CUST-NO:WS-CUST-NO-NN,                  
101500              :DCLVWMJ280.PAY-POST-DATE,                          
101600              :DCLVWMJ280.PAY-APPLIED-DATE,                       
101700              :DCLVWMJ280.PAY-NO,                                 
101800              :DCLVWMJ280.PAY-STAT-CODE,                          
101900              :DCLVWMJ280.DLR-NO:WS-DLR-NO-NN,                    
102000              :DCLVWMJ280.TRUST-NO:WS-TRUST-NO-NN,                
102100              :DCLVWMJ280.TRUST-LINE-NO,                          
102200              :DCLVWMJ280.CHRG-SEQ-NO,                            
102300              :DCLVWMJ280.BILL-LOC-NO,                            
102400              :DCLVWMJ280.BILL-DATE:WS-BILL-DATE-NN,              
102500              :DCLVWMJ280.RECV-BRANCH-NO:WS-RECV-BRANCH-NO-NN,    
102600              :DCLVWMJ280.PAY-APPLIED-AMT,                        
102700              :DCLVWMJ280.PAY-TYPE-CODE,                          
102800* AIMS-4398 START                                                 
102900              :DCLVWMJ280.CUST-ORG-CM-NO                          
103000* AIMS-4398 END                                                   
103100     END-EXEC.                                                    
103200                                                                  
103300     PERFORM Z-970-SET-DA-STATUS-DB2 THRU                         
103400             Z-970-SET-DA-STATUS-DB2-EXIT.                        
103500                                                                  
103600     EVALUATE TRUE                                                
103700         WHEN DA-OK                                               
103800             ADD +1 TO WS-VWMJ280-ROWS-FETCHED                    
103900                                                                  
104000         WHEN DA-NOTFOUND                                         
104100             SET NO-MORE-ROWS TO TRUE                             
104200                                                                  
104300         WHEN OTHER                                               
104400             SET ABT-DO-ABEND     TO TRUE                         
104500             SET ABT-ERROR-IS-DB2 TO TRUE                         
104600             MOVE 'FETCH   '      TO ABT-DA-FUNCTION              
104700             MOVE '5000-FETC'     TO ABT-ERROR-SECTION            
104800             MOVE 3606            TO ABT-ERROR-ABEND-CODE         
104900             MOVE 'VWMJ280 '      TO ABT-DA-ACCESS-NAME           
105000             PERFORM Z-980-ABNORMAL-TERM THRU                     
105100                     Z-980-ABNORMAL-TERM-RETURN                   
105200     END-EVALUATE.                                                
105300 5000-EXIT.                                                       
105400     EXIT.                                                        
105500                                                                  
105600***************************************************************** 
105700*                5500-SELECT-VWMTRLI                            * 
105800***************************************************************** 
105900                                                                  
106000 5500-SELECT-VWMTRLI.                                             
106100     EXEC SQL                                                     
106200       SELECT MODEL_NO                                            
106300             ,SERIAL_NO                                           
106400        INTO :MODEL-NO,                                           
106500             :SERIAL-NO                                           
106600         FROM VWMTRLI                                             
106700        WHERE DLR_NO         = :DCLVWMTRLI.DLR-NO                 
106800          AND TRUST_NO       = :DCLVWMTRLI.TRUST-NO               
106900          AND TRUST_LINE_NO  = :DCLVWMTRLI.TRUST-LINE-NO          
107000     END-EXEC.                                                    
107100                                                                  
107200     PERFORM Z-970-SET-DA-STATUS-DB2 THRU                         
107300             Z-970-SET-DA-STATUS-DB2-EXIT.                        
107400                                                                  
107500     IF DA-OK                                                     
107600         CONTINUE                                                 
107700     ELSE                                                         
107800         MOVE DLR-NO OF DCLVWMTRLI TO WS-DISPLAY-DLR-NO           
107900         MOVE TRUST-LINE-NO OF DCLVWMTRLI TO                      
108000              WS-DISPLAY-TRUST-LINE-NO                            
108100         DISPLAY ' '                                              
108200         DISPLAY 'TRUST LINE ITEM NOT FOUND'                      
108300         DISPLAY 'DEALER ' WS-DISPLAY-DLR-NO                      
108400         DISPLAY 'TRUST NO ' TRUST-NO OF DCLVWMTRLI               
108500         DISPLAY 'TRUST LINE NO ' WS-DISPLAY-TRUST-LINE-NO        
108600         DISPLAY ' '                                              
108700         SET ABT-DO-ABEND     TO TRUE                             
108800         SET ABT-ERROR-IS-DB2 TO TRUE                             
108900         MOVE 'SELECT  '      TO ABT-DA-FUNCTION                  
109000         MOVE '5500-SELE'     TO ABT-ERROR-SECTION                
109100         MOVE 3607            TO ABT-ERROR-ABEND-CODE             
109200         MOVE 'VWMTRLI '      TO ABT-DA-ACCESS-NAME               
109300         PERFORM Z-980-ABNORMAL-TERM THRU                         
109400                 Z-980-ABNORMAL-TERM-RETURN                       
109500     END-IF.                                                      
109600 5500-EXIT.                                                       
109700     EXIT.                                                        
109800                                                                  
109900***************************************************************** 
110000*                6000-SELECT-VWMCU00                            * 
110100***************************************************************** 
110200                                                                  
110300 6000-SELECT-VWMCU00.                                             
110400     MOVE SPACES TO LEGAL-NAME-TEXT OF DCLVWMCU00                 
110500                    ADDR1-NAME-TEXT OF DCLVWMCU00                 
110600                    ADDR2-NAME-TEXT OF DCLVWMCU00                 
110700                    CITY-NAME-TEXT  OF DCLVWMCU00.                
110800* TRACKER 9651 S                                                  
110900     MOVE SPACES TO DBA-NAME-TEXT OF DCLVWMCU00.                  
111000* TRACKER 9651 E                                                  
111100                                                                  
111200     EXEC SQL                                                     
111300       SELECT CUST_NO                                             
111400             ,CNTL_ENT_NO                                         
111500             ,DLR_REP_CODE                                        
111600             ,ST_PROV_CODE                                        
111700             ,ZIP_POSTAL_CODE                                     
111800             ,LEGAL_NAME                                          
111900             ,COUNTRY_CODE                                        
112000             ,ADDR1_NAME                                          
112100             ,ADDR2_NAME                                          
112200             ,CITY_NAME                                           
112300             ,LANGUAGE_CODE                                       
112400* TRACKER 9651 S                                                  
112500             ,DBA_NAME                                            
112600* TRACKER 9651 E                                                  
112700        INTO :DCLVWMCU00.CUST-NO                                  
112800            ,:DCLVWMCU00.CNTL-ENT-NO                              
112900            ,:DCLVWMCU00.DLR-REP-CODE:WS-DLR-REP-CODE-NN          
113000            ,:DCLVWMCU00.ST-PROV-CODE                             
113100            ,:DCLVWMCU00.ZIP-POSTAL-CODE                          
113200            ,:DCLVWMCU00.LEGAL-NAME                               
113300            ,:DCLVWMCU00.COUNTRY-CODE                             
113400            ,:DCLVWMCU00.ADDR1-NAME                               
113500            ,:DCLVWMCU00.ADDR2-NAME                               
113600            ,:DCLVWMCU00.CITY-NAME                                
113700            ,:DCLVWMCU00.LANGUAGE-CODE                            
113800* TRACKER 9651 S                                                  
113900            ,:DCLVWMCU00.DBA-NAME                                 
114000* TRACKER 9651 E                                                  
114100         FROM VWMCU00                                             
114200         WHERE CUST_NO  = :DCLVWMCU00.CUST-NO                     
114300     END-EXEC.                                                    
114400                                                                  
114500     PERFORM Z-970-SET-DA-STATUS-DB2 THRU                         
114600             Z-970-SET-DA-STATUS-DB2-EXIT.                        
114700                                                                  
114800     IF DA-OK                                                     
114900         CONTINUE                                                 
115000     ELSE                                                         
115100         SET ABT-DO-ABEND     TO TRUE                             
115200         SET ABT-ERROR-IS-DB2 TO TRUE                             
115300         MOVE 'SELECT  '      TO ABT-DA-FUNCTION                  
115400         MOVE '6000-SELE'     TO ABT-ERROR-SECTION                
115500         MOVE 3608            TO ABT-ERROR-ABEND-CODE             
115600         MOVE 'VWMCU00 '      TO ABT-DA-ACCESS-NAME               
115700         PERFORM Z-980-ABNORMAL-TERM THRU                         
115800                 Z-980-ABNORMAL-TERM-RETURN                       
115900     END-IF.                                                      
116000 6000-EXIT.                                                       
116100     EXIT.                                                        
116200***************************************************************** 
116300*                6500-SELECT-VWMRP00                            * 
116400***************************************************************** 
116500                                                                  
116600 6500-SELECT-VWMRP00.                                             
116700     MOVE SPACES TO DLR-REP-NAME OF DCLVWMRP00                    
116800                    PHONE-NO OF DCLVWMRP00.                       
116900                                                                  
117000     EXEC SQL                                                     
117100       SELECT DLR_REP_NAME                                        
117200             ,PHONE_NO                                            
117300        INTO :DCLVWMRP00.DLR-REP-NAME,                            
117400             :DCLVWMRP00.PHONE-NO                                 
117500         FROM VWMRP00                                             
117600         WHERE DLR_REP_CODE = :DCLVWMRP00.DLR-REP-CODE            
117700     END-EXEC.                                                    
117800                                                                  
117900     PERFORM Z-970-SET-DA-STATUS-DB2 THRU                         
118000             Z-970-SET-DA-STATUS-DB2-EXIT.                        
118100                                                                  
118200     IF DA-OK OR                                                  
118300        DA-NOTFOUND                                               
118400         CONTINUE                                                 
118500     ELSE                                                         
118600         SET ABT-DO-ABEND     TO TRUE                             
118700         SET ABT-ERROR-IS-DB2 TO TRUE                             
118800         MOVE 'SELECT  '      TO ABT-DA-FUNCTION                  
118900         MOVE '6500-SELE'     TO ABT-ERROR-SECTION                
119000         MOVE 3610            TO ABT-ERROR-ABEND-CODE             
119100         MOVE 'VWMRP00 '      TO ABT-DA-ACCESS-NAME               
119200         PERFORM Z-980-ABNORMAL-TERM THRU                         
119300                 Z-980-ABNORMAL-TERM-RETURN                       
119400     END-IF.                                                      
119500 6500-EXIT.                                                       
119600     EXIT.                                                        
119700                                                                  
119800***************************************************************** 
119900*                7000-SELECT-VWMCTUPD                           * 
120000***************************************************************** 
120100                                                                  
120200 7000-SELECT-VWMCTUPD.                                            
120300     EXEC SQL                                                     
120400       SELECT PROC_DATE                                           
120500             ,(PROC_DATE + 1 DAYS)                                
120600         INTO :DCLVWMCTUPD.PROC-DATE:WS-PROC-DATE-NN,             
120700              :WS-FROM-DATE:WS-FROM-DATE-NN                       
120800         FROM VWMCTUPD                                            
120900****  P0516718 S                                                  
121000*        WHERE SUBSYSTEM_ID_IND = :SUBSYSTEM-ID-IND               
121100*          AND SUBFUNCTION_CODE = :SUBFUNCTION-CODE               
121200         WHERE SUBSYSTEM_ID_IND = :DCLVWMCTUPD.SUBSYSTEM-ID-IND   
121300           AND SUBFUNCTION_CODE = :DCLVWMCTUPD.SUBFUNCTION-CODE   
121400****  P0516718 E                                                  
121500     END-EXEC.                                                    
121600                                                                  
121700     PERFORM Z-970-SET-DA-STATUS-DB2 THRU                         
121800             Z-970-SET-DA-STATUS-DB2-EXIT.                        
121900 7000-EXIT.                                                       
122000     EXIT.                                                        
122100                                                                  
122200***************************************************************** 
122300*                8000-WRITE-EXTRACT-RECORD                      * 
122400***************************************************************** 
122500                                                                  
122600 8000-WRITE-EXTRACT-RECORD.                                       
122700     WRITE STATEMENT-OF-CREDIT-EXTRACT-RC                         
122800         FROM MXAW21-CREDIT-GENERAL-RECORD.                       
122900                                                                  
123000     ADD +1 TO WS-EXTRACT-RECS-WRITTEN.                           
123100 8000-EXIT.                                                       
123200     EXIT.                                                        
123300                                                                  
123400***************************************************************** 
123500*                   9900-TERMINATION                            * 
123600***************************************************************** 
123700                                                                  
123800 9900-TERMINATION.                                                
123900     MOVE WS-VWMJ280-ROWS-FETCHED TO WS-DISPLAY-COUNTER.          
124000     DISPLAY ' '.                                                 
124100     DISPLAY 'NUMBER OF APPLIED PAYMENT VWMJ280 ROWS FETCHED '    
124200             WS-DISPLAY-COUNTER.                                  
124300                                                                  
124400     IF DAILY-PROCESSING                                          
124500         MOVE 'A045DALY' TO SUBFUNCTION-CODE                      
124600     ELSE                                                         
124700         MOVE 'A045WKLY' TO SUBFUNCTION-CODE                      
124800     END-IF.                                                      
124900                                                                  
125000****  P0516718 S                                                  
125100*    MOVE 'A' TO SUBSYSTEM-ID-IND.                                
125200     MOVE 'A' TO SUBSYSTEM-ID-IND OF DCLVWMCTUPD.                 
125300****  P0516718 E                                                  
125400     MOVE WS-TO-DATE TO PROC-DATE.                                
125500     PERFORM 9500-UPDATE-VWMCTUPD THRU 9500-EXIT.                 
125600                                                                  
125700     CLOSE STATEMENT-OF-CREDIT-EXTRACT.                           
125800                                                                  
125900     EXEC SQL                                                     
126000         CLOSE VWMJ280                                            
126100     END-EXEC.                                                    
126200                                                                  
126300     PERFORM Z-970-SET-DA-STATUS-DB2 THRU                         
126400             Z-970-SET-DA-STATUS-DB2-EXIT.                        
126500                                                                  
126600     IF NOT DA-OK                                                 
126700         SET ABT-DO-ABEND     TO TRUE                             
126800         SET ABT-ERROR-IS-DB2 TO TRUE                             
126900         MOVE 'CLOSE   '      TO ABT-DA-FUNCTION                  
127000         MOVE '9900-TERM'     TO ABT-ERROR-SECTION                
127100         MOVE 3611            TO ABT-ERROR-ABEND-CODE             
127200         MOVE 'VWMJ280 '      TO ABT-DA-ACCESS-NAME               
127300         PERFORM Z-980-ABNORMAL-TERM THRU                         
127400                 Z-980-ABNORMAL-TERM-RETURN                       
127500     END-IF.                                                      
127600                                                                  
127700     DISPLAY ' '.                                                 
127800     DISPLAY 'PROGRAM MXBPA045 SUCCESSFULLY COMPLETED'.           
127900 9900-EXIT.                                                       
128000     EXIT.                                                        
128100                                                                  
128200***************************************************************** 
128300*                9500-UPDATE-VWMCTUPD                           * 
128400***************************************************************** 
128500* PERFORMED FROM 9900-TERMINATION AFTER PROCESSING IS COMPLETE. * 
128600* THIS PARAGRAPH WILL UPDATE EITHER THE WEEKLY (A045WKLY) OR    * 
128700* DAILY (A045DALY) PROCSSING DATE IN PREPERATION FOR THE NEXT   * 
128800* RUN.                                                          * 
128900***************************************************************** 
129000                                                                  
129100 9500-UPDATE-VWMCTUPD.                                            
129200     EXEC SQL                                                     
129300       UPDATE VWMCTUPD                                            
129400         SET PROC_DATE  =  :PROC-DATE                             
129500****  P0516718 S                                                  
129600*        WHERE SUBSYSTEM_ID_IND = :SUBSYSTEM-ID-IND               
129700*          AND SUBFUNCTION_CODE = :SUBFUNCTION-CODE               
129800         WHERE SUBSYSTEM_ID_IND = :DCLVWMCTUPD.SUBSYSTEM-ID-IND   
129900           AND SUBFUNCTION_CODE = :DCLVWMCTUPD.SUBFUNCTION-CODE   
130000****  P0516718 E                                                  
130100     END-EXEC.                                                    
130200                                                                  
130300     PERFORM Z-970-SET-DA-STATUS-DB2 THRU                         
130400             Z-970-SET-DA-STATUS-DB2-EXIT.                        
130500                                                                  
130600     IF DA-OK                                                     
130700         CONTINUE                                                 
130800     ELSE                                                         
130900         SET ABT-DO-ABEND     TO TRUE                             
131000         SET ABT-ERROR-IS-DB2 TO TRUE                             
131100         MOVE 'UPDATE  '      TO ABT-DA-FUNCTION                  
131200         MOVE '9500-UPDA'     TO ABT-ERROR-SECTION                
131300         MOVE 3612            TO ABT-ERROR-ABEND-CODE             
131400         MOVE 'VWMCTUPD'      TO ABT-DA-ACCESS-NAME               
131500         PERFORM Z-980-ABNORMAL-TERM THRU                         
131600                 Z-980-ABNORMAL-TERM-RETURN                       
131700     END-IF.                                                      
131800 9500-EXIT.                                                       
131900     EXIT.                                                        
132000/                                                                 
132100*9000-LANGUAGE-TRANSLATION. PARAGRAPH IS IN FOLLOWING COPYBOOK    
132200     EXEC SQL                                                     
132300         INCLUDE MXWP35                                           
132400     END-EXEC.                                                    
132500/                                                                 
132600     EXEC SQL                                                     
132700         INCLUDE MXWP02                                           
132800     END-EXEC.                                                    
132900                                                                  