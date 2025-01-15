000010 IDENTIFICATION DIVISION.                                         
000020 PROGRAM-ID.  MXBPA046.
000030
000620 ENVIRONMENT DIVISION.
000640 CONFIGURATION SECTION.
000650 SOURCE-COMPUTER. IBM-3090.
000660 OBJECT-COMPUTER. IBM-3090.
000670
000680 INPUT-OUTPUT SECTION.
000690 FILE-CONTROL.
000700     SELECT CREDIT-INFILE
000710         ASSIGN TO UT-S-MXA046I1.
000720
000730     SELECT REPORT-OUTFILE
000740         ASSIGN TO UT-S-MXA046O1.
000750
000760 DATA DIVISION.
000770 FILE SECTION.
000780 FD  CREDIT-INFILE
000790     RECORDING MODE IS F.
000800 01  CREDIT-REC                          PIC X(162).
000820 FD  REPORT-OUTFILE
000830     RECORDING MODE IS F.
000840 01  REPORT-REC                          PIC X(132).
000850
000860 WORKING-STORAGE SECTION.
000870 01  WS-MISC-FIELDS.
000880     05  WS-SAVE-DATE                    PIC X(10).
000890     05  INPUT-EOF-SW                    PIC X(01) VALUE SPACES.
000900         88  INPUT-AT-END                          VALUE 'Y'.
000910     05  FIRST-CREDIT-MEMO-SW            PIC X(01) VALUE 'Y'.
000920         88  FIRST-CREDIT-MEMO                     VALUE 'Y'.
000930     05  FIRST-MEMO-DONE-SW              PIC X(01) VALUE SPACES.
000940         88  FIRST-MEMO-DONE                       VALUE 'Y'.
000950     05  CRITICAL-ERROR-SW               PIC X(01) VALUE SPACES.
000960         88  CRITICAL-ERROR                        VALUE 'Y'.
000970     05  READ-TYPE-3-SW                  PIC X(01) VALUE 'N'.
000980     05  PAGE-BREAK-SW                   PIC X(01) VALUE SPACES.
000990         88  HEADINGS-WERE-PRINTED                 VALUE 'Y'.
001000
001010****************************************************************
001020* INPUT RECORD LAYOUT.                                         *
001030****************************************************************
001040
001050     EXEC SQL
001060         INCLUDE MXAW21
001070     END-EXEC.
001080
001090     EXEC SQL
001100         INCLUDE MXWW03
001110     END-EXEC.
001120
001130     EXEC SQL
001140         INCLUDE VWMCTUPD
001150     END-EXEC.
001160
001170     EXEC SQL
001180         INCLUDE SQLCA
001190     END-EXEC.
001200
001210****************************************************************
001220*    COUNTERS & TOTALS                                         *
001230****************************************************************
001240
001250 01  WS-MISC-COUNTERS.
001260     05  PAGE-CNT                        PIC 9(06) VALUE 1.
001270     05  LINE-CNT                        PIC 9(02) VALUE 60.
001280     05  REC-CNT                         PIC S9(09) COMP-3
001290                                                    VALUE 0.
001300
001310 01  WS-CREDIT-MEMO-TOTALS.
001320     05  TOT-CREDIT-MEMO                 PIC S9(09)V99 VALUE 0.
001330     05  TOT-CREDIT-DLR                  PIC S9(09)V99 VALUE 0.
001340
001350 01  CUR-AREA.
001360     05  CUR-DLR-NBR                     PIC S9(09) COMP VALUE 0.
001370     05  CUR-ADDL-CUST-LIT               PIC X(15) VALUE SPACES.
001380     05  CUR-ADDL-CUST-NO                PIC X(13) VALUE SPACES.
001390     05  CUR-AREA-1.
001400        10  CUR-DIST-NAME                PIC X(35) VALUE SPACES.
001410        10  CUR-CREDIT-MEMO-NBR          PIC X(11) VALUE SPACES.
001420        10  CUR-LANG-IND                 PIC X(06) VALUE SPACES.
001430     05 CUR-AREA-2.
001440        10  CUR-DLR-ADDRESS1             PIC X(50) VALUE SPACES.
001450        10  CUR-DLR-ADDRESS2             PIC X(50) VALUE SPACES.
001460        10  CUR-DLR-ADDRESS3             PIC X(50) VALUE SPACES.
001470        10  CUR-DLR-ADDRESS4             PIC X(50) VALUE SPACES.
001480        10  CUR-DLR-ADDRESS5             PIC X(50) VALUE SPACES.
001490        10  CUR-REP-NAME                 PIC X(20) VALUE SPACES.
001500        10  CUR-REP-PHONE                PIC X(20) VALUE SPACES.
001510        10  CUR-DLR-NAME                 PIC X(35) VALUE SPACES.
001520        10  CUR-APPLIED-DATE             PIC X(10) VALUE SPACES.
001530        10  CUR-BRANCH                   PIC X(04) VALUE SPACES.
001540
001550 01  PRV-AREA.
001560     05  PRV-DLR-NBR                     PIC S9(09) COMP VALUE 0.
001570     05  PRV-ADDL-CUST-LIT               PIC X(15) VALUE SPACES.
001580     05  PRV-ADDL-CUST-NO                PIC X(13) VALUE SPACES.
001590     05  PRV-AREA-1.
001600         10  PRV-DIST-NAME               PIC X(35) VALUE SPACES.
001610         10  PRV-CREDIT-MEMO-NBR         PIC X(11) VALUE SPACES.
001620         10  PRV-LANG-IND                PIC X(06) VALUE SPACES.
001630     05  PRV-AREA-2.
001640         10  PRV-DLR-ADDRESS1            PIC X(50) VALUE SPACES.
001650         10  PRV-DLR-ADDRESS2            PIC X(50) VALUE SPACES.
001660         10  PRV-DLR-ADDRESS3            PIC X(50) VALUE SPACES.
001670         10  PRV-DLR-ADDRESS4            PIC X(50) VALUE SPACES.
001680         10  PRV-DLR-ADDRESS5            PIC X(50) VALUE SPACES.
001690         10  PRV-REP-NAME                PIC X(20) VALUE SPACES.
001700         10  PRV-REP-PHONE               PIC X(20) VALUE SPACES.
001710         10  PRV-DLR-NAME                PIC X(35) VALUE SPACES.
001720         10  PRV-APPLIED-DATE            PIC X(10) VALUE SPACES.
001730         10  PRV-BRANCH                  PIC X(04) VALUE SPACES.
001740
001750 01  ACCUM-TABLE.
001760     05  WS-CASH-AMT                     PIC S9(09)V99 COMP-3
001770                                                       VALUE 0.
001780     05  WS-TRANSFER-AMT                 PIC S9(09)V99 COMP-3
001790                                                       VALUE 0.
001800     05  WS-MISC-AMT                     PIC S9(09)V99 COMP-3
001810                                                       VALUE 0.
001820     05  WS-CHARGE-AMT                   PIC S9(09)V99 COMP-3
001830                                                       VALUE 0.
001840     05  WS-MEMO-TOT                     PIC S9(11)V99 COMP-3
001850                                                       VALUE 0.
001860     05  WS-DEALER-TOT                   PIC S9(13)V99 COMP-3
001870                                                       VALUE 0.
001880     05  WS-GRAND-TOT                    PIC S9(15)V99 COMP-3
001890                                                       VALUE 0.
001900     05  TOT-1-AMOUNT                    PIC S9(15)V99 COMP-3
001910                                                       VALUE 0.
001920     05  DET-1-AMOUNT                    PIC S9(15)V99 COMP-3
001930                                                       VALUE 0.
001940     05  DET-2-AMOUNT                    PIC S9(15)V99 COMP-3
001950                                                       VALUE 0.
001960     05  WS-DET-LINE-NO                  PIC 9(4)  VALUE 0.
001970
001980****************************************************************
001990*    ENGLISH  HEADER.                                          *
002000****************************************************************
002010
002020 01  HEAD-ENGL-1.
002030     05  FILLER                          PIC X(08) VALUE
002040                                                       'MXBPA046'.
002050     05  FILLER                          PIC X(36) VALUE SPACES.
002060     05  FILLER                          PIC X(43) VALUE
002070                    '    TEST TEST TEST TEST TEST TEST TEST     '.
002080     05  FILLER                          PIC X(27) VALUE SPACES.
002090     05  FILLER                          PIC X(10) VALUE
002100                                                     'RUN DATE: '.
002110     05  HEAD-ENGL-1-DATE                PIC X(08).
002120
002130 01  HEAD-ENGL-2.
002140     05  FILLER                          PIC X(04) VALUE SPACES.
002150     05  HEAD-ENGL-2-BRANCH              PIC X(04).
002160     05  FILLER                          PIC X(36) VALUE SPACES.
002170     05  FILLER                          PIC X(43) VALUE
002180                    '         CREDIT APPLIED DETAIL             '.
002190     05  FILLER                          PIC X(27) VALUE SPACES.
002200     05  FILLER                          PIC X(10) VALUE
002210                                                     '    PAGE: '.
002220     05  HEAD-ENGL-2-PAGE                PIC ZZ,ZZ9.
002230
002240 01  HEAD-ENGL-3.
002250     05  FILLER                          PIC X(08) VALUE SPACES.
002260     05  FILLER                          PIC X(36) VALUE SPACES.
002270     05  FILLER                          PIC X(13) VALUE
002280                                                  'CUSTOMER NO: '.
002290     05  HEAD-ENGL-3-CUST                PIC ZZZZZZ.
002300     05  FILLER                          PIC X(47) VALUE SPACES.
002310     05  FILLER                          PIC X(10) VALUE SPACES.
002320
002330 01  HEAD-ENGL-3A.
002340     05  FILLER                          PIC X(08) VALUE SPACES.
002350     05  FILLER                          PIC X(34) VALUE SPACES.
002360     05  HE-3A-ADDL-CUST-LIT             PIC X(15) VALUE SPACES.
002370     05  HE-3A-ADDL-CUST-NO              PIC X(13) VALUE SPACES.
002380     05  FILLER                          PIC X(47) VALUE SPACES.
002390     05  FILLER                          PIC X(10) VALUE SPACES.
002400
002410 01  HEAD-ENGL-4.
002420     05  FILLER                          PIC X(08) VALUE SPACES.
002430     05  FILLER                          PIC X(36) VALUE SPACES.
002440     05  FILLER                          PIC X(13) VALUE SPACES.
002450     05  HEAD-ENGL-4-DEAL-NAME           PIC X(40).
002460     05  FILLER                          PIC X(47) VALUE SPACES.
002470     05  FILLER                          PIC X(10) VALUE SPACES.
002480
002490 01  HEAD-ENGL-5.
002500     05  FILLER                          PIC X(08) VALUE SPACES.
002510     05  FILLER                          PIC X(36) VALUE SPACES.
002520     05  FILLER                          PIC X(13) VALUE SPACES.
002530     05  HEAD-ENGL-5-DEAL-ADDR1          PIC X(50).
002540     05  FILLER                          PIC X(37) VALUE SPACES.
002550     05  FILLER                          PIC X(10) VALUE SPACES.
002560
002570 01  HEAD-ENGL-6.
002580     05  FILLER                          PIC X(08) VALUE SPACES.
002590     05  FILLER                          PIC X(36) VALUE SPACES.
002600     05  FILLER                          PIC X(13) VALUE SPACES.
002610     05  HEAD-ENGL-6-DEAL-ADDR2          PIC X(50).
002620     05  FILLER                          PIC X(37) VALUE SPACES.
002630     05  FILLER                          PIC X(10) VALUE SPACES.
002640
002650 01  HEAD-ENGL-7A.
002660     05  FILLER                          PIC X(08) VALUE SPACES.
002670     05  FILLER                          PIC X(36) VALUE SPACES.
002680     05  FILLER                          PIC X(13) VALUE SPACES.
002690     05  HEAD-ENGL-7-DEAL-ADDR3          PIC X(50).
002700     05  FILLER                          PIC X(37) VALUE SPACES.
002710     05  FILLER                          PIC X(10) VALUE SPACES.
002720
002730 01  HEAD-ENGL-7B.
002740     05  FILLER                          PIC X(08) VALUE SPACES.
002750     05  FILLER                          PIC X(36) VALUE SPACES.
002760     05  FILLER                          PIC X(13) VALUE SPACES.
002770     05  HEAD-ENGL-7-DEAL-ADDR4          PIC X(50).
002780     05  FILLER                          PIC X(37) VALUE SPACES.
002790     05  FILLER                          PIC X(10) VALUE SPACES.
002800
002810 01  HEAD-ENGL-7C.
002820     05  FILLER                          PIC X(08) VALUE SPACES.
002830     05  FILLER                          PIC X(36) VALUE SPACES.
002840     05  FILLER                          PIC X(13) VALUE SPACES.
002850     05  HEAD-ENGL-7-DEAL-ADDR5          PIC X(50).
002860     05  FILLER                          PIC X(37) VALUE SPACES.
002870     05  FILLER                          PIC X(10) VALUE SPACES.
002880
002890 01  HEAD-ENGL-8.
002900     05  FILLER                          PIC X(02) VALUE SPACES.
002910     05  FILLER                          PIC X(22) VALUE
002920                                         'CUSTOMER SERVICE REP: '.
002930     05  HEAD-ENGL-8-CUST                PIC X(20).
002940     05  FILLER                          PIC X(01) VALUE SPACES.
002950     05  HEAD-ENGL-8-PHONE               PIC X(20).
002960     05  FILLER                          PIC X(10) VALUE SPACES.
002970
002980 01  HEAD-ENGL-9.
002990     05  FILLER                          PIC X(01) VALUE SPACES.
003000     05  FILLER                          PIC X(11) VALUE
003010                                                    'DIST NAME: '.
003020     05  HEAD-ENGL-9-DISNAME             PIC X(35).
003030     05  FILLER                          PIC X(01) VALUE SPACES.
003040     05  FILLER                          PIC X(04) VALUE SPACES.
003050     05  FILLER                          PIC X(07) VALUE
003060                                                        'CR NO: '.
003070     05  HEAD-ENGL-9-CREDNO              PIC X(11).
003080     05  FILLER                          PIC X(01) VALUE SPACES.
003090     05  FILLER                          PIC X(09) VALUE
003100                                                      'APL DAT: '.
003110     05  HEAD-ENGL-9-APDATE              PIC X(08).
003120
003130 01  HEAD-ENGL-10.
003140     05  FILLER                          PIC X(04) VALUE SPACES.
003150     05  FILLER                          PIC X(11) VALUE
003160                                                    'INVOICE    '.
003170     05  FILLER                          PIC X(03) VALUE SPACES.
003180     05  FILLER                          PIC X(10) VALUE
003190                                                     '    LINE  '.
003200     05  FILLER                          PIC X(03) VALUE SPACES.
003210     05  FILLER                          PIC X(12) VALUE
003220                                                   '  MODEL     '.
003230     05  FILLER                          PIC X(03) VALUE SPACES.
003240     05  FILLER                          PIC X(17) VALUE
003250                                              '      SERIAL     '.
003260     05  FILLER                          PIC X(06) VALUE SPACES.
003270     05  FILLER                          PIC X(14) VALUE
003280                                                 '   APPLIED    '.
003290     05  FILLER                          PIC X(03) VALUE SPACES.
003300     05  FILLER                          PIC X(11) VALUE
003310                                                    '  CHARGE   '.
003320     05  FILLER                          PIC X(05) VALUE SPACES.
003330     05  FILLER                          PIC X(11) VALUE
003340                                                    '           '.
003350
003360 01  HEAD-ENGL-11.
003370     05  FILLER                          PIC X(04) VALUE SPACES.
003380     05  FILLER                          PIC X(11) VALUE
003390                                                    'NUMBER     '.
003400     05  FILLER                          PIC X(03) VALUE SPACES.
003410     05  FILLER                          PIC X(10) VALUE
003420                                                     '     #    '.
003430     05  FILLER                          PIC X(03) VALUE SPACES.
003440     05  FILLER                          PIC X(12) VALUE
003450                                                   '  NUMBER    '.
003460     05  FILLER                          PIC X(03) VALUE SPACES.
003470     05  FILLER                          PIC X(17) VALUE
003480                                              '      NUMBER     '.
003490     05  FILLER                          PIC X(09) VALUE SPACES.
003500     05  FILLER                          PIC X(14) VALUE
003510                                                 'AMOUNT        '.
003520     05  FILLER                          PIC X(01) VALUE SPACES.
003530     05  FILLER                          PIC X(11) VALUE
003540                                                    '  TYPE     '.
003550     05  FILLER                          PIC X(05) VALUE SPACES.
003560     05  FILLER                          PIC X(11) VALUE
003570                                                    '           '.
003580
003590****************************************************************
003600*    FRENCH HEADER.                                            *
003610****************************************************************
003620
003630 01  HEAD-FRAN-1.
003640     05  FILLER                          PIC X(08) VALUE
003650                                                       'MXBPA046'.
003660     05  FILLER                          PIC X(36) VALUE SPACES.
003670     05  FILLER                          PIC X(50) VALUE
003680             '    TEST TEST TEST TEST TEST TEST TEST TEST       '.
003690     05  FILLER                          PIC X(20) VALUE SPACES.
003700     05  FILLER                          PIC X(10) VALUE
003710                                                     '    DATE: '.
003720     05  HEAD-FRAN-1-DATE                PIC X(08).
003730
003740 01  HEAD-FRAN-2.
003750     05  FILLER                          PIC X(04) VALUE SPACES.
003760     05  HEAD-FRAN-2-BRANCH              PIC X(04).
003770     05  FILLER                          PIC X(36) VALUE SPACES.
003780     05  FILLER                          PIC X(43) VALUE
003790                    '     DETAILS DES SOMMES CREDITEES          '.
003800     05  FILLER                          PIC X(27) VALUE SPACES.
003810     05  FILLER                          PIC X(10) VALUE
003820                                                     '    PAGE: '.
003830     05  HEAD-FRAN-2-PAGE                PIC ZZ,ZZ9.
003840
003850 01  HEAD-FRAN-3.
003860     05  FILLER                          PIC X(08) VALUE SPACES.
003870     05  FILLER                          PIC X(36) VALUE SPACES.
003880     05  FILLER                          PIC X(13) VALUE
003890                                                  '  CLIENT # : '.
003900     05  HEAD-FRAN-3-CUST                PIC ZZZZZZ.
003910     05  FILLER                          PIC X(47) VALUE SPACES.
003920     05  FILLER                          PIC X(10) VALUE SPACES.
003930
003940 01  HEAD-FRAN-4.
003950     05  FILLER                          PIC X(08) VALUE SPACES.
003960     05  FILLER                          PIC X(36) VALUE SPACES.
003970     05  FILLER                          PIC X(13) VALUE SPACES.
003980     05  HEAD-FRAN-4-DEAL-NAME           PIC X(40).
003990     05  FILLER                          PIC X(47) VALUE SPACES.
004000     05  FILLER                          PIC X(10) VALUE SPACES.
004010
004020 01  HEAD-FRAN-5.
004030     05  FILLER                          PIC X(08) VALUE SPACES.
004040     05  FILLER                          PIC X(36) VALUE SPACES.
004050     05  FILLER                          PIC X(13) VALUE SPACES.
004060     05  HEAD-FRAN-5-DEAL-ADDR1          PIC X(50).
004070     05  FILLER                          PIC X(37) VALUE SPACES.
004080     05  FILLER                          PIC X(10) VALUE SPACES.
004090
004100 01  HEAD-FRAN-6.
004110     05  FILLER                          PIC X(08) VALUE SPACES.
004120     05  FILLER                          PIC X(36) VALUE SPACES.
004130     05  FILLER                          PIC X(13) VALUE SPACES.
004140     05  HEAD-FRAN-6-DEAL-ADDR2          PIC X(50).
004150     05  FILLER                          PIC X(37) VALUE SPACES.
004160     05  FILLER                          PIC X(10) VALUE SPACES.
004170
004180 01  HEAD-FRAN-7A.
004190     05  FILLER                          PIC X(08) VALUE SPACES.
004200     05  FILLER                          PIC X(36) VALUE SPACES.
004210     05  FILLER                          PIC X(13) VALUE SPACES.
004220     05  HEAD-FRAN-7-DEAL-ADDR3          PIC X(50).
004230     05  FILLER                          PIC X(37) VALUE SPACES.
004240     05  FILLER                          PIC X(10) VALUE SPACES.
004250
004260 01  HEAD-FRAN-7B.
004270     05  FILLER                          PIC X(08) VALUE SPACES.
004280     05  FILLER                          PIC X(36) VALUE SPACES.
004290     05  FILLER                          PIC X(13) VALUE SPACES.
004300     05  HEAD-FRAN-7-DEAL-ADDR4          PIC X(50).
004310     05  FILLER                          PIC X(37) VALUE SPACES.
004320     05  FILLER                          PIC X(10) VALUE SPACES.
004330
004340 01  HEAD-FRAN-7C.
004350     05  FILLER                          PIC X(08) VALUE SPACES.
004360     05  FILLER                          PIC X(36) VALUE SPACES.
004370     05  FILLER                          PIC X(13) VALUE SPACES.
004380     05  HEAD-FRAN-7-DEAL-ADDR5          PIC X(50).
004390     05  FILLER                          PIC X(37) VALUE SPACES.
004400     05  FILLER                          PIC X(10) VALUE SPACES.
004410
004420 01  HEAD-FRAN-8.
004430     05  FILLER                          PIC X(02) VALUE SPACES.
004440     05  FILLER                          PIC X(22) VALUE
004450                                         'DIRECTEUR DE COMPTE : '.
004460     05  HEAD-FRAN-8-CUST                PIC X(20).
004470     05  FILLER                          PIC X(01) VALUE SPACES.
004480     05  HEAD-FRAN-8-PHONE               PIC X(15).
004490     05  FILLER                          PIC X(10) VALUE SPACES.
004500
004510 01  HEAD-FRAN-9.
004520     05  FILLER                          PIC X(10) VALUE
004530                                                     'NOM DIST: '.
004540     05  HEAD-FRAN-9-DISNAME             PIC X(35).
004550     05  FILLER                          PIC X(01) VALUE SPACES.
004560     05  FILLER                          PIC X(04) VALUE SPACES.
004570     05  FILLER                          PIC X(19) VALUE
004580                                            'NOTE DE CREDIT NO: '.
004590     05  HEAD-FRAN-9-CREDNO              PIC X(11).
004600     05  FILLER                          PIC X(15) VALUE
004610                                                'DATE CREDITEE: '.
004620     05  HEAD-FRAN-9-APDATE              PIC X(08).
004630
004640 01  HEAD-FRAN-10.
004650     05  FILLER                          PIC X(04) VALUE SPACES.
004660     05  FILLER                          PIC X(11) VALUE
004670                                                    'FACTURE    '.
004680     05  FILLER                          PIC X(03) VALUE SPACES.
004690     05  FILLER                          PIC X(10) VALUE
004700                                                     '    LIGNE '.
004710     05  FILLER                          PIC X(03) VALUE SPACES.
004720     05  FILLER                          PIC X(12) VALUE
004730                                                   '  MODELE    '.
004740     05  FILLER                          PIC X(05) VALUE SPACES.
004750     05  FILLER                          PIC X(17) VALUE
004760                                              '    SERIALE      '.
004770     05  FILLER                          PIC X(05) VALUE SPACES.
004780     05  FILLER                          PIC X(14) VALUE
004790                                                 ' MONTANT      '.
004800     05  FILLER                          PIC X(02) VALUE SPACES.
004810     05  FILLER                          PIC X(11) VALUE
004820                                                    ' TYPE DE   '.
004830     05  FILLER                          PIC X(04) VALUE SPACES.
004840     05  FILLER                          PIC X(11) VALUE
004850                                                    '           '.
004860
004870 01  HEAD-FRAN-11.
004880     05  FILLER                          PIC X(04) VALUE SPACES.
004890     05  FILLER                          PIC X(11) VALUE
004900                                                    '  NO.      '.
004910     05  FILLER                          PIC X(03) VALUE SPACES.
004920     05  FILLER                          PIC X(10) VALUE
004930                                                     '     NO.  '.
004940     05  FILLER                          PIC X(03) VALUE SPACES.
004950     05  FILLER                          PIC X(12) VALUE
004960                                                   '   NO.      '.
004970     05  FILLER                          PIC X(05) VALUE SPACES.
004980     05  FILLER                          PIC X(17) VALUE
004990                                              '      NO.        '.
005000     05  FILLER                          PIC X(05) VALUE SPACES.
005010     05  FILLER                          PIC X(14) VALUE
005020                                                 ' CREDITE      '.
005030     05  FILLER                          PIC X(02) VALUE SPACES.
005040     05  FILLER                          PIC X(11) VALUE
005050                                                    '  FRAIS    '.
005060     05  FILLER                          PIC X(04) VALUE SPACES.
005070     05  FILLER                          PIC X(11) VALUE
005080                                                    '           '.
005090
005100 01  HEAD-COMM-1.
005110     05  FILLER                          PIC X(04) VALUE SPACES.
005120     05  FILLER                          PIC X(11) VALUE
005130                                                    '-----------'.
005140     05  FILLER                          PIC X(03) VALUE SPACES.
005150     05  FILLER                          PIC X(10) VALUE
005160                                                     '    ----  '.
005170     05  FILLER                          PIC X(02) VALUE SPACES.
005180     05  FILLER                          PIC X(12) VALUE
005190                                                   '------------'.
005200     05  FILLER                          PIC X(05) VALUE SPACES.
005210     05  FILLER                          PIC X(17) VALUE
005220                                              '-----------------'.
005230     05  FILLER                          PIC X(05) VALUE SPACES.
005240     05  FILLER                          PIC X(14) VALUE
005250                                                 '--------------'.
005260     05  FILLER                          PIC X(02) VALUE SPACES.
005270     05  FILLER                          PIC X(11) VALUE
005280                                                    '-----------'.
005290     05  FILLER                          PIC X(05) VALUE SPACES.
005300     05  FILLER                          PIC X(11) VALUE
005310                                                    '           '.
005320
005330 01  HEAD-COMM-2.
005340     05  FILLER                          PIC X(132) VALUE ALL '-'.
005350
005360 01  HEAD-COMM-3.
005370     05  FILLER                          PIC X(30) VALUE SPACES.
005380     05  FILLER                          PIC X(54) VALUE
005390         '***** THERE WAS NO DATA TO PRINT FOR THIS REPORT *****'.
005400
005410****************************************************************
005420*    OUTPUT REPORT LINE FORMAT                                 *
005430****************************************************************
005440
005450 01  DETAIL-LINE-1.
005460     05  FILLER                          PIC X(04) VALUE SPACES.
005470     05  DET-1-INVOICE                   PIC X(11).
005480     05  FILLER                          PIC X(07) VALUE SPACES.
005490     05  DET-1-LINE-NO                   PIC ZZZZ.
005500     05  FILLER                          PIC X(04) VALUE SPACES.
005510     05  DET-1-MODE-NO                   PIC X(12).
005520     05  FILLER                          PIC X(05) VALUE SPACES.
005530     05  DET-1-SER-NO                    PIC X(17).
005540     05  FILLER                          PIC X(05) VALUE SPACES.
005550     05  DET-1-AMOUNT-ED                 PIC ZZZ,ZZZ,ZZZ.99-.
005560     05  FILLER                          PIC X(06) VALUE SPACES.
005570     05  DET-1-TYPE                      PIC X(05).
005580     05  FILLER                          PIC X(08) VALUE SPACES.
005590     05  DET-1-BILL-DATE                 PIC X(08).
005600
005610 01  DETAIL-LINE-2.
005620     05  FILLER                          PIC X(04) VALUE SPACES.
005630     05  DET-2-TEXT                      PIC X(39).
005640     05  FILLER                          PIC X(26) VALUE SPACES.
005650     05  DET-2-AMOUNT-ED                 PIC ZZZ,ZZZ,ZZZ.99-.
005660     05  FILLER                          PIC X(50) VALUE SPACES.
005670
005680 01  DETAIL-LINE-TEXT.
005690     05  DET-ENGL-1                      PIC X(32) VALUE
005700                               'PENDING APPLICATION - ON ACCOUNT'.
005710     05  DET-ENGL-2                      PIC X(21) VALUE
005720                                          'CHECK TO BE ISSUED   '.
005730     05  DET-ENGL-3                      PIC X(21) VALUE
005740                                          'MISCELLANEOUS        '.
005750     05  DET-ENGL-4                      PIC X(21) VALUE
005760                                          'FINANCE CHARGE       '.
005770     05  DET-ENGL-5                      PIC X(21) VALUE
005780                                          'CREDIT MEMO TOTAL    '.
005790     05  DET-ENGL-6                      PIC X(21) VALUE
005800                                          'GRAND TOTAL CREDITS  '.
005810     05  DET-FRAN-1                      PIC X(39) VALUE
005820                        'SOMME APPLICABLE AU COMPTE - EN SUSPENS'.
005830     05  DET-FRAN-2                      PIC X(21) VALUE
005840                                          'CHEQUE A EMETTRE     '.
005850     05  DET-FRAN-3                      PIC X(21) VALUE
005860                                          'DIVERS               '.
005870     05  DET-FRAN-4                      PIC X(21) VALUE
005880                                          'FRAIS DE FINANCEMENT '.
005890     05  DET-FRAN-5                      PIC X(21) VALUE
005900                                          'TOTAL NOTE DE CREDIT '.
005910     05  DET-FRAN-6                      PIC X(25) VALUE
005920                                        'GRAND TOTAL DES CREDITS'.
005930
005940 01  TOTAL-LINE.
005950     05  FILLER                          PIC X(04) VALUE SPACES.
005960     05  TOT-1-TEXT                      PIC X(25).
005970     05  FILLER                          PIC X(40) VALUE SPACES.
005980     05  TOT-1-AMOUNT-ED                 PIC ZZZ,ZZZ,ZZZ.99-.
005990     05  FILLER                          PIC X(06) VALUE SPACES.
006000
006010 01  TOTAL-LINE-UND.
006020     05  FILLER                          PIC X(04) VALUE SPACES.
006030     05  FILLER                          PIC X(21).
006040     05  FILLER                          PIC X(44) VALUE SPACES.
006050     05  FILLER                          PIC X(14) VALUE ALL '-'.
006060     05  FILLER                          PIC X(06) VALUE SPACES.
006070
006080
006090 PROCEDURE DIVISION.
006110****************************************************************
006120*    PROGRAM MAIN CONTROL ROUTINE FOR APPLIED CREDIT STATEMENT *
006130****************************************************************
006150 0000-MAINLINE.
006160     PERFORM 1000-INITIALIZATION THRU 1000-EXIT.
006170     IF CRITICAL-ERROR
006180         CONTINUE
006190     ELSE
006200         PERFORM 2000-PROCESS THRU 2000-EXIT
006210             UNTIL INPUT-AT-END
006220     END-IF.
006230
006240     PERFORM 3000-FINALIZATION THRU 3000-EXIT.
006250     GOBACK.
006260 0000-EXIT.
006270     EXIT.
006280
006290****************************************************************
006300*  OPEN FILES, INITIALIZE VARIABLES,                           *
006310*  READ INPUT RECORDS UNTIL A DEALER (RECORD CODE = 0001)      *
006320*  RECORD IS FOUND.                                            *
006330*  IF THERE IS DATA, SETUP THE FIRST DEALER.                   *
006340****************************************************************
006350
006360 1000-INITIALIZATION.
006370     PERFORM 1500-SELECT-VWMCTUPD THRU 1500-EXIT.
006380     OPEN INPUT  CREDIT-INFILE
006390          OUTPUT REPORT-OUTFILE.
006400
006410     READ CREDIT-INFILE INTO MXAW21-CREDIT-GENERAL-RECORD
006420         AT END MOVE 'Y' TO INPUT-EOF-SW.
006430
006440     IF INPUT-AT-END
006450         MOVE 'Y' TO CRITICAL-ERROR-SW
006460         DISPLAY '                     '
006470         DISPLAY '*********************'
006480         DISPLAY 'THERE WAS NO DATA TO '
006490         DISPLAY 'PRINT FOR THIS REPORT'
006500         DISPLAY '*********************'
006510         DISPLAY '                     '
006520         MOVE '0001' TO HEAD-ENGL-2-BRANCH
006530                        HEAD-FRAN-2-BRANCH
006540         ADD 1 TO PAGE-CNT
006550         WRITE REPORT-REC FROM HEAD-ENGL-1 AFTER ADVANCING PAGE
006560         WRITE REPORT-REC FROM HEAD-ENGL-2
006570         WRITE REPORT-REC FROM HEAD-COMM-3 AFTER ADVANCING 4
006580     ELSE
006590         IF MXAW21-SK-RECORD-TYPE = '00' AND
006600            MXAW21-SK-RECORD-TYPE-SEQ = 01
006610             MOVE MXAW21-SK-DLR-NBR TO CUR-DLR-NBR
006620                                       PRV-DLR-NBR
006630             MOVE MXAW21-SK-CREDIT-MEMO-NBR TO CUR-CREDIT-MEMO-NBR
006640                                               PRV-CREDIT-MEMO-NBR
006650             MOVE MXAW21-SK-DIST-NAME TO CUR-DIST-NAME
006660                                         PRV-DIST-NAME
006670             MOVE MXAW21-SK-LANG-IND TO CUR-LANG-IND
006680                                        PRV-LANG-IND
006690             MOVE MXAW21-0001-DLR-CNTL-ENT TO CUR-BRANCH
006700                                              PRV-BRANCH
006710             ADD 1 TO REC-CNT
006720         ELSE
006730             MOVE 'Y' TO CRITICAL-ERROR-SW
006740             DISPLAY 'INPUT FILE IS NOT SORTED'
006750         END-IF
006760     END-IF.
006770 1000-EXIT.
006780     EXIT.
006790
006800****************************************************************
006810*  READ APPLIED CREDIT STATEMENT INPUT FILE.                   *
006820****************************************************************
006830
006840 1100-READ-INPUT.
006850     IF READ-TYPE-3-SW = 'Y'
006860         MOVE 'N' TO READ-TYPE-3-SW
006870     ELSE
006880         READ CREDIT-INFILE INTO MXAW21-CREDIT-GENERAL-RECORD
006890             AT END MOVE 'Y' TO INPUT-EOF-SW
006900     END-IF.
006910
006920     IF INPUT-AT-END
006930         CONTINUE
006940     ELSE
006950         IF FIRST-CREDIT-MEMO
006960             MOVE MXAW21-SK-DLR-NBR TO CUR-DLR-NBR
006970                                       PRV-DLR-NBR
006980             MOVE MXAW21-SK-LANG-IND TO CUR-LANG-IND
006990                                        PRV-LANG-IND
007000             MOVE MXAW21-SK-CREDIT-MEMO-NBR TO CUR-CREDIT-MEMO-NBR
007010                                               PRV-CREDIT-MEMO-NBR
007020             MOVE MXAW21-SK-DIST-NAME TO CUR-DIST-NAME
007030                                         PRV-DIST-NAME
007040         ELSE
007050             MOVE MXAW21-SK-DLR-NBR TO CUR-DLR-NBR
007060             MOVE MXAW21-SK-LANG-IND TO CUR-LANG-IND
007070             MOVE MXAW21-SK-CREDIT-MEMO-NBR TO CUR-CREDIT-MEMO-NBR
007080             MOVE MXAW21-SK-DIST-NAME TO CUR-DIST-NAME
007090         END-IF
007100         ADD 1 TO REC-CNT
007110         EVALUATE MXAW21-SK-RECORD-TYPE ALSO
007120                  MXAW21-SK-RECORD-TYPE-SEQ
007130             WHEN '00' ALSO 01
007140                 MOVE MXAW21-0001-DLR-CNTL-ENT TO CUR-BRANCH
007150                 GO TO 1100-READ-INPUT
007160
007170             WHEN '00' ALSO 02
007180                 MOVE MXAW21-0002-ADDL-CUST-LIT TO
007190                      CUR-ADDL-CUST-LIT
007200                 MOVE MXAW21-0002-ADDL-CUST-NO TO CUR-ADDL-CUST-NO
007210                 GO TO 1100-READ-INPUT
007220
007230             WHEN '00' ALSO 03
007240                 MOVE MXAW21-0003-DLR-REP-NAME  TO CUR-REP-NAME
007250                 MOVE MXAW21-0003-DLR-REP-PHONE TO CUR-REP-PHONE
007260                 GO TO 1100-READ-INPUT
007270
007280             WHEN '00' ALSO 04
007290                 MOVE MXAW21-0004-DLR-NAME-ADDR
007300                   TO CUR-DLR-NAME
007310                 GO TO 1100-READ-INPUT
007320
007330             WHEN '00' ALSO 05
007340                 MOVE MXAW21-0004-DLR-NAME-ADDR
007350                   TO CUR-DLR-ADDRESS1
007360                 GO TO 1100-READ-INPUT
007370
007380             WHEN '00' ALSO 06
007390                 MOVE MXAW21-0004-DLR-NAME-ADDR
007400                   TO CUR-DLR-ADDRESS2
007410                 GO TO 1100-READ-INPUT
007420
007430             WHEN '00' ALSO 07
007440                 MOVE MXAW21-0004-DLR-NAME-ADDR
007450                   TO CUR-DLR-ADDRESS3
007460                 GO TO 1100-READ-INPUT
007470
007480             WHEN '00' ALSO 08
007490                 MOVE MXAW21-0004-DLR-NAME-ADDR
007500                   TO CUR-DLR-ADDRESS4
007510                 GO TO 1100-READ-INPUT
007520
007530             WHEN '00' ALSO 09
007540                 MOVE MXAW21-0004-DLR-NAME-ADDR
007550                   TO CUR-DLR-ADDRESS5
007560                 GO TO 1100-READ-INPUT
007570
007580             WHEN '01' ALSO 01
007590                 MOVE MXAW21-0101-DIST-NAME TO CUR-DIST-NAME
007600
007610             WHEN '01' ALSO 03
007620                 MOVE MXAW21-0103-APPLIED-DATE TO CUR-APPLIED-DATE
007630                 MOVE MXAW21-0103-CREDIT-NBR TO
007640                      CUR-CREDIT-MEMO-NBR
007650                 IF FIRST-CREDIT-MEMO
007660                     MOVE SPACES TO FIRST-CREDIT-MEMO-SW
007670                                    PRV-AREA-1
007680                 END-IF
007690       END-EVALUATE.
007700 1100-EXIT.
007710     EXIT.
007720
007730****************************************************************
007740*  PRINT HEADER AT TOP OF PAGE.                                *
007750****************************************************************
007760
007770 1110-PAGE-BREAK.
007780     IF NOT FIRST-CREDIT-MEMO
007790         MOVE 'Y' TO FIRST-MEMO-DONE-SW
007800         IF LINE-CNT > 50
007810             IF CUR-LANG-IND = 'USAENG' OR 'CANENG'
007820                 PERFORM 1200-PRINT-ENGL-HEADER THRU 1200-EXIT
007830             ELSE
007850                 PERFORM 1300-PRINT-FRAN-HEADER THRU 1300-EXIT
007870             END-IF
007880             SET HEADINGS-WERE-PRINTED TO TRUE
007890         ELSE
007900             PERFORM 1210-FORMAT-HEADER THRU 1210-EXIT
007910         END-IF
007920     END-IF.
007950
007960     IF CUR-AREA-1 NOT = PRV-AREA-1 OR
007970        HEADINGS-WERE-PRINTED
008000
008010         IF CUR-LANG-IND = 'USAENG' OR 'CANENG'
008020             WRITE REPORT-REC FROM HEAD-ENGL-9
008030                 AFTER ADVANCING 2 LINES
008040             WRITE REPORT-REC FROM HEAD-ENGL-10
008050                 AFTER ADVANCING 2 LINES
008060             WRITE REPORT-REC FROM HEAD-ENGL-11
008070             WRITE REPORT-REC FROM HEAD-COMM-1
008080         ELSE
008100             WRITE REPORT-REC FROM HEAD-FRAN-9
008110                 AFTER ADVANCING 2 LINES
008120             WRITE REPORT-REC FROM HEAD-FRAN-10
008130                 AFTER ADVANCING 2 LINES
008140             WRITE REPORT-REC FROM HEAD-FRAN-11
008150             WRITE REPORT-REC FROM HEAD-COMM-1
008170         END-IF
008180         ADD 6 TO LINE-CNT
008190         MOVE SPACES TO PAGE-BREAK-SW
008200     END-IF.
008210 1110-EXIT.
008220     EXIT.
008230
008240****************************************************************
008250*  PRINT HEADER AT TOP OF PAGE.                                *
008260****************************************************************
008270
008280 1200-PRINT-ENGL-HEADER.
008290     PERFORM 1210-FORMAT-HEADER THRU 1210-EXIT.
008300     IF LINE-CNT > 55
008310         ADD 1 TO PAGE-CNT
008320         WRITE REPORT-REC FROM HEAD-ENGL-1
008330             AFTER ADVANCING PAGE
008340         WRITE REPORT-REC FROM HEAD-ENGL-2
008350         WRITE REPORT-REC FROM HEAD-ENGL-3
008360             AFTER ADVANCING 2 LINES
008370         WRITE REPORT-REC FROM HEAD-ENGL-3A
008380         WRITE REPORT-REC FROM HEAD-ENGL-4
008390             AFTER ADVANCING 2 LINES
008400         WRITE REPORT-REC FROM HEAD-ENGL-5
008410         WRITE REPORT-REC FROM HEAD-ENGL-6
008420         WRITE REPORT-REC FROM HEAD-ENGL-7A
008430         WRITE REPORT-REC FROM HEAD-ENGL-7B
008440         WRITE REPORT-REC FROM HEAD-ENGL-7C
008450         WRITE REPORT-REC FROM HEAD-ENGL-8
008460             AFTER ADVANCING 2 LINES
008470         MOVE 16 TO LINE-CNT
008480     END-IF.
008490 1200-EXIT.
008500     EXIT.
008510
008520****************************************************************
008530*  FORMAT PAGE HEADER.                                         *
008540****************************************************************
008550
008560 1210-FORMAT-HEADER.
008570     MOVE PAGE-CNT TO HEAD-ENGL-2-PAGE
008580                      HEAD-FRAN-2-PAGE.
008590
008600     MOVE CUR-BRANCH TO HEAD-ENGL-2-BRANCH
008610                        HEAD-FRAN-2-BRANCH.
008620
008630     MOVE CUR-DLR-NBR TO HEAD-ENGL-3-CUST
008640                         HEAD-FRAN-3-CUST.
008650
008660     MOVE CUR-ADDL-CUST-LIT TO HE-3A-ADDL-CUST-LIT.
008670     MOVE CUR-ADDL-CUST-NO TO HE-3A-ADDL-CUST-NO.
008680     MOVE CUR-REP-NAME TO HEAD-ENGL-8-CUST
008690                          HEAD-FRAN-8-CUST.
008700
008710     MOVE CUR-REP-PHONE TO HEAD-ENGL-8-PHONE
008720                           HEAD-FRAN-8-PHONE.
008730
008740     MOVE CUR-DLR-NAME TO HEAD-ENGL-4-DEAL-NAME
008750                          HEAD-FRAN-4-DEAL-NAME.
008760
008770     MOVE CUR-DLR-ADDRESS1 TO HEAD-ENGL-5-DEAL-ADDR1
008780                              HEAD-FRAN-5-DEAL-ADDR1.
008790
008800     MOVE CUR-DLR-ADDRESS2 TO HEAD-ENGL-6-DEAL-ADDR2
008810                              HEAD-FRAN-6-DEAL-ADDR2.
008820
008830     MOVE CUR-DLR-ADDRESS3 TO HEAD-ENGL-7-DEAL-ADDR3
008840                              HEAD-FRAN-7-DEAL-ADDR3.
008850
008860     MOVE CUR-DLR-ADDRESS4 TO HEAD-ENGL-7-DEAL-ADDR4
008870                              HEAD-FRAN-7-DEAL-ADDR4.
008880
008890     MOVE CUR-DLR-ADDRESS5 TO HEAD-ENGL-7-DEAL-ADDR5
008900                              HEAD-FRAN-7-DEAL-ADDR5.
008910
008920     MOVE CUR-DIST-NAME TO HEAD-ENGL-9-DISNAME
008930                           HEAD-FRAN-9-DISNAME.
008940
008950     MOVE CUR-CREDIT-MEMO-NBR TO HEAD-ENGL-9-CREDNO
008960                                 HEAD-FRAN-9-CREDNO.
008970
008980     MOVE CUR-APPLIED-DATE(3:2) TO HEAD-ENGL-9-APDATE(7:2)
008990                                   HEAD-FRAN-9-APDATE(7:2).
009000
009010     MOVE '/' TO HEAD-ENGL-9-APDATE(6:1)
009020                 HEAD-FRAN-9-APDATE(6:1).
009030
009040     MOVE CUR-APPLIED-DATE(6:2) TO HEAD-ENGL-9-APDATE(1:2)
009050                                   HEAD-FRAN-9-APDATE(1:2).
009060
009070     MOVE '/' TO HEAD-ENGL-9-APDATE(3:1)
009080                 HEAD-FRAN-9-APDATE(3:1).
009090
009100     MOVE CUR-APPLIED-DATE(9:2) TO HEAD-ENGL-9-APDATE(4:2)
009110                                   HEAD-FRAN-9-APDATE(4:2).
009120 1210-EXIT.
009130     EXIT.
009140
009150****************************************************************
009160*  PRINT HEADER AT TOP OF PAGE.                                *
009170****************************************************************
009180
009190 1300-PRINT-FRAN-HEADER.
009200     PERFORM 1210-FORMAT-HEADER THRU 1210-EXIT.
009210     IF LINE-CNT > 55
009220         ADD 1 TO PAGE-CNT
009230         WRITE REPORT-REC FROM HEAD-FRAN-1
009240             AFTER ADVANCING PAGE
009250         WRITE REPORT-REC FROM HEAD-FRAN-2
009260         WRITE REPORT-REC FROM HEAD-FRAN-3
009270             AFTER ADVANCING 2 LINES
009280         WRITE REPORT-REC FROM HEAD-FRAN-4
009290             AFTER ADVANCING 2 LINES
009300         WRITE REPORT-REC FROM HEAD-FRAN-5
009310         WRITE REPORT-REC FROM HEAD-FRAN-6
009320         WRITE REPORT-REC FROM HEAD-FRAN-7A
009330         WRITE REPORT-REC FROM HEAD-FRAN-7B
009340         WRITE REPORT-REC FROM HEAD-FRAN-7C
009350         WRITE REPORT-REC FROM HEAD-FRAN-8
009360             AFTER ADVANCING 2 LINES
009370         MOVE 15 TO LINE-CNT
009380     END-IF.
009390 1300-EXIT.
009400     EXIT.
009410
009420*****************************************************************
009430* PERFORMED FROM 1000-INITIALIZATION, THIS PARAGRAPH IS EXECUTED*
009440* TWICE.  ONCE TO RETRIEVE THE CURRENT PROCESSING DATE AND A    *
009450* SECOND TIME TO RETRIEVE THE DATE RANGE THAT THIS PROGRAM WILL *
009460* USE FOR SELECTION CRITERIA.                                   *
009470*****************************************************************
009480
009490 1500-SELECT-VWMCTUPD.
009500     MOVE SPACES TO SUBSYSTEM-ID-IND
009510                     SUBFUNCTION-CODE.
009520
009530     EXEC SQL
009540       SELECT PROC_DATE
009550         INTO :DCLVWMCTUPD.PROC-DATE
009560         FROM VWMCTUPD
009570         WHERE SUBSYSTEM_ID_IND = :SUBSYSTEM-ID-IND
009580           AND SUBFUNCTION_CODE = :SUBFUNCTION-CODE
009590     END-EXEC.
009600
009610     PERFORM Z-970-SET-DA-STATUS-DB2 THRU
009620             Z-970-SET-DA-STATUS-DB2-EXIT.
009630
009640     IF DA-OK
009650         MOVE PROC-DATE (3:2) TO HEAD-ENGL-1-DATE(7:2)
009660                                 HEAD-FRAN-1-DATE(7:2)
009670         MOVE '/'             TO HEAD-ENGL-1-DATE(6:1)
009680                                 HEAD-FRAN-1-DATE(6:1)
009690         MOVE PROC-DATE (6:2) TO HEAD-ENGL-1-DATE(1:2)
009700                                 HEAD-FRAN-1-DATE(1:2)
009710         MOVE '/'             TO HEAD-ENGL-1-DATE(3:1)
009720                                 HEAD-FRAN-1-DATE(3:1)
009730         MOVE PROC-DATE (9:2) TO HEAD-ENGL-1-DATE(4:2)
009740                                 HEAD-FRAN-1-DATE(4:2)
009750     ELSE
009760         SET ABT-DO-ABEND     TO TRUE
009770         SET ABT-ERROR-IS-DB2 TO TRUE
009780         MOVE 'SELECT  '      TO ABT-DA-FUNCTION
009790         MOVE '1500-SELE'     TO ABT-ERROR-SECTION
009800         MOVE 3600            TO ABT-ERROR-ABEND-CODE
009810         MOVE 'VWMCTUPD'      TO ABT-DA-ACCESS-NAME
009820         PERFORM Z-980-ABNORMAL-TERM THRU
009830                 Z-980-ABNORMAL-TERM-RETURN
009840     END-IF.
009850 1500-EXIT.
009860     EXIT.
009870
009880****************************************************************
009890*  ROUTINE PROCESSES THE DATA RECORDS FROM THE EXTRACT PROGRAM *
009900*  TO CREATE THE REPORT DATA RECORDS.                          *
009910****************************************************************
009920
009930 2000-PROCESS.
009940     EVALUATE TRUE
009950         WHEN CUR-DLR-NBR NOT = PRV-DLR-NBR
009960             PERFORM 2100-MEMO-BREAK THRU 2100-EXIT
009970             PERFORM 2200-DEALER-BREAK THRU 2200-EXIT
009980
009990         WHEN CUR-AREA-1 NOT = PRV-AREA-1
010000             IF FIRST-MEMO-DONE
010010                 PERFORM 2100-MEMO-BREAK THRU 2100-EXIT
010020             END-IF
010030     END-EVALUATE.
010040
010050     PERFORM 2300-FORMAT-DETAIL THRU 2300-EXIT.
010060     MOVE CUR-AREA TO PRV-AREA.
010070     PERFORM 1100-READ-INPUT THRU 1100-EXIT.
010080 2000-EXIT.
010090     EXIT.
010100
010110****************************************************************
010120*  THIS ROUTINE PERFORMS CONTROL BREAK OPERATIONS EVERY TIME   *
010130*  WHEN DISTRIBUTOR NAME OR MEMO NUMBER IS CHANGED.            *
010140****************************************************************
010150
010160 2100-MEMO-BREAK.
010170     WRITE REPORT-REC FROM TOTAL-LINE-UND
010180         AFTER ADVANCING 2 LINES.
010190
010200     ADD 1 TO LINE-CNT.
010210     MOVE TOT-CREDIT-MEMO TO TOT-1-AMOUNT.
010220     MOVE TOT-1-AMOUNT TO TOT-1-AMOUNT-ED.
010230
010240     IF PRV-LANG-IND = 'USAENG' OR 'CANENG'
010250         MOVE DET-ENGL-5 TO TOT-1-TEXT
010260         WRITE REPORT-REC FROM TOTAL-LINE
010270         ADD 1 TO LINE-CNT
010280     ELSE
010300         MOVE TOT-CREDIT-MEMO TO TOT-1-AMOUNT
010310         MOVE TOT-1-AMOUNT TO TOT-1-AMOUNT-ED
010320         MOVE DET-FRAN-5 TO TOT-1-TEXT
010330         WRITE REPORT-REC FROM TOTAL-LINE
010340         ADD 1 TO LINE-CNT
010360     END-IF.
010370
010380     ADD TOT-CREDIT-MEMO TO TOT-CREDIT-DLR.
010390     INITIALIZE TOT-CREDIT-MEMO
010400                WS-CASH-AMT
010410                WS-TRANSFER-AMT
010420                WS-MISC-AMT
010430                WS-CHARGE-AMT.
010440
010450     WRITE REPORT-REC FROM HEAD-COMM-2
010460         AFTER ADVANCING 2 LINES.
010470
010480     ADD 1 TO LINE-CNT.
010490 2100-EXIT.
010500     EXIT.
010510
010520****************************************************************
010530*  THIS ROUTINE PERFORMS CONTROL BREAK OPERATIONS EVERY TIME   *
010540*  WHEN DEALER NUMBER IS CHANGED.                              *
010550****************************************************************
010560
010570 2200-DEALER-BREAK.
010580     IF PRV-LANG-IND = 'USAENG' OR 'CANENG'
010590         MOVE TOT-CREDIT-DLR TO TOT-1-AMOUNT
010600         MOVE TOT-1-AMOUNT TO TOT-1-AMOUNT-ED
010610         MOVE DET-ENGL-6 TO TOT-1-TEXT
010620         WRITE REPORT-REC FROM TOTAL-LINE
010630     ELSE
010650         MOVE TOT-CREDIT-DLR TO TOT-1-AMOUNT
010660         MOVE TOT-1-AMOUNT TO TOT-1-AMOUNT-ED
010670         MOVE DET-FRAN-6 TO TOT-1-TEXT
010680         WRITE REPORT-REC FROM TOTAL-LINE
010700     END-IF.
010710
010720     MOVE 60 TO LINE-CNT.
010730     MOVE 0 TO TOT-CREDIT-DLR.
010740 2200-EXIT.
010750     EXIT.
010760
010770****************************************************************
010780*  FORMATS DETAIL LINE AND CALLS PRINT ROUTINE.                *
010790****************************************************************
010800
010810 2300-FORMAT-DETAIL.
010820     PERFORM 1110-PAGE-BREAK THRU 1110-EXIT.
010830     EVALUATE MXAW21-SK-RECORD-TYPE ALSO
010840              MXAW21-SK-RECORD-TYPE-SEQ
010850         WHEN '02' ALSO ANY
010860             MOVE SPACES TO FIRST-CREDIT-MEMO-SW
010870             INITIALIZE DETAIL-LINE-1
010880             ADD MXAW21-0200-APPLIED-AMT TO TOT-CREDIT-MEMO
010890             MOVE MXAW21-0200-INVOICE-NBR TO DET-1-INVOICE
010900             MOVE MXAW21-0200-LINE-NBR TO WS-DET-LINE-NO
010910             MOVE WS-DET-LINE-NO TO DET-1-LINE-NO
010920             MOVE MXAW21-0200-MODEL-NBR TO DET-1-MODE-NO
010930             MOVE MXAW21-0200-SERIAL-NBR TO DET-1-SER-NO
010940             MOVE MXAW21-0200-APPLIED-AMT TO DET-1-AMOUNT
010950             MOVE DET-1-AMOUNT TO DET-1-AMOUNT-ED
010960             WRITE REPORT-REC FROM DETAIL-LINE-1
010970                 AFTER ADVANCING 1 LINE
010980             ADD 1 TO LINE-CNT
010990
011000         WHEN '03' ALSO ANY
011010             MOVE SPACES TO FIRST-CREDIT-MEMO-SW
011020             INITIALIZE DETAIL-LINE-1
011030             ADD MXAW21-0300-APPLIED-AMT TO TOT-CREDIT-MEMO
011040             MOVE MXAW21-0300-BILL-DATE TO WS-SAVE-DATE
011050             MOVE MXAW21-0300-CHARGE-TYPE TO DET-1-TYPE
011060             MOVE MXAW21-0300-BILL-DATE(3:2) TO DET-1-INVOICE(7:2)
011070             MOVE '/' TO DET-1-INVOICE(6:1)
011080             MOVE MXAW21-0300-BILL-DATE(6:2) TO DET-1-INVOICE(1:2)
011090             MOVE '/' TO DET-1-INVOICE(3:1)
011100             MOVE MXAW21-0300-BILL-DATE(9:2) TO DET-1-INVOICE(4:2)
011110             MOVE MXAW21-0300-APPLIED-AMT TO DET-1-AMOUNT
011120             PERFORM 2301-ACCUMULATE-CHARGES THRU 2301-EXIT
011130             MOVE DET-1-AMOUNT TO DET-1-AMOUNT-ED
011140             WRITE REPORT-REC FROM DETAIL-LINE-1
011150             ADD 1 TO LINE-CNT
011160
011170         WHEN '04' ALSO 01
011180             ADD MXAW21-0400-APPLIED-AMT TO TOT-CREDIT-MEMO
011190                                            WS-CASH-AMT
011200             INITIALIZE DETAIL-LINE-2
011210             IF WS-CASH-AMT NOT = 0
011220                 MOVE WS-CASH-AMT TO DET-2-AMOUNT
011230                 MOVE DET-2-AMOUNT TO DET-2-AMOUNT-ED
011240                 IF PRV-LANG-IND = 'USAENG' OR 'CANENG'
011250                     MOVE DET-ENGL-1 TO DET-2-TEXT
011260                 ELSE
011280                     MOVE DET-FRAN-1 TO DET-2-TEXT
011300                 END-IF
011310                 WRITE REPORT-REC FROM DETAIL-LINE-2
011320                 ADD 1 TO LINE-CNT
011330             END-IF
011340
011350         WHEN '04' ALSO 02
011360             ADD MXAW21-0400-APPLIED-AMT TO TOT-CREDIT-MEMO
011370                                            WS-TRANSFER-AMT
011380             INITIALIZE DETAIL-LINE-2
011390             IF WS-TRANSFER-AMT NOT = 0
011400                 MOVE WS-TRANSFER-AMT TO DET-2-AMOUNT
011410                 MOVE DET-2-AMOUNT TO DET-2-AMOUNT-ED
011420                 IF PRV-LANG-IND = 'USAENG' OR 'CANENG'
011430                     MOVE DET-ENGL-2 TO DET-2-TEXT
011440                 ELSE
011460                     MOVE DET-FRAN-2 TO DET-2-TEXT
011480                 END-IF
011490                 WRITE REPORT-REC FROM DETAIL-LINE-2
011500                 ADD 1 TO LINE-CNT
011510             END-IF
011520
011530         WHEN '04' ALSO 03
011540             ADD MXAW21-0400-APPLIED-AMT TO TOT-CREDIT-MEMO
011550                                            WS-MISC-AMT
011560             INITIALIZE DETAIL-LINE-2
011570             IF WS-MISC-AMT NOT = 0
011580                 MOVE WS-MISC-AMT TO DET-2-AMOUNT
011590                 MOVE DET-2-AMOUNT TO DET-2-AMOUNT-ED
011600                 IF PRV-LANG-IND = 'USAENG' OR 'CANENG'
011610                     MOVE DET-ENGL-3 TO DET-2-TEXT
011620                 ELSE
011640                     MOVE DET-FRAN-3 TO DET-2-TEXT
011660                 END-IF
011670                 WRITE REPORT-REC FROM DETAIL-LINE-2
011680                 ADD 1 TO LINE-CNT
011690             END-IF
011700
011710         WHEN '04' ALSO 04
011720             ADD MXAW21-0400-APPLIED-AMT TO TOT-CREDIT-MEMO
011730                                            WS-CHARGE-AMT
011740             INITIALIZE DETAIL-LINE-2
011750             IF WS-CHARGE-AMT NOT = 0
011760                 MOVE WS-CHARGE-AMT TO DET-2-AMOUNT
011770                 MOVE DET-2-AMOUNT TO DET-2-AMOUNT-ED
011780                 IF PRV-LANG-IND = 'USAENG' OR 'CANENG'
011790                     MOVE DET-ENGL-4 TO DET-2-TEXT
011800                 ELSE
011820                     MOVE DET-FRAN-4 TO DET-2-TEXT
011840                 END-IF
011850                 WRITE REPORT-REC FROM DETAIL-LINE-2
011860                 ADD 1 TO LINE-CNT
011870             END-IF
011880     END-EVALUATE.
011890 2300-EXIT.
011900     EXIT.
011910
011920****************************************************************
011930*  ACCUMULATE CHARGES BY BILL DATE AND CHARGE TYPE.            *
011940****************************************************************
011950
011960 2301-ACCUMULATE-CHARGES.
011970     MOVE 'Y' TO READ-TYPE-3-SW.
011980     READ CREDIT-INFILE INTO MXAW21-CREDIT-GENERAL-RECORD
011990         AT END MOVE 'Y' TO INPUT-EOF-SW.
012000
012010     IF INPUT-AT-END
012020         GO TO 2301-EXIT
012030     END-IF.
012040
012050     IF MXAW21-SK-RECORD-TYPE = '03'
012060         NEXT SENTENCE
012070     ELSE
012080         GO TO 2301-EXIT
012090     END-IF.
012100
012110
012120     IF MXAW21-0300-BILL-DATE   = WS-SAVE-DATE AND
012130        MXAW21-0300-CHARGE-TYPE = DET-1-TYPE
012140        ADD MXAW21-0300-APPLIED-AMT TO TOT-CREDIT-MEMO
012150                                       DET-1-AMOUNT
012160        GO TO 2301-ACCUMULATE-CHARGES
012170     END-IF.
012180 2301-EXIT.
012190     EXIT.
012200
012210****************************************************************
012220*  WRITE OUT TOTALS FOR LAST CREDIT MEMO AND DEALER AND CLOSE  *
012230*  FILES.                                                      *
012240****************************************************************
012250
012260 3000-FINALIZATION.
012270     IF CRITICAL-ERROR
012280         CONTINUE
012290     ELSE
012300         PERFORM 2100-MEMO-BREAK THRU 2100-EXIT
012310         PERFORM 2200-DEALER-BREAK THRU 2200-EXIT
012320     END-IF.
012330
012340     CLOSE CREDIT-INFILE
012350           REPORT-OUTFILE.
012360 3000-EXIT.
012370     EXIT.
012380
012390     EXEC SQL
012400          INCLUDE MXWP02
012410     END-EXEC.