//A1$DA045  PROC  SPACE1=,
//         LOADHLQ=SYSPL,
//         SYSTM=RISK,
//         CSTAGE=,
//         TSTAGE=,
//         PSTAGE=,
//         LOADLIB=LOADMOD,
//         LOADLIB1=LOADMOD,
//         PRGN=GEP,
//         TSTPRD=.,
//         POLE=P,
//         DB2S=DP4G,
//         TSTPRD1=,
//         DSLEV=PRDA1,
//         RGN=PP,
//         PARMLIB=SYSPL.CTLLIB,
//         HRGLASS1=SYS1.SCEERUN,
//         HRGLASS2=SYS1.SCEERUN
//*********************************************************************
//*           "DAILY" STATEMENT OF CREDIT JOB                         *
//*                                                                   *
//*     THIS JOB HAS 7 STEPS FOR THE CASH SUBSYSTEM                   *
//*                                                                   *
//*      1) DELETE WORK DATASETS.                                     *
//*                                                                   *
//*      2) CREATE THE STATEMENT OF CREDIT EXTRACT FILE.              *
//*                                                                   *
//*      3) THIS SORT STEP CREATES TWO SORTED DATA SETS.  ONE FOR     *
//*         STATEMENT OF CREDIT (XEROX PRINTER) AND ONE FOR RMS.      *
//*                                                                   *
//*      4) CREATES THE XEROX STATEMENT OF CREDIT.  XEROX OUTPUT      *
//*         GOES DIRECTLY TO SYSOUT                                   *
//*                                                                   *
//*      5) CREATES RMS CREDIT REPORTS.                               *
//*                                                                   *
//*      6) GEN THE OUTPUT FROM STEP ONE TO TAPE.                     *
//*                                                                   *
//*      7) DELETE WORK DATASETS.                                     *
//*                                                                   *
//*********************************************************************
//*********************************************************************
//* DELETE WORK DATASET                                               *
//* STEP DELETED FOR CA-11 - SMC 08/21/01                             *
//* STEP PUT BACK IN ACTION  SMC 10/09/01                             *
//*  RESTART:  STEP RESTARTABLE WITH NO OVRERRIDES                    *
//*  RESTART:  THIS STEP                                              *
//* DELETE REPORT FILE RPT1 PRODUCED IN EARLIER RUN FOR REVEAL        *
//*  RESTART : IF RPT1 EXISTS. MAKE SURE THAT IT HAS BEEN SENT TO     *
//*          : REVEAL BEFORE PROCEEDING WITH THE DELETE               *
//*********************************************************************
//DA045010 EXEC  PGM=IEFBR14
//B1       DD  DSN=&DSLEV..&RGN..DA045040.SORTED.ALL,
//             DISP=(MOD,DELETE),
//             UNIT=SYSDA,
//             SPACE=&SPACE1
//B2       DD  DSN=&DSLEV..&RGN..DA045020.CREDIT.DATA,
//             DISP=(MOD,DELETE),
//             UNIT=SYSDA,
//             SPACE=&SPACE1
//MXA046O1 DD  DSN=&DSLEV..&RGN..DA045080.RPT1,
//             UNIT=(SYSDA,3),
//             SPACE=(TRK,(100,50),RLSE),
//             DISP=(MOD,DELETE,DELETE),
//             DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)
//*
//*********************************************************************
//*  CREATE THE STATEMENT OF CREDIT DATA FILES                        *
//*                                                                   *
//*  RESTART:  RESTART AT TOP OF JOB WITH NO OVERRIDES                *
//*                                                                   *
//*  -911:  RESTART AT TOP OF JOB WITH NO OVERRIDES                   *
//*  RESTART : THIS STEP (DA045010 IF WITHOUT CA-11)                  *
//*  RESTART : IF NOT UNDER CA-11 CONTROL, RUN BR14 STEP!             *
//*********************************************************************
//DA045020 EXEC  PGM=MXBPA045
//STEPLIB  INCLUDE MEMBER=A1$$STEP
//MXPA045I DD  DSN=&PARMLIB.(A1&POLE.03152),
//             DISP=SHR
//MXPA045T DD  DSN=&DSLEV..&RGN..DA045020.CREDIT.DATA,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=(SYSDA,5),
//             SPACE=&SPACE1,
//**** P0516718 S
//*            DCB=(LRECL=140,BLKSIZE=0,RECFM=FB)
//**CLJB-2323-S,CLJB-2333-S
//*            DCB=(LRECL=162,BLKSIZE=0,RECFM=FB),
//             DCB=(LRECL=359,BLKSIZE=0,RECFM=FB),
//**CLJB-2323-E,CLJB-2333-E
//**** P0516718 E
//             DATACLAS=COMPZEDC
//SYSUDUMP DD  SYSOUT=D
//SYSOUT   DD  SYSOUT=*
//*
//*********************************************************************
//*  SORT THE CREDIT EXTRACT FILE IN KEY SEQUENCE.                    *
//*                                                                   *
//*  RESTART:  STEP RESTARTABLE WITH NO OVRERRIDES                    *
//*  RESTART: THIS STEP                                               *
//*********************************************************************
//DA045040 EXEC  PGM=SORT,COND=(5,LE)
//SYSIN    DD  DSN=&PARMLIB.(A1&POLE.03153),
//             DISP=SHR
//SORTIN   DD  DSN=&DSLEV..&RGN..DA045020.CREDIT.DATA,
//             DISP=SHR
//SORTOUT  DD  DSN=&DSLEV..&RGN..DA045040.SORTED.ALL,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=(SYSDA,5),
//             SPACE=&SPACE1,
//**** P0516718 S
//*            DCB=(LRECL=140,BLKSIZE=0,RECFM=FB)
//**CLJB-2323-S,CLJB-2333-S
//*            DCB=(LRECL=162,BLKSIZE=0,RECFM=FB),
//             DCB=(LRECL=359,BLKSIZE=0,RECFM=FB),
//**CLJB-2323-E,CLJB-2333-E
//**** P0516718 E
//             DATACLAS=COMPZEDC
//SORTWK01 DD  UNIT=SYSDA,SPACE=(TRK,(1500,150),RLSE)
//SORTWK02 DD  UNIT=SYSDA,SPACE=(TRK,(1500,150),RLSE)
//SORTWK03 DD  UNIT=SYSDA,SPACE=(TRK,(1500,150),RLSE)
//SORTWK04 DD  UNIT=SYSDA,SPACE=(TRK,(1500,150),RLSE)
//$ORTPARM DD  DSN=&PARMLIB.(A1&POLE.03147),DISP=SHR
//SYSOUT   DD  SYSOUT=*
//*
//*********************************************************************
//*  CREATE STATEMENT OF CREDIT                                       *
//*                                                                   *
//*  RESTART:  STEP RESTARTABLE WITH NO OVRERRIDES                    *
//*            DELETE ANY OUTPUT FROM THIS STEP THAT IS IN SDSF       *
//*  RESTART : THIS STEP                                              *
//*********************************************************************
//DA045060 EXEC  PGM=MXBPA047,REGION=7M,COND=(7,LE)
//STEPLIB  INCLUDE MEMBER=A1$$STEP
//MXA047I1 DD  DSN=&DSLEV..&RGN..DA045040.SORTED.ALL,
//             DISP=SHR
//MXA047I2 DD  DUMMY
//MXA047X1 DD  DSN=&DSLEV..&RGN..DA045060.XEROX(+1),
//             UNIT=(SYSDA,5),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(150,75),RLSE),
//             DCB=(LRECL=297,BLKSIZE=0,RECFM=VB),
//             DATACLAS=COMPZEDC
//MXPB510T DD  DSN=&DSLEV..&RGN..DA045060.CNTL(+1),
//             UNIT=(SYSDA,5),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(5,2),RLSE),
//             DCB=(LRECL=80,BLKSIZE=0,RECFM=FB),
//             DATACLAS=COMPZEDC
//SYSUDUMP DD  SYSOUT=D
//SYSOUT   DD  SYSOUT=*
//*
//****************************************************************
//**  STEPNAME   : TAPEBK01                                     **
//**  CRNO       : CR02667512                                   **
//**  INPUT      : &DSLEV..&RGN..DA045060.CNTL(+1)              **
//**  OUTPUT     : &DSLEV..&RGN..DA045060.CNTL.BK1(+1)          **
//****************************************************************
//TAPEBK01 EXEC PGM=SORT,COND=(7,LE)
//SORTIN   DD DISP=SHR,DSN=&DSLEV..&RGN..DA045060.CNTL(+1)
//SORTOUT  DD DSN=&DSLEV..&RGN..DA045060.CNTL.BK1(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=TAPE,RETPD=24,
//            DCB=*.SORTIN
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DSN=&PARMLIB(A1&POLE.03251),DISP=SHR
//*
//****************************************************************
//**  STEPNAME   : TAPEBK02                                     **
//**  CRNO       : CR02667512                                   **
//**  INPUT      : &DSLEV..&RGN..DA045060.XEROX(+1)             **
//**  OUTPUT     : &DSLEV..&RGN..DA045060.XEROX.BK1(+1)         **
//****************************************************************
//TAPEBK02 EXEC PGM=SORT,COND=(7,LE)
//SORTIN   DD DISP=SHR,DSN=&DSLEV..&RGN..DA045060.XEROX(+1)
//SORTOUT  DD DSN=&DSLEV..&RGN..DA045060.XEROX.BK1(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=TAPE,RETPD=24,
//            DCB=*.SORTIN
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DSN=&PARMLIB(A1&POLE.03251),DISP=SHR
//*
//*************************************************************
//* GENER THE DUMMY CONTROL FILE IF NOTHING TO PROCESS        *
//* RESTART:  CHANGE GENERATION VALUE OF CNTL FILE BELOW AND  *
//*           XEROX FILE OF STEP DA045070 TO (+0)'S.          *
//* RESTART : THIS STEP                                       *
//*************************************************************
//DA045065 EXEC PGM=IEBGENER,
//          COND=(5,NE,DA045060)
//SYSPRINT DD  SYSOUT=*
//*SYSUT1   DD  DSN=&DSLEV..&RGN..DUMMY.DA045060.CNTL,
//SYSUT1   DD  DSN=&PARMLIB.(A1&POLE.03154),
//             DISP=SHR
//SYSUT2   DD  DSN=&DSLEV..&RGN..DA045060.CNTL(+1),
//             DISP=MOD
//SYSIN    DD  DUMMY
//SYSUDUMP DD  SYSOUT=D
//*
//****************************************************************
//**  STEPNAME   : TAPEBK03                                     **
//**  CRNO       : CR02667512                                   **
//**  INPUT      : &DSLEV..&RGN..DA045060.CNTL(+1)              **
//**  OUTPUT     : &DSLEV..&RGN..DA045060.CNTL.BKP(+1)          **
//****************************************************************
//TAPEBK03 EXEC PGM=SORT,COND=(5,NE,DA045060)
//SORTIN   DD DISP=SHR,DSN=&DSLEV..&RGN..DA045060.CNTL(+1)
//SORTOUT  DD DSN=&DSLEV..&RGN..DA045060.CNTL.BKP(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=TAPE,RETPD=24,
//            DCB=*.SORTIN
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DSN=&PARMLIB(A1&POLE.03251),DISP=SHR
//*
//*************************************************************
//* GENER THE DUMMY XEROX FILE IF NOTHING TO PROCESS.         *
//* RESTART:  CHANGE GENERATION VALUE OF XEROX FILE TO (+0).  *
//* RESTART : THIS STEP                                       *
//*************************************************************
//DA045070 EXEC PGM=IEBGENER,
//          COND=(5,NE,DA045060)
//SYSPRINT DD  SYSOUT=*
//*SYSUT1   DD  DSN=&DSLEV..&RGN..DUMMY.DA045071.XEROX,
//SYSUT1   DD  DSN=&PARMLIB.(A1&POLE.03154),
//             DISP=SHR
//SYSUT2   DD  DSN=&DSLEV..&RGN..DA045060.XEROX(+1),
//             DISP=MOD
//SYSIN    DD  DUMMY
//SYSUDUMP DD  SYSOUT=D
//*
//****************************************************************
//**  STEPNAME   : TAPEBK04                                     **
//**  CRNO       : CR02667512                                   **
//**  INPUT      : &DSLEV..&RGN..DA045060.XEROX(+1)             **
//**  OUTPUT     : &DSLEV..&RGN..DA045060.XEROX.BKP(+1)         **
//****************************************************************
//TAPEBK04 EXEC PGM=SORT,COND=(5,NE,DA045060)
//SORTIN   DD DISP=SHR,DSN=&DSLEV..&RGN..DA045060.XEROX(+1)
//SORTOUT  DD DSN=&DSLEV..&RGN..DA045060.XEROX.BKP(+1),
//            DISP=(NEW,CATLG,DELETE),UNIT=TAPE,RETPD=24,
//            DCB=*.SORTIN
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DSN=&PARMLIB(A1&POLE.03251),DISP=SHR
//*
//*********************************************************************
//*  CREATE STATEMENT OF CREDIT REPORT (RMS)                          *
//*                                                                   *
//*  RESTART:  STEP RESTARTABLE WITH NO OVRERRIDES                    *
//* RESTART : THIS STEP                                              *
//*********************************************************************
//DA045080 EXEC  PGM=MXBPA046,REGION=7M,COND=(7,LE)
//STEPLIB  INCLUDE MEMBER=A1$$STEP
//MXA046I1 DD  DSN=&DSLEV..&RGN..DA045040.SORTED.ALL,
//             DISP=SHR
//MXA046O1 DD  DSN=&DSLEV..&RGN..DA045080.RPT1,
//             UNIT=(SYSDA,3),
//             SPACE=(TRK,(300,150),RLSE),
//             DISP=(NEW,CATLG,DELETE),
//             DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0),
//             DATACLAS=COMPZEDC
//SYSUDUMP DD  SYSOUT=D
//SYSOUT   DD  SYSOUT=*
//*
//*********************************************************************
//* GENER THE STATEMENT OF CREDIT FILE TO TAPE.                       *
//*                                                                   *
//*  RESTART:  STEP RESTARTABLE WITH NO OVRERRIDES                    *
//* RESTART : THIS STEP                                               *
//*********************************************************************
//DA045100 EXEC  PGM=IEBGENER,COND=(3,LE)
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DSN=&DSLEV..&RGN..DA045020.CREDIT.DATA,
//             DISP=SHR
//SYSUT2   DD  DSN=&DSLEV..&RGN..DA045100.CREDIT.XTRACT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=TAPE,RETPD=35,
//**** P0516718 S
//*            DCB=(LRECL=140,BLKSIZE=0,RECFM=FB)
//**CLJB-2323-S,CLJB-2333-S
//*            DCB=(LRECL=162,BLKSIZE=0,RECFM=FB)
//             DCB=(LRECL=359,BLKSIZE=0,RECFM=FB)
//**CLJB-2323-E,CLJB-2333-E
//**** P0516718 E
//SYSIN    DD  DUMMY
//SYSUDUMP DD  SYSOUT=D
//*
//*********************************************************************
//* DELETE WORK DATASET                                               *
//*                                                                   *
//*  RESTART:  STEP RESTARTABLE WITH NO OVRERRIDES                    *
//* RESTART : THIS STEP                                               *
//*********************************************************************
//DA045120 EXEC  PGM=IEFBR14,COND=(0,NE,DA045100)
//B1       DD  DSN=&DSLEV..&RGN..DA045040.SORTED.ALL,
//             DISP=(OLD,DELETE)
//B2       DD  DSN=&DSLEV..&RGN..DA045020.CREDIT.DATA,
//             DISP=(OLD,DELETE)
//*