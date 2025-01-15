//PA1DA045 JOB (A1P),'CREDIT MEMO STMTS',REGION=0M,
//             USER=A1PBPRD,SCHENV='SCH_DB2_DP4G',
//         CLASS=4,MSGCLASS=V,MSGLEVEL=(1,1)
//JCL JCLLIB ORDER=(SYSPL1.PRD.PROCLIB)
//      INCLUDE MEMBER=A1$$INPP
//*
//*********************************************************************
//* FOR STATEMENT OF CREDIT REPRINTS, SEE JOB MXPRA045.               *
//*********************************************************************
//*
//*********************************************************************
//* R E S T A R T   I N S T R U C T I O N S                           *
//*********************************************************************
//* THIS JOB HAS SEVEN STEPS.                                         *
//*                                                                   *
//* IF IT ABENDS IS STEP DA045010, RESTART IN THIS STEP.              *
//*                                                                   *
//* IF IT ABENDS IS STEP DA045020:                                    *
//*   1) IF THE ABEND IS A -911, YOU MUST CHECK IF DATASET            *
//*      PRDA1.PP.DA045020.CREDIT.DATA WAS CATALOGED.  IF IT WAS,   *
//*      RESTART IN STEP DA045010.                                    *
//*   2) ANY OTHER ABEND, RESTART IN THIS STEP.                       *
//*                                                                   *
//* IF IT ABENDS IS STEP DA045040, RESTART IN THIS STEP.              *
//*                                                                   *
//* IF IT ABENDS IS STEP DA045060:                                    *
//*   1) DELETE ANY OUTPUT THAT IS IN SDSF.                           *
//*   2) RESTART IN THIS STEP.                                        *
//*                                                                   *
//* IF IT ABENDS IS STEP DA045080, RESTART IN THIS STEP.              *
//*                                                                   *
//* IF IT ABENDS IS STEP DA045100, RESTART IN THIS STEP.              *
//*                                                                   *
//* IF IT ABENDS IS STEP DA045120, RESTART IN THIS STEP.              *
//*                                                                   *
//*********************************************************************
//*
//*********************************************************************
//* DAILY STATEMENT OF CREDIT                                         *
//*********************************************************************
//*
//DA045A20 EXEC A1$DA045,
//*CLJB-2224-S
//*        SPACE1='(TRK,(300,30),RLSE)'
//         SPACE1='(TRK,(300,30),RLSE)',
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
//*CLJB-2224-E
//*******************************************************************
//            IF (RC GT 6) THEN
//ABENDSTP EXEC A1$ABEND
//            ENDIF
//*******************************************************************