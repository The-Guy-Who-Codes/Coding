      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXDIM=300,NKAPM=7,
     &          NCENTM=6,IL4=24,MAXMV=5,MAXMV2=2*MAXMV)
C***********************************************************************C
C                                                                       C
C                                                                       C
C                                                                       C
C        SSSS    WW    WW IIIIII  RRRRRR    LL      EEEEEEE    SSSS     C
C      SS    SS  WW    WW   II    RR    RR  LL      EE       SS    SS   C
C      SS        WW WW WW   II    RR    RR  LL      EE       SS         C
C        SSSS    WW WW WW   II    RR    RR  LL      EEEEEEE    SSSS     C
C            SS  WWW  WWW   II    RRRRRR    LL      EE             SS   C
C      SS    SS  WWW  WWW   II    RR   RR   LL      EE       SS    SS   C
C        SSSS    WW    WW IIIIII  RR    RR  LLLLLLL EEEEEEE    SSSS     C
C                                                                       C
C                                                                       C
C           A RELATIVISTIC MOLECULAR ELECTRONIC STRUCTURE PROGRAM       C
C           BASED ON THE ANALYTIC FINITE BASIS SET METHOD               C
C                                                                       C
C     (c)   H.M.QUINEY, H SKAANE, I.P.GRANT (OXFORD, 1996)              C
C                                                                       C
C***********************************************************************C
      PARAMETER(TRESH=1.0D-10,MAXIT=35)
      PARAMETER(LWORK=64*MAXDIM)
      PARAMETER(PI=3.1415926535898D0)
C
      CHARACTER*1 DUMLIN,UPLO,JOB
      CHARACTER*2 ELMNT(103)
      CHARACTER*4 MSTRNG(-2*NKAPM:2*NKAPM)
      CHARACTER*40 VFILE
C
      DIMENSION S1(MAXDIM),S2(MAXDIM)
      DIMENSION RWORK(3*MAXDIM),AMASS(NCENTM)
      DIMENSION NSYMOC(MAXMV*2),MLABEL(MAXDIM)
C
      COMPLEX*16 FOCK,OVAP
      COMPLEX*16 C
      COMPLEX*16 WORK(LWORK)
C
      COMMON/SPEC/COORD(3,NCENTM),ZNUC(NCENTM),CNUC(NCENTM),
     &  EXPSET(MAXB,NKAPM,NCENTM),IZNUC(NCENTM),ICRGE(NCENTM),
     &  KVALS(NKAPM,NCENTM),NKAPPA(NCENTM),LMAXX(NCENTM),
     &  NFUNCT(NKAPM,NCENTM),NCENT,NDIM,NSHIFT,
     &  NOCC,ITER,IALL,IRUN
      COMMON/FMAT/FOCK(MAXDIM,MAXDIM),OVAP(MAXDIM,MAXDIM)
      COMMON/DMP/FOLD(MAXDIM,MAXDIM)
      COMMON/LABELS/LARGE(NCENTM,NKAPM,MAXMV2)
      COMMON/COEFF/C(MAXDIM,MAXDIM)
      COMMON/CLABEL/IOCCM0
      COMMON/EVECT/EIGEN(MAXDIM)
      COMMON/ENERGY/ETOTAL
      COMMON/TIMEE/ETIME
      COMMON/FTIME/FUNTIME
      COMMON/TIMER/RTIME
      COMMON/TERI/ERIT
      COMMON/NUCLEI/NCENTR
      COMMON/SHIFTR/SFACT
      COMMON/OPENSH/NOPEN
      COMMON/RLIGHT/CV,CV2
      COMMON/EXTFIL/VFILE
      COMMON/SYMARR/KAPLAB(MAXDIM),ICNLAB(MAXDIM),IMLAB(MAXDIM)
C
      DATA ELMNT/'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne',
     & 'Ne','Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca',
     & 'Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn',
     & 'Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y ','Zr',
     & 'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn',
     & 'Sb','Te','I ','Xe','Cs','Ba','La','Ce','Pr','Nd',
     & 'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb',
     & 'Lu','Hf','Ta','W ','Re','Os','Ir','Pt','Au','Hg',
     & 'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th',
     & 'Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es','Fm',
     & 'Md','No','Lr'/
C
      CV=1.370359898D2
      CV2=CV*CV
C
C
      ERIT=0.0D0
      FUNTIME=0.0D0
      ETIME=0.0D0
      RTIME=0.0D0
      CALL MAKEINDEX(IL4)
      CALL FACTRL
C*********************************************************************C
C     INPUT SECTION                                                   C
C*********************************************************************C
      READ(5,*) DUMLIN
      READ(5,*) INTYPE
      READ(5,*) DUMLIN
      READ(5,*) NCENT
      NCENTR=NCENT
      NDIM=0
      DO 10 ICENT=1,NCENT
      READ(5,*) DUMLIN
      READ(5,*) (COORD(J,ICENT),J=1,3)
      READ(5,*) DUMLIN
      READ(5,*) IZNUC(ICENT),AMASS(ICENT),LMAXX(ICENT),ICRGE(ICENT)
      READ(5,*) DUMLIN
C      READ(5,*) (COORD(J,ICENT),J=1,3)
      ZNUC(ICENT)=DFLOAT(IZNUC(ICENT))
      NKAPPA(ICENT)=2*LMAXX(ICENT)+1
      ATHIRD=AMASS(ICENT)**0.33333333333333D0
      CNUC(ICENT)=1.50D10*(0.529167/(0.836*ATHIRD+0.57))**2
C
C     GENERATE THE EVEN TEMPERED ORBITAL EXPONENTS
C
      IF(INTYPE.EQ.1) THEN
      DO 8 L=1,LMAXX(ICENT)+1
      LQN=L-1
      READ(5,*) APARAM,BPARAM,NFUNCT(L,ICENT)
      ZETA=APARAM
      DO 9 IFUN=1,NFUNCT(L,ICENT)
      EXPSET(IFUN,L,ICENT)=ZETA
      ZETA=ZETA*BPARAM
9     CONTINUE
      NDIM=NDIM+4*(2*LQN+1)*NFUNCT(L,ICENT)
      IF(LQN.EQ.0) THEN
      KVALS(1,ICENT)=-1
      ELSE
      KVALS(2*LQN,ICENT)=LQN
      KVALS(2*LQN+1,ICENT)=-(LQN+1)
      ENDIF
8     CONTINUE
C
      ELSE
C
C     READ IN THE OPTIMIZED ORBITAL EXPONENTS
C 
      DO 13 L=1,LMAXX(ICENT)+1
      LQN=L-1
      READ(5,*) NFUNCT(L,ICENT)
      DO 14 IFUN=1,NFUNCT(L,ICENT)
      READ(5,*) EXPSET(IFUN,L,ICENT)
14    CONTINUE
      NDIM=NDIM+4*(2*LQN+1)*NFUNCT(L,ICENT)
      IF(LQN.EQ.0) THEN
      KVALS(1,ICENT)=-1
      ELSE
      KVALS(2*LQN,ICENT)=LQN
      KVALS(2*LQN+1,ICENT)=-(LQN+1)
      ENDIF
13    CONTINUE
C
      ENDIF
C
10    CONTINUE
C
C
C*********************************************************************C
C     END OF INPUT SECTION                                            C
C*********************************************************************C
C
      WRITE(6,11) NDIM
11    FORMAT(2X,'TOTAL DIMENSION OF THE MATRIX: ',I6/)
      IF(NDIM.GT.MAXDIM) THEN
      WRITE(6,*) 'MATRIX DIMENSION TOO BIG: NDIM= ',NDIM
      STOP
      ENDIF
      NSHIFT=NDIM/2
C
C      WRITE(6,*) 'ENTER NUMBER OF CS ORBITALS AND OS ORBITALS'
      READ(5,*) DUMLIN
      READ(5,*) NOCC,NOPEN
C
C      WRITE(6,*) 'ENTER 1 FOR NEW RUN AND 0 FOR RESTART'
      READ(5,*) DUMLIN
      READ(5,*) IRUN
C
      READ(5,*) DUMLIN
      READ(5,*) SFACT0,SFACT1,SFACT2
C
      READ(5,*) DUMLIN
      READ(5,*) IALL
C
C     OVERRUN THIS FOR ATOMS
C
      IF(NCENT.EQ.1) THEN
      IALL=2
      ENDIF
C
      IF(IALL.EQ.0) THEN
      SFACT=SFACT0
      ELSEIF(IALL.EQ.1) THEN
      SFACT=SFACT1
      ELSEIF(IALL.EQ.2) THEN
      SFACT=SFACT2
      ENDIF
C
      READ(5,*) DUMLIN
      READ(5,*) IPRINT
C 
      READ(5,*) DUMLIN
      READ(5,*) VFILE
C
C     READ IN START VECTORS IF RESTART
      IF(IRUN.EQ.0) THEN
      OPEN (UNIT=10,FILE=VFILE,STATUS='UNKNOWN')
      REWIND(UNIT=10)
      DO 140 IOCC=1,NDIM
      READ(10,*) EIGEN(IOCC), (C(I,IOCC), I=1,NDIM)
 140  CONTINUE
      CLOSE(UNIT=10)
      ENDIF
C
      CALL FLABEL
C
C     NUCLEAR REPULSION ENERGY
      EREP=0.0D0
      DO 12 NUCA=1,NCENT
      DO 12 NUCB=1,NUCA-1
      DIST=DSQRT((COORD(1,NUCA)-COORD(1,NUCB))**2
     #    +(COORD(2,NUCA)-COORD(2,NUCB))**2
     #    +(COORD(3,NUCA)-COORD(3,NUCB))**2)
      EREP=EREP+ZNUC(NUCA)*ZNUC(NUCB)/DIST
 12   CONTINUE
C
C**********************************************************************C
C      WRITE OUT MOLECULAR GEOMETRY
C**********************************************************************C
C
      WRITE(6,*)
     &'************************************************************'
      WRITE(6,*) '  '
      WRITE(6,*) '  '
C
      WRITE(6,*)
     &'        MOLECULAR GEOMETRY A: CARTESIAN COORDINATES         '
      WRITE(6,*) '  '
      WRITE(6,*)
     &'CENTRE        X-COORDINATE    Y-COORDINATE    Z-COORDINATE'
      WRITE(6,*)
     &'============================================================'
      DO 119 NUCA=1,NCENT
      WRITE(6,1118) ELMNT(IZNUC(NUCA)),
     &              COORD(1,NUCA),COORD(2,NUCA),COORD(3,NUCA)
      WRITE(6,*)
     &'------------------------------------------------------------'
119   CONTINUE
C
1118  FORMAT(3X,A,6X,F14.6,2X,F14.6,2X,F14.6)
C
      WRITE(6,*) '  '
      WRITE(6,*) '  '
C
      WRITE(6,*)
     &'                MOLECULAR GEOMETRY B: Z-MATRIX              '
      WRITE(6,*) '  '
      WRITE(6,*)
     &'C1  C2       BOND DISTANCE  C1  C2  C3     ANGLE (DEGREES)  '
      WRITE(6,*)
     &'============================================================'
C
      NUCCNT=1
      DO 17 NUCB=2,NCENT
      R1X=COORD(1,NUCB)-COORD(1,NUCCNT)
      R1Y=COORD(2,NUCB)-COORD(2,NUCCNT)
      R1Z=COORD(3,NUCB)-COORD(3,NUCCNT)
      D1=DSQRT(R1X**2+R1Y**2+R1Z**2)
      WRITE(6,1119) ELMNT(IZNUC(NUCCNT)),ELMNT(IZNUC(NUCB)),D1
C
      DO 18 NUCC=2,NUCB-1
      R2X=COORD(1,NUCC)-COORD(1,NUCCNT)
      R2Y=COORD(2,NUCC)-COORD(2,NUCCNT)
      R2Z=COORD(3,NUCC)-COORD(3,NUCCNT)
C
      D2=DSQRT(R2X**2+R2Y**2+R2Z**2)
      SCPROD=(R1X*R2X+R1Y*R2Y+R1Z*R2Z)
      ANG=DACOS(SCPROD/(D1*D2))*(360.0D0/(2.0D0*PI))
      WRITE(6,1120) ELMNT(IZNUC(NUCB)),ELMNT(IZNUC(NUCCNT)),
     &           ELMNT(IZNUC(NUCC)),ANG  
C
18    CONTINUE
      WRITE(6,*)
     &'------------------------------------------------------------'
17    CONTINUE
C
1119  FORMAT(2X,A,2X,A,4X,F14.6)
1120  FORMAT(30X,A,2X,A,2X,A,2X,F14.6)
C
      WRITE(6,*) '  '
      WRITE(6,*) '  '
      WRITE(6,*) '  '
      WRITE(6,*) '  '
C
C**********************************************************************C
C     INITIALIZE THE ATOMIC DENSITIES ON EACH CENTRE                   C
C**********************************************************************C
      IF(IRUN.EQ.1) THEN
      IOCCM0=NSHIFT+1
      WRITE(6,*) 
     &'          INITIALZE THE ATOMIC DENSITIES ON EACH CENTRE   '
      WRITE(6,*) '  '
      WRITE(6,*) '  '
      DO 200 ICENT=1,NCENT
      CALL ATOMIC(EXPSET,AMASS,IZNUC,ICRGE,NFUNCT,LMAXX,
     &  KVALS,NKAPPA,ICENT,NSHIFT)
200   CONTINUE
      IOCCM0=IOCCM0-NSHIFT
      CALL DENS
      IOCCM0=NOCC
      ELSE
      IOCCM0=NOCC
      CALL DENS
      ENDIF
C
C     SCF ITERATIONS
C
      DIFF=100.0D0
C
      EOLD=1.0D0 
C
      RTRESH1=1.0D-5
      RTRESH2=1.0D-7
C
C
      DO 1000 ITER=1,MAXIT
C
      ERIT=0.0D0
      FUNTIME=0.0D0
      ETIME=0.0D0
      RTIME=0.0D0
C
C
      CALL SCFMAT
C--------------------------------------------------------------------
      DO 2342 I=1,NDIM
      DO 2342 J=1,NDIM
C
      FFRE=DREAL(FOCK(I,J))
      FFIM=DIMAG(FOCK(I,J))
C
      IF(DABS(FFRE).LT.(1.0D-10)) THEN
      FFRE=0.0D0
      ENDIF
C
      IF(DABS(FFIM).LT.(1.0D-10)) THEN
      FFIM=0.0D0
      ENDIF
C
      FOCK(I,J)=DCMPLX(FFRE,FFIM)
C
2342  CONTINUE
C--------------------------------------------------------------------
      CALL ONEEL
C
      IF((ITER.GT.1.AND.SFACT.NE.0.0D0).OR.IRUN.EQ.0) THEN
       CALL SHFTLV
      ENDIF
C
C*********************************************************************C
      DO 5533 I=1,NDIM
      FFRE=DREAL(FOCK(I,I))
      FOCK(I,I)=DCMPLX(FFRE,0.0D0)
5533  CONTINUE
C*********************************************************************C
      ITYPE = 1
      IFAIL = 0
      JOB = 'V'
      UPLO = 'L'
C
      CALL F02HDF(ITYPE,JOB,UPLO,NDIM,FOCK,MAXDIM,OVAP,MAXDIM,
     &            EIGEN,RWORK,WORK,LWORK,IFAIL)
C
C     ON EXIT FROM F02HDF ALL THE EIGENVECTORS ARE STORED IN FOCK
C
      DO 921 J=1,NDIM
      DO 921 I=1,NDIM
      C(I,J)=FOCK(I,J)
921   CONTINUE
C*********************************************************************C
C
C     WRITE EIGEN VECTORS TO FILE
C
      OPEN (UNIT=10,FILE=VFILE,STATUS='UNKNOWN')
      REWIND(UNIT=10)
      DO 141 IOCC=1,NDIM
      WRITE(10,*) EIGEN(IOCC), (C(I,IOCC), I=1,NDIM)
 141  CONTINUE
      CLOSE(UNIT=10)
C
C
C     CONSTRUCT DENSITY MATRIX USING THE FOCK MATRIX AS SCRRATCH SPACE
C     NB! C IS NOW THE DENSITY MATRIX
C
      CALL DENS
C
      IF(IPRINT.EQ.2) THEN
      WRITE(6,*) 'ITERATION NUMBER: ',ITER
      WRITE(6,*) 'LEVEL SHIFT PARAMETER= ',SFACT
      DO 20 ISTATE=1,NOCC+NOPEN
      WRITE(6,1319) 'EIGENVALUE NUMBER, E:',
     & ISTATE,EIGEN(ISTATE+NSHIFT)
20    CONTINUE
      WRITE(6,1319) 'LUMO               E:',
     & ISTATE,EIGEN(NOCC+NOPEN+NSHIFT+1)
      WRITE(6,*) '  '
      WRITE(6,1318) ' TOTAL ELECTRONIC ENERGY  :',ETOTAL
      WRITE(6,1318) '+NUCLEAR REPULSION ENERGY :',EREP
      WRITE(6,1318) '=TOTAL ENERGY             :',ETOTAL+EREP
      WRITE(6,*) ' '
      WRITE(6,*) '-------------------------------------------------'
      ENDIF
C
1319  FORMAT(2X,A21,2X,I2,4X,F16.10)
1318  FORMAT(2X,A27,2X,F16.10)
C      
C     CHECK FOR CONVERGENCE
C
      DIFF=DABS(EOLD-ETOTAL)/(DABS(ETOTAL)+1.0D0)
      IF (DIFF.LT.TRESH.AND.IALL.EQ.2) GOTO 2000
      IF (DIFF.LT.RTRESH1.AND.IALL.EQ.0) THEN
      IALL=1
      SFACT=SFACT1
      WRITE(6,*) '***INCLUDING (LL|SS),(SS|LL),(LS|LS) AND (SL|SL)***'
      ELSEIF(DIFF.LT.RTRESH2.AND.IALL.EQ.1) THEN
      WRITE(6,*) '***INCLUDING (SS|SS)***'
      IALL=2
      SFACT=SFACT2
      ENDIF
      EOLD=ETOTAL
1000  CONTINUE
      WRITE(6,*) 'CONVERGENCE NOT OBTAINED IN THE MAXIMUM NUMBER'
      WRITE(6,*) 'OF ITERATIONS'
2000  CONTINUE
C
C     PRINT OUT FINAL RESULTS
C
      WRITE(6,*) ' '
      WRITE(6,*) '**********************************************'
      WRITE(6,*) 'FINAL OUTPUT:'
      WRITE(6,*) 'CONVERGENCE OBTAINED IN',ITER,'     ITERATIONS'
      WRITE(6,*) ' '
      WRITE(6,*) 'BOND LENGTH =',COORD(3,2)
      WRITE(6,*) 'TOTAL ENERGY=',ETOTAL+EREP
      WRITE(6,*) ' '
      WRITE(6,*) '**********************************************'
      IALL=2
      IRUN=0
8000  CONTINUE
      CLOSE(UNIT=11)
      STOP
      END
C
C
C
      SUBROUTINE FLABEL
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXB=20,NKAPM=7,NCENTM=6,MAXMV=5,MAXMV2=2*MAXMV)
      PARAMETER(MAXDIM=300)
C****************************************************************C
C     FLABEL CALCUATES THE ADDRESSES OF THE FOCK-MATRIX BLOCKS   C
C****************************************************************C
      COMMON/SPEC/COORD(3,NCENTM),ZNUC(NCENTM),CNUC(NCENTM),
     &  EXPSET(MAXB,NKAPM,NCENTM),IZNUC(NCENTM),ICRGE(NCENTM),
     &  KVALS(NKAPM,NCENTM),NKAPPA(NCENTM),LMAXX(NCENTM),
     &  NFUNCT(NKAPM,NCENTM),NCENT,NDIM,NSHIFT,
     &  NOCC,ITER,IALL,IRUN
      COMMON/LABELS/LARGE(NCENTM,NKAPM,MAXMV2)
      COMMON/ILAB/IADR
      COMMON/SYMARR/KAPLAB(MAXDIM),ICNLAB(MAXDIM),IMLAB(MAXDIM)
C
      ICOUNT=0
      IDIG=1
      DO 11 ICENT=1,NCENT
      DO 10 IM=1,MAXMV
      MVAL=2*IM-1
C
C     LABEL NEGATIVE M-VALUE BLOCKS
C
      DO 20 KA=1,NKAPPA(ICENT)
      KAPPA=KVALS(KA,ICENT)
      IF(KAPPA.GT.0) THEN
       LQN=KAPPA
      ELSE
       LQN=-KAPPA-1
      ENDIF
      NFUN=NFUNCT(LQN+1,ICENT)
      MQMAX=2*IABS(KAPPA)-1
      IF(MQMAX.GE.MVAL) THEN
      LARGE(ICENT,KA,MVAL)=ICOUNT
      IDIG=IDIG+1
      DO 332 IFN=1,NFUN
      KAPLAB(ICOUNT+IFN)=KAPPA
      ICNLAB(ICOUNT+IFN)=ICENT
      IMLAB(ICOUNT+IFN)=MVAL
332   CONTINUE
      ICOUNT=ICOUNT+NFUN
      ENDIF
20    CONTINUE
C
C     LABEL POSITIVE M-VALUE BLOCKS
C
      DO 30 KA=1,NKAPPA(ICENT)
      KAPPA=KVALS(KA,ICENT)
      IF(KAPPA.GT.0) THEN
       LQN=KAPPA
      ELSE
       LQN=-KAPPA-1
      ENDIF
      NFUN=NFUNCT(LQN+1,ICENT)
      MQMAX=2*IABS(KAPPA)-1
      IF(MQMAX.GE.MVAL) THEN
      LARGE(ICENT,KA,MVAL+1)=ICOUNT
      IDIG=IDIG+1
      DO 333 IFN=1,NFUN
      KAPLAB(ICOUNT+IFN)=KAPPA
      ICNLAB(ICOUNT+IFN)=ICENT
      IMLAB(ICOUNT+IFN)=MVAL
333   CONTINUE
      ICOUNT=ICOUNT+NFUN
      ENDIF
30    CONTINUE
10    CONTINUE
      IF(ICENT.EQ.1) THEN
      IADR=ICOUNT
      ENDIF
11    CONTINUE
      RETURN
      END

C
C
      SUBROUTINE SHFTLV
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C********************************************************************C
C     LEVEL SHIFT THE FOCK MATRIX                                    C
C********************************************************************C
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXDIM=300,NKAPM=7,
     #          MAXR=969,MLL=165,NCENTM=6,MAXMV=5,MAXMV2=2*MAXMV,
     #          ZERO=0.0D0,
     #          PI=3.1415926535898D0,
     #          TWOPI=6.2831853071796D0,
     #          HALF=5.0D-1)
C
      CHARACTER*40 VFILE
C
      COMPLEX*16 FOCK,OVAP,C
      COMPLEX*16 SUMI
C
      DIMENSION RDUM(MAXDIM)
      COMMON/SPEC/COORD(3,NCENTM),ZNUC(NCENTM),CNUC(NCENTM),
     &  EXPSET(MAXB,NKAPM,NCENTM),IZNUC(NCENTM),ICRGE(NCENTM),
     &  KVALS(NKAPM,NCENTM),NKAPPA(NCENTM),LMAXX(NCENTM),
     &  NFUNCT(NKAPM,NCENTM),NCENT,NDIM,NSHIFT,
     &  NOCC,ITER,IALL,IRUN
      COMMON/FMAT/FOCK(MAXDIM,MAXDIM),OVAP(MAXDIM,MAXDIM)
      COMMON/COEFF/C(MAXDIM,MAXDIM)
      COMMON/SHIFTR/SFACT
      COMMON/OPENSH/NOPEN
      COMMON/EXTFIL/VFILE
C
C     READ IN VIRTUAL SPECTRUM INTO THE E-ARRAY
C
      OPEN (UNIT=10,FILE=VFILE,STATUS='UNKNOWN')
      REWIND(UNIT=10)
      DO 141 IOCC=1,NDIM
      READ(10,*) RDUM(IOCC), (C(I,IOCC), I=1,NDIM)
 141  CONTINUE
      CLOSE(UNIT=10)
C
      DO 3000 K=NSHIFT+NOCC+NOPEN+1,NDIM
      DO 1000 I=1,NDIM
      SUMI=DCMPLX(0.0D0,0.0D0)
      DO 2000 IBAS=1,NDIM
      SUMI=SUMI+C(IBAS,K)*OVAP(I,IBAS)
2000  CONTINUE
      C(I,1)=SUMI
1000  CONTINUE
C
      DO 4000 I=1,NDIM
      DO 4000 J=1,NDIM
      FOCK(I,J)=FOCK(I,J)+SFACT*C(I,1)*DCONJG(C(J,1))
4000  CONTINUE
3000  CONTINUE
      RETURN
      END
C
C
C***********************************************************************
C
      SUBROUTINE SCFMAT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXDIM=300,NKAPM=7,
     &          MAXR=969,MLL=165,NCENTM=6,MAXMV=5,MAXMV2=2*MAXMV,
     &          ZERO=0.0D0,PI=3.1415926535898D0,
     &          TWOPI=6.2831853071796D0,HALF=5.0D-1)
C***********************************************************************C
C      SCFMAT GENERATES ELECTRON REPULSION INTEGRALS IN BATCHES, AND    C
C      THEN USES THEM TO CONSTRUCT THE OPEN AND CLOSED-SHELL FOCK       C
C      MATRICES, EXPLOITING INTEGRAL SYMMETRY                           C
C***********************************************************************C
C
      COMPLEX*16 DENC,FOCK,OVAP,CZERO
      COMPLEX*16 RR(MAXB2,16)
C
      COMMON/SPEC/COORD(3,NCENTM),ZNUC(NCENTM),CNUC(NCENTM),
     &  EXPSET(MAXB,NKAPM,NCENTM),IZNUC(NCENTM),ICRGE(NCENTM),
     &  KVALS(NKAPM,NCENTM),NKAPPA(NCENTM),LMAXX(NCENTM),
     &  NFUNCT(NKAPM,NCENTM),NCENT,NDIM,NSHIFT,
     &  NOCC,ITER,IALL,IRUN
      COMMON/FMAT/FOCK(MAXDIM,MAXDIM),OVAP(MAXDIM,MAXDIM)
      COMMON/COEFF/DENC(MAXDIM,MAXDIM)
      COMMON/LABELS/IARGE(NCENTM,NKAPM,MAXMV2)
      COMMON/ENERGY/ETOTAL
      DIMENSION EXPT(MAXB,4),
     &          KAPPA(4),LQN(4),MQN(4),
     &          NFUNS(4),ITQN(2),IFLG(11),ISCF(11,6)
      DIMENSION INDEX(NCENTM,-NKAPM:NKAPM,2*(NKAPM+1)*NKAPM)
      COMMON/RLIGHT/CV,CV2
      COMMON/LOCGEO/XYZ(3,4)
      COMMON/MHOPE/NMID(MAXDIM)
      COMMON/SYMARR/KAPLAB(MAXDIM),ICNLAB(MAXDIM),IMLAB(MAXDIM)
      DATA ISCF/1,1,1,1,0,1,0,1,0,0,0,
     &          1,0,1,1,0,1,0,0,0,0,0,
     &          0,1,1,0,0,1,0,0,0,1,0,
     &          1,0,1,1,0,0,0,0,0,0,1,
     &          0,0,1,0,0,0,0,0,0,1,1,
     &          0,0,1,0,0,0,0,0,0,1,0/
      ETOTAL=ZERO
C
      CZERO=DCMPLX(0.0D0,0.0D0)
C
C-----------------------------------------------------------------------C
C     INDEXING ROUTINE
C-----------------------------------------------------------------------C
      ICOUNT=0
      DO 50 ICENT=1,NCENT
      DO 50 KN=1,NKAPPA(ICENT)
      KAPPAN=KVALS(KN,ICENT)
      MJMAX=2*IABS(KAPPAN)-1
      DO 50 MJ=1,MJMAX,2
      ICOUNT=ICOUNT+1
      INDEX(ICENT,KAPPAN,MJ)=ICOUNT
50    CONTINUE
C-----------------------------------------------------------------------C
C     ZERO OUT FOCK MATRIX
C
      DO 10 J=1,NDIM
      DO 10 I=1,NDIM
      FOCK(I,J)=CZERO
10    CONTINUE
C
C-----------------------------------------------------------------------C
C
C
      DO 2000 ICENTA=1,NCENT
      XYZ(1,1)=COORD(1,ICENTA)
      XYZ(2,1)=COORD(2,ICENTA)
      XYZ(3,1)=COORD(3,ICENTA)
      DO 2000 ICENTB=1,ICENTA
      IF(ICENTA.EQ.ICENTB) THEN
       INUCAB=1
      ELSE
       INUCAB=2
      ENDIF
      XYZ(1,2)=COORD(1,ICENTB)
      XYZ(2,2)=COORD(2,ICENTB)
      XYZ(3,2)=COORD(3,ICENTB)
C
C     LOOP OVER KAPPA(A) VALUES
C
      DO 2000 KA=1,NKAPPA(ICENTA)
      KAPPA(1)=KVALS(KA,ICENTA)
      IF(KAPPA(1).GT.0) THEN
       LQN(1)=KAPPA(1)
      ELSE
       LQN(1)=-KAPPA(1)-1
      ENDIF
C
      NFUNA=NFUNCT(LQN(1)+1,ICENTA)
      NFUNS(1)=NFUNA
      DO 70 IBAS=1,NFUNA
      EXPT(IBAS,1)=EXPSET(IBAS,LQN(1)+1,ICENTA)
70    CONTINUE
C
      IBASB=0
C
C     LOOP OVER KAPPA(B) VALUES
C
      DO 2000 KB=1,NKAPPA(ICENTB)
      KAPPA(2)=KVALS(KB,ICENTB)
      IF(KAPPA(2).GT.0) THEN
       LQN(2)=KAPPA(2)
      ELSE
       LQN(2)=-KAPPA(2)-1
      ENDIF
      NFUNB=NFUNCT(LQN(2)+1,ICENTB)
      NFUNS(2)=NFUNB
      DO 71 IBAS=1,NFUNB
      EXPT(IBAS,2)=EXPSET(IBAS,LQN(2)+1,ICENTB)
71    CONTINUE
C
C     LOOP OVER |MA| VALUES
C
      DO 2000 MA=1,IABS(KAPPA(1))
      MJA=(2*MA)-1
      MQN(1)=MJA
C
C     LOOP OVER |MB| VALUES
C
      DO 2000 MB=1,IABS(KAPPA(2))
      MJB=(2*MB)-1
      MQN(2)=MJB
C
C     LOOP OVER CENTRES C AND D
C
      DO 1000 ICENTC=1,NCENT
      XYZ(1,3)=COORD(1,ICENTC)
      XYZ(2,3)=COORD(2,ICENTC)
      XYZ(3,3)=COORD(3,ICENTC)
C
      DO 1000 ICENTD=1,NCENT
      XYZ(1,4)=COORD(1,ICENTD)
      XYZ(2,4)=COORD(2,ICENTD)
      XYZ(3,4)=COORD(3,ICENTD)
C
C     TEST WHETHER THIS COMBINATION IS OF ATOM-IN-MOLECULE TYPE
C
      IF(ICENTC.EQ.ICENTD) THEN
       INUCCD=1
      ELSE
       INUCCD=2
      ENDIF
      IF(INUCAB*INUCCD.EQ.1.AND.ICENTA.EQ.ICENTC) THEN
       IATOM=1
      ELSE
       IATOM=0
      ENDIF
C
C     LOOP OVER KAPPA(C) VALUES
C
      DO 1000 KC=1,NKAPPA(ICENTC)
      KAPPA(3)=KVALS(KC,ICENTC)
      IF(KAPPA(3).GT.0) THEN
       LQN(3)=KAPPA(3)
      ELSE
       LQN(3)=-KAPPA(3)-1
      ENDIF
      NFUNC=NFUNCT(LQN(3)+1,ICENTC)
      NFUNS(3)=NFUNC
      DO 72 IBAS=1,NFUNC
      EXPT(IBAS,3)=EXPSET(IBAS,LQN(3)+1,ICENTC)
72    CONTINUE
C
C     LOOP OVER KAPPA(D) VALUES
C
      DO 1000 KD=1,NKAPPA(ICENTD)
      KAPPA(4)=KVALS(KD,ICENTD)
      IF(KAPPA(4).GT.0) THEN
       LQN(4)=KAPPA(4)
      ELSE
       LQN(4)=-KAPPA(4)-1
      ENDIF
      NFUND=NFUNCT(LQN(4)+1,ICENTD)
      NFUNS(4)=NFUND
      DO 73 IBAS=1,NFUND
      EXPT(IBAS,4)=EXPSET(IBAS,LQN(4)+1,ICENTD)
73    CONTINUE
C
C     LOOP OVER |MC| AND |MD| VALUES
C
      DO 1000 MC=1,IABS(KAPPA(3))
      MJC=(2*MC)-1
      MQN(3)=MJC
C
      DO 1000 MD=1,IABS(KAPPA(4))
      MJD=(2*MD)-1
      MQN(4)=MJD
C
C
      MAXM=NFUNC*NFUND  
C
      IF(ICENTA.EQ.ICENTB) THEN
       INUCAB=1
      ELSE
       INUCAB=2
      ENDIF
      IF(ICENTC.EQ.ICENTD) THEN
       INUCCD=1
      ELSE
       INUCCD=2
      ENDIF
      IF(INUCAB*INUCCD.EQ.1.AND.ICENTA.EQ.ICENTC) THEN
       IATOM=1
      ELSE
       IATOM=0
      ENDIF
C
C     IATOM=1 ATOMIC INTEGRAL
C     IATOM=0 MULTICENTRE INTEGRAL
C
C
C**********************************************************************C
C
      IQ1=INDEX(ICENTA,KAPPA(1),MQN(1))
      IQ2=INDEX(ICENTB,KAPPA(2),MQN(2))
      IQ3=INDEX(ICENTC,KAPPA(3),MQN(3))
      IQ4=INDEX(ICENTD,KAPPA(4),MQN(4))
      IF (IQ1.LT.IQ2) GOTO 1999
      IF (IQ3.LT.IQ4) GOTO 1999
      IQ12 =(IQ1*(IQ1-1))/2+IQ2
      IQ34 =(IQ3*(IQ3-1))/2+IQ4
      IF (IQ12.LT.IQ34) GOTO 1999
      IQ1234=(IQ12*(IQ12-1))/2+IQ34
C
C     LOOP OVER COMPONENTS.
C
      DO 4000 IT1=1,4,3
      DO 4000 IT2=1,4,3
      ITQN(1)=IT1
      ITQN(2)=IT2
C
      IF((ITQN(1)+ITQN(2)).GT.2.AND.IALL.EQ.0.
     &    AND.IATOM.EQ.0) THEN
       GO TO 4001
      ENDIF
C
      IF((ITQN(1)+ITQN(2)).GT.5.AND.IALL.EQ.1.
     &    AND.IATOM.EQ.0) THEN
       GO TO 4001
      ENDIF
C
c      IF(ITQN(1).EQ.4.AND.ITQN(2).EQ.4.AND.IATOM.EQ.0) THEN
c       GO TO 4001
c      ENDIF
C
C     CALCULATE PHASE FACTORS FOR PERMUTING INTEGRALS.
C
      IF((KAPPA(1)*KAPPA(2)).GT.0) THEN 
       RKFAC1=1.0D0
      ELSE
       RKFAC1=-1.0D0
      ENDIF
      IF((KAPPA(3)*KAPPA(4)).GT.0) THEN 
       RKFAC2=1.0D0
      ELSE
       RKFAC2=-1.0D0
      ENDIF
C
      FACAB1=DFLOAT((-1)**((-MQN(1)+MQN(2))/2))
      FACAB2=DFLOAT((-1)**((MQN(1)+MQN(2))/2))
      FACCD1=DFLOAT((-1)**((-MQN(3)+MQN(4))/2))
      FACCD2=DFLOAT((-1)**((MQN(3)+MQN(4))/2))
C
C
      IF(ITQN(1).EQ.1) THEN
       NADDAB=0
      ELSE
       NADDAB=NSHIFT
      ENDIF
      IF(ITQN(2).EQ.1) THEN
       NADDCD=0
      ELSE
       NADDCD=NSHIFT
      ENDIF
C
      IA1=IARGE(ICENTA,KA,MJA)+NADDAB
      IB1=IARGE(ICENTB,KB,MJB)+NADDAB
      IC1=IARGE(ICENTC,KC,MJC)+NADDCD
      ID1=IARGE(ICENTD,KD,MJD)+NADDCD
C
      IA2=IARGE(ICENTA,KA,MJA+1)+NADDAB
      IB2=IARGE(ICENTB,KB,MJB+1)+NADDAB
      IC2=IARGE(ICENTC,KC,MJC+1)+NADDCD
      ID2=IARGE(ICENTD,KD,MJD+1)+NADDCD
C
      JA1=IARGE(ICENTA,KA,MJA)+NADDAB
      JB1=IARGE(ICENTB,KB,MJB)+NADDAB
      JC1=IARGE(ICENTC,KC,MJC)+NADDCD
      JD1=IARGE(ICENTD,KD,MJD)+NADDCD
C
      JA2=IARGE(ICENTA,KA,MJA+1)+NADDAB
      JB2=IARGE(ICENTB,KB,MJB+1)+NADDAB
      JC2=IARGE(ICENTC,KC,MJC+1)+NADDCD
      JD2=IARGE(ICENTD,KD,MJD+1)+NADDCD
C
C**********************************************************************C
C     GENERATE DECISION TREE FOR FOCK MATRIX CONSTRUCTION BASED        C
C     ON INTEGRAL SYMMETRY                                             C
C**********************************************************************C
      IF(IQ1.GT.IQ2.AND.IQ3.GT.IQ4.AND.IQ12.GT.IQ34) THEN
       ITSCF=1
      ELSEIF(IQ1.GT.IQ2.AND.IQ3.GT.IQ4.AND.IQ12.EQ.IQ34) THEN
       ITSCF=2
      ELSEIF(IQ1.GT.IQ2.AND.IQ3.EQ.IQ4.AND.IQ12.GT.IQ34) THEN
       ITSCF=3
      ELSEIF(IQ1.EQ.IQ2.AND.IQ3.GT.IQ4.AND.IQ12.GT.IQ34) THEN
       ITSCF=4
      ELSEIF(IQ1.EQ.IQ2.AND.IQ3.EQ.IQ4.AND.IQ12.GT.IQ34) THEN
       ITSCF=5
      ELSEIF(IQ1.EQ.IQ2.AND.IQ3.EQ.IQ4.AND.IQ12.EQ.IQ34) THEN
       ITSCF=6
      ELSE
       GO TO 1999
      ENDIF
C
      DO 35 M=1,11
      IFLG(M)=ISCF(M,ITSCF)
35    CONTINUE
C
C     INCLUDE SPECIAL CASES
C
      IF(ITSCF.EQ.1.AND.IQ1.EQ.IQ3) IFLG(5)=1
      IF(ITSCF.EQ.1.AND.IQ2.EQ.IQ4) IFLG(7)=1
      IF(ITSCF.EQ.1.AND.IQ2.EQ.IQ3) IFLG(9)=1
      IF(ITSCF.EQ.3.AND.IQ2.EQ.IQ3) IFLG(9)=1
      IF(ITSCF.EQ.4.AND.IQ2.EQ.IQ3) IFLG(9)=1
C
      IEMAKE=1
      ITYPE=1
      MAXM1=NFUNA*NFUNB
      MAXM2=NFUNC*NFUND
      ICOUNT=0
C
C*********************************************************************C
C     THERE ARE ELEVEN DISTINCT PERMUTATIONAL ALGORITHMS WHICH        C
C     GENERATE THE FOCK MATRIX FROM THE SPINOR INTEGRALS.             C
C     THESE INCLUDE IMPLICIT PHASE FACTORS FOR THE PERMUTATION OF     C
C     KAPPA(1) <-> KAPPA(2) AND MQN(1) <-> MQN(2)                     C
C*********************************************************************C  
      DO 3000 IFUN=1,NFUNA
      DO 3000 JFUN=1,NFUNB
C
C
      CALL ERI(RR,XYZ,KAPPA,MQN,EXPT,NFUNS,ITQN,IFUN,JFUN,
     &         IEMAKE,ITYPE,INUCAB,INUCCD)
      IEMAKE=0
C
      IF(IFLG(1).EQ.1) THEN
      F1=RKFAC2*FACCD1
      F2=RKFAC2*FACCD2
      M=0
      DO 100 K=1,NFUNC
      DO 100 L=1,NFUND
      M=M+1
C
      FOCK(IA1+IFUN,JB1+JFUN)=FOCK(IA1+IFUN,JB1+JFUN)
     &  +RR(M,1)*DENC(IC1+K,JD1+L)
     &  +F1*RR(M,4)*DENC(JD1+L,IC1+K)
     &  +RR(M,2)*(DENC(IC1+K,JD2+L)+F2*DENC(JD1+L,IC2+K))
     &  +RR(M,3)*(DENC(IC2+K,JD1+L)+F2*DENC(JD2+L,IC1+K))
     &  +RR(M,4)*DENC(IC2+K,JD2+L)
     &  +F1*RR(M,1)*DENC(JD2+L,IC2+K)
      FOCK(IA1+IFUN,JB2+JFUN)=FOCK(IA1+IFUN,JB2+JFUN)
     &  +RR(M,5)*DENC(IC1+K,JD1+L)
     &  +F1*RR(M,8)*DENC(JD1+L,IC1+K)
     &  +RR(M,6)*(DENC(IC1+K,JD2+L)+F2*DENC(JD1+L,IC2+K))
     &  +RR(M,7)*(DENC(IC2+K,JD1+L)+F2*DENC(JD2+L,IC1+K))
     &  +RR(M,8)*DENC(IC2+K,JD2+L)
     &  +F1*RR(M,5)*DENC(JD2+L,IC2+K)
      FOCK(IA2+IFUN,JB1+JFUN)=FOCK(IA2+IFUN,JB1+JFUN)
     &  +RR(M,9)*DENC(IC1+K,JD1+L)
     &  +F1*RR(M,12)*DENC(JD1+L,IC1+K)
     &  +RR(M,10)*(DENC(IC1+K,JD2+L)+F2*DENC(JD1+L,IC2+K))
     &  +RR(M,11)*(DENC(IC2+K,JD1+L)+F2*DENC(JD2+L,IC1+K))
     &  +RR(M,12)*DENC(IC2+K,JD2+L)
     &  +F1*RR(M,9)*DENC(JD2+L,IC2+K)
      FOCK(IA2+IFUN,JB2+JFUN)=FOCK(IA2+IFUN,JB2+JFUN)
     &  +RR(M,13)*DENC(IC1+K,JD1+L)
     &  +F1*RR(M,16)*DENC(JD1+L,IC1+K)
     &  +RR(M,14)*(DENC(IC1+K,JD2+L)+F2*DENC(JD1+L,IC2+K))
     &  +RR(M,15)*(DENC(IC2+K,JD1+L)+F2*DENC(JD2+L,IC1+K))
     &  +RR(M,16)*DENC(IC2+K,JD2+L)
     &  +F1*RR(M,13)*DENC(JD2+L,IC2+K)
100   CONTINUE
      ENDIF
      IF(IFLG(2).EQ.1) THEN
      F1=RKFAC1*FACAB1
      F2=RKFAC1*FACAB2
      M=0
      DO 110 K=1,NFUNC
      DO 110 L=1,NFUND
      M=M+1
      FOCK(IC1+K,JD1+L)=FOCK(IC1+K,JD1+L)
     & +RR(M,1 )*DENC(IA1+IFUN,JB1+JFUN)
     & +F1*RR(M,13)*DENC(JB1+JFUN,IA1+IFUN)
     & +RR(M,5 )*(DENC(IA1+IFUN,JB2+JFUN)+F2*DENC(JB1+JFUN,IA2+IFUN))
     & +RR(M,9 )*(DENC(IA2+IFUN,JB1+JFUN)+F2*DENC(JB2+JFUN,IA1+IFUN))
     & +RR(M,13)*DENC(IA2+IFUN,JB2+JFUN)
     & +F1*RR(M,1 )*DENC(JB2+JFUN,IA2+IFUN)
      FOCK(IC1+K,JD2+L)=FOCK(IC1+K,JD2+L)
     & +RR(M,2 )*DENC(IA1+IFUN,JB1+JFUN)
     & +F1*RR(M,14)*DENC(JB1+JFUN,IA1+IFUN)
     & +RR(M,6 )*(DENC(IA1+IFUN,JB2+JFUN)+F2*DENC(JB1+JFUN,IA2+IFUN))
     & +RR(M,10)*(DENC(IA2+IFUN,JB1+JFUN)+F2*DENC(JB2+JFUN,IA1+IFUN))
     & +RR(M,14)*DENC(IA2+IFUN,JB2+JFUN)      
     & +F1*RR(M,2 )*DENC(JB2+JFUN,IA2+IFUN)
      FOCK(IC2+K,JD1+L)=FOCK(IC2+K,JD1+L)
     & +RR(M,3 )*DENC(IA1+IFUN,JB1+JFUN)
     & +F1*RR(M,15)*DENC(JB1+JFUN,IA1+IFUN)
     & +RR(M,7 )*(DENC(IA1+IFUN,JB2+JFUN)+F2*DENC(JB1+JFUN,IA2+IFUN))
     & +RR(M,11)*(DENC(IA2+IFUN,JB1+JFUN)+F2*DENC(JB2+JFUN,IA1+IFUN))
     & +RR(M,15)*DENC(IA2+IFUN,JB2+JFUN)
     & +F1*RR(M,3 )*DENC(JB2+JFUN,IA2+IFUN)
      FOCK(IC2+K,JD2+L)=FOCK(IC2+K,JD2+L)
     & +RR(M,4 )*DENC(IA1+IFUN,JB1+JFUN)
     & +F1*RR(M,16)*DENC(JB1+JFUN,IA1+IFUN)
     & +RR(M,8 )*(DENC(IA1+IFUN,JB2+JFUN)+F2*DENC(JB1+JFUN,IA2+IFUN))
     & +RR(M,12)*(DENC(IA2+IFUN,JB1+JFUN)+F2*DENC(JB2+JFUN,IA1+IFUN))
     & +RR(M,16)*DENC(IA2+IFUN,JB2+JFUN)
     & +F1*RR(M,4 )*DENC(JB2+JFUN,IA2+IFUN)
110   CONTINUE
      ENDIF
      IF(IFLG(3).EQ.1) THEN
      DO 120 L=1,NFUND
      DO 120 K=1,NFUNC
      M1=(K-1)*NFUND+L
      FOCK(IA1+IFUN,JD1+L)=FOCK(IA1+IFUN,JD1+L)
     &                   -RR(M1,1 )*DENC(IC1+K,JB1+JFUN)
     &                   -RR(M1,3 )*DENC(IC2+K,JB1+JFUN)
     &                   -RR(M1,5 )*DENC(IC1+K,JB2+JFUN)
     &                   -RR(M1,7 )*DENC(IC2+K,JB2+JFUN)
      FOCK(IA1+IFUN,JD2+L)=FOCK(IA1+IFUN,JD2+L)
     &                   -RR(M1,2 )*DENC(IC1+K,JB1+JFUN)
     &                   -RR(M1,4 )*DENC(IC2+K,JB1+JFUN)
     &                   -RR(M1,6 )*DENC(IC1+K,JB2+JFUN)
     &                   -RR(M1,8 )*DENC(IC2+K,JB2+JFUN)
      FOCK(IA2+IFUN,JD1+L)=FOCK(IA2+IFUN,JD1+L)
     &                   -RR(M1,9 )*DENC(IC1+K,JB1+JFUN)
     &                   -RR(M1,11)*DENC(IC2+K,JB1+JFUN)
     &                   -RR(M1,13)*DENC(IC1+K,JB2+JFUN)
     &                   -RR(M1,15)*DENC(IC2+K,JB2+JFUN)
      FOCK(IA2+IFUN,JD2+L)=FOCK(IA2+IFUN,JD2+L)
     &                   -RR(M1,10)*DENC(IC1+K,JB1+JFUN)
     &                   -RR(M1,12)*DENC(IC2+K,JB1+JFUN)
     &                   -RR(M1,14)*DENC(IC1+K,JB2+JFUN)
     &                   -RR(M1,16)*DENC(IC2+K,JB2+JFUN)
120   CONTINUE
      ENDIF
      IF(IFLG(4).EQ.1) THEN
      F1=RKFAC2*FACCD1
      F2=RKFAC2*FACCD2
      M=0
      DO 130 K=1,NFUNC
      DO 130 L=1,NFUND
      M=M+1
      FOCK(IA1+IFUN,JC1+K)=FOCK(IA1+IFUN,JC1+K)
     &                   -RR(M,4 )*DENC(ID1+L,JB1+JFUN)*F1  
     &                   -RR(M,8 )*DENC(ID1+L,JB2+JFUN)*F1
     &                   -RR(M,3 )*DENC(ID2+L,JB1+JFUN)*F2
     &                   -RR(M,7 )*DENC(ID2+L,JB2+JFUN)*F2
      FOCK(IA1+IFUN,JC2+K)=FOCK(IA1+IFUN,JC2+K)
     &                   -RR(M,2 )*DENC(ID1+L,JB1+JFUN)*F2
     &                   -RR(M,6 )*DENC(ID1+L,JB2+JFUN)*F2
     &                   -RR(M,1 )*DENC(ID2+L,JB1+JFUN)*F1
     &                   -RR(M,5 )*DENC(ID2+L,JB2+JFUN)*F1
      FOCK(IA2+IFUN,JC1+K)=FOCK(IA2+IFUN,JC1+K)
     &                   -RR(M,12)*DENC(ID1+L,JB1+JFUN)*F1
     &                   -RR(M,16)*DENC(ID1+L,JB2+JFUN)*F1
     &                   -RR(M,11)*DENC(ID2+L,JB1+JFUN)*F2
     &                   -RR(M,15)*DENC(ID2+L,JB2+JFUN)*F2
      FOCK(IA2+IFUN,JC2+K)=FOCK(IA2+IFUN,JC2+K)
     &                   -RR(M,10)*DENC(ID1+L,JB1+JFUN)*F2
     &                   -RR(M,14)*DENC(ID1+L,JB2+JFUN)*F2
     &                   -RR(M,9 )*DENC(ID2+L,JB1+JFUN)*F1
     &                   -RR(M,13)*DENC(ID2+L,JB2+JFUN)*F1
130   CONTINUE
      ENDIF
      IF(IFLG(5).EQ.1) THEN
      F1=RKFAC1*FACAB1
      F2=RKFAC1*FACAB2
      M=0
      DO 140 K=1,NFUNC
      DO 140 L=1,NFUND
      M=M+1
      FOCK(IC1+K,JA1+IFUN)=FOCK(IC1+K,JA1+IFUN)
     &                   -RR(M,13)*DENC(JB1+JFUN,ID1+L)*F1
     &                   -RR(M,14)*DENC(JB1+JFUN,ID2+L)*F1
     &                   -RR(M,9 )*DENC(JB2+JFUN,ID1+L)*F2
     &                   -RR(M,10)*DENC(JB2+JFUN,ID2+L)*F2
      FOCK(IC1+K,JA2+IFUN)=FOCK(IC1+K,JA2+IFUN)
     &                   -RR(M,5 )*DENC(JB1+JFUN,ID1+L)*F2
     &                   -RR(M,6 )*DENC(JB1+JFUN,ID2+L)*F2
     &                   -RR(M,1 )*DENC(JB2+JFUN,ID1+L)*F1
     &                   -RR(M,2 )*DENC(JB2+JFUN,ID2+L)*F1
      FOCK(IC2+K,JA1+IFUN)=FOCK(IC2+K,JA1+IFUN)
     &                   -RR(M,15)*DENC(JB1+JFUN,ID1+L)*F1
     &                   -RR(M,16)*DENC(JB1+JFUN,ID2+L)*F1
     &                   -RR(M,11)*DENC(JB2+JFUN,ID1+L)*F2
     &                   -RR(M,12)*DENC(JB2+JFUN,ID2+L)*F2
      FOCK(IC2+K,JA2+IFUN)=FOCK(IC2+K,JA2+IFUN)
     &                   -RR(M,7 )*DENC(JB1+JFUN,ID1+L)*F2
     &                   -RR(M,8 )*DENC(JB1+JFUN,ID2+L)*F2
     &                   -RR(M,3 )*DENC(JB2+JFUN,ID1+L)*F1
     &                   -RR(M,4 )*DENC(JB2+JFUN,ID2+L)*F1
140   CONTINUE
      ENDIF
      IF(IFLG(6).EQ.1) THEN
      F1=RKFAC1*FACAB1
      F2=RKFAC1*FACAB2
      DO 150 L=1,NFUND
      DO 150 K=1,NFUNC
      M=(K-1)*NFUND+L
      FOCK(IB1+JFUN,JD1+L)=FOCK(IB1+JFUN,JD1+L)
     &                   -RR(M,13)*DENC(IC1+K,JA1+IFUN)*F1
     &                   -RR(M,5 )*DENC(IC1+K,JA2+IFUN)*F2
     &                   -RR(M,15)*DENC(IC2+K,JA1+IFUN)*F1
     &                   -RR(M,7 )*DENC(IC2+K,JA2+IFUN)*F2
      FOCK(IB1+JFUN,JD2+L)=FOCK(IB1+JFUN,JD2+L)
     &                   -RR(M,14)*DENC(IC1+K,JA1+IFUN)*F1
     &                   -RR(M,6 )*DENC(IC1+K,JA2+IFUN)*F2
     &                   -RR(M,16)*DENC(IC2+K,JA1+IFUN)*F1
     &                   -RR(M,8 )*DENC(IC2+K,JA2+IFUN)*F2
      FOCK(IB2+JFUN,JD1+L)=FOCK(IB2+JFUN,JD1+L)
     &                   -RR(M,9 )*DENC(IC1+K,JA1+IFUN)*F2
     &                   -RR(M,1 )*DENC(IC1+K,JA2+IFUN)*F1
     &                   -RR(M,11)*DENC(IC2+K,JA1+IFUN)*F2
     &                   -RR(M,3 )*DENC(IC2+K,JA2+IFUN)*F1
      FOCK(IB2+JFUN,JD2+L)=FOCK(IB2+JFUN,JD2+L)
     &                   -RR(M,10)*DENC(IC1+K,JA1+IFUN)*F2
     &                   -RR(M,2 )*DENC(IC1+K,JA2+IFUN)*F1
     &                   -RR(M,12)*DENC(IC2+K,JA1+IFUN)*F2
     &                   -RR(M,4 )*DENC(IC2+K,JA2+IFUN)*F1
150   CONTINUE
      ENDIF
C
      IF(IFLG(7).EQ.1) THEN
      F1=RKFAC2*FACCD1
      F2=RKFAC2*FACCD2
      DO 160 L=1,NFUND
      DO 160 K=1,NFUNC
      M=(K-1)*NFUND+L
      FOCK(ID1+L,JB1+JFUN)=FOCK(ID1+L,JB1+JFUN)
     &                   -RR(M,4 )*DENC(JA1+IFUN,IC1+K)*F1
     &                   -RR(M,2 )*DENC(JA1+IFUN,IC2+K)*F2
     &                   -RR(M,12)*DENC(JA2+IFUN,IC1+K)*F1
     &                   -RR(M,10)*DENC(JA2+IFUN,IC2+K)*F2
      FOCK(ID1+L,JB2+JFUN)=FOCK(ID1+L,JB2+JFUN)
     &                   -RR(M,8 )*DENC(JA1+IFUN,IC1+K)*F1
     &                   -RR(M,6 )*DENC(JA1+IFUN,IC2+K)*F2
     &                   -RR(M,16)*DENC(JA2+IFUN,IC1+K)*F1
     &                   -RR(M,14)*DENC(JA2+IFUN,IC2+K)*F2
      FOCK(ID2+L,JB1+JFUN)=FOCK(ID2+L,JB1+JFUN)
     &                   -RR(M,3 )*DENC(JA1+IFUN,IC1+K)*F2
     &                   -RR(M,1 )*DENC(JA1+IFUN,IC2+K)*F1
     &                   -RR(M,11)*DENC(JA2+IFUN,IC1+K)*F2
     &                   -RR(M,9 )*DENC(JA2+IFUN,IC2+K)*F1
      FOCK(ID2+L,JB2+JFUN)=FOCK(ID2+L,JB2+JFUN)
     &                   -RR(M,7 )*DENC(JA1+IFUN,IC1+K)*F2
     &                   -RR(M,5 )*DENC(JA1+IFUN,IC2+K)*F1
     &                   -RR(M,15)*DENC(JA2+IFUN,IC1+K)*F2
     &                   -RR(M,13)*DENC(JA2+IFUN,IC2+K)*F1
160   CONTINUE
      ENDIF
      IF(IFLG(8).EQ.1) THEN
      F11=RKFAC1*RKFAC2*FACAB1*FACCD1
      F12=RKFAC1*RKFAC2*FACAB1*FACCD2
      F21=RKFAC1*RKFAC2*FACAB2*FACCD1
      F22=RKFAC1*RKFAC2*FACAB2*FACCD2
      M=0
      DO 170 K=1,NFUNC
      DO 170 L=1,NFUND
      M=M+1
      FOCK(IB1+JFUN,JC1+K)=FOCK(IB1+JFUN,JC1+K)
     &                   -RR(M,16)*DENC(ID1+L,JA1+IFUN)*F11
     &                   -RR(M,8 )*DENC(ID1+L,JA2+IFUN)*F21
     &                   -RR(M,15)*DENC(ID2+L,JA1+IFUN)*F12
     &                   -RR(M,7 )*DENC(ID2+L,JA2+IFUN)*F22
      FOCK(IB1+JFUN,JC2+K)=FOCK(IB1+JFUN,JC2+K)
     &                   -RR(M,14)*DENC(ID1+L,JA1+IFUN)*F12
     &                   -RR(M,6 )*DENC(ID1+L,JA2+IFUN)*F22
     &                   -RR(M,13)*DENC(ID2+L,JA1+IFUN)*F11
     &                   -RR(M,5 )*DENC(ID2+L,JA2+IFUN)*F21
      FOCK(IB2+JFUN,JC1+K)=FOCK(IB2+JFUN,JC1+K)
     &                   -RR(M,12)*DENC(ID1+L,JA1+IFUN)*F21
     &                   -RR(M,4 )*DENC(ID1+L,JA2+IFUN)*F11
     &                   -RR(M,11)*DENC(ID2+L,JA1+IFUN)*F22
     &                   -RR(M,3 )*DENC(ID2+L,JA2+IFUN)*F12
      FOCK(IB2+JFUN,JC2+K)=FOCK(IB2+JFUN,JC2+K)
     &                   -RR(M,10)*DENC(ID1+L,JA1+IFUN)*F22
     &                   -RR(M,2 )*DENC(ID1+L,JA2+IFUN)*F12
     &                   -RR(M,9 )*DENC(ID2+L,JA1+IFUN)*F21
     &                   -RR(M,1 )*DENC(ID2+L,JA2+IFUN)*F11
170   CONTINUE
      ENDIF
C
      IF(IFLG(9).EQ.1) THEN
      M=0
      DO 180 K=1,NFUNC
      DO 180 L=1,NFUND
      M=M+1
      FOCK(IC1+K,JB1+JFUN)=FOCK(IC1+K,JB1+JFUN)
     &                   -RR(M,1 )*DENC(JA1+IFUN,ID1+L)
     &                   -RR(M,2 )*DENC(JA1+IFUN,ID2+L)
     &                   -RR(M,9 )*DENC(JA2+IFUN,ID1+L)
     &                   -RR(M,10)*DENC(JA2+IFUN,ID2+L)
      FOCK(IC1+K,JB2+JFUN)=FOCK(IC1+K,JB2+JFUN)
     &                   -RR(M,5 )*DENC(JA1+IFUN,ID1+L)
     &                   -RR(M,6 )*DENC(JA1+IFUN,ID2+L)
     &                   -RR(M,13)*DENC(JA2+IFUN,ID1+L)
     &                   -RR(M,14)*DENC(JA2+IFUN,ID2+L)
      FOCK(IC2+K,JB1+JFUN)=FOCK(IC2+K,JB1+JFUN)
     &                   -RR(M,3 )*DENC(JA1+IFUN,ID1+L)
     &                   -RR(M,4 )*DENC(JA1+IFUN,ID2+L)
     &                   -RR(M,11)*DENC(JA2+IFUN,ID1+L)
     &                   -RR(M,12)*DENC(JA2+IFUN,ID2+L)
      FOCK(IC2+K,JB2+JFUN)=FOCK(IC2+K,JB2+JFUN)
     &                   -RR(M,7 )*DENC(JA1+IFUN,ID1+L)
     &                   -RR(M,8 )*DENC(JA1+IFUN,ID2+L)
     &                   -RR(M,15)*DENC(JA2+IFUN,ID1+L)
     &                   -RR(M,16)*DENC(JA2+IFUN,ID2+L)
180   CONTINUE
      ENDIF
C
      IF(IFLG(10).EQ.1) THEN
      M=0
      DO 190 K=1,NFUNC
      DO 190 L=1,NFUND
      M=M+1
      FOCK(IA1+IFUN,JB1+JFUN)=FOCK(IA1+IFUN,JB1+JFUN)
     &                 +RR(M,1 )*DENC(IC1+K,JD1+L)
     &                 +RR(M,2 )*DENC(IC1+K,JD2+L)
     &                 +RR(M,3 )*DENC(IC2+K,JD1+L)
     &                 +RR(M,4 )*DENC(IC2+K,JD2+L)
      FOCK(IA1+IFUN,JB2+JFUN)=FOCK(IA1+IFUN,JB2+JFUN)
     &                 +RR(M,5 )*DENC(IC1+K,JD1+L)
     &                 +RR(M,6 )*DENC(IC1+K,JD2+L)
     &                 +RR(M,7 )*DENC(IC2+K,JD1+L)
     &                 +RR(M,8 )*DENC(IC2+K,JD2+L)
      FOCK(IA2+IFUN,JB1+JFUN)=FOCK(IA2+IFUN,JB1+JFUN)
     &                 +RR(M,9 )*DENC(IC1+K,JD1+L)
     &                 +RR(M,10)*DENC(IC1+K,JD2+L)
     &                 +RR(M,11)*DENC(IC2+K,JD1+L)
     &                 +RR(M,12)*DENC(IC2+K,JD2+L)
      FOCK(IA2+IFUN,JB2+JFUN)=FOCK(IA2+IFUN,JB2+JFUN)
     &                 +RR(M,13)*DENC(IC1+K,JD1+L)
     &                 +RR(M,14)*DENC(IC1+K,JD2+L)
     &                 +RR(M,15)*DENC(IC2+K,JD1+L)
     &                 +RR(M,16)*DENC(IC2+K,JD2+L)
190   CONTINUE
      ENDIF
C
      IF(IFLG(11).EQ.1) THEN
      M=0
      DO 200 K=1,NFUNC
      DO 200 L=1,NFUND
      M=M+1
      FOCK(IC1+K,JD1+L)=FOCK(IC1+K,JD1+L)
     &                 +RR(M,1 )*DENC(IA1+IFUN,JB1+JFUN)
     &                 +RR(M,5 )*DENC(IA1+IFUN,JB2+JFUN)
     &                 +RR(M,9 )*DENC(IA2+IFUN,JB1+JFUN)
     &                 +RR(M,13)*DENC(IA2+IFUN,JB2+JFUN)
      FOCK(IC1+K,JD2+L)=FOCK(IC1+K,JD2+L)
     &                 +RR(M,2 )*DENC(IA1+IFUN,JB1+JFUN)
     &                 +RR(M,6 )*DENC(IA1+IFUN,JB2+JFUN)
     &                 +RR(M,10)*DENC(IA2+IFUN,JB1+JFUN)
     &                 +RR(M,14)*DENC(IA2+IFUN,JB2+JFUN)
      FOCK(IC2+K,JD1+L)=FOCK(IC2+K,JD1+L)
     &                 +RR(M,3 )*DENC(IA1+IFUN,JB1+JFUN)
     &                 +RR(M,7 )*DENC(IA1+IFUN,JB2+JFUN)
     &                 +RR(M,11)*DENC(IA2+IFUN,JB1+JFUN)
     &                 +RR(M,15)*DENC(IA2+IFUN,JB2+JFUN)
      FOCK(IC2+K,JD2+L)=FOCK(IC2+K,JD2+L)
     &                 +RR(M,4 )*DENC(IA1+IFUN,JB1+JFUN)
     &                 +RR(M,8 )*DENC(IA1+IFUN,JB2+JFUN)
     &                 +RR(M,12)*DENC(IA2+IFUN,JB1+JFUN)
     &                 +RR(M,16)*DENC(IA2+IFUN,JB2+JFUN)
200   CONTINUE
      ENDIF
C
C
3000  CONTINUE
4001  CONTINUE
4000  CONTINUE
1999  CONTINUE
1000  CONTINUE
2000  CONTINUE
C
C
      DO 800 J=1,NSHIFT
      DO 800 I=1,J
C
      IF((ICNLAB(I).NE.ICNLAB(J)).OR.
     &   (KAPLAB(I).NE.KAPLAB(J)).OR.
     &   (IMLAB(I).NE.IMLAB(J))) THEN
C
      FOCK(I,J)=FOCK(I,J)+DCONJG(FOCK(J,I))
      FOCK(J,I)=DCONJG(FOCK(I,J))
C
      FOCK(I,J+NSHIFT)=FOCK(I,J+NSHIFT)+DCONJG(FOCK(J+NSHIFT,I))
      FOCK(J+NSHIFT,I)=DCONJG(FOCK(I,J+NSHIFT))
C
      FOCK(I+NSHIFT,J)=FOCK(I+NSHIFT,J)+DCONJG(FOCK(J,I+NSHIFT))
      FOCK(J,I+NSHIFT)=DCONJG(FOCK(I+NSHIFT,J))
C
      FOCK(I+NSHIFT,J+NSHIFT)=FOCK(I+NSHIFT,J+NSHIFT)
     &                       +DCONJG(FOCK(J+NSHIFT,I+NSHIFT))
      FOCK(J+NSHIFT,I+NSHIFT)=DCONJG(FOCK(I+NSHIFT,J+NSHIFT))
C
      ENDIF
C
800   CONTINUE
C
      ETOTAL=0.0D0
      DO 680 I=1,NDIM
      DO 680 J=1,NDIM
      ETOTAL=ETOTAL+DENC(I,J)*FOCK(I,J)
680   CONTINUE
      ETOTAL=ETOTAL*5.0D-1
C
C     
      RETURN
      END
C
C********************************************************************
C
      SUBROUTINE ERI(RINTG,XYZ,KAPPA,MQN,EXPT,NFUNS,ITQN,
     &      IFUN,JFUN,IEMAKE,ITYPE,INUCAB,INUCCD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXE=165,MAXR=969,IL4=24)
      PARAMETER(MAXLQN=4,MABLL=MAXE)
C****************************************************************C
C               ERI: ELECTRON REPULSION INTEGRALS                C
C                                                                C
C     ERI GENERATES BLOCKS OF ELECTRON REPULSION INTEGRALS       C
C     OVER KINETICALLY BALANCED G-SPINOR BASIS FUNCTIONS         C
C                                                                C
C     THE DENSITIES ARE EXPANDED IN A BASIS OF HERMITE GAUSSIANS C
C     AND THE INTEGRALS ARE GENERATED USING THE MCMURCHIE-       C
C     DAVIDSON ALGORITHM                                         C
C                                                                C
C                                                                C
C                      INPUT PARAMETERS                          C
C                                                                C
C     XYZ(3,4)    COORDINATES OF THE 4 NUCLEAR CENTRES           C
C     KAPPA(4)    KAPPA QUANTUM NUMBERS OF THE CENTRES           C
C     MQN(4)      |M|   QUANTUM NUMBERS OF THE CENTRES           C
C     EXPT(I,J)   EXPONENTS ON CENTRE J                          C
C     NFUNS(J)    NUMBER OF FUNCTIONS ON CENTRE J                C
C     ITQN(2)     COMPONENT PAIRS: ITQN(I)=1 - > LL              C
C                                  ITQN(I)=2 - > LS              C
C                                  ITQN(I)=3 - > SL              C
C                                  ITQN(I)=4 - > SS              C
C                                  I=1       - > AB              C
C                                  I=2       - > CD              C
C     I,J         INDEX FOR BASIS FUNCTION PAIR ON AB            C
C     IEMAKE      IEMAKE=0 DON'T RECALCULATE E-COEFFICIENTS      C
C                 IEMAKE=1 DO    RECALCULATE E-COEFFICIENTS      C
C     ITYPE       ITYPE=1 IS J-TYPE INTEGRAL                     C
C                 ITYPE=2 IS K-TYPE INTEGRAL                     C
C                                                                C
C****************************************************************C
C
      COMPLEX*16 RINTG(MAXB2,16)
C
      DIMENSION RR(MAXB2,16),RI(MAXB2,16),XYZ(3,4),RC(MAXB2,MAXR),
     & PQ(MAXB2,3),EXPT(MAXB,4),ALPHA(MAXB2),PREFAC(MAXB2)
      DIMENSION KAPPA(4),LQN(4),MQN(4),ITQN(2),NFUNS(4)
      DIMENSION  GRAB11(MAXB2,MAXE),GIAB11(MAXB2,MAXE),
     &           GRAB21(MAXB2,MAXE),GIAB21(MAXB2,MAXE)
      DIMENSION QR1(MAXB2),QR2(MAXB2),QI1(MAXB2),QI2(MAXB2) 
C
      COMMON/ESAVE/ERAB11(MAXB2,0:MAXE),EIAB11(MAXB2,0:MAXE),
     &             ERAB21(MAXB2,0:MAXE),EIAB21(MAXB2,0:MAXE),
     &             ERCD11(MAXB2,0:MAXE),EICD11(MAXB2,0:MAXE),
     &             ERCD21(MAXB2,0:MAXE),EICD21(MAXB2,0:MAXE)
      COMMON/COMP/IABR11(MAXE),IABR21(MAXE),
     &            IABI11(MAXE),IABI21(MAXE),
     &            ICDR11(MAXE),ICDR21(MAXE),
     &            ICDI11(MAXE),ICDI21(MAXE),
     &            ITAB(MAXE),JTAB(MAXE),KTAB(MAXE),
     &            ITCD(MAXE),JTCD(MAXE),KTCD(MAXE),
     &            IZAB(MAXE),IZCD(MAXE),IRC(MAXR)
      SAVE LAMAB,LAMCD
      COMMON/TERI/ERIT
      COMMON/NUCLEI/NCENTR
      COMMON/RNORM/RNLL(MAXB2),RNSL(MAXB2),RNLS(MAXB2),RNSS(MAXB2),
     #EXPA(MAXB2),EXPB(MAXB2),EXPAB(MAXB2)
      COMMON/INDSYS/INABCD(0:4*MAXLQN,0:4*MAXLQN,0:4*MAXLQN),
     &              IVEC(MABLL),JVEC(MABLL),KVEC(MABLL)
      DATA ROOTPI/1.7724538509055160D0/
C
      PIFACT=2.0D0*(ROOTPI**5)
      DO 1 L=1,4
      IF(KAPPA(L).LT.0) THEN
       LQN(L)=-KAPPA(L)-1
      ELSE
       LQN(L)=KAPPA(L)
      ENDIF
1     CONTINUE
C      
C****************************************************************C
C
      IF(IEMAKE.EQ.1) THEN
C
C****************************************************************C
C     GENERATE NEW E-COEFFICIENTS FOR THIS CASE                  C
C****************************************************************C
C
C     GENERATE J-TYPE COEFFICIENTS
C
      IF(ITQN(1).EQ.1.AND.ITQN(2).EQ.1) THEN
      LAMAB=LQN(1)+LQN(2)
      LAMCD=LQN(3)+LQN(4)
      ELSEIF(ITQN(1).EQ.1.AND.ITQN(2).EQ.4) THEN
      LAMAB=LQN(1)+LQN(2)
      LAMCD=LQN(3)+LQN(4)+2
      ELSEIF(ITQN(1).EQ.4.AND.ITQN(2).EQ.1) THEN
      LAMAB=LQN(1)+LQN(2)+2
      LAMCD=LQN(3)+LQN(4)
      ELSEIF(ITQN(1).EQ.4.AND.ITQN(2).EQ.4) THEN
      LAMAB=LQN(1)+LQN(2)+2
      LAMCD=LQN(3)+LQN(4)+2
      ELSE
       WRITE(6,*) 'INCORRECT CALL TO ERI'
       STOP 998
      ENDIF
C
      CALL RNORMF(EXPT,LQN,NFUNS,1,2)
      IALT=1
      IF (ITQN(1).EQ.1) THEN
      IF(INUCAB.EQ.2) THEN
      CALL EMAKELL(ERAB11,EIAB11,ERAB21,EIAB21,
     #             KAPPA,MQN,NFUNS,IALT,1,2)
      ELSE
      CALL OEMAKELL(ERAB11,EIAB11,ERAB21,EIAB21,
     #             KAPPA,MQN,NFUNS,IALT,1,2)
      ENDIF
      ELSEIF(ITQN(1).EQ.4) THEN                         
      IF(INUCAB.EQ.2) THEN
      CALL EMAKESS(ERAB11,EIAB11,ERAB21,EIAB21,
     #             KAPPA,MQN,NFUNS,IALT,1,2)
      ELSE
      CALL OEMAKESS(ERAB11,EIAB11,ERAB21,EIAB21,
     #             KAPPA,MQN,NFUNS,IALT,1,2)
      ENDIF
      ENDIF
C
      CALL RNORMF(EXPT,LQN,NFUNS,3,4)
      IALT=-1
      IF (ITQN(2).EQ.1) THEN
      IF(INUCCD.EQ.2) THEN
      CALL EMAKELL(ERCD11,EICD11,ERCD21,EICD21,
     #             KAPPA,MQN,NFUNS,IALT,3,4)
      ELSE
      CALL OEMAKELL(ERCD11,EICD11,ERCD21,EICD21,
     #             KAPPA,MQN,NFUNS,IALT,3,4)
      ENDIF
      ELSEIF(ITQN(2).EQ.4) THEN
      IF(INUCCD.EQ.2) THEN
      CALL EMAKESS(ERCD11,EICD11,ERCD21,EICD21,
     #             KAPPA,MQN,NFUNS,IALT,3,4)
      ELSE
      CALL OEMAKESS(ERCD11,EICD11,ERCD21,EICD21,
     #             KAPPA,MQN,NFUNS,IALT,3,4)
      ENDIF
      ENDIF
C
C     COMPUTE THE INDICES OF NON-ZERO ENTRIES
C
      MAXAB=NFUNS(1)*NFUNS(2)
      MAXCD=NFUNS(3)*NFUNS(4)
      IABLAM=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      ICDLAM=((LAMCD+1)*(LAMCD+2)*(LAMCD+3))/6
C
C     AB PAIRS
C
C**********************************************************************C
C     AB PAIRS                                                         C
C**********************************************************************C
      DO 201 IAB=1,IABLAM
C
C     REAL(1,1)
C
      TEST=F06EKF(MAXAB,ERAB11(1,IAB),1)
      IF(TEST.LE.1.0D-14) THEN
       IABR11(IAB)=0
      ELSE
       IABR11(IAB)=1
      ENDIF
C     
C     IMAGINARY (1,1)
C
      TEST=F06EKF(MAXAB,EIAB11(1,IAB),1)
      IF(TEST.LE.1.0D-14) THEN
       IABI11(IAB)=0
      ELSE
       IABI11(IAB)=1
      ENDIF
C
C     REAL(2,1)
C
      TEST=F06EKF(MAXAB,ERAB21(1,IAB),1)
      IF(TEST.LE.1.0D-14) THEN
       IABR21(IAB)=0
      ELSE
       IABR21(IAB)=1
      ENDIF
C     
C     IMAGINARY (2,1)
C
      TEST=F06EKF(MAXAB,EIAB21(1,IAB),1)
      IF(TEST.LE.1.0D-14) THEN
       IABI21(IAB)=0
      ELSE
       IABI21(IAB)=1
      ENDIF
201   CONTINUE
C
C     CD PAIRS
C
      DO 203 ICD=1,ICDLAM
C
C     REAL(1,1)
C
      TEST=F06EKF(MAXCD,ERCD11(1,ICD),1)
      IF(TEST.LE.1.0D-14) THEN
       ICDR11(ICD)=0
      ELSE
       ICDR11(ICD)=1
      ENDIF
C     
C     IMAGINARY (1,1)
C
      TEST=F06EKF(MAXCD,EICD11(1,ICD),1)
      IF(TEST.LE.1.0D-14) THEN
       ICDI11(ICD)=0
      ELSE
       ICDI11(ICD)=1
      ENDIF
C
C     REAL(2,1)
C
      TEST=F06EKF(MAXCD,ERCD21(1,ICD),1)
      IF(TEST.LE.1.0D-14) THEN
       ICDR21(ICD)=0
      ELSE
       ICDR21(ICD)=1
      ENDIF
C     
C     IMAGINARY (2,1)
C
      TEST=F06EKF(MAXCD,EICD21(1,ICD),1)
      IF(TEST.LE.1.0D-14) THEN
       ICDI21(ICD)=0
      ELSE
       ICDI21(ICD)=1
      ENDIF
203   CONTINUE
C
      IEMAKE=0
C
      ENDIF
C
C****************************************************************C
C     EVALUATE GEOMETRIC PARAMETERS FOR THE R-INTEGRALS          C
C****************************************************************C
C
      IABLAM=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      ICDLAM=((LAMCD+1)*(LAMCD+2)*(LAMCD+3))/6
      EIJ=EXPT(IFUN,1)+EXPT(JFUN,2)
      PX=(XYZ(1,1)*EXPT(IFUN,1)+XYZ(1,2)*EXPT(JFUN,2))/EIJ
      PY=(XYZ(2,1)*EXPT(IFUN,1)+XYZ(2,2)*EXPT(JFUN,2))/EIJ
      PZ=(XYZ(3,1)*EXPT(IFUN,1)+XYZ(3,2)*EXPT(JFUN,2))/EIJ
      M=0
      DO 10 K=1,NFUNS(3)
      DO 10 L=1,NFUNS(4)
      M=M+1
      EKL=EXPT(K,3)+EXPT(L,4)
      QX=(XYZ(1,3)*EXPT(K,3)+XYZ(1,4)*EXPT(L,4))/EKL
      QY=(XYZ(2,3)*EXPT(K,3)+XYZ(2,4)*EXPT(L,4))/EKL
      QZ=(XYZ(3,3)*EXPT(K,3)+XYZ(3,4)*EXPT(L,4))/EKL
      ALPHA(M)=(EIJ*EKL)/(EIJ+EKL)
      PQ(M,1)=QX-PX
      PQ(M,2)=QY-PY
      PQ(M,3)=QZ-PZ
      PREFAC(M)=PIFACT/(DSQRT(EIJ+EKL)*EIJ*EKL)
10    CONTINUE
      MAXM=NFUNS(3)*NFUNS(4)
C
      CALL RMAKE(RC,PQ,ALPHA,MAXM,LAMAB+LAMCD)
C
C     INITIALIZE ARRAY TO IMPLEMENT SPARSENESS IN R-VECTOR
C
      LABCD=LAMAB+LAMCD
      NABCD=((LABCD+1)*(LABCD+2)*(LABCD+3))/6
      DO 11 MRC=1,NABCD
      TEST=F06EKF(MAXM,RC(1,MRC),1)
      IF(TEST.LE.1.0D-14) THEN
       IRC(MRC)=0
      ELSE
       IRC(MRC)=1
      ENDIF
11    CONTINUE     
C
C
C****************************************************************C
C     CONSTRUCT INTERMEDIATE MATRICES FOR MCMURCHIE-DAVIDSON     C
C****************************************************************C
C
      DO 799 IAB=1,IABLAM
C
C----------------------------------------------------------------C
C
      IAB1=0
      IAB2=0
      IAB3=0
      IAB4=0
C
      IF(IABR11(IAB).EQ.1) THEN
      IAB1=1
      ENDIF
C
      IF(IABI11(IAB).EQ.1) THEN
      IAB2=1
      ENDIF
C
      IF(IABR21(IAB).EQ.1) THEN
      IAB3=1
      ENDIF
C
      IF(IABI21(IAB).EQ.1) THEN
      IAB4=1
      ENDIF
C----------------------------------------------------------------C
C
C
      IF((IAB1+IAB2+IAB3+IAB4).GT.0) THEN
      DO 703 M=1,MAXM
      GRAB11(M,IAB)=0.0D0
      GIAB11(M,IAB)=0.0D0
      GRAB21(M,IAB)=0.0D0
      GIAB21(M,IAB)=0.0D0
703   CONTINUE
      ENDIF
C
C
      DO 799 ICD=1,ICDLAM
      IRABCD=INABCD(IVEC(IAB)+IVEC(ICD),
     &       JVEC(IAB)+JVEC(ICD),KVEC(IAB)+KVEC(ICD))
C
      IF(IRC(IRABCD).EQ.0) GO TO 798
C
      IF(ICDR11(ICD).EQ.1) THEN
      DO 7001 M=1,MAXM
      GRAB11(M,IAB)=GRAB11(M,IAB)+ERCD11(M,ICD)*RC(M,IRABCD)
7001  CONTINUE
      ENDIF
      IF(ICDI11(ICD).EQ.1) THEN
      DO 7002 M=1,MAXM
      GIAB11(M,IAB)=GIAB11(M,IAB)+EICD11(M,ICD)*RC(M,IRABCD)
7002  CONTINUE
      ENDIF
      IF(ICDR21(ICD).EQ.1) THEN
      DO 7005 M=1,MAXM
      GRAB21(M,IAB)=GRAB21(M,IAB)+ERCD21(M,ICD)*RC(M,IRABCD)
7005  CONTINUE
      ENDIF
      IF(ICDI21(ICD).EQ.1) THEN
      DO 7006 M=1,MAXM
      GIAB21(M,IAB)=GIAB21(M,IAB)+EICD21(M,ICD)*RC(M,IRABCD)
7006  CONTINUE
      ENDIF
C 
798   CONTINUE
799   CONTINUE
C
C****************************************************************C
C     GENERATE ALL POSSIBLE TWO-ELECTRON INTEGRALS FROM THE      C
C     EAB COEFFICIENTS AND THE G-ARRAYS                          C
C****************************************************************C
C
      PHASE1=DFLOAT((-1)**((MQN(1)-MQN(2))/2))
      PHASE2=DFLOAT((-1)**((MQN(3)-MQN(4))/2))
      PHASE1=PHASE1*DFLOAT((KAPPA(1)*KAPPA(2))/
     &                 IABS(KAPPA(1)*KAPPA(2)))
      PHASE2=PHASE2*DFLOAT((KAPPA(3)*KAPPA(4))/
     &                IABS(KAPPA(3)*KAPPA(4)))
      PHASE3=PHASE1*PHASE2
C
C****************************************************************C
C
C     Integral : ( - - || - - )
C
      NAB=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      IJ=(IFUN-1)*NFUNS(2)+JFUN
C
      DO 1100 M=1,MAXM
      QR1(M)=0.0D0
      QI1(M)=0.0D0
      QR2(M)=0.0D0
      QI2(M)=0.0D0
1100  CONTINUE
C
      DO 1110 IAB=1,NAB
C
      IF(IABR11(IAB).EQ.1) THEN
      DO 1111 M=1,MAXM
      QR1(M)=QR1(M)+ERAB11(IJ,IAB)*GRAB11(M,IAB)
      QI2(M)=QI2(M)+ERAB11(IJ,IAB)*GIAB11(M,IAB)
1111  CONTINUE
      ENDIF
C
      IF(IABI11(IAB).EQ.1) THEN
      DO 1112 M=1,MAXM
      QR2(M)=QR2(M)-EIAB11(IJ,IAB)*GIAB11(M,IAB)
      QI1(M)=QI1(M)+EIAB11(IJ,IAB)*GRAB11(M,IAB)
1112  CONTINUE
      ENDIF
C
1110  CONTINUE
C
C
      DO 1115 M=1,MAXM
      RR(M,1)=(QR1(M)+QR2(M))*PREFAC(M)
      RI(M,1)=(QI1(M)+QI2(M))*PREFAC(M)
      RR(M,4)=PHASE2*(QR1(M)-QR2(M))*PREFAC(M)
      RI(M,4)=PHASE2*(QI1(M)-QI2(M))*PREFAC(M)
      RR(M,13)= PHASE3*RR(M,4)
      RI(M,13)=-PHASE3*RI(M,4)
      RR(M,16)= PHASE3*RR(M,1)
      RI(M,16)=-PHASE3*RI(M,1)
1115  CONTINUE
C
C****************************************************************C
C
C     Integral : ( - - || + - )
C
      NAB=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      IJ=(IFUN-1)*NFUNS(2)+JFUN
C
      DO 1200 M=1,MAXM
      QR1(M)=0.0D0
      QI1(M)=0.0D0
      QR2(M)=0.0D0
      QI2(M)=0.0D0
1200  CONTINUE
C
      DO 1210 IAB=1,NAB
C
      IF(IABR11(IAB).EQ.1) THEN
      DO 1211 M=1,MAXM
      QR1(M)=QR1(M)+ERAB11(IJ,IAB)*GRAB21(M,IAB)
      QI2(M)=QI2(M)+ERAB11(IJ,IAB)*GIAB21(M,IAB)
1211  CONTINUE
      ENDIF
C
      IF(IABI11(IAB).EQ.1) THEN
      DO 1212 M=1,MAXM
      QR2(M)=QR2(M)-EIAB11(IJ,IAB)*GIAB21(M,IAB)
      QI1(M)=QI1(M)+EIAB11(IJ,IAB)*GRAB21(M,IAB)
1212  CONTINUE
      ENDIF
C
1210  CONTINUE
C
C
      DO 1215 M=1,MAXM
      RR(M,3)=(QR1(M)+QR2(M))*PREFAC(M)
      RI(M,3)=(QI1(M)+QI2(M))*PREFAC(M)
      RR(M,2)=-PHASE2*(QR1(M)-QR2(M))*PREFAC(M)
      RI(M,2)=-PHASE2*(QI1(M)-QI2(M))*PREFAC(M)
      RR(M,15)=-PHASE3*RR(M,2)
      RI(M,15)=+PHASE3*RI(M,2)
      RR(M,14)=-PHASE3*RR(M,3)
      RI(M,14)=+PHASE3*RI(M,3)
1215  CONTINUE
C
C****************************************************************C
C
C     Integral : ( + - || - - )
C
      NAB=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      IJ=(IFUN-1)*NFUNS(2)+JFUN
C
      DO 1300 M=1,MAXM
      QR1(M)=0.0D0
      QI1(M)=0.0D0
      QR2(M)=0.0D0
      QI2(M)=0.0D0
1300  CONTINUE
C
      DO 1310 IAB=1,NAB
C
      IF(IABR21(IAB).EQ.1) THEN
      DO 1311 M=1,MAXM
      QR1(M)=QR1(M)+ERAB21(IJ,IAB)*GRAB11(M,IAB)
      QI2(M)=QI2(M)+ERAB21(IJ,IAB)*GIAB11(M,IAB)
1311  CONTINUE
      ENDIF
C
      IF(IABI21(IAB).EQ.1) THEN
      DO 1312 M=1,MAXM
      QR2(M)=QR2(M)-EIAB21(IJ,IAB)*GIAB11(M,IAB)
      QI1(M)=QI1(M)+EIAB21(IJ,IAB)*GRAB11(M,IAB)
1312  CONTINUE
      ENDIF
C
1310  CONTINUE
C
C
      DO 1315 M=1,MAXM
      RR(M,9)=(QR1(M)+QR2(M))*PREFAC(M)
      RI(M,9)=(QI1(M)+QI2(M))*PREFAC(M)
      RR(M,12)=PHASE2*(QR1(M)-QR2(M))*PREFAC(M)
      RI(M,12)=PHASE2*(QI1(M)-QI2(M))*PREFAC(M)
      RR(M,5)=-PHASE3*RR(M,12)
      RI(M,5)=+PHASE3*RI(M,12)
      RR(M,8)=-PHASE3*RR(M,9)
      RI(M,8)=+PHASE3*RI(M,9)
1315  CONTINUE
C
C****************************************************************C
C
C     Integral : ( + - || + - )
C
      NAB=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      IJ=(IFUN-1)*NFUNS(2)+JFUN
C
      DO 1400 M=1,MAXM
      QR1(M)=0.0D0
      QI1(M)=0.0D0
      QR2(M)=0.0D0
      QI2(M)=0.0D0
1400  CONTINUE
C
      DO 1410 IAB=1,NAB
C
      IF(IABR21(IAB).EQ.1) THEN
      DO 1411 M=1,MAXM
      QR1(M)=QR1(M)+ERAB21(IJ,IAB)*GRAB21(M,IAB)
      QI2(M)=QI2(M)+ERAB21(IJ,IAB)*GIAB21(M,IAB)
1411  CONTINUE
      ENDIF
C
      IF(IABI21(IAB).EQ.1) THEN
      DO 1412 M=1,MAXM
      QR2(M)=QR2(M)-EIAB21(IJ,IAB)*GIAB21(M,IAB)
      QI1(M)=QI1(M)+EIAB21(IJ,IAB)*GRAB21(M,IAB)
1412  CONTINUE
      ENDIF
C
1410  CONTINUE
C
C
      DO 1415 M=1,MAXM
      RR(M,11)=(QR1(M)+QR2(M))*PREFAC(M)
      RI(M,11)=(QI1(M)+QI2(M))*PREFAC(M)
      RR(M,10)=-PHASE2*(QR1(M)-QR2(M))*PREFAC(M)
      RI(M,10)=-PHASE2*(QI1(M)-QI2(M))*PREFAC(M)
      RR(M,7)= PHASE3*RR(M,10)
      RI(M,7)=-PHASE3*RI(M,10)
      RR(M,6)= PHASE3*RR(M,11)
      RI(M,6)=-PHASE3*RI(M,11)
1415  CONTINUE
C
C----------------------------------------------------------------C
C
C     WRITE INTEGRALS INTO COMPLEX INTEGRAL ARRAY
C
      DO 1515 ITG=1,16
      DO 1515 M=1,MAXM
      RINTG(M,ITG)=DCMPLX(RR(M,ITG),RI(M,ITG))
1515  CONTINUE
C----------------------------------------------------------------C
C
      RETURN
      END
C****************************************************************C
C
      SUBROUTINE ONEEL   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXDIM=300,NKAPM=7,
     #          MAXR=969,MLL=165,NCENTM=6,MAXMV=5,MAXMV2=2*MAXMV,
     #          ZERO=0.0D0,
     #          PI=3.1415926535898D0,
     #          TWOPI=6.2831853071796D0,
     #          HALF=5.0D-1)
      PARAMETER(MAXLQN=4,MABLL=MLL)
C****************************************************************C
C     FOCK GENERATES THE FOCK MATRIX FROM THE MOLECULAR          C
C     DENSITIES AND INTEGRALS GENERATED BY THE DIRECT G-SPINOR   C
C     PROCEDURE ERI                                              C
C****************************************************************C
      COMPLEX*16 FOCK,OVAP
      COMPLEX*16 C
      COMPLEX*16 CZERO
C
      COMMON/SPEC/COORD(3,NCENTM),ZNUC(NCENTM),CNUC(NCENTM),
     &  EXPSET(MAXB,NKAPM,NCENTM),IZNUC(NCENTM),ICRGE(NCENTM),
     &  KVALS(NKAPM,NCENTM),NKAPPA(NCENTM),LMAXX(NCENTM),
     &  NFUNCT(NKAPM,NCENTM),NCENT,NDIM,NSHIFT,
     &  NOCC,ITER,IALL,IRUN
      COMMON/FMAT/FOCK(MAXDIM,MAXDIM),OVAP(MAXDIM,MAXDIM)
      COMMON/COEFF/C(MAXDIM,MAXDIM)
      COMMON/LABELS/LARGE(NCENTM,NKAPM,MAXMV2)
      COMMON/ENERGY/ETOTAL
      COMMON/LOCGEO/XYZ(3,4)
      COMMON/INDSYS/INABCD(0:4*MAXLQN,0:4*MAXLQN,0:4*MAXLQN),
     &              IVEC(MABLL),JVEC(MABLL),KVEC(MABLL)
C
      DIMENSION RC(MAXB2,MAXR)
      DIMENSION ER11(MAXB2,0:MLL),EI11(MAXB2,0:MLL),
     #          ER21(MAXB2,0:MLL),EI21(MAXB2,0:MLL)
      COMPLEX*16 DLL11(MAXB2),DLL12(MAXB2),
     &          DLL21(MAXB2),DLL22(MAXB2),
     &          DLS11(MAXB2),DLS12(MAXB2),
     &          DLS21(MAXB2),DLS22(MAXB2),
     &          DSL11(MAXB2),DSL12(MAXB2),
     &          DSL21(MAXB2),DSL22(MAXB2),
     &          DSS11(MAXB2),DSS12(MAXB2),
     &          DSS21(MAXB2),DSS22(MAXB2)
      COMPLEX*16 HLL11(MAXB,MAXB),HLL12(MAXB,MAXB),
     &          HLL21(MAXB,MAXB),HLL22(MAXB,MAXB),
     &          HLS11(MAXB,MAXB),HLS12(MAXB,MAXB),
     &          HLS21(MAXB,MAXB),HLS22(MAXB,MAXB),
     &          HSL11(MAXB,MAXB),HSL12(MAXB,MAXB),
     &          HSL21(MAXB,MAXB),HSL22(MAXB,MAXB),
     &          HSS11(MAXB,MAXB),HSS12(MAXB,MAXB),
     &          HSS21(MAXB,MAXB),HSS22(MAXB,MAXB)
      COMPLEX*16 SLL11(MAXB,MAXB),SLL12(MAXB,MAXB),
     &          SLL21(MAXB,MAXB),SLL22(MAXB,MAXB),
     &          SSS11(MAXB,MAXB),SSS12(MAXB,MAXB),
     &          SSS21(MAXB,MAXB),SSS22(MAXB,MAXB)
      DIMENSION TR11(MAXB2),TR12(MAXB2),TR21(MAXB2),TR22(MAXB2) 
      DIMENSION TI11(MAXB2),TI12(MAXB2),TI21(MAXB2),TI22(MAXB2) 
      DIMENSION SR11(MAXB2),SR12(MAXB2),SR21(MAXB2),SR22(MAXB2) 
      DIMENSION SI11(MAXB2),SI12(MAXB2),SI21(MAXB2),SI22(MAXB2) 
      DIMENSION EXPT(MAXB,4),ALPHA(MAXB2),CP(MAXB2,3),
     &          PNC(MAXB2),KAPPA(4),LQN(4),MQN(4),
     &          NFUNS(4)
      COMMON/RLIGHT/CV,CV2
C
      CZERO=DCMPLX(0.0D0,0.0D0)
C
C****************************************************************C
C     LOOP OVER NUCLEAR CENTRES A AND B TO ESTABLISH POSITION    C
C     WITHIN THE FOCK MATRIX                                     C
C****************************************************************C
      DO 2001 ICENTA=1,NCENT
      XYZ(1,1)=COORD(1,ICENTA)
      XYZ(2,1)=COORD(2,ICENTA)
      XYZ(3,1)=COORD(3,ICENTA)
      DO 2001 ICENTB=1,ICENTA
c      DO 2001 ICENTB=1,NCENT
      IF(ICENTA.EQ.ICENTB) THEN
       INUCAB=1
      ELSE
       INUCAB=2
      ENDIF
      XYZ(1,2)=COORD(1,ICENTB)
      XYZ(2,2)=COORD(2,ICENTB)
      XYZ(3,2)=COORD(3,ICENTB)
C
C     LOOP OVER KAPPA(A) VALUES
C
      DO 2000 KA=1,NKAPPA(ICENTA)
      KAPPA(1)=KVALS(KA,ICENTA)
      IF(KAPPA(1).GT.0) THEN
       LQN(1)=KAPPA(1)
      ELSE
       LQN(1)=-KAPPA(1)-1
      ENDIF
C
      NFUNA=NFUNCT(LQN(1)+1,ICENTA)
      NFUNS(1)=NFUNA
      DO 70 IBAS=1,NFUNA
      EXPT(IBAS,1)=EXPSET(IBAS,LQN(1)+1,ICENTA)
70    CONTINUE
C
      IBASB=0
C
C     LOOP OVER KAPPA(B) VALUES
C
      DO 2000 KB=1,NKAPPA(ICENTB)
      KAPPA(2)=KVALS(KB,ICENTB)
      IF(KAPPA(2).GT.0) THEN
       LQN(2)=KAPPA(2)
      ELSE
       LQN(2)=-KAPPA(2)-1
      ENDIF
      NFUNB=NFUNCT(LQN(2)+1,ICENTB)
      NFUNS(2)=NFUNB
      DO 71 IBAS=1,NFUNB
      EXPT(IBAS,2)=EXPSET(IBAS,LQN(2)+1,ICENTB)
71    CONTINUE
C
C     LOOP OVER |MA| VALUES
C
      DO 2000 MA=1,IABS(KAPPA(1))
      MJA=(2*MA)-1
      MQN(1)=MJA
C
C     LOOP OVER |MB| VALUES
C
      DO 2000 MB=1,IABS(KAPPA(2))
      MJB=(2*MB)-1
      MQN(2)=MJB
C
c      IF(MQN(1).NE.MQN(2)) GO TO 2000
C
C****************************************************************C
C     AT THIS POINT, WE ARE WITHIN A BLOCK OF 4 COMBINATIONS     C
C     OF (MA,MB). FOR GIVEN (|MA|,|MB|), THE COMBINATIONS ARE    C
C     ORDERED                                                    C
C     11: = (-|MA|,-|MB|)                                        C
C     12: = (-|MA|,+|MB|)                                        C
C     21: = (+|MA|,-|MB|)                                        C
C     22: = (+|MA|,+|MB|)                                        C
C                                                                C
C     INITIALIZE THE FOCK-MATRIX BLOCKS TO ZERO                  C
C                                                                C
C****************************************************************C
C****************************************************************C
C     CONSTRUCT THE L-L ONE-ELECTRON MATRIX                      C
C****************************************************************C
      IALT=1
      LAMAB=LQN(1)+LQN(2)
      MAXAB=NFUNA*NFUNB
      ILMAX=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      PHASE1=DFLOAT((-1)**((MQN(1)-MQN(2))/2))
     &             *DFLOAT((KAPPA(1)*KAPPA(2))/
     &                 IABS(KAPPA(1)*KAPPA(2)))
      CALL RNORMF(EXPT,LQN,NFUNS,1,2)
      IF(INUCAB.EQ.2) THEN
      CALL EMAKELL(ER11,EI11,ER21,EI21,
     #             KAPPA,MQN,NFUNS,IALT,1,2)
      ELSE
      CALL OEMAKELL(ER11,EI11,ER21,EI21,
     #             KAPPA,MQN,NFUNS,IALT,1,2)
      ENDIF
      M=0
      DO 80 I=1,NFUNA
      DO 80 J=1,NFUNB
      M=M+1
      EIJ=EXPT(I,1)+EXPT(J,2)
      EROOT=DSQRT(PI/EIJ)**3
      SR11(M)=ER11(M,1)*EROOT
      SI11(M)=EI11(M,1)*EROOT
      SR12(M)=-PHASE1*ER21(M,1)*EROOT
      SI12(M)= PHASE1*EI21(M,1)*EROOT
      SR21(M)=ER21(M,1)*EROOT
      SI21(M)=EI21(M,1)*EROOT
      SR22(M)=PHASE1*ER11(M,1)*EROOT
      SI22(M)=-PHASE1*EI11(M,1)*EROOT
80    CONTINUE
C
C     LOOP OVER CENTRES FOR NUCLEAR ATTRACTION INTEGRALS
C
      DO 191 M=1,MAXAB
      TR11(M)=0.0D0
      TI11(M)=0.0D0
      TR12(M)=0.0D0
      TI12(M)=0.0D0
      TR21(M)=0.0D0
      TI21(M)=0.0D0
      TR22(M)=0.0D0
      TI22(M)=0.0D0
191   CONTINUE
      DO 190 ICENTZ=1,NCENT
      CX=COORD(1,ICENTZ)
      CY=COORD(2,ICENTZ)
      CZ=COORD(3,ICENTZ)
      M=0
      DO 90 I=1,NFUNA
      DO 90 J=1,NFUNB
      M=M+1
      EIJ=EXPT(I,1)+EXPT(J,2)
      PX=(XYZ(1,1)*EXPT(I,1)+XYZ(1,2)*EXPT(J,2))/EIJ
      PY=(XYZ(2,1)*EXPT(I,1)+XYZ(2,2)*EXPT(J,2))/EIJ
      PZ=(XYZ(3,1)*EXPT(I,1)+XYZ(3,2)*EXPT(J,2))/EIJ
      ALPHA(M)=(EIJ*CNUC(ICENTZ))/(EIJ+CNUC(ICENTZ))
      PNC(M)=TWOPI*DSQRT(CNUC(ICENTZ)/(CNUC(ICENTZ)+EIJ))
     &      *ZNUC(ICENTZ)/EIJ
      CP(M,1)=CX-PX
      CP(M,2)=CY-PY
      CP(M,3)=CZ-PZ
90    CONTINUE
      CALL RMAKE(RC,CP,ALPHA,MAXAB,LAMAB)
      DO 192 ITUV=1,ILMAX
      DO 192 M=1,MAXAB
      TR11(M)=TR11(M)+PNC(M)*ER11(M,ITUV)*RC(M,ITUV)
      TI11(M)=TI11(M)+PNC(M)*EI11(M,ITUV)*RC(M,ITUV)
      TR12(M)=TR12(M)-PHASE1*PNC(M)*ER21(M,ITUV)*RC(M,ITUV)
      TI12(M)=TI12(M)+PHASE1*PNC(M)*EI21(M,ITUV)*RC(M,ITUV)
      TR21(M)=TR21(M)+PNC(M)*ER21(M,ITUV)*RC(M,ITUV)
      TI21(M)=TI21(M)+PNC(M)*EI21(M,ITUV)*RC(M,ITUV)
      TR22(M)=TR22(M)+PHASE1*PNC(M)*ER11(M,ITUV)*RC(M,ITUV)
      TI22(M)=TI22(M)-PHASE1*PNC(M)*EI11(M,ITUV)*RC(M,ITUV)
192   CONTINUE
190   CONTINUE
      M=0
      DO 194 I=1,NFUNA
      DO 194 J=1,NFUNB
      M=M+1
      HLL11(I,J)=DCMPLX(-TR11(M),-TI11(M))
      HLL12(I,J)=DCMPLX(-TR12(M),-TI12(M))
      HLL21(I,J)=DCMPLX(-TR21(M),-TI21(M))
      HLL22(I,J)=DCMPLX(-TR22(M),-TI22(M))
      SLL11(I,J)=DCMPLX(SR11(M),SI11(M))
      SLL12(I,J)=DCMPLX(SR12(M),SI12(M))
      SLL21(I,J)=DCMPLX(SR21(M),SI21(M))
      SLL22(I,J)=DCMPLX(SR22(M),SI22(M))
194   CONTINUE
C****************************************************************C
C     CONSTRUCT THE S-S ONE-ELECTRON MATRIX                      C
C****************************************************************C
      IALT=1
      LAMAB=LQN(1)+LQN(2)+2
      MAXAB=NFUNA*NFUNB
      ILMAX=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      CALL RNORMF(EXPT,LQN,NFUNS,1,2)
      IF(INUCAB.EQ.2) THEN
      CALL EMAKESS(ER11,EI11,ER21,EI21,
     #             KAPPA,MQN,NFUNS,IALT,1,2)
      ELSE
      CALL OEMAKESS(ER11,EI11,ER21,EI21,
     #             KAPPA,MQN,NFUNS,IALT,1,2)
      ENDIF
      M=0
      DO 84 I=1,NFUNA
      DO 84 J=1,NFUNB
      M=M+1
      EIJ=EXPT(I,1)+EXPT(J,2)
      EROOT=DSQRT(PI/EIJ)**3
      SR11(M)=ER11(M,1)*EROOT
      SI11(M)=EI11(M,1)*EROOT
      SR12(M)=-PHASE1*ER21(M,1)*EROOT
      SI12(M)=+PHASE1*EI21(M,1)*EROOT
      SR21(M)=ER21(M,1)*EROOT
      SI21(M)=EI21(M,1)*EROOT
      SR22(M)=PHASE1*ER11(M,1)*EROOT
      SI22(M)=-PHASE1*EI11(M,1)*EROOT
84    CONTINUE
C
C     LOOP OVER CENTRES FOR NUCLEAR ATTRACTION INTEGRALS
C
      DO 491 M=1,MAXAB
      TR11(M)=0.0D0
      TI11(M)=0.0D0
      TR12(M)=0.0D0
      TI12(M)=0.0D0
      TR21(M)=0.0D0
      TI21(M)=0.0D0
      TR22(M)=0.0D0
      TI22(M)=0.0D0
491   CONTINUE
      DO 490 ICENTZ=1,NCENT
      CX=COORD(1,ICENTZ)
      CY=COORD(2,ICENTZ)
      CZ=COORD(3,ICENTZ)
      M=0
      DO 94 I=1,NFUNA
      DO 94 J=1,NFUNB
      M=M+1
      EIJ=EXPT(I,1)+EXPT(J,2)
      PX=(XYZ(1,1)*EXPT(I,1)+XYZ(1,2)*EXPT(J,2))/EIJ
      PY=(XYZ(2,1)*EXPT(I,1)+XYZ(2,2)*EXPT(J,2))/EIJ
      PZ=(XYZ(3,1)*EXPT(I,1)+XYZ(3,2)*EXPT(J,2))/EIJ
      ALPHA(M)=(EIJ*CNUC(ICENTZ))/(EIJ+CNUC(ICENTZ))
      PNC(M)=TWOPI*DSQRT(CNUC(ICENTZ)/(CNUC(ICENTZ)+EIJ))
     &      *ZNUC(ICENTZ)/EIJ
      CP(M,1)=CX-PX
      CP(M,2)=CY-PY
      CP(M,3)=CZ-PZ
94    CONTINUE
      CALL RMAKE(RC,CP,ALPHA,MAXAB,LAMAB)
      DO 492 ITUV=1,ILMAX
      DO 492 M=1,MAXAB
      TR11(M)=TR11(M)+PNC(M)*ER11(M,ITUV)*RC(M,ITUV)
      TI11(M)=TI11(M)+PNC(M)*EI11(M,ITUV)*RC(M,ITUV)
      TR12(M)=TR12(M)-PHASE1*PNC(M)*ER21(M,ITUV)*RC(M,ITUV)
      TI12(M)=TI12(M)+PHASE1*PNC(M)*EI21(M,ITUV)*RC(M,ITUV)
      TR21(M)=TR21(M)+PNC(M)*ER21(M,ITUV)*RC(M,ITUV)
      TI21(M)=TI21(M)+PNC(M)*EI21(M,ITUV)*RC(M,ITUV)
      TR22(M)=TR22(M)+PHASE1*PNC(M)*ER11(M,ITUV)*RC(M,ITUV)
      TI22(M)=TI22(M)-PHASE1*PNC(M)*EI11(M,ITUV)*RC(M,ITUV)
492   CONTINUE
490   CONTINUE
      M=0
      DO 494 I=1,NFUNA
      DO 494 J=1,NFUNB
      M=M+1
      HSS11(I,J)=DCMPLX(-2.0D0*CV2*SR11(M)-TR11(M),
     &                  -2.0D0*CV2*SI11(M)-TI11(M))
      HSS12(I,J)=DCMPLX(-2.0D0*CV2*SR12(M)-TR12(M),
     &                  -2.0D0*CV2*SI12(M)-TI12(M))
      HSS21(I,J)=DCMPLX(-2.0D0*CV2*SR21(M)-TR21(M),
     &                  -2.0D0*CV2*SI21(M)-TI21(M))
      HSS22(I,J)=DCMPLX(-2.0D0*CV2*SR22(M)-TR22(M),
     &                  -2.0D0*CV2*SI22(M)-TI22(M))
      SSS11(I,J)=DCMPLX(SR11(M),SI11(M))
      SSS12(I,J)=DCMPLX(SR12(M),SI12(M))
      SSS21(I,J)=DCMPLX(SR21(M),SI21(M))
      SSS22(I,J)=DCMPLX(SR22(M),SI22(M))
494   CONTINUE
C****************************************************************C
C     CONSTRUCT THE S-L ONE-ELECTRON MATRIX                      C
C****************************************************************C
      LAMAB=LQN(1)+LQN(2)+2
      ILMAX=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      M=0
      FACT=CV*DSQRT(DFLOAT(2*LQN(2)+3))
      DO 584 I=1,NFUNA
      DO 584 J=1,NFUNB
      M=M+1
      EIJ=EXPT(I,1)+EXPT(J,2)
      EJROOT=FACT*DSQRT(EXPT(J,2))
      EROOT=DSQRT(PI/EIJ)**3
      TR11(M)=EJROOT*ER11(M,1)*EROOT
      TI11(M)=EJROOT*EI11(M,1)*EROOT
      TR12(M)=-PHASE1*EJROOT*ER21(M,1)*EROOT
      TI12(M)=+PHASE1*EJROOT*EI21(M,1)*EROOT
      TR21(M)=EJROOT*ER21(M,1)*EROOT
      TI21(M)=EJROOT*EI21(M,1)*EROOT
      TR22(M)=PHASE1*EJROOT*ER11(M,1)*EROOT
      TI22(M)=-PHASE1*EJROOT*EI11(M,1)*EROOT
584   CONTINUE
      M=0
      DO 594 I=1,NFUNA
      DO 594 J=1,NFUNB
      M=M+1
      HSL11(I,J)=DCMPLX(TR11(M),TI11(M))
      HSL12(I,J)=DCMPLX(TR12(M),TI12(M))
      HSL21(I,J)=DCMPLX(TR21(M),TI21(M))
      HSL22(I,J)=DCMPLX(TR22(M),TI22(M))
594   CONTINUE
C****************************************************************C
C     CONSTRUCT THE L-S ONE-ELECTRON MATRIX                      C
C****************************************************************C
      IALT=1
      LAMAB=LQN(1)+LQN(2)+2
      ILMAX=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      MAXAB=NFUNA*NFUNB
      CALL RNORMF(EXPT,LQN,NFUNS,2,1)
      IF(INUCAB.EQ.2) THEN
      CALL EMAKESS(ER11,EI11,ER21,EI21,
     #             KAPPA,MQN,NFUNS,IALT,2,1)
      ELSE
      CALL OEMAKESS(ER11,EI11,ER21,EI21,
     #             KAPPA,MQN,NFUNS,IALT,2,1)
      ENDIF
      M=0
      FACT=CV*DSQRT(DFLOAT(2*LQN(1)+3))
      DO 684 J=1,NFUNB
      DO 684 I=1,NFUNA
      M=M+1
      EIJ=EXPT(J,2)+EXPT(I,1)
      EJROOT=FACT*DSQRT(EXPT(I,1))
      EROOT=DSQRT(PI/EIJ)**3
      TR11(M)=EJROOT*ER11(M,1)*EROOT
      TI11(M)=EJROOT*EI11(M,1)*EROOT
      TR12(M)=-PHASE1*EJROOT*ER21(M,1)*EROOT
      TI12(M)= PHASE1*EJROOT*EI21(M,1)*EROOT
      TR21(M)=EJROOT*ER21(M,1)*EROOT
      TI21(M)=EJROOT*EI21(M,1)*EROOT
      TR22(M)=PHASE1*EJROOT*ER11(M,1)*EROOT
      TI22(M)=-PHASE1*EJROOT*EI11(M,1)*EROOT
684   CONTINUE
      M=0
      DO 694 J=1,NFUNB
      DO 694 I=1,NFUNA
      M=M+1
      HLS11(I,J)=DCMPLX(TR11(M),-TI11(M))
      HLS12(I,J)=DCMPLX(TR21(M),-TI21(M))
      HLS21(I,J)=DCMPLX(TR12(M),-TI12(M))
      HLS22(I,J)=DCMPLX(TR22(M),-TI22(M))
694   CONTINUE
C
C****************************************************************C
C     CALCULATE COMPONENT OFFSETS                                C
C****************************************************************C
C
C
      IL1=LARGE(ICENTA,KA,MJA)
      IL2=LARGE(ICENTA,KA,MJA+1)
      JL1=LARGE(ICENTB,KB,MJB)
      JL2=LARGE(ICENTB,KB,MJB+1)
      IS1=IL1+NSHIFT
      IS2=IL2+NSHIFT
      JS1=JL1+NSHIFT
      JS2=JL2+NSHIFT
C
C****************************************************************C
C     WE NOW HAVE ALL PIECES OF THE FOCK MATRIX FOR THIS BLOCK   C
C     OVERLAY THE BLOCKS IN THE COMPLETE FOCK MATRIX             C
C****************************************************************C
C
C
C
C     L-L BLOCKS
C
      IF(IL1.GT.JL1) THEN
      DO 600 J=1,NFUNB
      DO 600 I=1,NFUNA
      FOCK(IL1+I,JL1+J)=FOCK(IL1+I,JL1+J)+HLL11(I,J)
      FOCK(JL1+J,IL1+I)=DCONJG(FOCK(IL1+I,JL1+J))
      FOCK(IL1+I,JL2+J)=FOCK(IL1+I,JL2+J)+HLL12(I,J)
      FOCK(JL2+J,IL1+I)=DCONJG(FOCK(IL1+I,JL2+J))
      FOCK(IL2+I,JL1+J)=FOCK(IL2+I,JL1+J)+HLL21(I,J)
      FOCK(JL1+J,IL2+I)=DCONJG(FOCK(IL2+I,JL1+J))
      FOCK(IL2+I,JL2+J)=FOCK(IL2+I,JL2+J)+HLL22(I,J)
      FOCK(JL2+J,IL2+I)=DCONJG(FOCK(IL2+I,JL2+J))
      OVAP(IL1+I,JL1+J)=SLL11(I,J)
      OVAP(JL1+J,IL1+I)=DCONJG(OVAP(IL1+I,JL1+J))
      OVAP(IL1+I,JL2+J)=SLL12(I,J)
      OVAP(JL2+J,IL1+I)=DCONJG(OVAP(IL1+I,JL2+J))
      OVAP(IL2+I,JL1+J)=SLL21(I,J)
      OVAP(JL1+J,IL2+I)=DCONJG(OVAP(IL2+I,JL1+J))
      OVAP(IL2+I,JL2+J)=SLL22(I,J)
      OVAP(JL2+J,IL2+I)=DCONJG(OVAP(IL2+I,JL2+J))
600   CONTINUE
      ENDIF
C
      IF(IL1.EQ.JL1) THEN
      DO 605 J=1,NFUNB
      DO 605 I=J,NFUNA
      FOCK(IL1+I,JL1+J)=FOCK(IL1+I,JL1+J)+HLL11(I,J)
      FOCK(JL1+J,IL1+I)=DCONJG(FOCK(IL1+I,JL1+J))
      FOCK(IL1+I,JL2+J)=FOCK(IL1+I,JL2+J)+HLL12(I,J)
      FOCK(JL2+J,IL1+I)=DCONJG(FOCK(IL1+I,JL2+J))
      FOCK(IL2+I,JL1+J)=FOCK(IL2+I,JL1+J)+HLL21(I,J)
      FOCK(JL1+J,IL2+I)=DCONJG(FOCK(IL2+I,JL1+J))
      FOCK(IL2+I,JL2+J)=FOCK(IL2+I,JL2+J)+HLL22(I,J)
      FOCK(JL2+J,IL2+I)=DCONJG(FOCK(IL2+I,JL2+J))
      OVAP(IL1+I,JL1+J)=SLL11(I,J)
      OVAP(JL1+J,IL1+I)=DCONJG(OVAP(IL1+I,JL1+J))
      OVAP(IL1+I,JL2+J)=SLL12(I,J)
      OVAP(JL2+J,IL1+I)=DCONJG(OVAP(IL1+I,JL2+J))
      OVAP(IL2+I,JL1+J)=SLL21(I,J)
      OVAP(JL1+J,IL2+I)=DCONJG(OVAP(IL2+I,JL1+J))
      OVAP(IL2+I,JL2+J)=SLL22(I,J)
      OVAP(JL2+J,IL2+I)=DCONJG(OVAP(IL2+I,JL2+J))
605   CONTINUE
      ENDIF
C
C     L-S BLOCKS
C
      IF(IL1.GE.JL1) THEN
      DO 601 J=1,NFUNB
      DO 601 I=1,NFUNA
      FOCK(IL1+I,JS1+J)=FOCK(IL1+I,JS1+J)+HLS11(I,J)
      FOCK(JS1+J,IL1+I)=DCONJG(FOCK(IL1+I,JS1+J))
      FOCK(IL1+I,JS2+J)=FOCK(IL1+I,JS2+J)+HLS12(I,J)
      FOCK(JS2+J,IL1+I)=DCONJG(FOCK(IL1+I,JS2+J))
      FOCK(IL2+I,JS1+J)=FOCK(IL2+I,JS1+J)+HLS21(I,J)
      FOCK(JS1+J,IL2+I)=DCONJG(FOCK(IL2+I,JS1+J))
      FOCK(IL2+I,JS2+J)=FOCK(IL2+I,JS2+J)+HLS22(I,J)
      FOCK(JS2+J,IL2+I)=DCONJG(FOCK(IL2+I,JS2+J))
      OVAP(IL1+I,JS1+J)=CZERO
      OVAP(JS1+J,IL1+I)=OVAP(IL1+I,JS1+J)
      OVAP(IL1+I,JS2+J)=CZERO
      OVAP(JS2+J,IL1+I)=OVAP(IL1+I,JS2+J)
      OVAP(IL2+I,JS1+J)=CZERO
      OVAP(JS1+J,IL2+I)=OVAP(IL2+I,JS1+J)
      OVAP(IL2+I,JS2+J)=CZERO
      OVAP(JS2+J,IL2+I)=OVAP(IL2+I,JS2+J)
601   CONTINUE
      ENDIF
C
C
C     S-L BLOCKS
C
      IF(IL1.GT.JL1) THEN
      DO 602 J=1,NFUNB
      DO 602 I=1,NFUNA
      FOCK(IS1+I,JL1+J)=FOCK(IS1+I,JL1+J)+HSL11(I,J)
      FOCK(JL1+J,IS1+I)=DCONJG(FOCK(IS1+I,JL1+J))
      FOCK(IS1+I,JL2+J)=FOCK(IS1+I,JL2+J)+HSL12(I,J)
      FOCK(JL2+J,IS1+I)=DCONJG(FOCK(IS1+I,JL2+J))
      FOCK(IS2+I,JL1+J)=FOCK(IS2+I,JL1+J)+HSL21(I,J)
      FOCK(JL1+J,IS2+I)=DCONJG(FOCK(IS2+I,JL1+J))
      FOCK(IS2+I,JL2+J)=FOCK(IS2+I,JL2+J)+HSL22(I,J)
      FOCK(JL2+J,IS2+I)=DCONJG(FOCK(IS2+I,JL2+J))
      OVAP(IS1+I,JL1+J)=CZERO
      OVAP(JL1+J,IS1+I)=OVAP(IS1+I,JL1+J)
      OVAP(IS1+I,JL2+J)=CZERO
      OVAP(JL2+J,IS1+I)=OVAP(IS1+I,JL2+J)
      OVAP(IS2+I,JL1+J)=CZERO
      OVAP(JL1+J,IS2+I)=OVAP(IS2+I,JL1+J)
      OVAP(IS2+I,JL2+J)=CZERO
      OVAP(JL2+J,IS2+I)=OVAP(IS2+I,JL2+J)
602   CONTINUE
      ENDIF
C
C
C     S-S BLOCKS
C
      IF(IL1.GT.JL1) THEN
      DO 603 J=1,NFUNB
      DO 603 I=1,NFUNA
      FOCK(IS1+I,JS1+J)=FOCK(IS1+I,JS1+J)+HSS11(I,J)
      FOCK(JS1+J,IS1+I)=DCONJG(FOCK(IS1+I,JS1+J))
      FOCK(IS1+I,JS2+J)=FOCK(IS1+I,JS2+J)+HSS12(I,J)
      FOCK(JS2+J,IS1+I)=DCONJG(FOCK(IS1+I,JS2+J))
      FOCK(IS2+I,JS1+J)=FOCK(IS2+I,JS1+J)+HSS21(I,J)
      FOCK(JS1+J,IS2+I)=DCONJG(FOCK(IS2+I,JS1+J))
      FOCK(IS2+I,JS2+J)=FOCK(IS2+I,JS2+J)+HSS22(I,J)
      FOCK(JS2+J,IS2+I)=DCONJG(FOCK(IS2+I,JS2+J))
      OVAP(IS1+I,JS1+J)=SSS11(I,J)
      OVAP(JS1+J,IS1+I)=DCONJG(OVAP(IS1+I,JS1+J))
      OVAP(IS1+I,JS2+J)=SSS12(I,J)
      OVAP(JS2+J,IS1+I)=DCONJG(OVAP(IS1+I,JS2+J))
      OVAP(IS2+I,JS1+J)=SSS21(I,J)
      OVAP(JS1+J,IS2+I)=DCONJG(OVAP(IS2+I,JS1+J))
      OVAP(IS2+I,JS2+J)=SSS22(I,J)
      OVAP(JS2+J,IS2+I)=DCONJG(OVAP(IS2+I,JS2+J))
603   CONTINUE
      ENDIF
C
      IF(IL1.EQ.JL1) THEN
      DO 607 J=1,NFUNB
      DO 607 I=J,NFUNA
      FOCK(IS1+I,JS1+J)=FOCK(IS1+I,JS1+J)+HSS11(I,J)
      FOCK(JS1+J,IS1+I)=DCONJG(FOCK(IS1+I,JS1+J))
      FOCK(IS1+I,JS2+J)=FOCK(IS1+I,JS2+J)+HSS12(I,J)
      FOCK(JS2+J,IS1+I)=DCONJG(FOCK(IS1+I,JS2+J))
      FOCK(IS2+I,JS1+J)=FOCK(IS2+I,JS1+J)+HSS21(I,J)
      FOCK(JS1+J,IS2+I)=DCONJG(FOCK(IS2+I,JS1+J))
      FOCK(IS2+I,JS2+J)=FOCK(IS2+I,JS2+J)+HSS22(I,J)
      FOCK(JS2+J,IS2+I)=DCONJG(FOCK(IS2+I,JS2+J))
      OVAP(IS1+I,JS1+J)=SSS11(I,J)
      OVAP(JS1+J,IS1+I)=DCONJG(OVAP(IS1+I,JS1+J))
      OVAP(IS1+I,JS2+J)=SSS12(I,J)
      OVAP(JS2+J,IS1+I)=DCONJG(OVAP(IS1+I,JS2+J))
      OVAP(IS2+I,JS1+J)=SSS21(I,J)
      OVAP(JS1+J,IS2+I)=DCONJG(OVAP(IS2+I,JS1+J))
      OVAP(IS2+I,JS2+J)=SSS22(I,J)
      OVAP(JS2+J,IS2+I)=DCONJG(OVAP(IS2+I,JS2+J))
607   CONTINUE
      ENDIF
C*****************************************************************C
C     CALCULATE THE ONE ELECTRON CONTRIBUTION TO THE TOTAL ENERGY C
C*****************************************************************C
      DO 651 M=1,NFUNA*NFUNB
      DLL11(M)=CZERO
      DLL12(M)=CZERO
      DLL21(M)=CZERO
      DLL22(M)=CZERO
      DLS11(M)=CZERO
      DLS12(M)=CZERO
      DLS21(M)=CZERO
      DLS22(M)=CZERO
      DSL11(M)=CZERO
      DSL12(M)=CZERO
      DSL21(M)=CZERO
      DSL22(M)=CZERO
      DSS11(M)=CZERO
      DSS12(M)=CZERO
      DSS21(M)=CZERO
      DSS22(M)=CZERO
651   CONTINUE
      IL1=LARGE(ICENTA,KA,MJA)
      IL2=LARGE(ICENTA,KA,MJA+1)
      JL1=LARGE(ICENTB,KB,MJB)
      JL2=LARGE(ICENTB,KB,MJB+1)
      IS1=IL1+NSHIFT
      IS2=IL2+NSHIFT
      JS1=JL1+NSHIFT
      JS2=JL2+NSHIFT
C
      M=0
      DO 680 I=1,NFUNA
      DO 680 J=1,NFUNB
      M=M+1
C
      DLL11(M)=C(IL1+I,JL1+J)
      DLL12(M)=C(IL1+I,JL2+J)
      DLL21(M)=C(IL2+I,JL1+J)
      DLL22(M)=C(IL2+I,JL2+J)
C
      DLS11(M)=C(IL1+I,JS1+J)
      DLS12(M)=C(IL1+I,JS2+J)
      DLS21(M)=C(IL2+I,JS1+J)
      DLS22(M)=C(IL2+I,JS2+J)
C
      DSL11(M)=C(IS1+I,JL1+J)
      DSL12(M)=C(IS1+I,JL2+J)
      DSL21(M)=C(IS2+I,JL1+J)
      DSL22(M)=C(IS2+I,JL2+J)
C
      DSS11(M)=C(IS1+I,JS1+J)
      DSS12(M)=C(IS1+I,JS2+J)
      DSS21(M)=C(IS2+I,JS1+J)
      DSS22(M)=C(IS2+I,JS2+J)
C
680   CONTINUE
C
      IF(ICENTA.EQ.ICENTB) THEN
       SYMFAC=1.0D0
      ELSE
       SYMFAC=2.0D0
      ENDIF
      M=0
      ETEMP=ZERO
      DO 690 I=1,NFUNA
      DO 690 J=1,NFUNB
      M=M+1
      ETEMP=ETEMP
     & + DLL11(M)*HLL11(I,J)
     & + DLL12(M)*HLL12(I,J)
     & + DLL21(M)*HLL21(I,J)
     & + DLL22(M)*HLL22(I,J)
     & + DLS11(M)*HLS11(I,J)
     & + DLS12(M)*HLS12(I,J)
     & + DLS21(M)*HLS21(I,J)
     & + DLS22(M)*HLS22(I,J)
     & + DSL11(M)*HSL11(I,J)
     & + DSL12(M)*HSL12(I,J)
     & + DSL21(M)*HSL21(I,J)
     & + DSL22(M)*HSL22(I,J)
     & + DSS11(M)*HSS11(I,J)
     & + DSS12(M)*HSS12(I,J)
     & + DSS21(M)*HSS21(I,J)
     & + DSS22(M)*HSS22(I,J)
690   CONTINUE
      ETOTAL=ETOTAL+SYMFAC*ETEMP
C
C
2000  CONTINUE
C
2001  CONTINUE
C
      RETURN
      END
C
C****************************************************************C
C
      SUBROUTINE RMAKE(RC,QP,ALPHA,MAXM,LAMBDA)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXL=26,MAXR=969,
     #          ZERO = 0.0D0)
      PARAMETER(MLL=165)
      PARAMETER(MAXLQN=4,MABLL=MLL)
      DIMENSION FS(MAXB2,MAXL),ALPHA(MAXB2),QP(MAXB2,3),
     #RC(MAXB2,MAXR),RC2(MAXB2,MAXR)
      DIMENSION X1(MAXB2),X2(MAXB2),X3(MAXB2),X0(MAXB2),I1(MAXB2),
     #I2(MAXB2),I3(MAXB2),I0(MAXB2)
      DIMENSION F1(MAXB2,MAXL),F2(MAXB2,MAXL),
     #          F3(MAXB2,MAXL),F0(MAXB2,MAXL)
      COMMON/FTIME/FUNTIME
      COMMON/TIMER/RTIME
      COMMON/INDSYS/INABCD(0:4*MAXLQN,0:4*MAXLQN,0:4*MAXLQN),
     &              IVEC(MABLL),JVEC(MABLL),KVEC(MABLL)
C
C     CALCULATE FS     
C
C******************Special code for the Sun**********************C
      KL=0
      N0=0
      N1=0
      N2=0
      N3=0
C
      DO 11 M=1,MAXM
      X=ALPHA(M)*(QP(M,1)*QP(M,1)+QP(M,2)*QP(M,2)+QP(M,3)*QP(M,3))
C      X=ALPHA(M)*QP(M,3)*QP(M,3)
      IF(X.LE.1.D-11) THEN
       N0=N0+1
       X0(N0)=X
       I0(N0)=M
      ELSEIF(X.LE.1.80D1.AND.X.GT.1.D-11) THEN
       N1=N1+1
       X1(N1)=X
       I1(N1)=M
      ELSEIF(X.GT.1.80D1.AND.X.LE.3.0D1) THEN
       N2=N2+1
       X2(N2)=X
       I2(N2)=M
      ELSE
       N3=N3+1
       X3(N3)=X
       I3(N3)=M
      ENDIF
  11  CONTINUE
C     
      CALL FUNFM(F0,X0,N0,LAMBDA,0)
      DO 140 JJ=1,LAMBDA+1
      DO 140 M=1,N0
      FS(I0(M),JJ)=F0(M,JJ)
140   CONTINUE
      CALL FUNFM(F1,X1,N1,LAMBDA,1)
      DO 150 JJ=1,LAMBDA+1
      DO 150 M=1,N1
      FS(I1(M),JJ)=F1(M,JJ)
150   CONTINUE
      CALL FUNFM(F2,X2,N2,LAMBDA,2)
      DO 160 JJ=1,LAMBDA+1
      DO 160 M=1,N2
      FS(I2(M),JJ)=F2(M,JJ)
160   CONTINUE
      CALL FUNFM(F3,X3,N3,LAMBDA,3)
      DO 170 JJ=1,LAMBDA+1
      DO 170 M=1,N3
      FS(I3(M),JJ)=F3(M,JJ)
170   CONTINUE
C
C
C*****************End special routine*******************************C
C
C
C     CONSTRUCT TOP-LEVEL
C
      DO 199 M=1,MAXM
      RC(M,1)=((-2.0D0*ALPHA(M))**(LAMBDA))*FS(M,LAMBDA+1)
199   CONTINUE
C
      IF(MOD(LAMBDA,2).EQ.0) THEN
       ITUVMIN=1
      ELSE
       ITUVMIN=2
      ENDIF
C
      ITUV=-1
      DO 500 ILEVEL=LAMBDA-1,ITUVMIN,-2
C
C-------------------------------------------------
C
      ITUV=ITUV+1
      DO 300 IT=0,ITUV
      RIT=DFLOAT(IT)
      DO 300 IU=0,ITUV-IT
      RIU=DFLOAT(IU)
      DO 300 IV=0,ITUV-IT-IU
      RIV=DFLOAT(IV)
C
C
      N1=INABCD(IT+1,IU  ,IV  )
      N2=INABCD(IT  ,IU+1,IV  )
      N3=INABCD(IT  ,IU  ,IV+1)
C
      IF(IT.NE.0) THEN
      IF(IU.NE.0) THEN
      IF(IV.NE.0) THEN      
C
C     CASE (1 1 1)
C   
      K1=INABCD(IT,IU,IV)
      M1=INABCD(IT-1,IU,IV)
      M2=INABCD(IT,IU-1,IV)
      M3=INABCD(IT,IU,IV-1)
      DO 200 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)+RIT*RC(M,M1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)+RIU*RC(M,M2)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)+RIV*RC(M,M3)
200   CONTINUE
      ELSE
C
C     CASE (1 1 0)
C
      K1=INABCD(IT,IU,IV)
      M1=INABCD(IT-1,IU,IV)
      M2=INABCD(IT,IU-1,IV)
      DO 210 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)+RIT*RC(M,M1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)+RIU*RC(M,M2)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)
210   CONTINUE
      ENDIF
      ELSE
      IF(IV.NE.0) THEN
C
C     CASE (1 0 1)
C
      K1=INABCD(IT,IU,IV)
      M1=INABCD(IT-1,IU,IV)
      M3=INABCD(IT,IU,IV-1)
      DO 220 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)+RIT*RC(M,M1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)+RIV*RC(M,M3)
220   CONTINUE
      ELSE
C
C     CASE (1 0 0)
C
      K1=INABCD(IT,IU,IV)
      M1=INABCD(IT-1,IU,IV)
      DO 230 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)+RIT*RC(M,M1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)
230   CONTINUE
      ENDIF
      ENDIF
      ELSE
      IF(IU.NE.0) THEN
      IF(IV.NE.0) THEN
C
C     CASE (0 1 1)
C
      K1=INABCD(IT,IU,IV)
      M2=INABCD(IT,IU-1,IV)
      M3=INABCD(IT,IU,IV-1)
      DO 240 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)+RIU*RC(M,M2)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)+RIV*RC(M,M3)
240   CONTINUE
      ELSE
C
C     CASE (0 1 0)
C
      K1=INABCD(IT,IU,IV)
      M2=INABCD(IT,IU-1,IV)
      DO 250 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)+RIU*RC(M,M2)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)
250   CONTINUE
      ENDIF
      ELSE
      IF(IV.NE.0) THEN
C
C     CASE (0 0 1)
C
      K1=INABCD(IT,IU,IV)
      M3=INABCD(IT,IU,IV-1)
      DO 260 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)+RIV*RC(M,M3)
260   CONTINUE
      ELSE
C
C     CASE (0 0 0)
C
      K1=INABCD(IT,IU,IV)
      DO 270 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)
270   CONTINUE
      ENDIF
      ENDIF
      ENDIF
C
300   CONTINUE
C
C     ADD IN (TI=0,IU=0,IV=0) CASE
C
      DO 280 M=1,MAXM
      RC2(M,1)=((-2.0D0*ALPHA(M))**(ILEVEL))*FS(M,ILEVEL+1)
280   CONTINUE
C
C-------------------------------------------------
C
      ITUV=ITUV+1
      DO 301 IT=0,ITUV
      RIT=DFLOAT(IT)
      DO 301 IU=0,ITUV-IT
      RIU=DFLOAT(IU)
      DO 301 IV=0,ITUV-IT-IU
      RIV=DFLOAT(IV)
C
      N1=INABCD(IT+1,IU,IV)
      N2=INABCD(IT,IU+1,IV)
      N3=INABCD(IT,IU,IV+1)
C
      IF(IT.NE.0) THEN
      IF(IU.NE.0) THEN
      IF(IV.NE.0) THEN      
C
C     CASE (1 1 1)
C   
      K1=INABCD(IT,IU,IV)
      M1=INABCD(IT-1,IU,IV)
      M2=INABCD(IT,IU-1,IV)
      M3=INABCD(IT,IU,IV-1)
      DO 201 M=1,MAXM
      RC(M,N1)=-QP(M,1)*RC2(M,K1)+RIT*RC2(M,M1)
      RC(M,N2)=-QP(M,2)*RC2(M,K1)+RIU*RC2(M,M2)
      RC(M,N3)=-QP(M,3)*RC2(M,K1)+RIV*RC2(M,M3)
201   CONTINUE
C
      ELSE
C
C     CASE (1 1 0)
C
      K1=INABCD(IT,IU,IV)
      M1=INABCD(IT-1,IU,IV)
      M2=INABCD(IT,IU-1,IV)
      DO 211 M=1,MAXM
      RC(M,N1)=-QP(M,1)*RC2(M,K1)+RIT*RC2(M,M1)
      RC(M,N2)=-QP(M,2)*RC2(M,K1)+RIU*RC2(M,M2)
      RC(M,N3)=-QP(M,3)*RC2(M,K1)
211   CONTINUE
C
      ENDIF
C
      ELSE
      IF(IV.NE.0) THEN
C
C     CASE (1 0 1)
C
      K1=INABCD(IT,IU,IV)
      M1=INABCD(IT-1,IU,IV)
      M3=INABCD(IT,IU,IV-1)
      DO 221 M=1,MAXM
      RC(M,N1)=-QP(M,1)*RC2(M,K1)+RIT*RC2(M,M1)
      RC(M,N2)=-QP(M,2)*RC2(M,K1)
      RC(M,N3)=-QP(M,3)*RC2(M,K1)+RIV*RC2(M,M3)
221   CONTINUE
      ELSE
C
C     CASE (1 0 0)
C
      K1=INABCD(IT,IU,IV)
      M1=INABCD(IT-1,IU,IV)
      DO 231 M=1,MAXM
      RC(M,N1)=-QP(M,1)*RC2(M,K1)+RIT*RC2(M,M1)
      RC(M,N2)=-QP(M,2)*RC2(M,K1)
      RC(M,N3)=-QP(M,3)*RC2(M,K1)
231   CONTINUE
      ENDIF
      ENDIF
      ELSE
      IF(IU.NE.0) THEN
      IF(IV.NE.0) THEN
C
C     CASE (0 1 1)
C
      K1=INABCD(IT,IU,IV)
      M2=INABCD(IT,IU-1,IV)
      M3=INABCD(IT,IU,IV-1)
      DO 241 M=1,MAXM
      RC(M,N1)=-QP(M,1)*RC2(M,K1)
      RC(M,N2)=-QP(M,2)*RC2(M,K1)+RIU*RC2(M,M2)
      RC(M,N3)=-QP(M,3)*RC2(M,K1)+RIV*RC2(M,M3)
241   CONTINUE
      ELSE
C
C     CASE (0 1 0)
C
      K1=INABCD(IT,IU,IV)
      M2=INABCD(IT,IU-1,IV)
      DO 251 M=1,MAXM
      RC(M,N1)=-QP(M,1)*RC2(M,K1)
      RC(M,N2)=-QP(M,2)*RC2(M,K1)+RIU*RC2(M,M2)
      RC(M,N3)=-QP(M,3)*RC2(M,K1)
251   CONTINUE
      ENDIF
      ELSE
      IF(IV.NE.0) THEN
C
C     CASE (0 0 1)
C
      K1=INABCD(IT,IU,IV)
      M3=INABCD(IT,IU,IV-1)
      DO 261 M=1,MAXM
      RC(M,N1)=-QP(M,1)*RC2(M,K1)
      RC(M,N2)=-QP(M,2)*RC2(M,K1)
      RC(M,N3)=-QP(M,3)*RC2(M,K1)+RIV*RC2(M,M3)
261   CONTINUE
      ELSE
C
C     CASE (0 0 0)
C
      K1=INABCD(IT,IU,IV)
      DO 271 M=1,MAXM
      RC(M,N1)=-QP(M,1)*RC2(M,K1)
      RC(M,N2)=-QP(M,2)*RC2(M,K1)
      RC(M,N3)=-QP(M,3)*RC2(M,K1)
271   CONTINUE
      ENDIF
      ENDIF
      ENDIF
C
301   CONTINUE
C
C     ADD IN (TI=0,IU=0,IV=0) CASE
C
      DO 281 M=1,MAXM
      RC(M,1)=((-2.0D0*ALPHA(M))**(ILEVEL-1))*FS(M,ILEVEL)
281   CONTINUE
C
500   CONTINUE
C
C**************************************************************************************
C
      IF(MOD(LAMBDA,2).EQ.1) THEN
C
      ITUV=ITUV+1
      DO 302 IT=0,ITUV
      RIT=DFLOAT(IT)
      DO 302 IU=0,ITUV-IT
      RIU=DFLOAT(IU)
      DO 302 IV=0,ITUV-IT-IU
      RIV=DFLOAT(IV)
C
C
      N1=INABCD(IT+1,IU,IV)
      N2=INABCD(IT,IU+1,IV)
      N3=INABCD(IT,IU,IV+1)
C
      IF(IT.NE.0) THEN
      IF(IU.NE.0) THEN
      IF(IV.NE.0) THEN      
C
C     CASE (1 1 1)
C   
      K1=INABCD(IT,IU,IV)
      M1=INABCD(IT-1,IU,IV)
      M2=INABCD(IT,IU-1,IV)
      M3=INABCD(IT,IU,IV-1)
      DO 202 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)+RIT*RC(M,M1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)+RIU*RC(M,M2)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)+RIV*RC(M,M3)
202   CONTINUE
      ELSE
C
C     CASE (1 1 0)
C
      K1=INABCD(IT,IU,IV)
      M1=INABCD(IT-1,IU,IV)
      M2=INABCD(IT,IU-1,IV)
      DO 212 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)+RIT*RC(M,M1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)+RIU*RC(M,M2)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)
212   CONTINUE
      ENDIF
      ELSE
      IF(IV.NE.0) THEN
C
C     CASE (1 0 1)
C
      K1=INABCD(IT,IU,IV)
      M1=INABCD(IT-1,IU,IV)
      M3=INABCD(IT,IU,IV-1)
      DO 222 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)+RIT*RC(M,M1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)+RIV*RC(M,M3)
222   CONTINUE
      ELSE
C
C     CASE (1 0 0)
C
      K1=INABCD(IT,IU,IV)
      M1=INABCD(IT-1,IU,IV)
      DO 232 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)+RIT*RC(M,M1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)
232   CONTINUE
      ENDIF
      ENDIF
      ELSE
      IF(IU.NE.0) THEN
      IF(IV.NE.0) THEN
C
C     CASE (0 1 1)
C
      K1=INABCD(IT,IU,IV)
      M2=INABCD(IT,IU-1,IV)
      M3=INABCD(IT,IU,IV-1)
      DO 242 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)+RIU*RC(M,M2)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)+RIV*RC(M,M3)
242   CONTINUE
      ELSE
C
C     CASE (0 1 0)
C
      K1=INABCD(IT,IU,IV)
      M2=INABCD(IT,IU-1,IV)
      DO 252 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)+RIU*RC(M,M2)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)
252   CONTINUE
      ENDIF
      ELSE
      IF(IV.NE.0) THEN
C
C     CASE (0 0 1)
C
      K1=INABCD(IT,IU,IV)
      M3=INABCD(IT,IU,IV-1)
      DO 262 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)+RIV*RC(M,M3)
262   CONTINUE
      ELSE
C
C     CASE (0 0 0)
C
      K1=INABCD(IT,IU,IV)
      DO 272 M=1,MAXM
      RC2(M,N1)=-QP(M,1)*RC(M,K1)
      RC2(M,N2)=-QP(M,2)*RC(M,K1)
      RC2(M,N3)=-QP(M,3)*RC(M,K1)
272   CONTINUE
      ENDIF
      ENDIF
      ENDIF
C
302   CONTINUE
C
C     ADD IN (TI=0,IU=0,IV=0) CASE
C
      DO 282 M=1,MAXM
c      RC2(M,1)=((-2.0D0*ALPHA(M))**(ILEVEL))*FS(M,ILEVEL+1)
      RC2(M,1)=FS(M,1)
282   CONTINUE
C
C-------------------------------------------------
C
C     WRITE ARRAY RC2 INTO RC
C
      ITMAX=((LAMBDA+1)*(LAMBDA+2)*(LAMBDA+3))/6
      DO 4300 IT=1,ITMAX
      DO 4300 M=1,MAXM
      RC(M,IT)=RC2(M,IT)
4300  CONTINUE
C
      ENDIF
C
      RETURN
      END
C**********************************************************************C
C**********************************************************************C
C
C
      SUBROUTINE MAKEINDEX(LAMDAM)
      PARAMETER(IESIZE=455,IRSIZE=2925,
     #ILMAX=6,IL2=2*ILMAX,IL4=4*ILMAX,IJKSIZE=61425)
      COMMON/ACCSS/INABCD(0:IL4,0:IL4,0:IL4,0:IL4)
      LOOPIJK=1
      DO 1000 LAMBDA=0,LAMDAM
      LOOP=0
      DO 1010 IMU = 0,LAMBDA
      IT = LAMBDA - IMU
      DO 1020 INU = 0,IMU
      JT =IMU -  INU
      DO 1030 ITAU = 0,INU
      KT = INU - ITAU
      LOOP = LOOP + 1
      INABCD(IT,JT,KT,LAMBDA) = LOOP
 1030 CONTINUE
 1020 CONTINUE
 1010 CONTINUE
 1000 CONTINUE
      END
C**********************************************************************C
C
C*********************************************************************C
C
      SUBROUTINE RNORMF(EXPT,LQN,NFUNS,IND1,IND2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB)

      DIMENSION EXPT(MAXB,4),NFUNS(4),LQN(4)
      DIMENSION RNAL(MAXB),RNAS(MAXB),RNBL(MAXB),RNBS(MAXB)
      COMMON/GAMMAF/GAMMAL(100),GAMMAF(100)
      COMMON/RNORM/RNLL(MAXB2),RNSL(MAXB2),RNLS(MAXB2),RNSS(MAXB2),
     #EXPA(MAXB2),EXPB(MAXB2),EXPAB(MAXB2)

      DATA TWOLOG,HALF1,HALF3,TWO
     # /6.931471805599453D-1,5.0D-1,1.5D0,2.0D0/
C
      LA=LQN(IND1)
      LB=LQN(IND2)
      NFUNA=NFUNS(IND1)
      NFUNB=NFUNS(IND2)
      MAXM=NFUNA*NFUNB
      RLA=DFLOAT(LA)
      RLB=DFLOAT(LB)
      GA1=TWOLOG-GAMMAL(2*LA+3)
      GA2=TWOLOG-GAMMAL(2*LA+5)
      GB1=TWOLOG-GAMMAL(2*LB+3)
      GB2=TWOLOG-GAMMAL(2*LB+5)
      RA1=RLA+HALF3
      RA2=RLA+HALF1
      RB1=RLB+HALF3
      RB2=RLB+HALF1
      DO 10 I=1,NFUNA
      ELOG=DLOG(TWO*EXPT(I,IND1))
      RNAL(I)=DEXP(HALF1*(GA1+RA1*ELOG))
      RNAS(I)=DEXP(HALF1*(GA2+RA2*ELOG))
10    CONTINUE
      DO 15 J=1,NFUNB
      ELOG=DLOG(TWO*EXPT(J,IND2))
      RNBL(J)=DEXP(HALF1*(GB1+RB1*ELOG))
      RNBS(J)=DEXP(HALF1*(GB2+RB2*ELOG))
15    CONTINUE


C
C     RNLL(M) ARE THE LL NORMALIZATION CONSTANTS
C     RNSL(M) ARE THE SL NORMALIZATION CONSTANTS
C     RNSS(M) ARE THE SS NORMALIZATION CONSTANTS
C
      M=0
      DO 20 I=1,NFUNA
      DO 20 J=1,NFUNB
      M=M+1
      RNLL(M)=RNAL(I)*RNBL(J)
      RNSL(M)=RNAS(I)*RNBL(J)
      RNLS(M)=RNAL(I)*RNBS(J)
      RNSS(M)=RNAS(I)*RNBS(J)
      EXPA(M)=EXPT(I,IND1)
      EXPB(M)=EXPT(J,IND2)
20    CONTINUE

      MAXM=M
      DO 30 M=1,MAXM
      EXPAB(M)=EXPA(M)*EXPB(M)
30    CONTINUE
      RETURN
      END
C
C****************************************************************C
C
      SUBROUTINE FUNFM(FM,T,LEN,MMAX,ITYPE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXL=26,MSERIES=40)
C****************************************************************C
C     FUNFM EVALUATES THE INTEGRAL                               C
C                                                                C
C     INT_{0}^{1} U^{2M} EXP(-TU^{2}) DU                         C
C                                                                C
C     FOR ALL VALUES OF M IN THE RANGE 0<M<MMAX                  C
C     FOR ALL VALUES OF T IN THE RANGE T>0                       C
C                                                                C
C     ITYPE = 0  SPECIAL CASE T=0                                C
C                                                                C
C     ITYPE = 1  POWER SERIES AND REVERSE RECURRENCE             C
C                ONLY MSERIES TERMS WILL BE USED, SO A VALUE     C
C                APPROPRIATE TO THE LARGEST VALUE OF T  IN THIS  C
C                BATCH MUST BE SUPPLIED.                         C
C                                                                C
C     ITYPE = 2  ASYMPTOTIC EXPANSION AND FORWARD RECURRENCE     C        
C                                                                C
C     ITYPE = 3  ASYMPTOTIC EXPANSION AND FORWARD RECURRENCE.    C
C                ALL TERMS DEPENDING ON EXP(-T) ARE OMITTED TO   C
C                AVOID NUMERICAL UNDERFLOW PROBLEMS.  MSERIES    C
C                IS NOT REQUIRED.                                C
C                                                                C
C                                                                C
C****************************************************************C
      DIMENSION FM(MAXB2,MAXL),T(MAXB2),TMMAX(MAXB2),TT2(MAXB2),
     &          TEXP(MAXB2),TROOT(MAXB2)
      DATA ZERO,ONE,TWO/0.0D0,1.0D0,2.0D0/
      DATA PIROOT,A0,B0/
     & 8.86226925452758D-1, 4.994501191201870D-1,
     & 4.551838436668326D-1/
C
      IF(ITYPE.EQ.0) THEN
C****************************************************************C
C     ITYPE=0: SPECIAL CASE FOR T=0                              C
C****************************************************************C
      DO 6 K=1,MMAX+1
      MVAL=K-1
      VALUE=ONE/DFLOAT(MVAL+MVAL+1)
      DO 5 M=1,LEN
      FM(M,K)=VALUE
5     CONTINUE
6     CONTINUE
      RETURN
C
      ELSEIF(ITYPE.EQ.1) THEN
C
C****************************************************************C
C     ITYPE=1: POWER SERIES EVALUATION                           C
C     INITIALIZE THE POWER SERIES FOR M=MMAX                     C
C****************************************************************C
C
      DO 110 M=1,LEN
      TEXP(M)=DEXP(-T(M))
      TT2(M)=TWO*T(M)
      TMMAX(M)=ONE
      FM(M,MMAX+1)=ONE
110   CONTINUE
C
C****************************************************************C
C     LOOP OVER TERMS IN THE POWER SERIES                        C
C                                                                C
C     CONVERGENCE IS ACHIEVED WHEN EXP(-T)*TERM(K)< 1.0E-14      C
C     WHERE TERM(K) IS THE KTH TERM IN THE POWER SERIES          C
C     NOTE THAT THE TERMS ARE ALWAYS POSITIVE SO THAT THERE IS   C
C     NO NEED TO TEST FOR ABSOLUTE VALUE                         C
C                                                                C
C****************************************************************C
C
      DO 121 K=1,MSERIES
C
      DMMAX=DFLOAT(2*(MMAX+K)+1)
C
      DO 120 M=1,LEN
      TMMAX(M)=TMMAX(M)*(TT2(M)/DMMAX)
      FM(M,MMAX+1)=FM(M,MMAX+1)+TMMAX(M)
120   CONTINUE
121   CONTINUE
C
C****************************************************************C
C     RESCALE BY THE PREFACTOR                                   C
C****************************************************************C
C
      DEN=DFLOAT((2*MMAX)+1)
      DO 130 M=1,LEN
      FM(M,MMAX+1)=FM(M,MMAX+1)*TEXP(M)/DEN
130   CONTINUE
C
C****************************************************************C
C     NOW COMPLETE TABLE BY BACKWARDS RECURRENCE                 C
C****************************************************************C
C
      DO 140 I=1,MMAX
      MIND=MMAX-I+1
      MVAL=MIND-1
      COEFF=DFLOAT(MVAL+MVAL+1)
      DO 135 M=1,LEN
      FM(M,MIND)=(TT2(M)*FM(M,MIND+1)+TEXP(M))/COEFF
135   CONTINUE
140   CONTINUE
      ELSEIF(ITYPE.EQ.2) THEN
C
C****************************************************************C
C     ITYPE=2: ASYMPTOTIC EXPANSION WITH VERY LARGE ARGUMENT     C
C     INITIALIZE THE ASYMPTOTIC EXPANSION                        C
C****************************************************************C
C
      DO 210 M=1,LEN
      TEXP(M)=DEXP(-T(M))
      TT2(M)=TWO*T(M)
      TROOT(M)=DSQRT(T(M)) 
210   CONTINUE
      DO 221 M=1,LEN
      FM(M,1)=A0/(B0+T(M))
221   CONTINUE
C
C****************************************************************C
C     RESCALE BY THE PREFACTOR                                   C
C****************************************************************C
C
      DO 230 M=1,LEN
      FM(M,1)=(PIROOT/TROOT(M))-(TEXP(M)*FM(M,1))
230   CONTINUE
C
C****************************************************************C
C     NOW COMPLETE TABLE BY FORWARD RECURRENCE                   C
C****************************************************************C
C
      DO 240 MIND=1,MMAX
      MVAL=MIND-1
      COEFF=DFLOAT(MVAL+MVAL+1)
      DO 235 M=1,LEN
      FM(M,MIND+1)=(COEFF*FM(M,MIND)-TEXP(M))/TT2(M)
235   CONTINUE
240   CONTINUE
      ELSEIF(ITYPE.EQ.3) THEN
C
C****************************************************************C
C     ITYPE=3: ASYMPTOTIC EXPANSION WITH VERY LARGE ARGUMENT     C
C     INITIALIZE THE ASYMPTOTIC EXPANSION                        C
C****************************************************************C
C
      DO 310 M=1,LEN
      TT2(M)=TWO*T(M)
      FM(M,1)=PIROOT/DSQRT(T(M))
310   CONTINUE
C
C****************************************************************C
C     NOW COMPLETE TABLE BY FORWARD RECURRENCE                   C
C****************************************************************C
C
      DO 340 MIND=1,MMAX
      MVAL=MIND-1
      COEFF=DFLOAT(MVAL+MVAL+1)
      DO 335 M=1,LEN
      FM(M,MIND+1)=(COEFF*FM(M,MIND))/TT2(M)
335   CONTINUE
340   CONTINUE
      ELSE
C****************************************************************C
C     ITYPE OUT OF RANGE: INVALID INPUT TO FUNFM                 C
C****************************************************************C
      WRITE(6,901) ITYPE
901   FORMAT(2X,'INVALID CALL TO FUNFM: TYPE = ',I4)
      STOP
      ENDIF
C
      END
C
C
      SUBROUTINE FACTRL
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***************************************************************C
C     FACTRL EVALUATES THE LOG OF THE GAMMA FUNCTIONS ACCORDING C
C     TO THE DEFINITION                                         C
C                                                               C
C     GAMMAL(I) = LOG(GAMMA(I/2))                               C
C                                                               C
C     THE STARTING VALUES ARE                                   C
C     GAMMAL(1)=DLOG(DSQRT(PI))                                 C
C     GAMMAL(2)=DLOG(0!)=0                                      C
C                                                               C
C***************************************************************C 
      COMMON/GAMMAF/GAMMAL(100),GAMMAF(100)
      DATA ZERO,ONE,PI/0.0D0,1.0D0,3.14159265358979D0/
      DATA FOUR/4.0D0/
C
      T1=DSQRT(PI)
      F1=0.5D0
      F2=ONE
      GAMMAL(1)=DLOG(T1)
      GAMMAL(2)=ZERO
      GAMMAF(1)=T1
      GAMMAF(2)=F2
      DO 10 M=2,25
      GAMMAL((2*M)-1)=GAMMAL((2*M)-3)+DLOG(F1)
      GAMMAL(2*M)=GAMMAL((2*M)-2)+DLOG(F2)
      GAMMAF((2*M)-1)=GAMMAF((2*M)-3)*F1
      GAMMAF(2*M)=GAMMAF((2*M)-2)*F2
      F1=F1+ONE
      F2=F2+ONE
10    CONTINUE
      RETURN
      END
C*********************************************************************C
C                                                                     C
C     ATOMIC SECTION FOR STARTING DENSITIES                           C
C                                                                     C
C*********************************************************************C
      SUBROUTINE ATOMIC(EXPSET,AWICHT,IZN,ICRG,NFUN,LMAXM,
     & KVALS,NKAPPA,ICENT,NSHIFT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MAXB=20,MAXB2=MAXB*MAXB,MAXAB=20,MXDIMA=MAXB*2,
     & NKAPM=7,NSYMD=2*NKAPM+1,MAXIT=30,NCENTM=6,MAXDIM=300,
     & MAXMV=5,MAXMV2=2*MAXMV,MAXORB=26)
C*********************************************************************C
C     ATOMIC SOLVES THE CLOSED SHELL AVERAGE OF CONFIGURATION         C
C     EQUATIONS FOR THE GROUND STATE OF A NEUTRAL ATOM OF CHARGE      C
C     IZNUC IN A BASIS OF SPHERICAL GAUSSIAN FUNCTIONS. THE EXPANSION C
C     COEFFICIENTS ARE STORED FOR USE AS STARTING VECTORS IN THE      C
C     MOLECULAR ORBITAL PROCEDURES                                    C
C                                                                     C
C     THE CONFIGURATION IS DETERMINED AUTOMATICALLY BY THE PROCEDURE  C
C     AUFBAU                                                          C
C*********************************************************************C 
C
      COMPLEX*16 C
C
      COMMON/GEN/EXL1(MAXB),EXL2(MAXB)
      COMMON/IJ/EIJ(MAXAB),RNIJ(3),EI,EJ
      COMMON/TEMP/F,G,NFUN1,NFUN2,L1,L2,NF2
      COMMON/NUC/PNUC
      COMMON/RLIGHT/CV,CV2
      COMMON/COEFF/C(MAXDIM,MAXDIM)
      COMMON/ATDEN/CATOM(MAXB2,MAXORB,NCENTM),LISTA(NKAPM,NCENTM)
      COMMON/CLABEL/IOCCM0
      COMMON/LABELS/LARGE(NCENTM,NKAPM,MAXMV2)
      DIMENSION FMAT1(MXDIMA,MXDIMA), FMAT2(MXDIMA,MXDIMA),
     &          FMAT3(MXDIMA,MXDIMA), FMAT4(MXDIMA,MXDIMA),
     &          OVAP1(MXDIMA,MXDIMA), OVAP2(MXDIMA,MXDIMA),
     &          HMAT1(MXDIMA,MXDIMA), HMAT2(MXDIMA,MXDIMA),
     &          DEN1(MAXB2,3),DEN2(MAXB2,3),
     &          NFUN(NKAPM,NCENTM),NOCC(NKAPM),
     &          NORB(NKAPM,NKAPM),QEFF(NKAPM),
     &          QAVE(NKAPM),EXPSET(MAXB,NKAPM,NCENTM),
     &          AWICHT(NCENTM),IZN(NCENTM),ICRG(NCENTM),
     &          LMAXM(NCENTM),KVALS(NKAPM,NCENTM),NKAPPA(NCENTM)
      DIMENSION CC1(MXDIMA,MXDIMA),CC2(MXDIMA,MXDIMA),
     &          EIGEN1(MXDIMA),EIGEN2(MXDIMA),
     &          S1(MXDIMA),S2(MXDIMA)
      DIMENSION DENLL(MAXB2,NSYMD),DENSL(MAXB2,NSYMD),
     &          DENSS(MAXB2,NSYMD),
     &          DFNLL(MAXB2,NSYMD),DFNSL(MAXB2,NSYMD),
     &          DFNSS(MAXB2,NSYMD)
      DIMENSION DLTLL1(MAXB2),DLTSL1(MAXB2),DLTSS1(MAXB2),
     &          DLTLL2(MAXB2),DLTSL2(MAXB2),DLTSS2(MAXB2)
C
      DATA ZERO,TWO,EPS/0.0D0,2.0D0,1.0D-8/
C
      IZNUC=IZN(ICENT)
      ICRGE=ICRG(ICENT)
      ZNUC=DFLOAT(IZNUC)
      AMASS=AWICHT(ICENT)
      LMAXX=LMAXM(ICENT)
      CV2=CV*CV
      CALL FACTRL3
      CALL FACTRL4
C
C     DETERMINE THE GROUND-STATE CONFIGURATION
C
      CALL AUFBAU(IZNUC,ICRGE,NORB,NOCC,LMAX)
      IF(LMAXX.LT.LMAX) THEN
      WRITE(6,*) 'INSUFFICIENT ANGULAR TYPES PRESENT IN BASIS'
       STOP 911
      ENDIF
      DO 1 L=1,LMAX+1
      WRITE(6,2) L-1,(NORB(L,J),J=1,NOCC(L))
1     CONTINUE
2     FORMAT(2X,'LQN: ',I2,' OCC: ',7(2X,I2))
C
C
C     CALCULATE NUCLEAR RADIUS EXPONENTIAL PARAMETER
C
      ATHIRD=AMASS**0.33333333333333D0
      PNUC=1.50D10*(0.529167/(0.836*ATHIRD+0.57))**2
C********************************************************************C
C     ENTER ITERATIVE SELF-CONSISTENT FIELD PROCEDURE                C
C********************************************************************C
      EOLD=ZERO
      WRITE(6,5) IZNUC
5     FORMAT(2X,40('-')/
     &       2X,'INITIALIZING ATOMIC DENSITIES FOR Z= ',I2/2X,40('-'))
      DO 1000 ITER=1,MAXIT
      E1SUM=ZERO
      E2SUM=ZERO
      ICOUNT=0
      IOCCML=IOCCM0
      DO 100 I1=1,LMAX+1
      L1=I1-1
      NFUN1=NFUN(I1,ICENT)
C********************************************************************C
C     SET UP EXPONENT VECTORS FOR L1 AND L2                          C
C********************************************************************C
      DO 30 M=1,NFUN1
      EXL1(M)=EXPSET(M,I1,ICENT)
30    CONTINUE
      IF(L1.EQ.0) THEN
       KAPL1=-1
       IJ=0
       WICHT1=TWO
       DO 31 I=1,NFUN1
       DO 31 J=1,NFUN1
       IJ=IJ+1
       DLTLL1(IJ)=WICHT1*DFNLL(IJ,1)
       DLTSL1(IJ)=WICHT1*DFNSL(IJ,1)
       DLTSS1(IJ)=WICHT1*DFNSS(IJ,1)
31     CONTINUE
       CALL REL1E(HMAT1,OVAP1,EXL1,ZNUC,KAPL1,NFUN1)
      ELSE
       KAPL1=L1
       KAPL2=-L1-1
       WICHT1=DFLOAT(2*IABS(KAPL1))
       WICHT2=DFLOAT(2*IABS(KAPL2))
       IJ1=2*L1
       IJ2=IJ1+1
       IJ=0
       DO 32 I=1,NFUN1
       DO 32 J=1,NFUN1
       IJ=IJ+1
       DLTLL1(IJ)=WICHT1*DFNLL(IJ,IJ1)
       DLTSL1(IJ)=WICHT1*DFNSL(IJ,IJ1)
       DLTSS1(IJ)=WICHT1*DFNSS(IJ,IJ1)
       DLTLL2(IJ)=WICHT2*DFNLL(IJ,IJ2)
       DLTSL2(IJ)=WICHT2*DFNSL(IJ,IJ2)
       DLTSS2(IJ)=WICHT2*DFNSS(IJ,IJ2)
32     CONTINUE
       CALL REL1E(HMAT1,OVAP1,EXL1,ZNUC,KAPL1,NFUN1)
       CALL REL1E(HMAT2,OVAP2,EXL1,ZNUC,KAPL2,NFUN1)
      ENDIF
C
C********************************************************************C
C     SPECIAL NON-ITERATIVE EXIT FOR Z=1                             C
C********************************************************************C
C
      IF(IZNUC.EQ.1) THEN
      MATDIM=NFUN1*2
C
      CALL F02AEF(HMAT1,MXDIMA,OVAP1,MXDIMA,MATDIM,
     & EIGEN1,CC1,MXDIMA,S1,S2,IFAIL)
C
C***********************************************************************C
C     COPY THE SYMMETRY-REDUCED COEFFICIENT MATRIX INTO THE MASTER      C
C     ATOMIC LIST  FOR Z=1, KAPPA=-1                                    C
C***********************************************************************C
      DO 33 KA=1,NKAPPA(ICENT)
      K1=KVALS(KA,ICENT)
      IF(K1.EQ.KAPL1) THEN
       KINDX=KA
      ENDIF
      IF(K1.EQ.KAPL2) THEN
       KINDX=KA
      ENDIF
      IF(K1.EQ.-1) THEN
      INDM1=LARGE(ICENT,KINDX,1)
      INDM2=LARGE(ICENT,KINDX,2)
      IQRGE=IZN(ICENT)-ICRG(ICENT)
      IF(IQRGE.EQ.0) THEN
       QEFFTV=0.0D0
      ELSE
       QEFFTV=DSQRT(DFLOAT(IQRGE)/2.0D0)
      ENDIF
      DO 34 M=1,NFUN1
       C(INDM1+M,IOCCML)         =
     & DCMPLX(QEFFTV*CC1(M,NFUN1+1), 0.0D0)
       C(INDM2+M,IOCCML+1)       =
     & DCMPLX(QEFFTV*CC1(M,NFUN1+1), 0.0D0)
       C(INDM1+M+NSHIFT,IOCCML)  =
     & DCMPLX(QEFFTV*CC1(M+NFUN1,NFUN1+1), 0.0D0)
       C(INDM2+M+NSHIFT,IOCCML+1)=
     & DCMPLX(QEFFTV*CC1(M+NFUN1,NFUN1+1), 0.0D0)
34    CONTINUE
      IOCCML=IOCCML+2
      ENDIF
33    CONTINUE
      RETURN
      ENDIF
C
C********************************************************************C
C     SUM OVER SYMMETRY TYPES                                        C
C********************************************************************C
C
      DO 200 I2=1,LMAX+1
      L2=I2-1
      NFUN2=NFUN(I2,ICENT)
      NF2=NFUN2*NFUN2
      DO 40 M=1,NFUN2
      EXL2(M)=EXPSET(M,I2,ICENT)
40    CONTINUE
      IF(L2.EQ.0.AND.I1.EQ.I2) THEN
      KAPR1=-1
      DO 41 M=1,NF2
      DEN1(M,1)=DENLL(M,1)
      DEN1(M,2)=DENSL(M,1)
      DEN1(M,3)=DENSS(M,1)
41    CONTINUE
      ELSEIF(L2.EQ.0.AND.I1.NE.I2) THEN
      KAPR1=-1
      DO 42 M=1,NF2
      DEN1(M,1)=DFNLL(M,1)
      DEN1(M,2)=DFNSL(M,1)
      DEN1(M,3)=DFNSS(M,1)
42    CONTINUE
      ELSEIF(L2.NE.0.AND.I1.EQ.I2) THEN
      KAPR1=L2
      KAPR2=-L2-1
      K1=2*L2
      K2=K1+1
      DO 43 M=1,NF2
      DEN1(M,1)=DENLL(M,K1)
      DEN1(M,2)=DENSL(M,K1)
      DEN1(M,3)=DENSS(M,K1)
      DEN2(M,1)=DENLL(M,K2)
      DEN2(M,2)=DENSL(M,K2)
      DEN2(M,3)=DENSS(M,K2)
43    CONTINUE
      ELSE
      KAPR1=L2
      KAPR2=-L2-1
      K1=2*L2
      K2=K1+1
      DO 44 M=1,NF2
      DEN1(M,1)=DFNLL(M,K1)
      DEN1(M,2)=DFNSL(M,K1)
      DEN1(M,3)=DFNSS(M,K1)
      DEN2(M,1)=DFNLL(M,K2)
      DEN2(M,2)=DFNSL(M,K2)
      DEN2(M,3)=DFNSS(M,K2)
44    CONTINUE
      ENDIF
      MATDIM=2*NFUN1
C********************************************************************C
C     GENERATE ALL POSSIBLE FOCK MATRICES                            C
C********************************************************************C 
      IF(ITER.NE.1) THEN
      CALL ANGMAT
      CALL  FOCK(FMAT1,FMAT2,FMAT3,FMAT4,DEN1,DEN2)
C
      IF(L1.NE.0.AND.L2.NE.0) THEN
      WEIGHT1=DFLOAT(2*IABS(KAPR1))
      WEIGHT2=DFLOAT(2*IABS(KAPR2))
      DO 50 J=1,MATDIM
      DO 50 I=1,MATDIM
      FMAT1(I,J)=WEIGHT1*FMAT1(I,J)
      FMAT2(I,J)=WEIGHT1*FMAT2(I,J)
      FMAT3(I,J)=WEIGHT2*FMAT3(I,J)
      FMAT4(I,J)=WEIGHT2*FMAT4(I,J)
      HMAT1(I,J)=HMAT1(I,J)+FMAT1(I,J)+FMAT3(I,J)
      HMAT2(I,J)=HMAT2(I,J)+FMAT2(I,J)+FMAT4(I,J)
50    CONTINUE
      IJ=0
      DO 56 I=1,NFUN1
      DO 56 J=1,NFUN1
      IJ=IJ+1
      E2SUM=E2SUM+DLTLL1(IJ)*(FMAT1(I,J)+FMAT3(I,J))+
     #TWO*DLTSL1(IJ)*(FMAT1(I+NFUN1,J)+FMAT3(I+NFUN1,J))+
     #DLTSS1(IJ)*(FMAT1(I+NFUN1,J+NFUN1)+FMAT3(I+NFUN1,J+NFUN1))+
     #            DLTLL2(IJ)*(FMAT2(I,J)+FMAT4(I,J))+
     #TWO*DLTSL2(IJ)*(FMAT2(I+NFUN1,J)+FMAT4(I+NFUN1,J))+
     #DLTSS2(IJ)*(FMAT2(I+NFUN1,J+NFUN1)+FMAT4(I+NFUN1,J+NFUN1))
56    CONTINUE
      ELSEIF(L1.NE.0.AND.L2.EQ.0) THEN
      WEIGHT1=2.0D0
      WEIGHT2=2.0D0
      DO 51 J=1,MATDIM
      DO 51 I=1,MATDIM
      FMAT1(I,J)=WEIGHT1*FMAT1(I,J)
      FMAT2(I,J)=WEIGHT2*FMAT2(I,J)
      HMAT1(I,J)=HMAT1(I,J)+FMAT1(I,J)
      HMAT2(I,J)=HMAT2(I,J)+FMAT2(I,J)
51    CONTINUE
      IJ=0
      DO 57 I=1,NFUN1
      DO 57 J=1,NFUN1
      IJ=IJ+1
      E2SUM=E2SUM+DLTLL1(IJ)*FMAT1(I,J)+
     #TWO*DLTSL1(IJ)*FMAT1(I+NFUN1,J)+
     #DLTSS1(IJ)*FMAT1(I+NFUN1,J+NFUN1)+
     #            DLTLL2(IJ)*FMAT2(I,J)+
     #TWO*DLTSL2(IJ)*FMAT2(I+NFUN1,J)+
     #DLTSS2(IJ)*FMAT2(I+NFUN1,J+NFUN1)
57    CONTINUE
      ELSEIF(L1.EQ.0.AND.L2.NE.0) THEN
      WEIGHT1=DFLOAT(2*IABS(KAPR1))
      WEIGHT2=DFLOAT(2*IABS(KAPR2))
      DO 52 J=1,MATDIM
      DO 52 I=1,MATDIM
      FMAT1(I,J)=WEIGHT1*FMAT1(I,J)
      FMAT2(I,J)=WEIGHT2*FMAT2(I,J)
      HMAT1(I,J)=HMAT1(I,J)+FMAT1(I,J)+FMAT2(I,J)
52    CONTINUE
      IJ=0
      DO 58 I=1,NFUN1
      DO 58 J=1,NFUN1
      IJ=IJ+1
      E2SUM=E2SUM+DLTLL1(IJ)*(FMAT1(I,J)+FMAT2(I,J))+
     #TWO*DLTSL1(IJ)*(FMAT1(I+NFUN1,J)+FMAT2(I+NFUN1,J))+
     #DLTSS1(IJ)*(FMAT1(I+NFUN1,J+NFUN1)+FMAT2(I+NFUN1,J+NFUN1))
58    CONTINUE
      ELSE
      WEIGHT1=2.0D0
      DO 53 J=1,MATDIM
      DO 53 I=1,MATDIM
      FMAT1(I,J)=WEIGHT1*FMAT1(I,J)
      HMAT1(I,J)=HMAT1(I,J)+FMAT1(I,J)
53    CONTINUE
      IJ=0
      DO 59 I=1,NFUN1
      DO 59 J=1,NFUN1
      IJ=IJ+1
      E2SUM=E2SUM+DLTLL1(IJ)*FMAT1(I,J)+
     #TWO*DLTSL1(IJ)*FMAT1(I+NFUN1,J)+
     #DLTSS1(IJ)*FMAT1(I+NFUN1,J+NFUN1)
59    CONTINUE
      ENDIF
      ENDIF
200   CONTINUE
201   CONTINUE
C
C
C
      NCLOSE=(4*L1)+2
      DO 63 IOCC=1,NOCC(I1)
      NQ=NORB(I1,IOCC)
      IF(NQ.EQ.NCLOSE) THEN
       QEFF(IOCC)=1.0D0
      ELSE
       QEFF(IOCC)=DFLOAT(NQ-1)/DFLOAT(NCLOSE-1)
      ENDIF    
      QAVE(IOCC)=DFLOAT(NQ)/DFLOAT(NCLOSE)  
63    CONTINUE
      IF(L1.EQ.0) THEN
      CALL F02AEF(HMAT1,MXDIMA,OVAP1,MXDIMA,MATDIM,
     & EIGEN1,CC1,MXDIMA,S1,S2,IFAIL)
C***********************************************************************C
C     COPY THE SYMMETRY-REDUCED COEFFICIENT MATRIX INTO THE MASTER      C
C     ATOMIC LIST                                                       C
C***********************************************************************C
C
C     KAPPA=-1
      DO 450 KA=1,NKAPPA(ICENT)
      KA1=KVALS(KA,ICENT)
      IF(KA1.EQ.-1) THEN
      KINDX=KA
      INDX1=LARGE(ICENT,KINDX,1)
      INDX2=LARGE(ICENT,KINDX,2)
      DO 451 IOCC=1,NOCC(I1)
      IF(QAVE(IOCC).LE.0.0D0) THEN
       QEFFTV=0.0D0
      ELSE
       QEFFTV=DSQRT(QAVE(IOCC))
      ENDIF
      DO 453 M=1,NFUN1
      C(INDX1+M,IOCCML)          =
     &DCMPLX(QEFFTV*CC1(M,NFUN1+IOCC), 0.0D0)
      C(INDX2+M,IOCCML+1)        =
     &DCMPLX(QEFFTV*CC1(M,NFUN1+IOCC), 0.0D0)
      C(INDX1+M+NSHIFT,IOCCML)   =
     &DCMPLX(QEFFTV*CC1(M+NFUN1,NFUN1+IOCC), 0.0D0)
      C(INDX2+M+NSHIFT,IOCCML+1) =
     &DCMPLX(QEFFTV*CC1(M+NFUN1,NFUN1+IOCC), 0.0D0)
453   CONTINUE
      IOCCML=IOCCML+2
451   CONTINUE       
      ENDIF
450   CONTINUE
C
      IJ=0
      DO 60 I=1,NFUN1
      DO 60 J=1,NFUN1
      IJ=IJ+1
      DENLL(IJ,1)=QEFF(1)*CC1(I,NFUN1+1)*CC1(J,NFUN1+1)
      DENSL(IJ,1)=QEFF(1)*CC1(I+NFUN1,NFUN1+1)*CC1(J,NFUN1+1)
      DENSS(IJ,1)=QEFF(1)*CC1(I+NFUN1,NFUN1+1)*CC1(J+NFUN1,NFUN1+1)
      DFNLL(IJ,1)=QAVE(1)*CC1(I,NFUN1+1)*CC1(J,NFUN1+1)
      DFNSL(IJ,1)=QAVE(1)*CC1(I+NFUN1,NFUN1+1)*CC1(J,NFUN1+1)
      DFNSS(IJ,1)=QAVE(1)*CC1(I+NFUN1,NFUN1+1)*CC1(J+NFUN1,NFUN1+1)
60    CONTINUE
      WEIGHT1=DFLOAT(2*IABS(KAPL1))
      E1SUM=E1SUM+QAVE(1)*WEIGHT1*EIGEN1(NFUN1+1)
      IF(NOCC(I1).GT.1) THEN
      DO 61 IOCC=2,NOCC(I1)
      E1SUM=E1SUM+QAVE(IOCC)*WEIGHT1*EIGEN1(NFUN1+IOCC)
      IJ=0
      DO 61 I=1,NFUN1
      DO 61 J=1,NFUN1
      IJ=IJ+1
      DENLL(IJ,1)=DENLL(IJ,1)+
     &     QEFF(IOCC)*CC1(I,NFUN1+IOCC)*CC1(J,NFUN1+IOCC)
      DENSL(IJ,1)=DENSL(IJ,1)+
     &     QEFF(IOCC)*CC1(I+NFUN1,NFUN1+IOCC)*CC1(J,NFUN1+IOCC)
      DENSS(IJ,1)=DENSS(IJ,1)+
     &     QEFF(IOCC)*CC1(I+NFUN1,NFUN1+IOCC)*CC1(J+NFUN1,NFUN1+IOCC)
      DFNLL(IJ,1)=DFNLL(IJ,1)+
     &     QAVE(IOCC)*CC1(I,NFUN1+IOCC)*CC1(J,NFUN1+IOCC)
      DFNSL(IJ,1)=DFNSL(IJ,1)+
     &     QAVE(IOCC)*CC1(I+NFUN1,NFUN1+IOCC)*CC1(J,NFUN1+IOCC)
      DFNSS(IJ,1)=DFNSS(IJ,1)+
     &     QAVE(IOCC)*CC1(I+NFUN1,NFUN1+IOCC)*CC1(J+NFUN1,NFUN1+IOCC)
61    CONTINUE
      ENDIF
      ELSE
C
      CALL F02AEF(HMAT1,MXDIMA,OVAP1,MXDIMA,MATDIM,
     & EIGEN1,CC1,MXDIMA,S1,S2,IFAIL)
      CALL F02AEF(HMAT2,MXDIMA,OVAP2,MXDIMA,MATDIM,
     & EIGEN2,CC2,MXDIMA,S1,S2,IFAIL)
C***********************************************************************C
C     COPY THE SYMMETRY-REDUCED COEFFICIENT MATRIX INTO THE MASTER      C
C     ATOMIC LIST                                                       C
C***********************************************************************C
C
C     POSITIVE KAPPA CASE 
C
      DO 550 KA=1,NKAPPA(ICENT)
      KA1=KVALS(KA,ICENT)
      IF(KA1.EQ.KAPL1) THEN
       KINDX=KA
       NMVALS=IABS(KAPL1)
       DO 540 IMVAL=1,NMVALS
       INDX1=LARGE(ICENT,KINDX,IMVAL*2-1)
       INDX2=LARGE(ICENT,KINDX,IMVAL*2)
       DO 530 IOCC=1,NOCC(I1)
       IF(QAVE(IOCC).LE.0.0D0) THEN
        QEFFTV=0.0D0
       ELSE
        QEFFTV=DSQRT(QAVE(IOCC))
       ENDIF
       DO 520 M=1,NFUN1
       C(INDX1+M,IOCCML)          =
     & DCMPLX(QEFFTV*CC1(M,NFUN1+IOCC), 0.0D0)
       C(INDX2+M,IOCCML+1)        =
     & DCMPLX(QEFFTV*CC1(M,NFUN1+IOCC), 0.0D0)
       C(INDX1+M+NSHIFT,IOCCML)   =
     & DCMPLX(QEFFTV*CC1(M+NFUN1,NFUN1+IOCC), 0.0D0)
       C(INDX2+M+NSHIFT,IOCCML+1) =
     & DCMPLX(QEFFTV*CC1(M+NFUN1,NFUN1+IOCC), 0.0D0)
520    CONTINUE
       IOCCML=IOCCML+2
530    CONTINUE
540    CONTINUE
      ENDIF
550   CONTINUE      
C
C     NEGATIVE KAPPA CASE 
C
      DO 650 KA=1,NKAPPA(ICENT)
      KA1=KVALS(KA,ICENT)
      IF(KA1.EQ.KAPL2) THEN
       KINDX=KA
       NMVALS=IABS(KAPL2)
       DO 640 IMVAL=1,NMVALS
       INDX1=LARGE(ICENT,KINDX,IMVAL*2-1)
       INDX2=LARGE(ICENT,KINDX,IMVAL*2)
       DO 630 IOCC=1,NOCC(I1)
       IF(QAVE(IOCC).LE.0.0D0) THEN
        QEFFTV=0.0D0
       ELSE
        QEFFTV=DSQRT(QAVE(IOCC))
       ENDIF
       DO 620 M=1,NFUN1
       C(INDX1+M,IOCCML)          =
     & DCMPLX(QEFFTV*CC2(M,NFUN1+IOCC), 0.0D0)
       C(INDX2+M,IOCCML+1)        =
     & DCMPLX(QEFFTV*CC2(M,NFUN1+IOCC), 0.0D0)
       C(INDX1+M+NSHIFT,IOCCML)   =
     & DCMPLX(QEFFTV*CC2(M+NFUN1,NFUN1+IOCC), 0.0D0)
       C(INDX2+M+NSHIFT,IOCCML+1) =
     & DCMPLX(QEFFTV*CC2(M+NFUN1,NFUN1+IOCC), 0.0D0)
620    CONTINUE
       IOCCML=IOCCML+2
630    CONTINUE
640    CONTINUE
      ENDIF
650   CONTINUE      
C
      WEIGHT1=DFLOAT(2*IABS(KAPL1))
      WEIGHT2=DFLOAT(2*IABS(KAPL2))
      E1SUM=E1SUM+QAVE(1)*WEIGHT1*EIGEN1(NFUN1+1)
     &           +QAVE(1)*WEIGHT2*EIGEN2(NFUN1+1)
C
      J1=2*L1
      J2=J1+1
      IJ=0
      DO 70 I=1,NFUN1
      DO 70 J=1,NFUN1
      IJ=IJ+1
      DENLL(IJ,J1)=QEFF(1)*CC1(I,NFUN1+1)*CC1(J,NFUN1+1)
      DENSL(IJ,J1)=QEFF(1)*CC1(I+NFUN1,NFUN1+1)*CC1(J,NFUN1+1)
      DENSS(IJ,J1)=QEFF(1)*CC1(I+NFUN1,NFUN1+1)*CC1(J+NFUN1,NFUN1+1)
      DENLL(IJ,J2)=QEFF(1)*CC2(I,NFUN1+1)*CC2(J,NFUN1+1)
      DENSL(IJ,J2)=QEFF(1)*CC2(I+NFUN1,NFUN1+1)*CC2(J,NFUN1+1)
      DENSS(IJ,J2)=QEFF(1)*CC2(I+NFUN1,NFUN1+1)*CC2(J+NFUN1,NFUN1+1)
C
      DFNLL(IJ,J1)=QAVE(1)*CC1(I,NFUN1+1)*CC1(J,NFUN1+1)
      DFNSL(IJ,J1)=QAVE(1)*CC1(I+NFUN1,NFUN1+1)*CC1(J,NFUN1+1)
      DFNSS(IJ,J1)=QAVE(1)*CC1(I+NFUN1,NFUN1+1)*CC1(J+NFUN1,NFUN1+1)
      DFNLL(IJ,J2)=QAVE(1)*CC2(I,NFUN1+1)*CC2(J,NFUN1+1)
      DFNSL(IJ,J2)=QAVE(1)*CC2(I+NFUN1,NFUN1+1)*CC2(J,NFUN1+1)
      DFNSS(IJ,J2)=QAVE(1)*CC2(I+NFUN1,NFUN1+1)*CC2(J+NFUN1,NFUN1+1)
70    CONTINUE
      IF(NOCC(I1).GT.1) THEN
      DO 71 IOCC=2,NOCC(I1)
      E1SUM=E1SUM+QAVE(IOCC)*WEIGHT1*EIGEN1(NFUN1+IOCC)
     &           +QAVE(IOCC)*WEIGHT2*EIGEN2(NFUN1+IOCC)
      IJ=0
      DO 71 I=1,NFUN1
      DO 71 J=1,NFUN1
      IJ=IJ+1
      DENLL(IJ,J1)=DENLL(IJ,J1)+
     &     QEFF(IOCC)*CC1(I,NFUN1+IOCC)*CC1(J,NFUN1+IOCC)
      DENSL(IJ,J1)=DENSL(IJ,J1)+
     &     QEFF(IOCC)*CC1(I+NFUN1,NFUN1+IOCC)*CC1(J,NFUN1+IOCC)
      DENSS(IJ,J1)=DENSS(IJ,J1)+
     &     QEFF(IOCC)*CC1(I+NFUN1,NFUN1+IOCC)*CC1(J+NFUN1,NFUN1+IOCC)
      DENLL(IJ,J2)=DENLL(IJ,J2)+
     &     QEFF(IOCC)*CC2(I,NFUN1+IOCC)*CC2(J,NFUN1+IOCC)
      DENSL(IJ,J2)=DENSL(IJ,J2)+
     &     QEFF(IOCC)*CC2(I+NFUN1,NFUN1+IOCC)*CC2(J,NFUN1+IOCC)
      DENSS(IJ,J2)=DENSS(IJ,J2)+
     &     QEFF(IOCC)*CC2(I+NFUN1,NFUN1+IOCC)*CC2(J+NFUN1,NFUN1+IOCC)
C
      DFNLL(IJ,J1)=DFNLL(IJ,J1)+
     &     QAVE(IOCC)*CC1(I,NFUN1+IOCC)*CC1(J,NFUN1+IOCC)
      DFNSL(IJ,J1)=DFNSL(IJ,J1)+
     &     QAVE(IOCC)*CC1(I+NFUN1,NFUN1+IOCC)*CC1(J,NFUN1+IOCC)
      DFNSS(IJ,J1)=DFNSS(IJ,J1)+
     &     QAVE(IOCC)*CC1(I+NFUN1,NFUN1+IOCC)*CC1(J+NFUN1,NFUN1+IOCC)
      DFNLL(IJ,J2)=DFNLL(IJ,J2)+
     &     QAVE(IOCC)*CC2(I,NFUN1+IOCC)*CC2(J,NFUN1+IOCC)
      DFNSL(IJ,J2)=DFNSL(IJ,J2)+
     &     QAVE(IOCC)*CC2(I+NFUN1,NFUN1+IOCC)*CC2(J,NFUN1+IOCC)
      DFNSS(IJ,J2)=DFNSS(IJ,J2)+
     &     QAVE(IOCC)*CC2(I+NFUN1,NFUN1+IOCC)*CC2(J+NFUN1,NFUN1+IOCC)
71    CONTINUE
      ENDIF
      ENDIF
100   CONTINUE
      E2SUM=E2SUM/TWO
      ETOTAL=E1SUM-E2SUM
      WRITE(6,1020) ITER,ETOTAL
      ETEST=DABS((EOLD-ETOTAL)/ETOTAL)
      IF(ETEST.LE.EPS) GO TO 1001
      EOLD=ETOTAL      
1000  CONTINUE
1001  CONTINUE
      IOCCM0=IOCCML
1020  FORMAT(2X,'ITERATION:',2X,I2,' TOTAL ENERGY: ',F14.6)
      RETURN
      END
C
C
C
      SUBROUTINE REL1E(HMAT,OVAP,EXL,ZNUC,KAPPA,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C********************************************************************C
C               REL1E CALCULATES THE DIRAC AND OVERLAP               C
C                 MATRICES FOR ANY SYMMETRY TYPE KAPPA               C
C                                                                    C
C     THIS VERSION MODFIED TO USE EVEN-TEMPERED SPHERICAL GAUSSIAN   C
C                          BASIS FUNCTIONS                           C
C                                                                    C
C********************************************************************C
      PARAMETER (MAXB=20,MAXB2=MAXB*MAXB,MAXAB=20,MXDIMA=MAXB*2)
      DIMENSION HMAT(MXDIMA,MXDIMA),OVAP(MXDIMA,MXDIMA),EXL(MAXB),
     & RN(MAXB2,3)
      COMMON/RLIGHT/CVEL,CV2
      COMMON/GAMMAF2/GAMMAF(100)
      DATA ZERO,ONE,TWO,THREE,FOUR,HALF/
     &   0.0D0,1.0D0,2.0D0,3.0D0,4.0D0,5.0D-1/
C
      IF(KAPPA.LT.0) THEN
       L=-KAPPA-1
      ELSE
       L=KAPPA
      ENDIF
      RL=DFLOAT(L)
      CALL RNORMA(RN,EXL,N,L)
C 
      IF(KAPPA.GT.0) THEN
C
C     DIRAC MATRIX FOR KAPPA > 0
C
      G=DFLOAT(2*L+1)
      M=0
      DO 10 I=1,N
      EI=EXL(I)
      DO 10 J=1,N
      M=M+1
      EJ=EXL(J)
      EIJ=EI+EJ
      EIJP=EI*EJ
      t3 = RINT(2*L+1,EIJ)
      t6 = RL+HALF
      t7 = EIJ**t6
      t8 = 1.0D0/t7
      t10 = G**2
      t14 = RL+0.15D1
      t16 = EIJ**2
      t20 = t10*HALF-G*t6+TWO*EIJP*t6*t14/t16
      t21 = GAMMAF(2*L+1)*t8*t20
      t34 = CVEL**2
      t40 = EIJ**t14
      F1 = -ZNUC*RN(M,1)*t3
      F2 = CVEL*RN(M,2)*t21
      F3 = -ZNUC*RN(M,3)*(t10*RINT(2*L-1,EIJ)-0.2D1*EIJ*G*t3+0.4D1*EIJ
     #P*RINT(2*L+3,EIJ))-TWO*t34*RN(M,3)*t21
      F4 = RN(M,1)*HALF*GAMMAF(2*L+3)/t40
      F5 = RN(M,3)*GAMMAF(2*L+1)*t8*t20
      HMAT(I,J) =F1
      HMAT(I+N,J) =F2
      HMAT(J,I+N)=HMAT(I+N,J)
      HMAT(I+N,J+N) =F3
      OVAP(I,J) =F4
      OVAP(I+N,J+N) =F5
      OVAP(I+N,J)=ZERO
      OVAP(J,I+N)=ZERO
10    CONTINUE
      ELSE
C
C     DIRAC MATRIX FOR KAPPA < 0
C
      M=0
      DO 20 I=1,N
      EI=EXL(I)
      DO 20 J=1,N
      M=M+1
      EJ=EXL(J)
      EIJ=EI+EJ
      EIJP=EI*EJ
      t6 = RL+HALF
      t7 = EIJ**t6
      t8 = 1.0D0/t7
      t12 = RL+0.15D1
      t14 = EIJ**2
      t15 = 1.0D0/t14
      t16 = t6*t12*t15
      t24 = TWO**2
      t25 = CVEL**2
      t27 = RN(M,3)*GAMMAF(2*L+1)
      t34 = EIJ**t12
      F1 = -ZNUC*RN(M,1)*RINT(2*L+1,EIJ)
      F2 = CVEL*RN(M,2)*GAMMAF(2*L+1)*t8*TWO*EIJP*t16
      F3 =-0.4D1*ZNUC*RN(M,3)*EIJP*RINT(2*L+3,EIJ)-t24*t25*t27*t8*EIJ
     #P*t16
      F4 = RN(M,1)*HALF*GAMMAF(2*L+3)/t34
      F5 = t27*t8*TWO*EIJP*t6*t12*t15
      HMAT(I,J) =F1
      HMAT(I+N,J) = F2
      HMAT(J,I+N)=HMAT(I+N,J)
      HMAT(I+N,J+N) = F3
      OVAP(I,J) = F4
      OVAP(I+N,J+N) = F5
      OVAP(I+N,J)=ZERO
      OVAP(J,I+N)=ZERO
20    CONTINUE
      ENDIF
      RETURN
      END
C
C
C
      SUBROUTINE FOCK(FMAT1,FMAT2,FMAT3,FMAT4,DEN1,DEN2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C********************************************************************C
C     FOCK CONSTRUCTS THE FOCK MATRIX FROM LISTS OF INTEGRALS        C
C     AND THE DENSITY MATRIX                                         C
C********************************************************************C
      PARAMETER (MAXB=20,MAXB2=MAXB*MAXB,MAXAB=20,MXDIMA=MAXB*2)
      COMMON/TEMP/F,G,NFUN1,NFUN2,L1,L2,NF2
      COMMON/GEN/EXL1(MAXB),EXL2(MAXB)
      COMMON/IJ/EIJ(MAXAB),RNIJ(3),EI,EJ
      COMMON/INTS/RJSSSS(MAXB2,4),RJLLSS(MAXB2,2),
     & RJSSLL(MAXB2,2),RJLLLL(MAXB2),RKLLLL(MAXB2,4),
     & RKSLSL(MAXB2,4),RKSSSS(MAXB2,4)
      COMMON/KL/EKL(MAXB2,MAXAB),EKL0(MAXB2),
     & EIK(MAXB2,MAXAB),EJL(MAXB2,MAXAB),
     & B1(MAXB2,MAXAB,MAXAB),B2(MAXB2,MAXAB,MAXAB),
     & B3(MAXB2,MAXAB,MAXAB),B4(MAXB2,MAXAB,MAXAB),
     & EK(MAXB2),EL(MAXB2),RNKL(MAXB2,3),EKL1(MAXB2)
      DIMENSION FMAT1(MXDIMA,MXDIMA), FMAT2(MXDIMA,MXDIMA),
     &          FMAT3(MXDIMA,MXDIMA), FMAT4(MXDIMA,MXDIMA),
     &          DEN1(MAXB2,3),DEN2(MAXB2,3),RNIJA(MAXB2,3)
      DATA ZERO,TWO,CV,EPS/0.0D0,2.0D0,1.370359898D2,1.0D-8/
C
      G=DFLOAT(2*L1+1)
      F=DFLOAT(2*L2+1)
C
      CALL KLSET
      CALL RNORMA(RNIJA,EXL1,NFUN1,L1)
C
      IJ=0
      DO 100 I=1,NFUN1
      EI=EXL1(I)
      DO 100 J=1,NFUN1
      EJ=EXL1(J)
      IJ=IJ+1
      EIJ0=EI+EJ
      EIJR=DSQRT(EIJ0)
      EIJA=EIJ0**(-L1)
      DO 30 K=1,6
      EIJ(K)=EIJA
      EIJA=EIJA/EIJR
30    CONTINUE
      RNIJ(1)=RNIJA(IJ,1)
      RNIJ(2)=RNIJA(IJ,2)
      RNIJ(3)=RNIJA(IJ,3)
      CALL KLINIT
      CALL E2C1
C
      F1LL=ZERO
      F2LL=ZERO
      F3LL=ZERO
      F4LL=ZERO
      F1SL=ZERO
      F2SL=ZERO
      F3SL=ZERO
      F4SL=ZERO
      F1SS=ZERO
      F2SS=ZERO
      F3SS=ZERO
      F4SS=ZERO
      IF(L1.NE.0.AND.L2.NE.0) THEN
      DO 33 M=1,NF2
      F1LL=F1LL + RJLLLL(M)*DEN1(M,1)
     &    +RJLLSS(M,1)*DEN1(M,3)
     &    -RKLLLL(M,1)*DEN1(M,1)
      F2LL=F2LL + RJLLLL(M)*DEN1(M,1)
     &    +RJLLSS(M,1)*DEN1(M,3)
     &    -RKLLLL(M,2)*DEN1(M,1)
      F3LL=F3LL + RJLLLL(M)*DEN2(M,1)
     &    +RJLLSS(M,2)*DEN2(M,3)
     &    -RKLLLL(M,3)*DEN2(M,1)
      F4LL=F4LL + RJLLLL(M)*DEN2(M,1)
     &    +RJLLSS(M,2)*DEN2(M,3)
     &    -RKLLLL(M,4)*DEN2(M,1)
      F1SL=F1SL-RKSLSL(M,1)*DEN1(M,2)
      F2SL=F2SL-RKSLSL(M,2)*DEN1(M,2)
      F3SL=F3SL-RKSLSL(M,3)*DEN2(M,2)
      F4SL=F4SL-RKSLSL(M,4)*DEN2(M,2)
      F1SS=F1SS+RJSSSS(M,1)*DEN1(M,3)
     &    +RJSSLL(M,1)*DEN1(M,1)
     &    -RKSSSS(M,1)*DEN1(M,3)
      F2SS=F2SS+RJSSSS(M,2)*DEN1(M,3)
     &    +RJSSLL(M,2)*DEN1(M,1)
     &    -RKSSSS(M,2)*DEN1(M,3)
      F3SS=F3SS+RJSSSS(M,3)*DEN2(M,3)
     &    +RJSSLL(M,1)*DEN2(M,1)
     &    -RKSSSS(M,3)*DEN2(M,3)
      F4SS=F4SS+RJSSSS(M,4)*DEN2(M,3)
     &    +RJSSLL(M,2)*DEN2(M,1)
     &    -RKSSSS(M,4)*DEN2(M,3)
  33  CONTINUE
      FMAT1(I,J)=F1LL
      FMAT1(I+NFUN1,J)=F1SL
      FMAT1(J,I+NFUN1)=F1SL
      FMAT1(I+NFUN1,J+NFUN1)=F1SS
      FMAT2(I,J)=F2LL
      FMAT2(I+NFUN1,J)=F2SL
      FMAT2(J,I+NFUN1)=F2SL
      FMAT2(I+NFUN1,J+NFUN1)=F2SS
      FMAT3(I,J)=F3LL
      FMAT3(I+NFUN1,J)=F3SL
      FMAT3(J,I+NFUN1)=F3SL
      FMAT3(I+NFUN1,J+NFUN1)=F3SS
      FMAT4(I,J)=F4LL
      FMAT4(I+NFUN1,J)=F4SL
      FMAT4(J,I+NFUN1)=F4SL
      FMAT4(I+NFUN1,J+NFUN1)=F4SS
      ELSEIF(L1.NE.0.AND.L2.EQ.0) THEN
      DO 35 M=1,NF2
      F1LL=F1LL+RJLLLL(M)*DEN1(M,1)
     &    +RJLLSS(M,1)*DEN1(M,3)
     &    -RKLLLL(M,1)*DEN1(M,1)
      F2LL=F2LL+RJLLLL(M)*DEN1(M,1)
     &    +RJLLSS(M,1)*DEN1(M,3)
     &    -RKLLLL(M,2)*DEN1(M,1)
      F1SL=F1SL-RKSLSL(M,1)*DEN1(M,2)
      F2SL=F2SL-RKSLSL(M,2)*DEN1(M,2)
      F1SS=F1SS+RJSSSS(M,1)*DEN1(M,3)
     &    +RJSSLL(M,1)*DEN1(M,1)
     &    -RKSSSS(M,1)*DEN1(M,3)
      F2SS=F2SS+RJSSSS(M,2)*DEN1(M,3)
     &    +RJSSLL(M,2)*DEN1(M,1)
     &    -RKSSSS(M,2)*DEN1(M,3)
  35  CONTINUE
      FMAT1(I,J)=F1LL
      FMAT1(I+NFUN1,J)=F1SL
      FMAT1(J,I+NFUN1)=F1SL
      FMAT1(I+NFUN1,J+NFUN1)=F1SS
      FMAT2(I,J)=F2LL
      FMAT2(I+NFUN1,J)=F2SL
      FMAT2(J,I+NFUN1)=F2SL
      FMAT2(I+NFUN1,J+NFUN1)=F2SS
      ELSEIF(L1.EQ.0.AND.L2.NE.0) THEN
      DO 34 M=1,NF2
      F1LL=F1LL+RJLLLL(M)*DEN1(M,1)
     &    +RJLLSS(M,1)*DEN1(M,3)
     &    -RKLLLL(M,1)*DEN1(M,1)
      F2LL=F2LL+RJLLLL(M)*DEN2(M,1)
     &    +RJLLSS(M,2)*DEN2(M,3)
     &    -RKLLLL(M,2)*DEN2(M,1)
      F1SL=F1SL-RKSLSL(M,1)*DEN1(M,2)
      F2SL=F2SL-RKSLSL(M,2)*DEN2(M,2)
      F1SS=F1SS+RJSSSS(M,1)*DEN1(M,3)
     &    +RJSSLL(M,1)*DEN1(M,1)
     &    -RKSSSS(M,1)*DEN1(M,3)
      F2SS=F2SS+RJSSSS(M,2)*DEN2(M,3)
     &    +RJSSLL(M,1)*DEN2(M,1)
     &    -RKSSSS(M,2)*DEN2(M,3)
  34  CONTINUE
      FMAT1(I,J)=F1LL
      FMAT1(I+NFUN1,J)=F1SL
      FMAT1(J,I+NFUN1)=F1SL
      FMAT1(I+NFUN1,J+NFUN1)=F1SS
      FMAT2(I,J)=F2LL
      FMAT2(I+NFUN1,J)=F2SL
      FMAT2(J,I+NFUN1)=F2SL
      FMAT2(I+NFUN1,J+NFUN1)=F2SS
      ELSEIF(L1.EQ.0.AND.L2.EQ.0) THEN
      DO 36 M=1,NF2
      F1LL=F1LL+RJLLLL(M)*DEN1(M,1)
     &    +RJLLSS(M,1)*DEN1(M,3)
     &    -RKLLLL(M,1)*DEN1(M,1)
      F1SL=F1SL-RKSLSL(M,1)*DEN1(M,2)
      F1SS=F1SS+RJSSSS(M,1)*DEN1(M,3)
     &    +RJSSLL(M,1)*DEN1(M,1)
     &    -RKSSSS(M,1)*DEN1(M,3)
  36  CONTINUE
      FMAT1(I,J)=F1LL
      FMAT1(I+NFUN1,J)=F1SL
      FMAT1(J,I+NFUN1)=F1SL
      FMAT1(I+NFUN1,J+NFUN1)=F1SS
      ENDIF
C
100   CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE RNORMA(RN,EXL,NFUN,LQN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB)
C*******************************************************************C
C     RNORM CALCULATES PRODUCTS OF NORMALIZATION CONSTANTS RNTT FOR C
C     BOTH SIGNS OF KAPPA, FIXED LQN, EXPONENTS EXL, DIMENSION NFUN C
C*******************************************************************C
      DIMENSION RN(MAXB2,3),EXL(MAXB)
      DIMENSION RNL(MAXB),RNS(MAXB)
      COMMON/GAMMAT/GAMMAL(100),GAMMAF(100)
      DATA TWOLOG,HALF1,HALF3,TWO
     & /6.931471805599453D-1,5.0D-1,1.5D0,2.0D0/
C
      NF2=NFUN*NFUN
      RLQN=DFLOAT(LQN)
      G1=TWOLOG-GAMMAL(2*LQN+3)
      G2=TWOLOG-GAMMAL(2*LQN+5)
      R1=RLQN+HALF3
      R2=RLQN+HALF1
      DO 10 M=1,NFUN
      ELOG=DLOG(TWO*EXL(M))
      RNL(M)=DEXP(HALF1*(G1+R1*ELOG))
      RNS(M)=DEXP(HALF1*(G2+R2*ELOG))
10    CONTINUE
C
C     RN(M,1) ARE THE LL NORMALIZATION CONSTANTS
C     RN(M,2) ARE THE SL NORMALIZATION CONSTANTS
C     RN(M,3) ARE THE SS NORMALIZATION CONSTANTS
C
      M=0
      DO 20 I=1,NFUN
      DO 20 J=1,NFUN
      M=M+1
      RN(M,1)=RNL(I)*RNL(J)
      RN(M,2)=RNS(I)*RNL(J)
      RN(M,3)=RNS(I)*RNS(J)
20    CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE KLSET
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MAXB=20,MAXB2=MAXB*MAXB,MAXAB=20,NUMAX=10)
      COMMON/TEMP/F,G,NFUN1,NFUN2,L1,L2,NF2
      COMMON/GEN/EXL1(MAXB),EXL2(MAXB)
      COMMON/IJ/EIJ(MAXAB),RNIJ(3),EI,EJ
      COMMON/KL/EKL(MAXB2,MAXAB),EKL0(MAXB2),
     & EIK(MAXB2,MAXAB),EJL(MAXB2,MAXAB),
     & B1(MAXB2,MAXAB,MAXAB),B2(MAXB2,MAXAB,MAXAB),
     & B3(MAXB2,MAXAB,MAXAB),B4(MAXB2,MAXAB,MAXAB),
     & EK(MAXB2),EL(MAXB2),RNKL(MAXB2,3),EKL1(MAXB2)
      COMMON/ANG/ BK1(NUMAX),BK2(NUMAX),BK3(NUMAX),BK4(NUMAX),
     & NUS(NUMAX),NNUS
      COMMON/INDX/IKIND(MAXB2),JLIND(MAXB2)
      COMMON/GAMMAT/GAMMAL(100),GAMMAF(100)
C
      KL = 0
      DO 22 K=1, NFUN2
      EK0 = EXL2(K)
      DO 22 L=1, NFUN2
      EL0 = EXL2(L)
      KL=KL+1
      IKIND(KL)=K
      JLIND(KL)=L
      EK(KL)=EK0
      EL(KL)=EL0
      EKL1(KL)=EK0*EL0
      EKL0(KL) = EK0+EL0
      EKLR = DSQRT(EKL0(KL))
      EKLA = EKL0(KL)**(-L2)
      DO 23 I=1,6
      EKL(KL,I) = EKLA
      EKLA = EKLA/EKLR
   23 CONTINUE  
   22 CONTINUE
C
C********************************************************************C
C     NORMALIZATION CONSTANTS                                        C
C********************************************************************C
C
      CALL RNORMA(RNKL,EXL2,NFUN2,L2)
C
      RETURN
      END
C
C
C
      SUBROUTINE KLINIT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MAXB=20,MAXB2=MAXB*MAXB,MAXAB=20,NUMAX=10)
      COMMON/TEMP/F,G,NFUN1,NFUN2,L1,L2,NF2
      COMMON/GEN/EXL1(MAXB),EXL2(MAXB)
      COMMON/IJ/EIJ(MAXAB),RNIJ(3),EI,EJ
      COMMON/KL/EKL(MAXB2,MAXAB),EKL0(MAXB2),
     & EIK(MAXB2,MAXAB),EJL(MAXB2,MAXAB),
     & B1(MAXB2,MAXAB,MAXAB),B2(MAXB2,MAXAB,MAXAB),
     & B3(MAXB2,MAXAB,MAXAB),B4(MAXB2,MAXAB,MAXAB),
     & EK(MAXB2),EL(MAXB2),RNKL(MAXB2,3),EKL1(MAXB2)
      COMMON/ANG/ BK1(NUMAX),BK2(NUMAX),BK3(NUMAX),BK4(NUMAX),
     & NUS(NUMAX),NNUS
      COMMON/INDX/IKIND(MAXB2),JLIND(MAXB2)
      DIMENSION XJ(MAXB2,2),XK(MAXB2,2),
     & RTIK(MAXB2),RTJL(MAXB2),RTIK0(MAXB),RTJL0(MAXB),
     & PTIK0(MAXB),PTJL0(MAXB),TTIK0(MAXB),TTJL0(MAXB)
       DIMENSION BETA(MAXB2),BETA1(MAXB2),XROOT(MAXB2),
     &          SUM(MAXB2),TERM(MAXB2),IAA(2),IBB(2)
      COMMON/GAMMAT/GAMMAL(100),GAMMAF(100)
      DATA ZERO,ONE,TWO,HALF/0.0D0,1.0D0,2.0D0,5.0D-1/
C
      NU0=NUS(NNUS)
      IPOWER=L1+L2-NU0
      DO 5 K=1,NFUN2
      TTIK0(K)=EI+EXL2(K)
      TTJL0(K)=EJ+EXL2(K)
      RTIK0(K)=DSQRT(TTIK0(K))
      RTJL0(K)=DSQRT(TTJL0(K))
      PTIK0(K)=RTIK0(K)**(-IPOWER)
      PTJL0(K)=RTJL0(K)**(-IPOWER)
5     CONTINUE
      TIJ0=EI+EJ
      DO 10 M=1,NF2
      TIK0=TTIK0(IKIND(M))
      TJL0=TTJL0(JLIND(M))
      TIJKL=TIJ0+EKL0(M)
      XJ(M,1)=TIJ0/TIJKL
      XJ(M,2)=EKL0(M)/TIJKL
      XK(M,1)=TIK0/TIJKL
      XK(M,2)=TJL0/TIJKL
      RTIK(M)=RTIK0(IKIND(M))
      RTJL(M)=RTJL0(JLIND(M))
      EIK(M,1)=PTIK0(IKIND(M))
      EJL(M,1)=PTJL0(JLIND(M))
10    CONTINUE
      DO 20 INU=2,2*NU0+6
      DO 20 M=1,NF2
      EIK(M,INU)=EIK(M,INU-1)/RTIK(M)
      EJL(M,INU)=EJL(M,INU-1)/RTJL(M)
20    CONTINUE
C
C********************************************************************C
C     GENERATE ALL OF THE INCOMPLETE BETA FUNCTIONS FOR J-TYPE       C
C********************************************************************C
      NVALS=3
      DO 30 I1=1,NVALS
      NT1=2*(I1-1)
      IAA(1)=2*L1+NT1+1
      IAA(2)=2*L2+NT1+1
      DO 30 I2=1,NVALS
      NT2=2*(I2-1)
      IBB(1)=2*L2+NT2
      IBB(2)=2*L1+NT2
C********************************************************************C
C     BEGIN INLINE BETA FUNCTION CODE                                C
C********************************************************************C
      DO 900 IBETA=1,2
      IA=(IAA(IBETA)-1)/2
      IB=IBB(IBETA)/2
C
C**************************************************************C
C     CASE I: IB>1                                             C
C**************************************************************C
C
      IF(IB.GT.1) THEN
      FACTOR=DFLOAT(IA)+HALF
      IFACTOR=2*IA+1
      DO 200 M=1,NF2
      BETA1(M)=(DSQRT(XJ(M,IBETA))**IFACTOR)/FACTOR
200   CONTINUE
      RA=FACTOR
      RB=DFLOAT(1-IB)
      RC=FACTOR+ONE
      RD=ONE
      FACT=RA*RB/(RC*RD)
      DO 2011 M=1,NF2
      TERM(M)=FACT*XJ(M,IBETA)
      SUM(M)=ONE+TERM(M)
2011  CONTINUE
      RA=RA+ONE
      RB=RB+ONE
      RC=RC+ONE
      RD=RD+ONE
      DO 202 IT=2,IB-1
      FACT=RA*RB/(RC*RD)
      DO 201 M=1,NF2
      TERM(M)=FACT*TERM(M)*XJ(M,IBETA)
      SUM(M)=SUM(M)+TERM(M)
201   CONTINUE
      RA=RA+ONE
      RB=RB+ONE
      RC=RC+ONE
      RD=RD+ONE
202   CONTINUE
      DO 203 M=1,NF2
      BETA(M)=BETA1(M)*SUM(M)
203   CONTINUE
C
C**************************************************************C
C     CASE II: IB=1                                            C
C**************************************************************C
C
      ELSEIF(IB.EQ.1) THEN
      FACTOR=DFLOAT(IA)+HALF
      IFACTOR=2*IA+1
      DO 204 M=1,NF2
      BETA(M)=(DSQRT(XJ(M,IBETA))**IFACTOR)/FACTOR
204   CONTINUE
C
C**************************************************************C
C     CASE III: IB=0                                           C
C**************************************************************C
C
      ELSEIF(IB.EQ.0) THEN
      DO 205 M=1,NF2
      XROOT(M)=DSQRT(XJ(M,IBETA))
      BETA1(M)=DLOG((ONE+XROOT(M))/(ONE-XROOT(M)))
      SUM(M)=ONE
      TERM(M)=XJ(M,IBETA)
205   CONTINUE
C
      IF(IA.GT.1) THEN
       DO 206 K=2,IA
       KK=K-1
       FACTOR=ONE/DFLOAT(KK+KK+1)
       DO 206 M=1,NF2
       SUM(M)=SUM(M)+FACTOR*TERM(M)
       TERM(M)=TERM(M)*XJ(M,IBETA)
206    CONTINUE
       DO 207 M=1,NF2
       BETA(M)=BETA1(M)-TWO*XROOT(M)*SUM(M)
207    CONTINUE
      ELSEIF(IA.EQ.1) THEN
       DO 208 M=1,NF2
       BETA(M)=BETA1(M)-TWO*XROOT(M)
208    CONTINUE
      ELSE
       DO 209 M=1,NF2
       BETA(M)=BETA1(M)
209    CONTINUE
      ENDIF
C
      ENDIF
C
      IF(IBETA.EQ.1) THEN
      DO 800 M=1,NF2
      B1(M,I1,I2)=BETA(M)
800   CONTINUE
      ELSE
      DO 801 M=1,NF2
      B2(M,I1,I2)=BETA(M)
801   CONTINUE
      ENDIF    
900   CONTINUE
30    CONTINUE
C********************************************************************C
C     END INLINE BETA FUNCTION CODE                                  C
C********************************************************************C
C
 
C********************************************************************C
C     GENERATE ALL OF THE INCOMPLETE BETA FUNCTIONS FOR K-TYPE       C
C*********************************************************************C
      NVALS=((NUS(NNUS)-NUS(1))/2)+3
      DO 40 I1=1,NVALS
      NA=NUS(1)+2*(I1-1)
      IAA(1)=L1+L2+NA+1
      IAA(2)=L1+L2+NA+1
      DO 40 I2=1,NVALS
      NB=2*(I2-1)-NUS(NNUS)
      IBB(1)=L1+L2+NB
      IBB(2)=L1+L2+NB
C********************************************************************C
C     BEGIN INLINE BETA FUNCTION CODE                                C
C********************************************************************C
      DO 901 IBETA=1,2
      IA=(IAA(IBETA)-1)/2
      IB=IBB(IBETA)/2
C
C**************************************************************C
C     CASE I: IB>1                                             C
C**************************************************************C
C
      IF(IB.GT.1) THEN
      FACTOR=DFLOAT(IA)+HALF
      IFACTOR=2*IA+1
      DO 300 M=1,NF2
      BETA1(M)=(DSQRT(XK(M,IBETA))**IFACTOR)/FACTOR
300   CONTINUE
      RA=FACTOR
      RB=DFLOAT(1-IB)
      RC=FACTOR+ONE
      RD=ONE
      FACT=RA*RB/(RC*RD)
      DO 3011 M=1,NF2
      TERM(M)=FACT*XK(M,IBETA)
      SUM(M)=ONE+TERM(M)
3011  CONTINUE
      RA=RA+ONE
      RB=RB+ONE
      RC=RC+ONE
      RD=RD+ONE
      DO 302 IT=2,IB-1
      FACT=RA*RB/(RC*RD)
      DO 301 M=1,NF2
      TERM(M)=FACT*TERM(M)*XK(M,IBETA)
      SUM(M)=SUM(M)+TERM(M)
301   CONTINUE
      RA=RA+ONE
      RB=RB+ONE
      RC=RC+ONE
      RD=RD+ONE
302   CONTINUE
      DO 303 M=1,NF2
      BETA(M)=BETA1(M)*SUM(M)
303   CONTINUE
C
C**************************************************************C
C     CASE II: IB=1                                            C
C**************************************************************C
C
      ELSEIF(IB.EQ.1) THEN
      FACTOR=DFLOAT(IA)+HALF
      IFACTOR=2*IA+1
      DO 304 M=1,NF2
      BETA(M)=(DSQRT(XK(M,IBETA))**IFACTOR)/FACTOR
304   CONTINUE
C
C**************************************************************C
C     CASE III: IB=0                                           C
C**************************************************************C
C
      ELSEIF(IB.EQ.0) THEN
      DO 305 M=1,NF2
      XROOT(M)=DSQRT(XK(M,IBETA))
      BETA1(M)=DLOG((ONE+XROOT(M))/(ONE-XROOT(M)))
      SUM(M)=ONE
      TERM(M)=XK(M,IBETA)
305   CONTINUE
C
      IF(IA.GT.1) THEN
       DO 306 K=2,IA
       KK=K-1
       FACTOR=ONE/DFLOAT(KK+KK+1)
       DO 306 M=1,NF2
       SUM(M)=SUM(M)+FACTOR*TERM(M)
       TERM(M)=TERM(M)*XK(M,IBETA)
306    CONTINUE
       DO 307 M=1,NF2
       BETA(M)=BETA1(M)-TWO*XROOT(M)*SUM(M)
307   CONTINUE
      ELSEIF(IA.EQ.1) THEN
       DO 308 M=1,NF2
       BETA(M)=BETA1(M)-TWO*XROOT(M)
308   CONTINUE
      ELSE
       DO 309 M=1,NF2
       BETA(M)=BETA1(M)
309   CONTINUE
      ENDIF
C
      ENDIF
C
      IF(IBETA.EQ.1) THEN
      DO 802 M=1,NF2
      B3(M,I1,I2)=BETA(M)
802   CONTINUE
      ELSE
      DO 803 M=1,NF2
      B4(M,I1,I2)=BETA(M)
803   CONTINUE
      ENDIF    
901   CONTINUE
40    CONTINUE
C********************************************************************C
C     END INLINE BETA FUNCTION CODE                                  C
C********************************************************************C
C
      RETURN
      END
C
C
C
      SUBROUTINE E2C1
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MAXB=20,MAXB2=MAXB*MAXB,MAXAB=20,NUMAX=10)
      COMMON/TEMP/F,G,NFUN1,NFUN2,L1,L2,NF2
      COMMON/IJ/EIJ(MAXAB),RNIJ(3),EI,EJ
      COMMON/KL/EKL(MAXB2,MAXAB),EKL0(MAXB2),
     & EIK(MAXB2,MAXAB),EJL(MAXB2,MAXAB),
     & B1(MAXB2,MAXAB,MAXAB),B2(MAXB2,MAXAB,MAXAB),
     & B3(MAXB2,MAXAB,MAXAB),B4(MAXB2,MAXAB,MAXAB),
     & EK(MAXB2),EL(MAXB2),RNKL(MAXB2,3),EKL1(MAXB2)
      COMMON/INTS/RJSSSS(MAXB2,4),RJLLSS(MAXB2,2),
     & RJSSLL(MAXB2,2),RJLLLL(MAXB2),RKLLLL(MAXB2,4),
     & RKSLSL(MAXB2,4),RKSSSS(MAXB2,4)
      COMMON/ANG/ BK1(NUMAX),BK2(NUMAX),BK3(NUMAX),BK4(NUMAX),
     & NUS(NUMAX),NNUS
      COMMON/GAMMAT/GAMMAL(100),GAM(100)
      DIMENSION RJ(9),RK(12)
      DATA ZERO,TWO,FOUR,EIGHT,SIXTN
     & /0.0D0,2.0D0,4.0D0,8.0D0,1.6D1/
C********************************************************************C
C     SECTION I: GENERATION OF THE COULOMB INTEGRALS                 C
C     RJSSSS(M,1) IS KAPPA(1) > 0 AND KAPPA(2) > 0                   C
C     RJSSSS(M,2) IS KAPPA(1) < 0 AND KAPPA(2) > 0                   C
C     RJSSSS(M,3) IS KAPPA(1) > 0 AND KAPPA(2) < 0                   C
C     RJSSSS(M,4) IS KAPPA(1) < 0 AND KAPPA(2) < 0                   C
C     RJLLSS(M,1) IS KAPPA(2) > 0                                    C
C     RJLLSS(M,2) IS KAPPA(2) < 0                                    C
C     RJSSLL(M,1) IS KAPPA(1) > 0                                    C
C     RJSSLL(M,2) IS KAPPA(1) < 0                                    C
C     RJLLLL(M)                                                      C
C                                                                    C
C     THERE IS AN SPECIAL BRANCHING SCHEME FOR WHEN L1 OR L2 ARE     C
C     EQUAL TO ZERO (S-STATE CASES)                                  C
C********************************************************************C
C
      IF(L1.NE.0.AND.L2.NE.0) THEN
C
C
      IQ12=2*(L1+L2)
      t2 = G**2
      t3 = F**2
      t13 = t2*F*TWO
      t45 = FOUR*G*F
      t58 = EIGHT*G
      t68 = TWO*EI
      t73 = FOUR*EI
      t90 = EIGHT*EI*EJ
      t102 = SIXTN*EI
      t150 = t2*FOUR*GAM(IQ12+5)
      t155 = t58*EJ
      t160 = t3*GAM(IQ12+3)
      t165 = t58*EI
      t170 = t73*EJ*t3*GAM(IQ12+5)
      t175 = t90*F 
      t180 = t102*EJ*GAM(IQ12+9)
      t185 = t2*t3*GAM(IQ12+1)
      t190 = t13*GAM(IQ12+3)
      t195 = TWO*G*t3*EJ*GAM(IQ12+3)
      t200 = t45*EJ 
      t205 = TWO*t200  
      t210 = t68*G
      t215 = t73*G
      t220 = F*GAM(IQ12+5)
      t225 = G*GAM(IQ12+5)
      t230 = EJ*GAM(IQ12+9)*t102
      t235 = FOUR*GAM(IQ12+7)
      t240 = t2*GAM(IQ12+3)
      t245 = EJ*GAM(IQ12+7)
      t250 = FOUR*EI*EJ
      t255 = TWO*EJ
C
      DO 1000 M=1,NF2
      t1 = RNIJ(3)*RNKL(M,3)
      t19 = EIJ(2)*EKL(M,3)*B1(M,1,2)+EIJ(1)*EKL(M,4)*B2(M,2,1)
      t33 = EKL1(M)*t150*(EIJ(2)*EKL(M,5)*B1(M,1,3)+EIJ(1
     #)*EKL(M,6)*B2(M,3,1))
      t41 = EIJ(4)*EKL(M,1)*B1(M,2,1)+EIJ(3)*EKL(M,2)*B2(M,1,2)
      t51 = EIJ(4)*EKL(M,3)*B1(M,2,2)+EIJ(3)*EKL(M,4)*B2(M,2,2)
      t52 = GAM(IQ12+5)*t51
      t65 = EIJ(4)*EKL(M,5)*B1(M,2,3)+EIJ(3)*EKL(M,6)*B2(M,3,2)
      t66 = EKL1(M)*GAM(IQ12+7)*t65
      t67 = t155*t66
      t71 = t160*t41
      t75 = F*EL(M)
      t79 = t165*t66
      t88 = t170*(EIJ(6)*EKL(M,1)*B1(M,3,1)+EIJ(5)*EKL(
     #M,2)*B2(M,1,3))
      t95 = EIJ(6)*EKL(M,3)*B1(M,3,2)+EIJ(5)*EKL(M,4)*B2(M,2,3)
      t96 = GAM(IQ12+7)*t95
      t98 = t90*t75*t96
      t101 = t175*EK(M)*t96
      t109 = EIJ(6)*EKL(M,5)*B1(M,3,3)+EIJ(5)*EKL(M,6)*B2(M,3,3)
      t111 = EKL1(M)*t180*t109
      t112 = t185*(EIJ(2)*EKL(M,1)*B1(M,1,1)+EIJ(1)*EKL(M,2
     #)*B2(M,1,1))-EL(M)*t190*t19-EK(M)*t190*t19+t
     #33-t195*t41+t200*EL(M)*t52+2*t45*EJ*EK(M)*t52
     #-t67-t210*t71+t215*t75*t52-t79+t88-t98-t101+t111
      t123 = RNIJ(1)*RNKL(M,3)
      t126 = t220*t51
      t137 = RNIJ(3)*RNKL(M,1)
      t142 = t225*t51
C
      RJSSSS(M,1)= t1*t112
      RJSSSS(M,2)= t1*(t88-t98-t101+t111)
      RJSSSS(M,3)= t1*(t33-t67-t79+t111)
      RJSSSS(M,4)= t1*EKL1(M)*t230*t109
      RJLLSS(M,1)=t123*(t71-TWO*EK(M)*t126-TWO*EL(M)*t126+t235*EKL1(M)
     #*t65)
      RJLLSS(M,2)= t123*FOUR*t66
      RJSSLL(M,1)=t137*(t240*t19-t255*t142-t68*t142+t73*t245*t95)
      RJSSLL(M,2)= t137*t250*t96
      RJLLLL(M)= RNIJ(1)*RNKL(M,1)*t52
1000  CONTINUE
C
      DO 100 L=1,4
      DO 100 M=1,NF2
      RKLLLL(M,L)=ZERO
      RKSLSL(M,L)=ZERO
      RKSSSS(M,L)=ZERO
100   CONTINUE
C
      DO 3000 INU=1,NNUS
      NU=NUS(INU)
      IR1=NU+NUS(NNUS)+2
      IR2=NUS(NNUS)-NU+2
      IR12=2*(L1+L2)
      NX=(NU-NUS(1)+2)/2
      NY=(NUS(NNUS)-NU+2)/2
C
C*******************************************************************C
C     G = KAPPA(1)+L1+1 (REFERENCES TO THE (I,J) PAIR)              C
C     F = KAPPA(2)+L2+1 (REFERENCES TO THE (K,L) PAIR)              C
C     RKSSSS(M,1) IS SSSS KAPPA(1) > 0 AND KAPPA(2) > 0             C
C     RKSSSS(M,2) IS SSSS KAPPA(1) < 0 AND KAPPA(2) > 0             C
C     RKSSSS(M,3) IS SSSS KAPPA(1) > 0 AND KAPPA(2) < 0             C
C     RKSSSS(M,4) IS SSSS KAPPA(1) < 0 AND KAPPA(2) < 0             C
C     RKSLSL(M,1) IS SLSL KAPPA(1) > 0 AND KAPPA(2) > 0             C 
C     RKSLSL(M,2) IS SLSL KAPPA(1) < 0 AND KAPPA(2) > 0             C
C     RKSLSL(M,3) IS SLSL KAPPA(1) > 0 AND KAPPA(2) < 0             C
C     RKSLSL(M,4) IS SLSL KAPPA(1) < 0 AND KAPPA(2) < 0             C
C     RKLLLL(M,1) IS LLLL KAPPA(1) > 0 AND KAPPA(2) > 0             C
C     RKLLLL(M,2) IS LLLL KAPPA(1) < 0 AND KAPPA(2) > 0             C
C     RKLLLL(M,3) IS LLLL KAPPA(1) > 0 AND KAPPA(2) < 0             C
C     RKLLLL(M,4) IS LLLL KAPPA(1) < 0 AND KAPPA(2) < 0             C
C*******************************************************************C
      t1 = G**2
      t2 = F**2
      t12 = t1*F*TWO
      t44 = FOUR*G*F
      t58 = EIGHT*G
      t68 = TWO*EI
      t73 = FOUR*EI
      t100 = EIGHT*EI*EJ
      t150 = t1*FOUR*GAM(IR12+5)
      t155 = t58*EJ
      t160 = t58*EI
      t165 = t73*EJ*t2*GAM(IR12+5)
      t170 = t100*GAM(IR12+7)
      t175 = t100*F*GAM(IR12+7)
      t180 = SIXTN*EI*EJ*GAM(IR12+9)
      t185 = t1*t2*GAM(IR12+1)
      t190 = t12*GAM(IR12+3)
      t195 = TWO*G*t2*EJ*GAM(IR12+3)
      t200 = t44*EJ*GAM(IR12+5)
      t205 = t44*EJ
      t210 = t68*G*t2*GAM(IR12+3)
      t215 = TWO*G*GAM(IR12+5)
      t220 = t68*F*GAM(IR12+5)
      t225 = G*F*GAM(IR12+3)
      t230 = t73*G
      t235 = t44*EI*GAM(IR12+5)
      t240 = FOUR*EI
C
      DO 3000 M=1,NF2
      t18 = EIK(M,IR1)*EJL(M,IR2+1)*B3(M,NX,1+NY)+EIK(M,IR2-1)*EJL(M,IR1
     #+2)*B4(M,1+NX,NY)
      t26 = EIK(M,IR1+2)*EJL(M,IR2-1)*B3(M,1+NX,NY)+EIK(M,IR2+1)*EJL(M,I
     #R1)*B4(M,NX,1+NY)
      t35 = EIK(M,IR1+2)*EJL(M,IR2+1)*B3(M,1+NX,1+NY)+EIK(M,IR2+1)*EJL(M
     #,IR1+2)*B4(M,1+NX,1+NY)
      t37 = EKL1(M)*t150*t35
      t55 = GAM(IR12+5)*t35
      t60 = EKL1(M)*GAM(IR12+7)
      t65 = EIK(M,IR1+2)*EJL(M,IR2+3)*B3(M,1+NX,2+NY)+EIK(M,IR2+1)*EJL(M
     #,IR1+4)*B4(M,2+NX,1+NY)
      t67 = t155*t60*t65
      t75 = F*EL(M)
      t92 = EIK(M,IR1+4)*EJL(M,IR2+1)*B3(M,2+NX,1+NY)+EIK(M,IR2+3)*EJL(M
     #,IR1+2)*B4(M,1+NX,2+NY)
      t94 = t160*t60*t92
      t98 = t165*t35
      t103 = t75*t170*t65
      t107 = EK(M)*t175*t92
      t115 = EIK(M,IR1+4)*EJL(M,IR2+3)*B3(M,2+NX,2+NY)+EIK(M,IR2+3)*EJL(
     #M,IR1+4)*B4(M,2+NX,2+NY)
      t117 = EKL1(M)*t180*t115
      t118 = t185*(EIK(M,IR1)*EJL(M,IR2-1)*B3(M,NX,NY)+EIK(
     #M,IR2-1)*EJL(M,IR1)*B4(M,NX,NY))-EL(M)*t190*t18-EK(
     #M)*t190*t26+t37-t195*t18+EL(M)*
     #t200*(EIK(M,IR1)*EJL(M,IR2+3)*B3(M,NX,2+NY)+EIK(M,IR2-1)*EJL(
     #M,IR1+4)*B4(M,2+NX,NY))+t205*EK(M)*t55-t67-t210*
     #t26+t230*t75*t55+EK(M)*t235*(EIK(M,IR1+4)*EJL(M,IR2
     #-1)*B3(M,2+NX,NY)+EIK(M,IR2+3)*EJL(M,IR1)*B4(M,NX,2+NY))-t94+t98-t
     #103-t107+t117
      t136 = EK(M)*t215*t35
      t139 = t220*t35
      t141 = EK(M)*GAM(IR12+7)*t92
      t142 = t73*t141
      RKSSSS(M,1)=RKSSSS(M,1)+BK1(INU)*t118
      RKSSSS(M,2)=RKSSSS(M,2)+BK2(INU)*(t98-t103-t107+t117)
      RKSSSS(M,3)=RKSSSS(M,3)+BK3(INU)*(t37-t67-t94+t117)
      RKSSSS(M,4)=RKSSSS(M,4)+BK4(INU)*EKL1(M)*t180*t115
      RKSLSL(M,1)=RKSLSL(M,1)+BK1(INU)*(t225*t18-t136-t139+t142)
      RKSLSL(M,2)=RKSLSL(M,2)+BK2(INU)*(-t139+t142)
      RKSLSL(M,3)=RKSLSL(M,3)+BK3(INU)*(-t136+t142)
      RKSLSL(M,4)=RKSLSL(M,4)+BK4(INU)*t240*t141
      RKLLLL(M,1)=RKLLLL(M,1)+BK1(INU)*GAM(IR12+5)*t35
      RKLLLL(M,2)=RKLLLL(M,2)+BK2(INU)*GAM(IR12+5)*t35
      RKLLLL(M,3)=RKLLLL(M,3)+BK3(INU)*GAM(IR12+5)*t35
      RKLLLL(M,4)=RKLLLL(M,4)+BK4(INU)*GAM(IR12+5)*t35
3000  CONTINUE
C*******************************************************************C
C     NORMALIZE ACCUMULATED K-TYPE INTEGRALS                        C       
C*******************************************************************C
      DO 4000 M=1,NF2
      T0LLLL=RNIJ(1)*RNKL(M,1)
      T0SLSL=RNIJ(2)*RNKL(M,2)
      T0SSSS=RNIJ(3)*RNKL(M,3)
      RKSSSS(M,1)=RKSSSS(M,1)*T0SSSS
      RKSSSS(M,2)=RKSSSS(M,2)*T0SSSS
      RKSSSS(M,3)=RKSSSS(M,3)*T0SSSS
      RKSSSS(M,4)=RKSSSS(M,4)*T0SSSS
      RKSLSL(M,1)=RKSLSL(M,1)*T0SLSL
      RKSLSL(M,2)=RKSLSL(M,2)*T0SLSL
      RKSLSL(M,3)=RKSLSL(M,3)*T0SLSL
      RKSLSL(M,4)=RKSLSL(M,4)*T0SLSL
      RKLLLL(M,1)=RKLLLL(M,1)*T0LLLL
      RKLLLL(M,2)=RKLLLL(M,2)*T0LLLL
      RKLLLL(M,3)=RKLLLL(M,3)*T0LLLL
      RKLLLL(M,4)=RKLLLL(M,4)*T0LLLL
4000  CONTINUE 
C***********************************************************************C
C     CASE WHERE L1.EQ.0 AND L2.NE.0                                    C
C***********************************************************************C
      ELSEIF(L1.EQ.0.AND.L2.NE.0) THEN
C
C
      IQ12=2*(L1+L2)
      t4 = F**2
      t14 = EIGHT*EI*EJ
      t27 = SIXTN*EI
      t150 = F*GAM(IQ12+5)
      t155 = FOUR*EI*EJ*t4*GAM(IQ12+5)
      t160 = t14*F
      t165 = t27*EJ*GAM(IQ12+9)
      t170 = FOUR*GAM(IQ12+7)
      t175 = FOUR*EI*EJ
      t180 = t4*GAM(IQ12+3)
      t185 = t27*EJ*GAM(IQ12+9)
C
      DO 1001 M=1,NF2
      t1 = RNIJ(3)*RNKL(M,3)
      t21 =GAM(IQ12+7)*(EIJ(6)*EKL(M,3)*B1(M,3,2)+EIJ(5)*EKL(M,4)*B2(M,
     #2,3))
      t34 = EIJ(6)*EKL(M,5)*B1(M,3,3)+EIJ(5)*EKL(M,6)*B2(M,3,3)
      t44 = RNIJ(1)*RNKL(M,3)
      t58 = EIJ(4)*EKL(M,3)*B1(M,2,2)+EIJ(3)*EKL(M,4)*B2(M,2,2)
      t59 = t150*t58
      t68 = EIJ(4)*EKL(M,5)*B1(M,2,3)+EIJ(3)*EKL(M,6)*B2(M,3,2)
      RJSSSS(M,1) =t1*(t155*(EIJ(6)*EKL(M,1)*B1(M,3,1)+E
     #IJ(5)*EKL(M,2)*B2(M,1,3))-t160*EL(M)*t21-t160*EK(M)*t21+E
     #KL1(M)*t165*t34)
      RJSSSS(M,2) =t1*EKL1(M)*t185*t34
      RJLLSS(M,1) = t44*(t180*(EIJ(4)*EKL(M,1)*B1(M,2,1)+EIJ(3)*EKL(
     #M,2)*B2(M,1,2))-TWO*EK(M)*t59-TWO*EL(M)*t59+EKL1(M)*
     #t170*t68)
      RJLLSS(M,2) = t44*EKL1(M)*t170*t68
      RJSSLL(M,1) = RNIJ(3)*RNKL(M,1)*t175*t21
      RJLLLL(M) =RNIJ(1)*RNKL(M,1)*GAM(IQ12+5)*t58
1001  CONTINUE
C
      DO 101 L=1,2
      DO 101 M=1,NF2
      RKLLLL(M,L)=ZERO
      RKSLSL(M,L)=ZERO
      RKSSSS(M,L)=ZERO
101   CONTINUE
C
      DO 3001 INU=1,NNUS
      NU=NUS(INU)
      IR1=NU+NUS(NNUS)+2
      IR2=NUS(NNUS)-NU+2
      IR12=2*(L1+L2)
      NX=(NU-NUS(1)+2)/2
      NY=(NUS(NNUS)-NU+2)/2
C
C*******************************************************************C
C     G = KAPPA(1)+L1+1 (REFERENCES TO THE (I,J) PAIR)              C
C     F = KAPPA(2)+L2+1 (REFERENCES TO THE (K,L) PAIR)              C
C     RKSSSS(M,1) IS SSSS KAPPA(1) < 0 AND KAPPA(2) > 0             C
C     RKSSSS(M,2) IS SSSS KAPPA(1) < 0 AND KAPPA(2) < 0             C
C     RKSLSL(M,1) IS SLSL KAPPA(1) < 0 AND KAPPA(2) > 0             C
C     RKSLSL(M,2) IS SLSL KAPPA(1) < 0 AND KAPPA(2) < 0             C
C     RKLLLL(M,1) IS LLLL KAPPA(1) < 0 AND KAPPA(2) > 0             C
C     RKLLLL(M,2) IS LLLL KAPPA(1) < 0 AND KAPPA(2) < 0             C
C*******************************************************************C
      t1 = FOUR*EI
      t3 = F**2
      t13 = EIGHT*EI*EJ
      t150 = t1*EJ*t3*GAM(IR12+5)
      t155 = t13*F*GAM(IR12+7)
      t160 = t13*F
      t165 = SIXTN*EI*EJ*GAM(IR12+9)
      t170 = -TWO*EI*F*GAM(IR12+5)
C
      DO 3001 M=1,NF2
      t9 = EIK(M,IR1+2)*EJL(M,IR2+1)*B3(M,1+NX,1+NY)+EIK(M,IR2+1)*EJL(M,
     #IR1+2)*B4(M,1+NX,1+NY)
      t28 = EIK(M,IR1+4)*EJL(M,IR2+1)*B3(M,2+NX,1+NY)+EIK(M,IR2+3)*EJL(M
     #,IR1+2)*B4(M,1+NX,2+NY)
      t39 = EIK(M,IR1+4)*EJL(M,IR2+3)*B3(M,2+NX,2+NY)+EIK(M,IR2+3)*EJL(M
     #,IR1+4)*B4(M,2+NX,2+NY)
      t55 = EK(M)*GAM(IR12+7)*t28
      RKSSSS(M,1)=RKSSSS(M,1)+BK2(INU)*(t150*t9-EL(M)*t155*
     #(EIK(M,IR1+2)*EJL(M,IR2+3)*B3(M,1+NX,2+NY)+EIK(M,IR2+1)*EJL(M,IR1+
     #4)*B4(M,2+NX,1+NY))-t155*EK(M)*t28+EKL1(M)*t165*t39)
      RKSSSS(M,2)=RKSSSS(M,2)+BK4(INU)*EKL1(M)*t165*t39
      RKSLSL(M,1)=RKSLSL(M,1)+BK2(INU)*(t170*t9+t1*t55)
      RKSLSL(M,2)=RKSLSL(M,2)+BK4(INU)*t1*t55
      RKLLLL(M,1)=RKLLLL(M,1)+BK2(INU)*GAM(IR12+5)*t9
      RKLLLL(M,2)=RKLLLL(M,2)+BK4(INU)*GAM(IR12+5)*t9
3001  CONTINUE
C
      DO 4001 M=1,NF2
      T0LLLL=RNIJ(1)*RNKL(M,1)
      T0SLSL=RNIJ(2)*RNKL(M,2)
      T0SSSS=RNIJ(3)*RNKL(M,3)
      RKSSSS(M,1)=RKSSSS(M,1)*T0SSSS
      RKSSSS(M,2)=RKSSSS(M,2)*T0SSSS
      RKSLSL(M,1)=RKSLSL(M,1)*T0SLSL
      RKSLSL(M,2)=RKSLSL(M,2)*T0SLSL
      RKLLLL(M,1)=RKLLLL(M,1)*T0LLLL
      RKLLLL(M,2)=RKLLLL(M,2)*T0LLLL
4001  CONTINUE 
C***********************************************************************C
C     CASE WHERE L1.NE.0 AND L2.EQ.0                                    C
C***********************************************************************C
      ELSEIF(L1.NE.0.AND.L2.EQ.0) THEN
C
C
      IQ12=2*(L1+L2)
      t2 = G**2
      t12 = EIGHT*G
      t24 = SIXTN*EI
      t150 =  G*GAM(IQ12+5)
      t155 = t2*FOUR*GAM(IQ12+5)
      t160 = t12*EJ
      t165 = t12*EI
      t170 = t24*EJ*GAM(IQ12+9)
c      t175 = t1*t24*EJ*GAM(IQ12+9)
      t180 = TWO*EJ
      t185 = TWO*EI
      t190 = FOUR*EI*EJ*GAM(IQ12+7)
C
      DO 1002 M=1,NF2
      t1 = RNIJ(3)*RNKL(M,3)
      t20 = EKL1(M)*GAM(IQ12+7)*(EIJ(4)*EKL(M,5)*B1(M,2,3)+EIJ(3)*EKL(M,
     #6)*B2(M,3,2))
      t31 = EIJ(6)*EKL(M,5)*B1(M,3,3)+EIJ(5)*EKL(M,6)*B2(M,3,3)
      t44 = RNIJ(3)*RNKL(M,1)
      t58 = EIJ(4)*EKL(M,3)*B1(M,2,2)+EIJ(3)*EKL(M,4)*B2(M,2,2)
      t59 = t150*t58
      t69 = EIJ(6)*EKL(M,3)*B1(M,3,2)+EIJ(5)*EKL(M,4)*B2(M,2,3)
      RJ(1) = t1*(EKL1(M)*t155*(EIJ(2)*EKL(M,5)*B1(M,1,3)
     #+EIJ(1)*EKL(M,6)*B2(M,3,1))-t160*t20-t165*t20+t170*EKL1(M)*t31)
      RJ(2) = t1*EKL1(M)*t170*t31
      RJ(3) = RNIJ(1)*RNKL(M,3)*FOUR*t20
      RJ(4) = t44*(t2*GAM(IQ12+3)*(EIJ(2)*EKL(M,3)*B1(M,1,2)+EIJ(1)*EKL(
     #M,4)*B2(M,2,1))-t180*t59-t185*t59+t190*t69)
      RJ(5) = t44*t190*t69
      RJ(6) = RNIJ(1)*RNKL(M,1)*GAM(IQ12+5)*t58
      RJSSSS(M,1) =t1*(EKL1(M)*t155*(EIJ(2)*EKL(M,5)*B1(M,1,3)
     #+EIJ(1)*EKL(M,6)*B2(M,3,1))-t160*t20-t165*t20+t170*EKL1(M)*t31)
      RJSSSS(M,2) =t1*EKL1(M)*t170*t31 
      RJLLSS(M,1) =RNIJ(1)*RNKL(M,3)*FOUR*t20 
      RJSSLL(M,1) =t44*(t2*GAM(IQ12+3)*(EIJ(2)*EKL(M,3)*B1(M,1,2)
     # +EIJ(1)*EKL(M,4)*B2(M,2,1))-t180*t59-t185*t59+t190*t69)
      RJSSLL(M,2) =t44*t190*t69
      RJLLLL(M) = RNIJ(1)*RNKL(M,1)*GAM(IQ12+5)*t58
1002  CONTINUE
C
      DO 102 L=1,2
      DO 102 M=1,NF2
      RKLLLL(M,L)=ZERO
      RKSLSL(M,L)=ZERO
      RKSSSS(M,L)=ZERO
102   CONTINUE
C
      DO 3002 INU=1,NNUS
      NU=NUS(INU)
      IR1=NU+NUS(NNUS)+2
      IR2=NUS(NNUS)-NU+2
      IR12=2*(L1+L2)
      NX=(NU-NUS(1)+2)/2
      NY=(NUS(NNUS)-NU+2)/2
C
C*******************************************************************C
C     G = KAPPA(1)+L1+1 (REFERENCES TO THE (I,J) PAIR)              C
C     F = KAPPA(2)+L2+1 (REFERENCES TO THE (K,L) PAIR)              C
C     RKSSSS(M,1) IS SSSS KAPPA(1) > 0 AND KAPPA(2) < 0             C
C     RKSSSS(M,2) IS SSSS KAPPA(1) < 0 AND KAPPA(2) < 0             C
C     RKSLSL(M,1) IS SLSL KAPPA(1) > 0 AND KAPPA(2) < 0             C
C     RKSLSL(M,2) IS SLSL KAPPA(1) < 0 AND KAPPA(2) < 0             C
C     RKLLLL(M,1) IS LLLL KAPPA(1) > 0 AND KAPPA(2) < 0             C
C     RKLLLL(M,2) IS LLLL KAPPA(1) < 0 AND KAPPA(2) < 0             C
C*******************************************************************C
      t1 = G**2
      t12 = EIGHT*G
      t150 = t1*FOUR*GAM(IR12+5)
      t155 = t12*EJ
      t160 = t12*EI
      t165 = SIXTN*EI*EJ*GAM(IR12+9)
      t170 = -TWO*G*GAM(IR12+5)
      t175 = FOUR*EI 
C
      DO 3002 M=1,NF2
      t9 = EIK(M,IR1+2)*EJL(M,IR2+1)*B3(M,1+NX,1+NY)+EIK(M,IR2+1)*EJL(M,
     #IR1+2)*B4(M,1+NX,1+NY)
      t14 = EKL1(M)*GAM(IR12+7)
      t27 = EIK(M,IR1+4)*EJL(M,IR2+1)*B3(M,2+NX,1+NY)+EIK(M,IR2+3)*EJL(M
     #,IR1+2)*B4(M,1+NX,2+NY)
      t37 = EIK(M,IR1+4)*EJL(M,IR2+3)*B3(M,2+NX,2+NY)+EIK(M,IR2+3)*EJL(M
     #,IR1+4)*B4(M,2+NX,2+NY)
      t54 = EK(M)*GAM(IR12+7)*t27
      RKSSSS(M,1)=RKSSSS(M,1)+BK3(INU)*(EK(M)*EL(M)*t150*t9-t155*t14*(E
     #IK(M,IR1+2)*EJL(M,IR2+3)*B3(M,1+NX,2+NY)+EIK(M,IR2+1)*EJL(M,IR1+4)
     #*B4(M,2+NX,1+NY))-t160*t14*t27+EKL1(M)*t165*t37)
      RKSSSS(M,2)=RKSSSS(M,2)+BK4(INU)*EKL1(M)*t165*t37
      RKSLSL(M,1)=RKSLSL(M,1)+BK3(INU)*(EK(M)*t170*t9+t175*t54)
      RKSLSL(M,2)=RKSLSL(M,2)+BK4(INU)*t175*t54
      RKLLLL(M,1)=RKLLLL(M,1)+BK3(INU)*GAM(IR12+5)*t9
      RKLLLL(M,2)=RKLLLL(M,2)+BK4(INU)*GAM(IR12+5)*t9
3002  CONTINUE
C*******************************************************************C
C     NORMALIZE ACCUMULATED K-TYPE INTEGRALS                        C       
C*******************************************************************C
      DO 4002 M=1,NF2
      T0LLLL=RNIJ(1)*RNKL(M,1)
      T0SLSL=RNIJ(2)*RNKL(M,2)
      T0SSSS=RNIJ(3)*RNKL(M,3)
      RKSSSS(M,1)=RKSSSS(M,1)*T0SSSS
      RKSSSS(M,2)=RKSSSS(M,2)*T0SSSS
      RKSLSL(M,1)=RKSLSL(M,1)*T0SLSL
      RKSLSL(M,2)=RKSLSL(M,2)*T0SLSL
      RKLLLL(M,1)=RKLLLL(M,1)*T0LLLL
      RKLLLL(M,2)=RKLLLL(M,2)*T0LLLL
4002  CONTINUE 
C***********************************************************************C
C     CASE WHERE L1.EQ.0 AND L2.EQ.0                                    C
C***********************************************************************C
      ELSEIF(L1.EQ.0.AND.L2.EQ.0) THEN
C
C
      IQ12=2*(L1+L2)
      t150 = RNIJ(3)*SIXTN*EI*EJ*GAM(IQ12+9)
      t155 = RNIJ(1)*FOUR*GAM(IQ12+7)
      t160 = RNIJ(3)*FOUR*EI*EJ*GAM(IQ12+7)
c      t165 = RNIJ(1)*GAM(IQ12+5)
C
      DO 1003 M=1,NF2
      RJ(1) = RNKL(M,3)*EKL1(M)*t150*(EIJ(6)*
     #EKL(M,5)*B1(M,3,3)+EIJ(5)*EKL(M,6)*B2(M,3,3))
      RJ(2) = RNKL(M,3)*EKL1(M)*t155*(EIJ(4)*EKL(M,5
     #)*B1(M,2,3)+EIJ(3)*EKL(M,6)*B2(M,3,2))
      RJ(3) = RNKL(M,1)*t160*(EIJ(6)*EKL(M,3)*
     #B1(M,3,2)+EIJ(5)*EKL(M,4)*B2(M,2,3))
      RJ(4)=RNIJ(1)*RNKL(M,1)*GAM(IQ12+5)*(EIJ(4)*EKL(M,3)*B1(M,2,2)+E
     #IJ(3)*EKL(M,4)*B2(M,2,2))
      RJSSSS(M,1) = RJ(1)
      RJLLSS(M,1) = RJ(2)
      RJSSLL(M,1) = RJ(3)
      RJLLLL(M) = RJ(4)
1003  CONTINUE
C
      DO 103 M=1,NF2
      RKLLLL(M,1)=ZERO
      RKSLSL(M,1)=ZERO
      RKSSSS(M,1)=ZERO
103   CONTINUE
C
      DO 3003 INU=1,NNUS
      NU=NUS(INU)
      IR1=NU+NUS(NNUS)+2
      IR2=NUS(NNUS)-NU+2
      IR12=2*(L1+L2)
      NX=(NU-NUS(1)+2)/2
      NY=(NUS(NNUS)-NU+2)/2
C
C*******************************************************************C
C     G = KAPPA(1)+L1+1 (REFERENCES TO THE (I,J) PAIR)              C
C     F = KAPPA(2)+L2+1 (REFERENCES TO THE (K,L) PAIR)              C
C     RKSSSS(M,1) IS SSSS KAPPA(1) < 0 AND KAPPA(2) < 0             C
C     RKSLSL(M,1) IS SLSL KAPPA(1) < 0 AND KAPPA(2) < 0             C
C     RKLLLL(M,1) IS LLLL KAPPA(1) < 0 AND KAPPA(2) < 0             C
C*******************************************************************C
      t150 = SIXTN*EI*EJ*GAM(IR12+9)
C
      DO 3003 M=1,NF2
      RK(1) = BK4(INU)*EKL1(M)*t150*(EIK(M,IR1+4)*EJL
     #(M,IR2+3)*B3(M,2+NX,2+NY)+EIK(M,IR2+3)*EJL(M,IR1+4)*B4(M,2+NX,2+NY
     #))
      RK(2) = BK4(INU)*FOUR*EI*EK(M)*GAM(IR12+7)*(EIK(M,IR1+4)*EJL(M,IR2
     #+1)*B3(M,2+NX,1+NY)+EIK(M,IR2+3)*EJL(M,IR1+2)*B4(M,1+NX,2+NY))
      RK(3) = BK4(INU)*GAM(IR12+5)*(EIK(M,IR1+2)*EJL(M,IR2+1)*B3(M,1+NX,
     #1+NY)+EIK(M,IR2+1)*EJL(M,IR1+2)*B4(M,1+NX,1+NY))
      RKSSSS(M,1)=RKSSSS(M,1)+RK(1)
      RKSLSL(M,1)=RKSLSL(M,1)+RK(2)
      RKLLLL(M,1)=RKLLLL(M,1)+RK(3)
3003  CONTINUE
C*******************************************************************C
C     NORMALIZE ACCUMULATED K-TYPE INTEGRALS                        C       
C*******************************************************************C
      DO 4003 M=1,NF2
      T0LLLL=RNIJ(1)*RNKL(M,1)
      T0SLSL=RNIJ(2)*RNKL(M,2)
      T0SSSS=RNIJ(3)*RNKL(M,3)
      RKSSSS(M,1)=RKSSSS(M,1)*T0SSSS
      RKSLSL(M,1)=RKSLSL(M,1)*T0SLSL
      RKLLLL(M,1)=RKLLLL(M,1)*T0LLLL
4003  CONTINUE 
      ENDIF
C*******************************************************************C
C     END OF INTEGRAL BATCH GENERATION                              C
C*******************************************************************C
C
      RETURN
      END
C
C
      SUBROUTINE FACTRL3
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***************************************************************C
C     FACTRL EVALUATES THE LOG OF THE GAMMA FUNCTIONS ACCORDING C
C     TO THE DEFINITION                                         C
C                                                               C
C     GAMMAL(I) = LOG(GAMMA(I/2))                               C
C                                                               C
C     THE STARTING VALUES ARE                                   C
C     GAMMAL(1)=DLOG(DSQRT(PI))                                 C
C     GAMMAL(2)=DLOG(0!)=0                                      C
C                                                               C
C***************************************************************C 
      COMMON/GAMMAT/GAMMAL(100),GAMMAF(100)
      DATA ZERO,ONE,PI/0.0D0,1.0D0,3.14159265358979D0/
      DATA FOUR/4.0D0/
C
      T1=DSQRT(PI)
      F1=0.5D0
      F2=ONE
      GAMMAL(1)=DLOG(T1)
      GAMMAL(2)=ZERO
      GAMMAF(1)=T1/FOUR
      GAMMAF(2)=F2/FOUR
      DO 10 M=2,25
      GAMMAL((2*M)-1)=GAMMAL((2*M)-3)+DLOG(F1)
      GAMMAL(2*M)=GAMMAL((2*M)-2)+DLOG(F2)
      GAMMAF((2*M)-1)=GAMMAF((2*M)-3)*F1
      GAMMAF(2*M)=GAMMAF((2*M)-2)*F2
      F1=F1+ONE
      F2=F2+ONE
10    CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE FACTRL4
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***************************************************************C
C     FACTRL EVALUATES THE LOG OF THE GAMMA FUNCTIONS ACCORDING C
C     TO THE DEFINITION                                         C
C                                                               C
C     GAMMAL(I) = LOG(GAMMA(I/2))                               C
C                                                               C
C     THE STARTING VALUES ARE                                   C
C     GAMMAL(1)=DLOG(DSQRT(PI))                                 C
C     GAMMAL(2)=DLOG(0!)=0                                      C
C                                                               C
C***************************************************************C 
      COMMON/GAMMAF2/GAMMAF(100)
      DATA ZERO,ONE,PI/0.0D0,1.0D0,3.14159265358979D0/
      DATA FOUR/4.0D0/
C
      T1=DSQRT(PI)
      F1=0.5D0
      F2=ONE
      GAMMAF(1)=T1
      GAMMAF(2)=F2
      DO 10 M=2,25
      GAMMAF((2*M)-1)=GAMMAF((2*M)-3)*F1
      GAMMAF(2*M)=GAMMAF((2*M)-2)*F2
      F1=F1+ONE
      F2=F2+ONE
10    CONTINUE
      RETURN
      END
C
C
C
       SUBROUTINE ANGMAT
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************C
C                          SWIRLES MODULE 21:                           C
C                                                                       C
C      ANGMAT EVALUATES THE ANGULAR COEFFICIENTS OF THE COULOMB         C
C      INTERACTIONS FOR CLOSED SHELLS (K1,K2) BY A CALL                 C
C      TO THE COULOMB ROUTINE COULMAT                                   C
C                                                                       C
C      K1=KAPPA(1)                                                      C
C      K2=KAPPA(2)                                                      C
C      BCFS(I)=B-COEFFICIENTS (SAME FOR LL,LS,SS,SS)                    C
C      NUS(I) = CORRESPONDING NU-VALUES                                 C
C      NONUS= NUMBER OF NU VALUES                                       C
C                                                                       C
C***********************************************************************C
       PARAMETER (NUMAX=10,MAXAB=20)
       COMMON/ANG/BK1(NUMAX),BK2(NUMAX),BK3(NUMAX),BK4(NUMAX),
     & NUS(NUMAX),NNUS
       COMMON/TEMP/F,G,NFUN1,NFUN2,L1,L2,NF2
       COMMON/PRNT/IPRNT
       DATA ZERO/0.0D0/
C
       IPRNT = 0
C
       KL1=L1
       KL2=-L1-1
       KR1=L2
       KR2=-L2-1
       JL1=2*IABS(KL1)-1
       JL2=2*IABS(KL2)-1
       JR1=2*IABS(KR1)-1
       JR2=2*IABS(KR2)-1
       CALL DFAC  
C***********************************************************************C
C      OVERWRITE THE VECTOR NUS WITH THE NNUS VALUES OF THE TENSOR      C
C      ORDER WHICH ARE COMMON TO ALL FOUR CASES.                        C
C***********************************************************************C 
       NUSTRT=IABS(L1-L2)
       NUSTOP=L1+L2+1
       NNUS=0
       DO 10 INU=NUSTRT+1,NUSTOP+1
       NU=INU-1
       LTEST=L1+L2+NU
       LEVEN=2*(LTEST/2)
       IF(LTEST.EQ.LEVEN) THEN
       NNUS=NNUS+1
       NUS(NNUS)=NU
       CALL SYM3JSQ(JL1,JR1,NU,VALUE1)
       CALL SYM3JSQ(JL2,JR1,NU,VALUE2)
       CALL SYM3JSQ(JL1,JR2,NU,VALUE3)
       CALL SYM3JSQ(JL2,JR2,NU,VALUE4)
       BK1(NNUS)=VALUE1
       BK2(NNUS)=VALUE2
       BK3(NNUS)=VALUE3
       BK4(NNUS)=VALUE4
       ENDIF
10     CONTINUE    
       RETURN
       END
C
       SUBROUTINE SYM3JSQ(J1,J2,K,VALUE)
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C************************************************************************C
C                          SWIRLES MODULE 31:                            C
C                                                                        C
C      SYM3JSQ EVALUATES THE 3-J SYMBOL FOR THE COULOMB AND BREIT        C
C      ANGULAR COEFFICIENT ROUTINES                                      C
C                                                                        C
C************************************************************************C
       DATA ZERO,ONE/0.0D0,1.0D0/
       COMMON/FACTS/RFACT(21),RDFACT(21)
       IF(K.LT.IABS((J1-J2)/2).OR.K.GT.(J1+J2)/2) THEN
         VALUE=0.0D0
         RETURN
       ELSEIF(J1.LE.0.OR.J2.LE.0) THEN
         VALUE=0.0D0
         RETURN
       ENDIF
       JJK=(J1+J2)/2 + K
       IF((JJK/2)*2.EQ.JJK) THEN
        IS=K
       ELSE
        IS=K+1
       ENDIF
       JJ1=((J1+J2)/2) + 1
       JJ2=((J1-J2)/2) + 1
       JJ3=((J2-J1)/2)+1
       RF1=RFACT(JJ1-K)
       RF2=RFACT(JJ3+K)
       RF3=RFACT(JJ2+K)
       RF4=RDFACT(JJ1+IS)
       RF5=DFLOAT(J1+1)
       RF6=DFLOAT(J2+1)
       RF7=RFACT(JJ1+K+1)
       RF8=RDFACT(JJ1-IS)
       RF9=RDFACT(JJ2+IS-1)
       RF10=RDFACT(JJ3+IS-1)
       RPHASE=(-ONE)**((J2-(3*J1))/2+IS)
       RF789=RF8*RF9*RF10
       RNUM=RPHASE*RF1*RF2*RF3*RF4*RF4
       RDEN=RF5*RF6*RF7*RF789*RF789
       VALUE=RNUM/RDEN
       RETURN
       END
C
C***********************************************************************C
       SUBROUTINE DFAC   
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************C
C                          SWIRLES MODULE 33:                           C
C      FACTRL EVALUATES THE FACTORIAL AND DOUBLE FACTORIAL FUNCTIONS    C
C      AS REAL NUMBERS                                                  C
C                                                                       C
C***********************************************************************C
       COMMON/FACTS/RFACT(21),RDFACT(21)
       DATA ZERO,ONE/0.0D0,1.0D0/
       RFACT(1)=ONE
       RFACT(2)=ONE
       RDFACT(1)=ONE
       RDFACT(2)=ONE
       DO 10 I=3,21
       RNUMBER=DFLOAT(I-1)
       RFACT(I)=RNUMBER*RFACT(I-1)
       RDFACT(I)=RNUMBER*RDFACT(I-2)
10     CONTINUE
       RETURN
       END
C************************************************************************
      FUNCTION RINT(N,ZETA)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/NUC/PNUC
      IF (N.EQ.1) THEN
      RINT =  0.5D0*DSQRT(PNUC)/ZETA/DSQRT(PNUC+ZETA)
C
      ELSEIF (N.EQ.3) THEN
      t1 = DSQRT(PNUC)
      t2 = t1**2
      t4 = ZETA**2
      t8 = DSQRT(PNUC+ZETA)
      t9 = t8**2
      t11 = 1.0D0/t9/t8
      RINT= 0.5D0/t4*t2*t1*t11+0.75D0*t1/ZETA*t11
C
      ELSEIF (N.EQ.5) THEN
      t1 = DSQRT(PNUC)
      t2 = t1**2
      t3 = t2**2
      t5 = ZETA**2
      t10 = DSQRT(PNUC+ZETA)
      t11 = t10**2
      t12 = t11**2
      t14 = 1.0D0/t12/t10
      RINT=t3*t1/t5/ZETA*t14+0.25D1*t2*t1/t5*t14+0.1875D1*t1/ZETA*t14
C
      ELSEIF (N.EQ.7) THEN
      t1 = DSQRT(PNUC)
      t2 = t1**2
      t3 = t2*t1
      t4 = t2**2
      t6 = ZETA**2
      t7 = t6**2
      t11 = DSQRT(PNUC+ZETA)
      t12 = t11**2
      t14 = t12**2
      t16 = 1.0D0/t14/t12/t11
      RINT=3.D0*t4*t3/t7*t16+0.105D2*t4*t1/t6/ZETA*t16+0.13125D2*t3/t6
     #*t16+0.65625D1*t1/ZETA*t16
C
      ELSEIF (N.EQ.9) THEN
      t1 = DSQRT(PNUC)
      t2 = t1**2
      t3 = t2**2
      t4 = t3**2
      t6 = ZETA**2
      t7 = t6**2
      t12 = DSQRT(PNUC+ZETA)
      t13 = t12**2
      t14 = t13**2
      t15 = t14**2
      t17 = 1.0D0/t15/t12
      t19 = t2*t1
      RINT=12.D0*t4*t1/t7/ZETA*t17+54.D0*t3*t19/t7*t17+0.945D2*t3*t1/t
     #6/ZETA*t17+0.7875D2*t19/t6*t17+0.2953125D2*t1/ZETA*t17
C
      ELSEIF (N.EQ.11) THEN
      t1 = DSQRT(PNUC)
      t2 = t1**2
      t3 = t2*t1
      t4 = t2**2
      t5 = t4**2
      t7 = ZETA**2
      t8 = t7**2
      t13 = DSQRT(PNUC+ZETA)
      t14 = t13**2
      t16 = t14**2
      t17 = t16**2
      t19 = 1.0D0/t17/t14/t13
      RINT=60.D0*t5*t3/t8/t7*t19+330.D0*t5*t1/t8/ZETA*t19+0.7425D3*t4*
     #t3/t8*t19+0.86625D3*t4*t1/t7/ZETA*t19+0.54140625D3*t3/t7*t19+0.16
     #2421875D3*t1/ZETA*t19
      ELSE 
       WRITE(6,*) 'INVALID N SENT TO RINT'
      ENDIF
      RETURN
      END
C
C
C
      SUBROUTINE AUFBAU(IZNUC,ICRGE,NORB,NOCC,LMAX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NKAPM=7)
C**********************************************************************C
C     AUFBAU DETERMINES THE GROUND STATE ELECTRONIC CONFIGURATION      C
C     OF A NEUTRAL ATOM OF CHARGE IZNUC                                C
C**********************************************************************C
      DIMENSION NORB(NKAPM,NKAPM),NOCC(NKAPM),IAUF(18)
c      DATA IAUF/0,0,1,0,1,0,2,1,0,2,1,0,3,2,1,0,2,3/
      DATA IAUF/0,0,1,0,1,0,2,1,0,2,1,3,0,2,1,0,2,3/
C
      DO 1 I=1,4
1     NOCC(I)=0
      LMAX=0
      ILEFT=IZNUC-ICRGE
      DO 10 M=1,18
      IF(ILEFT.EQ.0) GO TO 20
      LQN=IAUF(M)
      IF(LQN.GT.LMAX) LMAX=LQN
      NOCC(LQN+1)=NOCC(LQN+1)+1
      IFULL=4*LQN+2
      IF(ILEFT.GT.IFULL) THEN
      NORB(LQN+1,NOCC(LQN+1))=IFULL
      ILEFT=ILEFT-IFULL
      ELSE
      NORB(LQN+1,NOCC(LQN+1))=ILEFT
      GO TO 20
      ENDIF
10    CONTINUE
20    CONTINUE
      RETURN
      END
C
C
C**********************************************************************C
C
      SUBROUTINE EMAKELL(ERLL11,EILL11,ERLL21,EILL21,
     #                   KAPPA,MQN,NFUNS,IALT,IND1,IND2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************C
C     EMAKELL GENERATES BLOCKS OF SPHERICAL SPINOR E-COEFFICIENTS       C
C     BY CONTRACTING ON THEIR CARTESIAN TENSOR COMPONENTS               C
C     (CURRENTLY LIMITED TO LMAX=4)                                     C
C***********************************************************************C
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXDIM=300,NKAPM=7,
     &  NCENTM=6,IL4=24,MAXMV=5,MAXMV2=2*MAXMV)
      PARAMETER(MLL=165,MAXIJ=125,MAXLQN=4)
c      PARAMETER(MABLL=((MAXLQN+1)*(MAXLQN+2)*(MAXLQN+3))/6)
      PARAMETER(MABLL=MLL)
C
      COMMON/RNORM/RNLL(MAXB2),RNSL(MAXB2),RNLS(MAXB2),RNSS(MAXB2),
     #EXPA(MAXB2),EXPB(MAXB2),EXPAB(MAXB2)
      COMMON/INDSYS/INABCD(0:4*MAXLQN,0:4*MAXLQN,0:4*MAXLQN),
     &              IVEC(MABLL),JVEC(MABLL),KVEC(MABLL)
C
      COMMON/LOCGEO/XYZ(3,4)
      COMMON/TIMEE/ETIME
C
      COMMON/SPEC/COORD(3,NCENTM),ZNUC(NCENTM),CNUC(NCENTM),
     &  EXPSET(MAXB,NKAPM,NCENTM),IZNUC(NCENTM),ICRGE(NCENTM),
     &  KVALS(NKAPM,NCENTM),NKAPPA(NCENTM),LMAXX(NCENTM),
     &  NFUNCT(NKAPM,NCENTM),NCENT,NDIM,NSHIFT,
     &  NOCC,ITER,IALL,IRUN
      DIMENSION     ERLL11(MAXB2,0:MLL),EILL11(MAXB2,0:MLL),
     #              ERLL21(MAXB2,0:MLL),EILL21(MAXB2,0:MLL)
      DIMENSION RRAU1(84),RIAU1(84),RRAU2(84),RIAU2(84),
     &          RRBU1(84),RIBU1(84),
     &          RRAL1(84),RIAL1(84),RRAL2(84),RIAL2(84),
     &          RRBL1(84),RIBL1(84),
     &          RRBU2(84),RIBU2(84),RRBL2(84),RIBL2(84)
      DIMENSION EX(MAXB2,MAXIJ),EY(MAXB2,MAXIJ),EZ(MAXB2,MAXIJ),
     &          PAX(MAXB2),PAY(MAXB2),PAZ(MAXB2),
     &          PBX(MAXB2),PBY(MAXB2),PBZ(MAXB2),
     &          P2(MAXB2),RKAB(MAXB2)
      DIMENSION IED(5,5) 
      DIMENSION IEXYZ(MAXIJ),IEEXP(MAXIJ)
      DIMENSION KAPPA(4),MQN(4),NFUNS(4)
C
      DATA IED/1,16,36,61,91,
     &         2,18,39,65,96,
     &         4,21,43,70,102,
     &         7,25,48,76,109,
     &        11,30,54,83,117/
C
      DATA IEXYZ/1,0,1,1,0,1,0,3,0,1,3,0,6,0,1,0,1,1,0,1,
     &         0,3,0,1,3,0,6,0,1,0,15,0,10,0,1,1,0,1,0,3,
     &      0,1,3,0,6,0,1,0,15,0,10,0,1,15,0,45,0,15,0,1,
     &      0,3,0,1,3,0,6,0,1,0,15,0,10,0,1,15,0,45,0,15,
     &    0,1,0,105,0,105,0,21,0,1,3,0,6,0,1,0,15,0,10,0,
     & 1,15,0,45,0,15,0,1,0,105,0,105,0,21,0,1,105,0,420,0,
     &          210,0,28,0,1/
C
      DATA IEEXP/0,0,1,1,0,2,0,2,0,3,2,0,3,0,4,0,1,1,0,2,
     &          0,2,0,3,2,0,3,0,4,0,3,0,4,0,5,1,0,2,0,2,
     &          0,3,2,0,3,0,4,0,3,0,4,0,5,3,0,4,0,5,0,6,
     &          0,2,0,3,2,0,3,0,4,0,3,0,4,0,5,3,0,4,0,5,
     &          0,6,0,4,0,5,0,6,0,7,2,0,3,0,4,0,3,0,4,0,
     &          5,3,0,4,0,5,0,6,0,4,0,5,0,6,0,7,4,0,5,0,
     &          6,0,7,0,8/
C
C
      KAPPAA=KAPPA(IND1)
      KAPPAB=KAPPA(IND2)
      MJA2=MQN(IND1)
      MJB2=MQN(IND2)
      MAXM=NFUNS(IND1)*NFUNS(IND2)
C
C
C     SET GEOMETRIC PARAMETERS
C    
      NFUNA=NFUNS(IND1)
      NFUNB=NFUNS(IND2)
      IF(KAPPAA.GT.0) THEN
      LA=KAPPAA
      ELSE
      LA=-KAPPAA-1
      ENDIF
      IF(KAPPAB.GT.0) THEN
      LB=KAPPAB
      ELSE
      LB=-KAPPAB-1
      ENDIF
C
      CXA=XYZ(1,IND1)
      CXB=XYZ(1,IND2)
      CYA=XYZ(2,IND1)
      CYB=XYZ(2,IND2)
      CZA=XYZ(3,IND1)
      CZB=XYZ(3,IND2)
      DXAB=CXB-CXA
      DYAB=CYB-CYA
      DZAB=CZB-CZA
      RSQ=(DXAB*DXAB)+(DYAB*DYAB)+(DZAB*DZAB)
C
      DO 10 M=1,MAXM
      P=EXPA(M)+EXPB(M)
      PXVEC=(EXPA(M)*CXA+EXPB(M)*CXB)/P
      PYVEC=(EXPA(M)*CYA+EXPB(M)*CYB)/P
      PZVEC=(EXPA(M)*CZA+EXPB(M)*CZB)/P
      P2(M)=0.5D0/P
      PAX(M)=PXVEC-CXA
      PBX(M)=PXVEC-CXB
      PAY(M)=PYVEC-CYA
      PBY(M)=PYVEC-CYB
      PAZ(M)=PZVEC-CZA
      PBZ(M)=PZVEC-CZB
      RKAB(M)=DEXP(-EXPA(M)*EXPB(M)*RSQ/P)
10    CONTINUE
C
C
C
      CALL ECART(EX,PAX,PBX,P2,MAXM,LA,LB)
      CALL ECART(EY,PAY,PBY,P2,MAXM,LA,LB)
      CALL ECART(EZ,PAZ,PBZ,P2,MAXM,LA,LB)
C
C
C
      LABMAX=LA+LB
      LAMAB=LA+LB
C
      ITUVMX=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      DO 300 ITUV=1,ITUVMX
      DO 300 M=1,MAXM
      ERLL11(M,ITUV)=0.0D0
      EILL11(M,ITUV)=0.0D0
      ERLL21(M,ITUV)=0.0D0
      EILL21(M,ITUV)=0.0D0
300   CONTINUE 
C
C*****************************************************************C
C     TRANSFORM FROM CARTESIAN SCALARS TO SPHERICAL SPINORS       C
C*****************************************************************C
C
      CALL SLMAKE(RRAU1,RIAU1,RRAL1,RIAL1,KAPPAA,-MJA2)
      CALL SLMAKE(RRAU2,RIAU2,RRAL2,RIAL2,KAPPAA,MJA2)
C
      CALL SLMAKE(RRBU1,RIBU1,RRBL1,RIBL1,KAPPAB,-MJB2)
      CALL SLMAKE(RRBU2,RIBU2,RRBL2,RIBL2,KAPPAB,MJB2)
C
      LAMAB=LA+LB
      NTRI1=((LA+1)*(LA+2)*(LA+3))/6
      NTRI2=((LB+1)*(LB+2)*(LB+3))/6
      NTUV=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      DO 100 ITRI1=1,NTRI1
      I1=IVEC(ITRI1)
      J1=JVEC(ITRI1)
      K1=KVEC(ITRI1)
C
      IF((I1+J1+K1).EQ.LA) THEN
C
      DO 101 ITRI2=1,NTRI2
      I2=IVEC(ITRI2)
      J2=JVEC(ITRI2)
      K2=KVEC(ITRI2)
C
      IF((I2+J2+K2).EQ.LB) THEN
C
      ISTRT=IED(I1+1,I2+1)
      JSTRT=IED(J1+1,J2+1)
      KSTRT=IED(K1+1,K2+1)
C
C     CONSTRUCT SCALAR PRODUCT OF EXPANSION COEFFICIENTS (COMPONENTWISE)
C
      FR11=RRAU1(ITRI1)*RRBU1(ITRI2)+RIAU1(ITRI1)*RIBU1(ITRI2)
     &    +RRAL1(ITRI1)*RRBL1(ITRI2)+RIAL1(ITRI1)*RIBL1(ITRI2)
      FI11=RRAU1(ITRI1)*RIBU1(ITRI2)-RIAU1(ITRI1)*RRBU1(ITRI2)
     &    +RRAL1(ITRI1)*RIBL1(ITRI2)-RIAL1(ITRI1)*RRBL1(ITRI2)
      FR21=RRAU2(ITRI1)*RRBU1(ITRI2)+RIAU2(ITRI1)*RIBU1(ITRI2)
     &    +RRAL2(ITRI1)*RRBL1(ITRI2)+RIAL2(ITRI1)*RIBL1(ITRI2)
      FI21=RRAU2(ITRI1)*RIBU1(ITRI2)-RIAU2(ITRI1)*RRBU1(ITRI2)
     &    +RRAL2(ITRI1)*RIBL1(ITRI2)-RIAL2(ITRI1)*RRBL1(ITRI2)
C
C     ACCUMULATE 3-D CARTESIAN HERMITE INDICES
C
      DO 200 ITAU=0,I1+I2
c      IF(IEXYZ(ISTRT+ITAU).EQ.0) GO TO 199
      DO 210 IMU=0,J1+J2
c      IF(IEXYZ(JSTRT+IMU).EQ.0)  GO TO 209
      DO 220 INU=0,K1+K2
C
      ITUV=INABCD(ITAU,IMU,INU)
C
      DO 90 M=1,MAXM
      P2M=P2(M)
      EXOCE=IEXYZ(ISTRT+ITAU)*(P2M**IEEXP(ISTRT+ITAU))
      EYOCE=IEXYZ(JSTRT+IMU)*(P2M**IEEXP(JSTRT+IMU))
      ECT=EX(M,ISTRT+ITAU)*EY(M,JSTRT+IMU)*EZ(M,KSTRT+INU)
c      ECT=EXOCE*EYOCE*EZ(M,KSTRT+INU)
      ERLL11(M,ITUV)=ERLL11(M,ITUV)+FR11*ECT
      EILL11(M,ITUV)=EILL11(M,ITUV)+FI11*ECT
      ERLL21(M,ITUV)=ERLL21(M,ITUV)+FR21*ECT
      EILL21(M,ITUV)=EILL21(M,ITUV)+FI21*ECT
90    CONTINUE
C 
219   CONTINUE
220   CONTINUE
C
209   CONTINUE
210   CONTINUE
C
199   CONTINUE
200   CONTINUE
C
98    CONTINUE
      ENDIF
101   CONTINUE
99    CONTINUE
      ENDIF
100   CONTINUE
C
C
      ITUV=0
      DO 400 MTUV=0,LAMAB
      RPH=DFLOAT((IALT)**(MTUV))
      DO 402 MDUM=1,((MTUV+1)*(MTUV+2))/2
      ITUV=ITUV+1
      DO 401 M=1,MAXM
      ERLL11(M,ITUV)=ERLL11(M,ITUV)*RKAB(M)*RNLL(M)*RPH
      EILL11(M,ITUV)=EILL11(M,ITUV)*RKAB(M)*RNLL(M)*RPH
      ERLL21(M,ITUV)=ERLL21(M,ITUV)*RKAB(M)*RNLL(M)*RPH
      EILL21(M,ITUV)=EILL21(M,ITUV)*RKAB(M)*RNLL(M)*RPH
401   CONTINUE
402   CONTINUE
400   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE SLMAKE(RCRU,RCIU,RCRL,RCIL,KAPPA,MQN2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(PI=3.1415926535898D0)
C
      DIMENSION RCRU(84),RCIU(84),RCRL(84),RCIL(84)
      COMMON/CONSTS/NCR(84,25),NCI(84,25),MCR(84,25),MCI(84,25),
     &              NRNUM(25),NRROOT(25),NRDEN(25)
C
      SQPINV=DSQRT(1.0D0/PI)
C 
C     CONSTRUCT LARGE COMPONENT SPINORS
C
      IF(KAPPA.GT.0) THEN
      LQN=KAPPA
      JQN2=2*KAPPA-1
      CGCU=-DSQRT(DFLOAT(JQN2+2-MQN2)/(2.0D0*DFLOAT(JQN2+2)))
      CGCL= DSQRT(DFLOAT(JQN2+2+MQN2)/(2.0D0*DFLOAT(JQN2+2)))
      ELSE
      LQN=-KAPPA-1
      JQN2=-2*KAPPA-1
      CGCU= DSQRT(DFLOAT(JQN2+MQN2)/(2.0D0*DFLOAT(JQN2)))
      CGCL= DSQRT(DFLOAT(JQN2-MQN2)/(2.0D0*DFLOAT(JQN2)))
      ENDIF
C
      MLQNU=(MQN2-1)/2
      MLQNL=(MQN2+1)/2
C
      IADRU=LQN*(LQN+1)+MLQNU+1
      IADRL=LQN*(LQN+1)+MLQNL+1
C
      ILEN=((LQN+1)*(LQN+2)*(LQN+3))/6
C
      IF(IABS(MLQNU).LE.LQN) THEN
      RFACTU=SQPINV*CGCU*DFLOAT(NRNUM(IADRU))*
     &  DSQRT(DFLOAT(NRROOT(IADRU)))/DFLOAT(NRDEN(IADRU))
      DO 10 I=1,ILEN
      RCRU(I)=RFACTU*DFLOAT(NCR(I,IADRU))
      RCIU(I)=RFACTU*DFLOAT(NCI(I,IADRU))
10    CONTINUE
      ELSE
      DO 11 I=1,ILEN
      RCRU(I)=0.0D0
      RCIU(I)=0.0D0
11    CONTINUE
      ENDIF
C
      IF(IABS(MLQNL).LE.LQN) THEN
      RFACTL=SQPINV*CGCL*DFLOAT(NRNUM(IADRL))*
     &  DSQRT(DFLOAT(NRROOT(IADRL)))/DFLOAT(NRDEN(IADRL))
      DO 20 I=1,ILEN
      RCRL(I)=RFACTL*DFLOAT(NCR(I,IADRL))
      RCIL(I)=RFACTL*DFLOAT(NCI(I,IADRL))
20    CONTINUE
      ELSE
      DO 21 I=1,ILEN
      RCRL(I)=0.0D0
      RCIL(I)=0.0D0
21    CONTINUE
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE EMAKESS(ERSS11,EISS11,ERSS21,EISS21,
     #                   KAPPA,MQN,NFUNS,IALT,IND1,IND2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************C
C     EMAKELL GENERATES BLOCKS OF SPHERICAL SPINOR E-COEFFICIENTS       C
C     BY CONTRACTING ON THEIR CARTESIAN TENSOR COMPONENTS               C
C     (CURRENTLY LIMITED TO LMAX=4)                                     C
C***********************************************************************C
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXDIM=300,NKAPM=7,
     &  NCENTM=6,IL4=24,MAXMV=5,MAXMV2=2*MAXMV)
      PARAMETER(MLL=165,MAXIJ=125,MAXLQN=4)
C      PARAMETER(MABLL=((MAXLQN+1)*(MAXLQN+2)*(MAXLQN+3))/6)
      PARAMETER(MABLL=MLL)
C
      COMMON/RNORM/RNLL(MAXB2),RNSL(MAXB2),RNLS(MAXB2),RNSS(MAXB2),
     #EXPA(MAXB2),EXPB(MAXB2),EXPAB(MAXB2)
      COMMON/INDSYS/INABCD(0:4*MAXLQN,0:4*MAXLQN,0:4*MAXLQN),
     &              IVEC(MABLL),JVEC(MABLL),KVEC(MABLL)
C
      COMMON/LOCGEO/XYZ(3,4)
      COMMON/TIMEE/ETIME
C
      COMMON/SPEC/COORD(3,NCENTM),ZNUC(NCENTM),CNUC(NCENTM),
     &  EXPSET(MAXB,NKAPM,NCENTM),IZNUC(NCENTM),ICRGE(NCENTM),
     &  KVALS(NKAPM,NCENTM),NKAPPA(NCENTM),LMAXX(NCENTM),
     &  NFUNCT(NKAPM,NCENTM),NCENT,NDIM,NSHIFT,
     &  NOCC,ITER,IALL,IRUN
      DIMENSION     ERSS11(MAXB2,0:MLL),EISS11(MAXB2,0:MLL),
     #              ERSS21(MAXB2,0:MLL),EISS21(MAXB2,0:MLL)
      DIMENSION RRAU1(84),RIAU1(84),RRAU2(84),RIAU2(84),
     &          RRBU1(84),RIBU1(84),
     &          RRAL1(84),RIAL1(84),RRAL2(84),RIAL2(84),
     &          RRBL1(84),RIBL1(84),
     &          RRBU2(84),RIBU2(84),RRBL2(84),RIBL2(84)
      DIMENSION RRAU1S(84),RIAU1S(84),RRAU2S(84),RIAU2S(84),
     &          RRBU1S(84),RIBU1S(84),
     &          RRAL1S(84),RIAL1S(84),RRAL2S(84),RIAL2S(84),
     &          RRBL1S(84),RIBL1S(84),
     &          RRBU2S(84),RIBU2S(84),RRBL2S(84),RIBL2S(84)
      DIMENSION EX(MAXB2,MAXIJ),EY(MAXB2,MAXIJ),EZ(MAXB2,MAXIJ),
     &          PAX(MAXB2),PAY(MAXB2),PAZ(MAXB2),
     &          PBX(MAXB2),PBY(MAXB2),PBZ(MAXB2),
     &          P2(MAXB2),RKAB(MAXB2)
      DIMENSION IED(5,5) 
      DIMENSION KAPPA(4),MQN(4),NFUNS(4)
      DIMENSION IEXYZ(MAXIJ),IEEXP(MAXIJ)
C
      DATA IED/1,16,36,61,91,
     &         2,18,39,65,96,
     &         4,21,43,70,102,
     &         7,25,48,76,109,
     &        11,30,54,83,117/
C
C
      DATA IEXYZ/1,0,1,1,0,1,0,3,0,1,3,0,6,0,1,0,1,1,0,1,
     &         0,3,0,1,3,0,6,0,1,0,15,0,10,0,1,1,0,1,0,3,
     &      0,1,3,0,6,0,1,0,15,0,10,0,1,15,0,45,0,15,0,1,
     &      0,3,0,1,3,0,6,0,1,0,15,0,10,0,1,15,0,45,0,15,
     &    0,1,0,105,0,105,0,21,0,1,3,0,6,0,1,0,15,0,10,0,
     & 1,15,0,45,0,15,0,1,0,105,0,105,0,21,0,1,105,0,420,0,
     &          210,0,28,0,1/
C
      DATA IEEXP/0,0,1,1,0,2,0,2,0,3,2,0,3,0,4,0,1,1,0,2,
     &          0,2,0,3,2,0,3,0,4,0,3,0,4,0,5,1,0,2,0,2,
     &          0,3,2,0,3,0,4,0,3,0,4,0,5,3,0,4,0,5,0,6,
     &          0,2,0,3,2,0,3,0,4,0,3,0,4,0,5,3,0,4,0,5,
     &          0,6,0,4,0,5,0,6,0,7,2,0,3,0,4,0,3,0,4,0,
     &          5,3,0,4,0,5,0,6,0,4,0,5,0,6,0,7,4,0,5,0,
     &          6,0,7,0,8/
C
C
      KAPPAA=KAPPA(IND1)
      KAPPAB=KAPPA(IND2)
      MJA2=MQN(IND1)
      MJB2=MQN(IND2)
      MAXM=NFUNS(IND1)*NFUNS(IND2)
C
C     SET GEOMETRIC PARAMETERS
C    
      NFUNA=NFUNS(IND1)
      NFUNB=NFUNS(IND2)
      IF(KAPPAA.GT.0) THEN
      LA=KAPPAA-1
      NA=1
      AFAC=DFLOAT(2*KAPPAA+1)
      ELSE
      LA=-KAPPAA
      NA=0
      AFAC=0.0D0
      ENDIF
      IF(KAPPAB.GT.0) THEN
      LB=KAPPAB-1
      NB=1
      BFAC=DFLOAT(2*KAPPAB+1)
      ELSE
      LB=-KAPPAB
      NB=0
      BFAC=0.0D0
      ENDIF
C
      NA2=2*NA
      NB2=2*NB
      LEFFA=LA+NA2
      LEFFB=LB+NB2
      LEFFAB=LA+LB+(NA2+NB2)
C
      CXA=XYZ(1,IND1)
      CXB=XYZ(1,IND2)
      CYA=XYZ(2,IND1)
      CYB=XYZ(2,IND2)
      CZA=XYZ(3,IND1)
      CZB=XYZ(3,IND2)
      DXAB=CXB-CXA
      DYAB=CYB-CYA
      DZAB=CZB-CZA
      RSQ=(DXAB*DXAB)+(DYAB*DYAB)+(DZAB*DZAB)
C
      DO 10 M=1,MAXM
      P=EXPA(M)+EXPB(M)
      PXVEC=(EXPA(M)*CXA+EXPB(M)*CXB)/P
      PYVEC=(EXPA(M)*CYA+EXPB(M)*CYB)/P
      PZVEC=(EXPA(M)*CZA+EXPB(M)*CZB)/P
      P2(M)=0.5D0/P
      PAX(M)=PXVEC-CXA
      PBX(M)=PXVEC-CXB
      PAY(M)=PYVEC-CYA
      PBY(M)=PYVEC-CYB
      PAZ(M)=PZVEC-CZA
      PBZ(M)=PZVEC-CZB
      RKAB(M)=DEXP(-EXPA(M)*EXPB(M)*RSQ/P)
10    CONTINUE
C 
C
      CALL ECART(EX,PAX,PBX,P2,MAXM,LEFFA,LEFFB)
      CALL ECART(EY,PAY,PBY,P2,MAXM,LEFFA,LEFFB)
      CALL ECART(EZ,PAZ,PBZ,P2,MAXM,LEFFA,LEFFB)
C
C
C
      LABMAX=LEFFAB
      LAMAB=LEFFAB
C
      ITUVMX=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      DO 300 ITUV=1,ITUVMX
      DO 300 M=1,MAXM
      ERSS11(M,ITUV)=0.0D0
      EISS11(M,ITUV)=0.0D0
      ERSS21(M,ITUV)=0.0D0
      EISS21(M,ITUV)=0.0D0
300   CONTINUE 
C
C*****************************************************************C
C     TRANSFORM FROM CARTESIAN SCALARS TO SPHERICAL SPINORS       C
C*****************************************************************C
C
      IF(KAPPAA.LT.0) THEN
C     
      NQN=0
      CALL SSMAKE(RRAU1S,RIAU1S,RRAL1S,RIAL1S,KAPPAA,-MJA2,NQN)
      CALL SSMAKE(RRAU2S,RIAU2S,RRAL2S,RIAL2S,KAPPAA,MJA2,NQN)
C
      ELSE
C
      NQN=0
      CALL SSMAKE(RRAU1,RIAU1,RRAL1,RIAL1,KAPPAA,-MJA2,NQN)
      CALL SSMAKE(RRAU2,RIAU2,RRAL2,RIAL2,KAPPAA,MJA2,NQN)
      NQN=1
      CALL SSMAKE(RRAU1S,RIAU1S,RRAL1S,RIAL1S,KAPPAA,-MJA2,NQN)
      CALL SSMAKE(RRAU2S,RIAU2S,RRAL2S,RIAL2S,KAPPAA,MJA2,NQN)
C
      ENDIF
C
      IF(KAPPAB.LT.0) THEN
C
      NQN=0
      CALL SSMAKE(RRBU1S,RIBU1S,RRBL1S,RIBL1S,KAPPAB,-MJB2,NQN)
      CALL SSMAKE(RRBU2S,RIBU2S,RRBL2S,RIBL2S,KAPPAB,MJB2,NQN)
C
      ELSE
C
      NQN=0
      CALL SSMAKE(RRBU1,RIBU1,RRBL1,RIBL1,KAPPAB,-MJB2,NQN)
      CALL SSMAKE(RRBU2,RIBU2,RRBL2,RIBL2,KAPPAB,MJB2,NQN)
      NQN=1
      CALL SSMAKE(RRBU1S,RIBU1S,RRBL1S,RIBL1S,KAPPAB,-MJB2,NQN)
      CALL SSMAKE(RRBU2S,RIBU2S,RRBL2S,RIBL2S,KAPPAB,MJB2,NQN)
C
      ENDIF

C
      LAMAB=LEFFAB
      NTRI1=((LEFFA+1)*(LEFFA+2)*(LEFFA+3))/6
      NTRI2=((LEFFB+1)*(LEFFB+2)*(LEFFB+3))/6
      NTUV=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      DO 100 ITRI1S=1,NTRI1
      I1=IVEC(ITRI1S)
      J1=JVEC(ITRI1S)
      K1=KVEC(ITRI1S)
C
      ITRI1=INABCD(I1,J1,K1)
C
      DO 101 ITRI2S=1,NTRI2
      I2=IVEC(ITRI2S)
      J2=JVEC(ITRI2S)
      K2=KVEC(ITRI2S)
C
      ITRI2=INABCD(I2,J2,K2)
C
      ISTRT=IED(I1+1,I2+1)
      JSTRT=IED(J1+1,J2+1)
      KSTRT=IED(K1+1,K2+1)
C
C
C     CONSTRUCT SCALAR PRODUCT OF EXPANSION COEFFICIENTS (COMPONENTWISE)
C
C-----------------------------------------------------------------------C
C
      IF((I1+J1+K1).EQ.LEFFA) THEN
      IF((I2+J2+K2).EQ.LEFFB) THEN
C
      FR11=RRAU1S(ITRI1S)*RRBU1S(ITRI2S)+RIAU1S(ITRI1S)*RIBU1S(ITRI2S)
     &    +RRAL1S(ITRI1S)*RRBL1S(ITRI2S)+RIAL1S(ITRI1S)*RIBL1S(ITRI2S)
      FI11=RRAU1S(ITRI1S)*RIBU1S(ITRI2S)-RIAU1S(ITRI1S)*RRBU1S(ITRI2S)
     &    +RRAL1S(ITRI1S)*RIBL1S(ITRI2S)-RIAL1S(ITRI1S)*RRBL1S(ITRI2S)
      FR21=RRAU2S(ITRI1S)*RRBU1S(ITRI2S)+RIAU2S(ITRI1S)*RIBU1S(ITRI2S)
     &    +RRAL2S(ITRI1S)*RRBL1S(ITRI2S)+RIAL2S(ITRI1S)*RIBL1S(ITRI2S)
      FI21=RRAU2S(ITRI1S)*RIBU1S(ITRI2S)-RIAU2S(ITRI1S)*RRBU1S(ITRI2S)
     &    +RRAL2S(ITRI1S)*RIBL1S(ITRI2S)-RIAL2S(ITRI1S)*RRBL1S(ITRI2S)
C
C     ACCUMULATE 3-D CARTESIAN HERMITE INDICES
C
      DO 230 ITAU=0,I1+I2
c      IF(IEXYZ(ISTRT+ITAU).EQ.0) GO TO 229
      DO 240 IMU=0,J1+J2
c      IF(IEXYZ(JSTRT+IMU).EQ.0)  GO TO 239
      DO 250 INU=0,K1+K2
c      IF(ITST(KSTRT+INU).EQ.0.AND.ICNT.EQ.1) GO TO 249
C
      ITUV=INABCD(ITAU,IMU,INU)
C
      DO 90 M=1,MAXM
      P2M=P2(M)
c      EXOCE=IEXYZ(ISTRT+ITAU)*(P2M**IEEXP(ISTRT+ITAU))
c      EYOCE=IEXYZ(JSTRT+IMU)*(P2M**IEEXP(JSTRT+IMU))
      ECT=EX(M,ISTRT+ITAU)*EY(M,JSTRT+IMU)*EZ(M,KSTRT+INU)
c      ECT=EXOCE*EYOCE*EZ(M,KSTRT+INU)
      RFCT=4.0D0*EXPA(M)*EXPB(M)
      ERSS11(M,ITUV)=ERSS11(M,ITUV)+FR11*ECT*RFCT
      EISS11(M,ITUV)=EISS11(M,ITUV)+FI11*ECT*RFCT
      ERSS21(M,ITUV)=ERSS21(M,ITUV)+FR21*ECT*RFCT
      EISS21(M,ITUV)=EISS21(M,ITUV)+FI21*ECT*RFCT
90    CONTINUE
C 
249   CONTINUE
250   CONTINUE
C 
239   CONTINUE
240   CONTINUE
C 
229    CONTINUE
230   CONTINUE
C
      ENDIF
      ENDIF
C
C-----------------------------------------------------------------------C
C
      IF(BFAC.NE.0.0D0) THEN
      IF((I1+J1+K1).EQ.LEFFA) THEN
      IF((I2+J2+K2).EQ.LB) THEN
C
C
      FR11=RRAU1S(ITRI1S)*RRBU1(ITRI2)+RIAU1S(ITRI1S)*RIBU1(ITRI2)
     &    +RRAL1S(ITRI1S)*RRBL1(ITRI2)+RIAL1S(ITRI1S)*RIBL1(ITRI2)
      FI11=RRAU1S(ITRI1S)*RIBU1(ITRI2)-RIAU1S(ITRI1S)*RRBU1(ITRI2)
     &    +RRAL1S(ITRI1S)*RIBL1(ITRI2)-RIAL1S(ITRI1S)*RRBL1(ITRI2)
      FR21=RRAU2S(ITRI1S)*RRBU1(ITRI2)+RIAU2S(ITRI1S)*RIBU1(ITRI2)
     &    +RRAL2S(ITRI1S)*RRBL1(ITRI2)+RIAL2S(ITRI1S)*RIBL1(ITRI2)
      FI21=RRAU2S(ITRI1S)*RIBU1(ITRI2)-RIAU2S(ITRI1S)*RRBU1(ITRI2)
     &    +RRAL2S(ITRI1S)*RIBL1(ITRI2)-RIAL2S(ITRI1S)*RRBL1(ITRI2)
C
C     ACCUMULATE 3-D CARTESIAN HERMITE INDICES
C
      DO 330 ITAU=0,I1+I2
c      IF(IEXYZ(ISTRT+ITAU).EQ.0) GO TO 329
      DO 340 IMU=0,J1+J2
c      IF(IEXYZ(JSTRT+IMU).EQ.0)  GO TO 339
      DO 350 INU=0,K1+K2
c      IF(IEXYZ(KSTRT+INU).EQ.0.AND.ICNT.EQ.1) GO TO 349 
C
      ITUV=INABCD(ITAU,IMU,INU)
C
      DO 590 M=1,MAXM
      P2M=P2(M)
c      EXOCE=IEXYZ(ISTRT+ITAU)*(P2M**IEEXP(ISTRT+ITAU))
c      EYOCE=IEXYZ(JSTRT+IMU)*(P2M**IEEXP(JSTRT+IMU))
      ECT=EX(M,ISTRT+ITAU)*EY(M,JSTRT+IMU)*EZ(M,KSTRT+INU)
c      ECT=EXOCE*EYOCE*EZ(M,KSTRT+INU)
      RFCT=-2.0D0*EXPA(M)*BFAC
      ERSS11(M,ITUV)=ERSS11(M,ITUV)+FR11*ECT*RFCT
      EISS11(M,ITUV)=EISS11(M,ITUV)+FI11*ECT*RFCT
      ERSS21(M,ITUV)=ERSS21(M,ITUV)+FR21*ECT*RFCT
      EISS21(M,ITUV)=EISS21(M,ITUV)+FI21*ECT*RFCT
590    CONTINUE
C
349   CONTINUE
350   CONTINUE
C
339   CONTINUE
340   CONTINUE
C
329   CONTINUE
330   CONTINUE
C
C
      ENDIF
      ENDIF
      ENDIF
C
C
C-----------------------------------------------------------------------C
C
      IF(AFAC.NE.0.0D0) THEN
      IF((I1+J1+K1).EQ.LA) THEN
      IF((I2+J2+K2).EQ.LEFFB) THEN
C
      FR11=RRAU1(ITRI1)*RRBU1S(ITRI2S)+RIAU1(ITRI1)*RIBU1S(ITRI2S)
     &    +RRAL1(ITRI1)*RRBL1S(ITRI2S)+RIAL1(ITRI1)*RIBL1S(ITRI2S)
      FI11=RRAU1(ITRI1)*RIBU1S(ITRI2S)-RIAU1(ITRI1)*RRBU1S(ITRI2S)
     &    +RRAL1(ITRI1)*RIBL1S(ITRI2S)-RIAL1(ITRI1)*RRBL1S(ITRI2S)
      FR21=RRAU2(ITRI1)*RRBU1S(ITRI2S)+RIAU2(ITRI1)*RIBU1S(ITRI2S)
     &    +RRAL2(ITRI1)*RRBL1S(ITRI2S)+RIAL2(ITRI1)*RIBL1S(ITRI2S)
      FI21=RRAU2(ITRI1)*RIBU1S(ITRI2S)-RIAU2(ITRI1)*RRBU1S(ITRI2S)
     &    +RRAL2(ITRI1)*RIBL1S(ITRI2S)-RIAL2(ITRI1)*RRBL1S(ITRI2S)
C
C     ACCUMULATE 3-D CARTESIAN HERMITE INDICES
C
      DO 430 ITAU=0,I1+I2
c      IF(IEXYZ(ISTRT+ITAU).EQ.0) GO TO 429
      DO 440 IMU=0,J1+J2
c      IF(IEXYZ(JSTRT+IMU).EQ.0)  GO TO 439
      DO 450 INU=0,K1+K2
c      IF(IEXYZ(KSTRT+INU).EQ.0.AND.ICNT.EQ.1) GO TO 449 
C
      ITUV=INABCD(ITAU,IMU,INU)
C
      DO 690 M=1,MAXM
      P2M=P2(M)
c      EXOCE=IEXYZ(ISTRT+ITAU)*(P2M**IEEXP(ISTRT+ITAU))
c      EYOCE=IEXYZ(JSTRT+IMU)*(P2M**IEEXP(JSTRT+IMU))
      ECT=EX(M,ISTRT+ITAU)*EY(M,JSTRT+IMU)*EZ(M,KSTRT+INU)
c      ECT=EXOCE*EYOCE*EZ(M,KSTRT+INU)
      RFCT=-2.0D0*EXPB(M)*AFAC
      ERSS11(M,ITUV)=ERSS11(M,ITUV)+FR11*ECT*RFCT
      EISS11(M,ITUV)=EISS11(M,ITUV)+FI11*ECT*RFCT
      ERSS21(M,ITUV)=ERSS21(M,ITUV)+FR21*ECT*RFCT
      EISS21(M,ITUV)=EISS21(M,ITUV)+FI21*ECT*RFCT
690    CONTINUE
C
449   CONTINUE
450   CONTINUE
C
439   CONTINUE
440   CONTINUE
C
429   CONTINUE
430   CONTINUE
C
      ENDIF
      ENDIF
      ENDIF
C
C-----------------------------------------------------------------------C
C
      IF(AFAC*BFAC.NE.0.0D0) THEN
      IF((I1+J1+K1).EQ.LA) THEN
      IF((I2+J2+K2).EQ.LB) THEN
C
      FR11=RRAU1(ITRI1)*RRBU1(ITRI2)+RIAU1(ITRI1)*RIBU1(ITRI2)
     &    +RRAL1(ITRI1)*RRBL1(ITRI2)+RIAL1(ITRI1)*RIBL1(ITRI2)
      FI11=RRAU1(ITRI1)*RIBU1(ITRI2)-RIAU1(ITRI1)*RRBU1(ITRI2)
     &    +RRAL1(ITRI1)*RIBL1(ITRI2)-RIAL1(ITRI1)*RRBL1(ITRI2)
      FR21=RRAU2(ITRI1)*RRBU1(ITRI2)+RIAU2(ITRI1)*RIBU1(ITRI2)
     &    +RRAL2(ITRI1)*RRBL1(ITRI2)+RIAL2(ITRI1)*RIBL1(ITRI2)
      FI21=RRAU2(ITRI1)*RIBU1(ITRI2)-RIAU2(ITRI1)*RRBU1(ITRI2)
     &    +RRAL2(ITRI1)*RIBL1(ITRI2)-RIAL2(ITRI1)*RRBL1(ITRI2)
C
C     ACCUMULATE 3-D CARTESIAN HERMITE INDICES
C
      DO 530 ITAU=0,I1+I2
c      IF(IEXYZ(ISTRT+ITAU).EQ.0) GO TO 529
      DO 540 IMU=0,J1+J2
c      IF(IEXYZ(JSTRT+IMU).EQ.0)  GO TO 539 
      DO 550 INU=0,K1+K2
C      IF(IEXYZ(KSTRT+INU).EQ.0.AND.ICNT.EQ.1) GO TO 549 
C
      ITUV=INABCD(ITAU,IMU,INU)
C
      DO 790 M=1,MAXM
      P2M=P2(M)
c      EXOCE=IEXYZ(ISTRT+ITAU)*(P2M**IEEXP(ISTRT+ITAU))
c      EYOCE=IEXYZ(JSTRT+IMU)*(P2M**IEEXP(JSTRT+IMU))
      ECT=EX(M,ISTRT+ITAU)*EY(M,JSTRT+IMU)*EZ(M,KSTRT+INU)
c      ECT=EXOCE*EYOCE*EZ(M,KSTRT+INU)
      RFCT=AFAC*BFAC
      ERSS11(M,ITUV)=ERSS11(M,ITUV)+FR11*ECT*RFCT
      EISS11(M,ITUV)=EISS11(M,ITUV)+FI11*ECT*RFCT
      ERSS21(M,ITUV)=ERSS21(M,ITUV)+FR21*ECT*RFCT
      EISS21(M,ITUV)=EISS21(M,ITUV)+FI21*ECT*RFCT
790    CONTINUE
C
549   CONTINUE
550   CONTINUE
C
539   CONTINUE
540   CONTINUE
C
529   CONTINUE
530   CONTINUE
C
      ENDIF
      ENDIF
      ENDIF
C
C-----------------------------------------------------------------------C
98    CONTINUE
101   CONTINUE
99    CONTINUE
100   CONTINUE
C
      ITUV=0
      DO 400 MTUV=0,LAMAB
      RPH=DFLOAT((IALT)**(MTUV))
      DO 402 MDUM=1,((MTUV+1)*(MTUV+2))/2
      ITUV=ITUV+1
      DO 401 M=1,MAXM
      ERSS11(M,ITUV)=ERSS11(M,ITUV)*RKAB(M)*RNSS(M)*RPH
      EISS11(M,ITUV)=EISS11(M,ITUV)*RKAB(M)*RNSS(M)*RPH
      ERSS21(M,ITUV)=ERSS21(M,ITUV)*RKAB(M)*RNSS(M)*RPH
      EISS21(M,ITUV)=EISS21(M,ITUV)*RKAB(M)*RNSS(M)*RPH
401   CONTINUE
402   CONTINUE
400   CONTINUE
C
C
      RETURN
      END
C
C
C
      SUBROUTINE SSMAKE(RCRU,RCIU,RCRL,RCIL,KAPPA,MQN2,NQN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(PI=3.1415926535898D0)
C
      DIMENSION RCRU(84),RCIU(84),RCRL(84),RCIL(84)
      COMMON/CONSTS/NCR(84,25),NCI(84,25),MCR(84,25),MCI(84,25),
     &              NRNUM(25),NRROOT(25),NRDEN(25)
C
C
      SQPINV=DSQRT(1.0D0/PI)
C 
c      write(6,*) 'SSMAKE'
C     CONSTRUCT LARGE COMPONENT SPINORS
C
      IF(KAPPA.GT.0) THEN
      LQN=KAPPA-1
      JQN2=2*KAPPA-1
      CGCU= DSQRT(DFLOAT(JQN2+MQN2)/(2.0D0*DFLOAT(JQN2)))
      CGCL= DSQRT(DFLOAT(JQN2-MQN2)/(2.0D0*DFLOAT(JQN2)))
      ELSE
      LQN=-KAPPA
      JQN2=-2*KAPPA-1
      CGCU=-DSQRT(DFLOAT(JQN2+2-MQN2)/(2.0D0*DFLOAT(JQN2+2)))
      CGCL= DSQRT(DFLOAT(JQN2+2+MQN2)/(2.0D0*DFLOAT(JQN2+2)))
      ENDIF
C
      MLQNU=(MQN2-1)/2
      MLQNL=(MQN2+1)/2
C
      IADRU=LQN*(LQN+1)+MLQNU+1
      IADRL=LQN*(LQN+1)+MLQNL+1
C
C
      IF(NQN.EQ.0) THEN
      ILEN=((LQN+1)*(LQN+2)*(LQN+3))/6
C
      IF(IABS(MLQNU).LE.LQN) THEN
      RFACTU=SQPINV*CGCU*DFLOAT(NRNUM(IADRU))*
     &  DSQRT(DFLOAT(NRROOT(IADRU)))/DFLOAT(NRDEN(IADRU))
      DO 10 I=1,ILEN
      RCRU(I)=RFACTU*DFLOAT(NCR(I,IADRU))
      RCIU(I)=RFACTU*DFLOAT(NCI(I,IADRU))
10    CONTINUE
      ELSE
      DO 11 I=1,ILEN
      RCRU(I)=0.0D0
      RCIU(I)=0.0D0
11    CONTINUE
      ENDIF
C
      IF(IABS(MLQNL).LE.LQN) THEN
      RFACTL=SQPINV*CGCL*DFLOAT(NRNUM(IADRL))*
     &  DSQRT(DFLOAT(NRROOT(IADRL)))/DFLOAT(NRDEN(IADRL))
      DO 20 I=1,ILEN
      RCRL(I)=RFACTL*DFLOAT(NCR(I,IADRL))
      RCIL(I)=RFACTL*DFLOAT(NCI(I,IADRL))
20    CONTINUE
      ELSE
      DO 21 I=1,ILEN
      RCRL(I)=0.0D0
      RCIL(I)=0.0D0
21    CONTINUE
      ENDIF
C
      ELSE
      ILEN=((LQN+3)*(LQN+4)*(LQN+5))/6
C
      IF(IABS(MLQNU).LE.LQN) THEN
      RFACTU=SQPINV*CGCU*DFLOAT(NRNUM(IADRU))*
     &  DSQRT(DFLOAT(NRROOT(IADRU)))/DFLOAT(NRDEN(IADRU))
      DO 30 I=1,ILEN
      RCRU(I)=RFACTU*DFLOAT(MCR(I,IADRU))
      RCIU(I)=RFACTU*DFLOAT(MCI(I,IADRU))
30    CONTINUE
      ELSE
      DO 31 I=1,ILEN
      RCRU(I)=0.0D0
      RCIU(I)=0.0D0
31    CONTINUE
      ENDIF
C    
      IF(IABS(MLQNL).LE.LQN) THEN
      RFACTL=SQPINV*CGCL*DFLOAT(NRNUM(IADRL))*
     &  DSQRT(DFLOAT(NRROOT(IADRL)))/DFLOAT(NRDEN(IADRL))
      DO 40 I=1,ILEN
      RCRL(I)=RFACTL*DFLOAT(MCR(I,IADRL))
      RCIL(I)=RFACTL*DFLOAT(MCI(I,IADRL))
40    CONTINUE
      ELSE
      DO 41 I=1,ILEN
      RCRL(I)=0.0D0
      RCIL(I)=0.0D0
41    CONTINUE
      ENDIF
C
      ENDIF
C
      RETURN
      END
C***********************************************************************C
C                                                                       C
C     SECTION FOR GENERATING ONE-CENTRE E-COEFFICIENTS                  C
C                                                                       C
C***********************************************************************C
C
C**********************************************************************C
C
      SUBROUTINE OEMAKELL(ERLL11,EILL11,ERLL21,EILL21,
     #                   KAPPA,MQN,NFUNS,IALT,IND1,IND2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************C
C     EMAKELL GENERATES BLOCKS OF SPHERICAL SPINOR E-COEFFICIENTS       C
C     BY CONTRACTING ON THEIR CARTESIAN TENSOR COMPONENTS               C
C     (CURRENTLY LIMITED TO LMAX=4)                                     C
C***********************************************************************C
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXDIM=300,NKAPM=7,
     &  NCENTM=6,IL4=24,MAXMV=5,MAXMV2=2*MAXMV)
      PARAMETER(MLL=165,MAXIJ=125,MAXLQN=4)
C      PARAMETER(MABLL=((MAXLQN+1)*(MAXLQN+2)*(MAXLQN+3))/6)
      PARAMETER(MABLL=MLL)
      PARAMETER(PI=3.1415926535898D0)
      PARAMETER(SQPINV=0.5641895835477563D0)
C
      COMMON/RNORM/RNLL(MAXB2),RNSL(MAXB2),RNLS(MAXB2),RNSS(MAXB2),
     #EXPA(MAXB2),EXPB(MAXB2),EXPAB(MAXB2)
      COMMON/INDSYS/INABCD(0:4*MAXLQN,0:4*MAXLQN,0:4*MAXLQN),
     &              IVEC(MABLL),JVEC(MABLL),KVEC(MABLL)
C
      COMMON/LOCGEO/XYZ(3,4)
      COMMON/TIMEE/ETIME
C
      COMMON/SPEC/COORD(3,NCENTM),ZNUC(NCENTM),CNUC(NCENTM),
     &  EXPSET(MAXB,NKAPM,NCENTM),IZNUC(NCENTM),ICRGE(NCENTM),
     &  KVALS(NKAPM,NCENTM),NKAPPA(NCENTM),LMAXX(NCENTM),
     &  NFUNCT(NKAPM,NCENTM),NCENT,NDIM,NSHIFT,
     &  NOCC,ITER,IALL,IRUN
C
      COMMON/CGCSPH/NA1UNU,NA1UDE,NA2UNU,NA2UDE,NB1UNU,NB1UDE,
     &              NA1URT,NA1LRT,NA2URT,NA2LRT,NB1URT,NB1LRT
C
      DIMENSION     ERLL11(MAXB2,0:MLL),EILL11(MAXB2,0:MLL),
     #              ERLL21(MAXB2,0:MLL),EILL21(MAXB2,0:MLL)
      DIMENSION NRAU1(84),NIAU1(84),NRAU2(84),NIAU2(84),
     &          NRBU1(84),NIBU1(84),
     &          NRAL1(84),NIAL1(84),NRAL2(84),NIAL2(84),
     &          NRBL1(84),NIBL1(84)
      DIMENSION IEXYZ(MAXIJ),IEEXP(MAXIJ),
     &          P2(MAXB2),RKAB(MAXB2)
      DIMENSION IED(5,5) 
      DIMENSION KAPPA(4),MQN(4),NFUNS(4)
C
      DIMENSION NAR11U(MABLL,0:2*MAXLQN),NAR11L(MABLL,0:2*MAXLQN),
     &          NAI11U(MABLL,0:2*MAXLQN),NAI11L(MABLL,0:2*MAXLQN),
     &          NAR21U(MABLL,0:2*MAXLQN),NAR21L(MABLL,0:2*MAXLQN),
     &          NAI21U(MABLL,0:2*MAXLQN),NAI21L(MABLL,0:2*MAXLQN)
C
C
      DATA IED/1,16,36,61,91,
     &         2,18,39,65,96,
     &         4,21,43,70,102,
     &         7,25,48,76,109,
     &        11,30,54,83,117/
C
      DATA IEXYZ/1,0,1,1,0,1,0,3,0,1,3,0,6,0,1,0,1,1,0,1,
     &         0,3,0,1,3,0,6,0,1,0,15,0,10,0,1,1,0,1,0,3,
     &      0,1,3,0,6,0,1,0,15,0,10,0,1,15,0,45,0,15,0,1,
     &      0,3,0,1,3,0,6,0,1,0,15,0,10,0,1,15,0,45,0,15,
     &    0,1,0,105,0,105,0,21,0,1,3,0,6,0,1,0,15,0,10,0,
     & 1,15,0,45,0,15,0,1,0,105,0,105,0,21,0,1,105,0,420,0,
     &          210,0,28,0,1/
C
      DATA IEEXP/0,0,1,1,0,2,0,2,0,3,2,0,3,0,4,0,1,1,0,2,
     &          0,2,0,3,2,0,3,0,4,0,3,0,4,0,5,1,0,2,0,2,
     &          0,3,2,0,3,0,4,0,3,0,4,0,5,3,0,4,0,5,0,6,
     &          0,2,0,3,2,0,3,0,4,0,3,0,4,0,5,3,0,4,0,5,
     &          0,6,0,4,0,5,0,6,0,7,2,0,3,0,4,0,3,0,4,0,
     &          5,3,0,4,0,5,0,6,0,4,0,5,0,6,0,7,4,0,5,0,
     &          6,0,7,0,8/
C
C
      KAPPAA=KAPPA(IND1)
      KAPPAB=KAPPA(IND2)
      MJA2=MQN(IND1)
      MJB2=MQN(IND2)
      MAXM=NFUNS(IND1)*NFUNS(IND2)
C
C
C     SET GEOMETRIC PARAMETERS
C    
      NFUNA=NFUNS(IND1)
      NFUNB=NFUNS(IND2)
      IF(KAPPAA.GT.0) THEN
      LA=KAPPAA
      ELSE
      LA=-KAPPAA-1
      ENDIF
      IF(KAPPAB.GT.0) THEN
      LB=KAPPAB
      ELSE
      LB=-KAPPAB-1
      ENDIF
C
      CXA=XYZ(1,IND1)
      CXB=XYZ(1,IND2)
      CYA=XYZ(2,IND1)
      CYB=XYZ(2,IND2)
      CZA=XYZ(3,IND1)
      CZB=XYZ(3,IND2)
      DXAB=CXB-CXA
      DYAB=CYB-CYA
      DZAB=CZB-CZA
      RSQ=(DXAB*DXAB)+(DYAB*DYAB)+(DZAB*DZAB)
C
C
      DO 10 M=1,MAXM
      P=EXPA(M)+EXPB(M)
      P2(M)=0.5D0/P
      RKAB(M)=DEXP(-EXPA(M)*EXPB(M)*RSQ/P)
10    CONTINUE
C
C
C
      LABMAX=LA+LB
      LAMAB=LA+LB
C
      ITUVMX=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      DO 300 ITUV=1,ITUVMX
      DO 300 M=1,MAXM
      ERLL11(M,ITUV)=0.0D0
      EILL11(M,ITUV)=0.0D0
      ERLL21(M,ITUV)=0.0D0
      EILL21(M,ITUV)=0.0D0
300   CONTINUE 
C
      DO 255 ITUV=1,ITUVMX
      DO 255 IP=0,8
      NAR11U(ITUV,IP)=0
      NAR11L(ITUV,IP)=0
      NAI11U(ITUV,IP)=0
      NAI11L(ITUV,IP)=0
      NAR21U(ITUV,IP)=0
      NAR21L(ITUV,IP)=0
      NAI21U(ITUV,IP)=0
      NAI21L(ITUV,IP)=0
255   CONTINUE
C
C*****************************************************************C
C     TRANSFORM FROM CARTESIAN SCALARS TO SPHERICAL SPINORS       C
C*****************************************************************C
C
      CALL ILMAKE(NRAU1,NIAU1,NRAL1,NIAL1,KAPPAA,-MJA2,
     &            NA1UN,NA1UR,NA1UD,NA1LN,NA1LR,NA1LD)
      CALL ILMAKE(NRAU2,NIAU2,NRAL2,NIAL2,KAPPAA,MJA2,
     &            NA2UN,NA2UR,NA2UD,NA2LN,NA2LR,NA2LD)
C
      CALL ILMAKE(NRBU1,NIBU1,NRBL1,NIBL1,KAPPAB,-MJB2,
     &            NB1UN,NB1UR,NB1UD,NB1LN,NB1LR,NB1LD)
C
      LAMAB=LA+LB
      NTRI1=((LA+1)*(LA+2)*(LA+3))/6
      NTRI2=((LB+1)*(LB+2)*(LB+3))/6
      NTUV=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      DO 100 ITRI1=1,NTRI1
      I1=IVEC(ITRI1)
      J1=JVEC(ITRI1)
      K1=KVEC(ITRI1)
C
      IF((I1+J1+K1).EQ.LA) THEN
C
      DO 101 ITRI2=1,NTRI2
      I2=IVEC(ITRI2)
      J2=JVEC(ITRI2)
      K2=KVEC(ITRI2)
C
      IF((I2+J2+K2).EQ.LB) THEN
C
      ISTRT=IED(I1+1,I2+1)
      JSTRT=IED(J1+1,J2+1)
      KSTRT=IED(K1+1,K2+1)
C
C     CONSTRUCT SCALAR PRODUCT OF EXPANSION COEFFICIENTS (COMPONENTWISE)
C
      IFR11U=NRAU1(ITRI1)*NRBU1(ITRI2)+NIAU1(ITRI1)*NIBU1(ITRI2)
      IFR11L=NRAL1(ITRI1)*NRBL1(ITRI2)+NIAL1(ITRI1)*NIBL1(ITRI2)
      IFI11U=NRAU1(ITRI1)*NIBU1(ITRI2)-NIAU1(ITRI1)*NRBU1(ITRI2)
      IFI11L=NRAL1(ITRI1)*NIBL1(ITRI2)-NIAL1(ITRI1)*NRBL1(ITRI2)
      IFR21U=NRAU2(ITRI1)*NRBU1(ITRI2)+NIAU2(ITRI1)*NIBU1(ITRI2)
      IFR21L=NRAL2(ITRI1)*NRBL1(ITRI2)+NIAL2(ITRI1)*NIBL1(ITRI2)
      IFI21U=NRAU2(ITRI1)*NIBU1(ITRI2)-NIAU2(ITRI1)*NRBU1(ITRI2)
      IFI21L=NRAL2(ITRI1)*NIBL1(ITRI2)-NIAL2(ITRI1)*NRBL1(ITRI2)
C
C     ACCUMULATE 3-D CARTESIAN HERMITE INDICES
C
      DO 200 ITAU=0,I1+I2
      IF(IEXYZ(ISTRT+ITAU).EQ.0) GO TO 199
      DO 210 IMU=0,J1+J2
      IF(IEXYZ(JSTRT+IMU).EQ.0)  GO TO 209
      DO 220 INU=0,K1+K2
      IF(IEXYZ(KSTRT+INU).EQ.0)  GO TO 219
C
      ITUV=INABCD(ITAU,IMU,INU)
C
      NPOWER=IEEXP(ISTRT+ITAU)+IEEXP(JSTRT+IMU)+IEEXP(KSTRT+INU)
C
      IECT=IEXYZ(ISTRT+ITAU)*IEXYZ(JSTRT+IMU)*IEXYZ(KSTRT+INU)
      NAR11U(ITUV,NPOWER)=NAR11U(ITUV,NPOWER)+IFR11U*IECT
      NAR11L(ITUV,NPOWER)=NAR11L(ITUV,NPOWER)+IFR11L*IECT
      NAI11U(ITUV,NPOWER)=NAI11U(ITUV,NPOWER)+IFI11U*IECT
      NAI11L(ITUV,NPOWER)=NAI11L(ITUV,NPOWER)+IFI11L*IECT
      NAR21U(ITUV,NPOWER)=NAR21U(ITUV,NPOWER)+IFR21U*IECT
      NAR21L(ITUV,NPOWER)=NAR21L(ITUV,NPOWER)+IFR21L*IECT
      NAI21U(ITUV,NPOWER)=NAI21U(ITUV,NPOWER)+IFI21U*IECT
      NAI21L(ITUV,NPOWER)=NAI21L(ITUV,NPOWER)+IFI21L*IECT
C 
219   CONTINUE
220   CONTINUE
C
209   CONTINUE
210   CONTINUE
C
199   CONTINUE
200   CONTINUE
C
98    CONTINUE
      ENDIF
101   CONTINUE
99    CONTINUE
      ENDIF
100   CONTINUE
C
C-----------------------------------------------------------------------C
C
      RA1U=SQPINV*DFLOAT(NA1UN)*DSQRT(DFLOAT(NA1UR))/DFLOAT(NA1UD)
      RA1L=SQPINV*DFLOAT(NA1LN)*DSQRT(DFLOAT(NA1LR))/DFLOAT(NA1LD)
C
      RA2U=SQPINV*DFLOAT(NA2UN)*DSQRT(DFLOAT(NA2UR))/DFLOAT(NA2UD)
      RA2L=SQPINV*DFLOAT(NA2LN)*DSQRT(DFLOAT(NA2LR))/DFLOAT(NA2LD)
C
      RB1U=SQPINV*DFLOAT(NB1UN)*DSQRT(DFLOAT(NB1UR))/DFLOAT(NB1UD)
      RB1L=SQPINV*DFLOAT(NB1LN)*DSQRT(DFLOAT(NB1LR))/DFLOAT(NB1LD)
C
      CG11U=RA1U*RB1U
      CG11L=RA1L*RB1L
      CG21U=RA2U*RB1U
      CG21L=RA2L*RB1L
C
      MAXPOW=LA+LB
C
      DO 650 ITUV=1,ITUVMX
      DO 660 M=1,MAXM
      AR11=(CG11U*DFLOAT(NAR11U(ITUV,MAXPOW))+
     &      CG11L*DFLOAT(NAR11L(ITUV,MAXPOW)))
      AI11=(CG11U*DFLOAT(NAI11U(ITUV,MAXPOW))+
     &      CG11L*DFLOAT(NAI11L(ITUV,MAXPOW)))
      AR21=(CG21U*DFLOAT(NAR21U(ITUV,MAXPOW))+
     &      CG21L*DFLOAT(NAR21L(ITUV,MAXPOW)))
      AI21=(CG21U*DFLOAT(NAI21U(ITUV,MAXPOW))+
     &      CG21L*DFLOAT(NAI21L(ITUV,MAXPOW)))
      P2M=P2(M)
      DO 670 IPOWER=MAXPOW,1,-1
      AR11=AR11*P2M+(CG11U*DFLOAT(NAR11U(ITUV,IPOWER-1))+
     &               CG11L*DFLOAT(NAR11L(ITUV,IPOWER-1)))
      AI11=AI11*P2M+(CG11U*DFLOAT(NAI11U(ITUV,IPOWER-1))+
     &               CG11L*DFLOAT(NAI11L(ITUV,IPOWER-1)))
      AR21=AR21*P2M+(CG21U*DFLOAT(NAR21U(ITUV,IPOWER-1))+
     &               CG21L*DFLOAT(NAR21L(ITUV,IPOWER-1)))
      AI21=AI21*P2M+(CG21U*DFLOAT(NAI21U(ITUV,IPOWER-1))+
     &               CG21L*DFLOAT(NAI21L(ITUV,IPOWER-1)))
670   CONTINUE
      ERLL11(M,ITUV)=AR11
      EILL11(M,ITUV)=AI11
      ERLL21(M,ITUV)=AR21
      EILL21(M,ITUV)=AI21
660   CONTINUE
650   CONTINUE
C-----------------------------------------------------------------------C
C
      ITUV=0
      DO 400 MTUV=0,LAMAB
      RPH=DFLOAT((IALT)**(MTUV))
      DO 402 MDUM=1,((MTUV+1)*(MTUV+2))/2
      ITUV=ITUV+1
      DO 401 M=1,MAXM
      ERLL11(M,ITUV)=ERLL11(M,ITUV)*RKAB(M)*RNLL(M)*RPH
      EILL11(M,ITUV)=EILL11(M,ITUV)*RKAB(M)*RNLL(M)*RPH
      ERLL21(M,ITUV)=ERLL21(M,ITUV)*RKAB(M)*RNLL(M)*RPH
      EILL21(M,ITUV)=EILL21(M,ITUV)*RKAB(M)*RNLL(M)*RPH
401   CONTINUE
402   CONTINUE
400   CONTINUE
C
C
      RETURN
      END
C
C
C
      SUBROUTINE ILMAKE(NCRU,NCIU,NCRL,NCIL,KAPPA,MQN2,
     &            NUNUM,NUROOT,NUDEN,NLNUM,NLROOT,NLDEN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(PI=3.1415926535898D0)
C
      DIMENSION NCRU(84),NCIU(84),NCRL(84),NCIL(84)
      COMMON/CONSTS/NCR(84,25),NCI(84,25),MCR(84,25),MCI(84,25),
     &              NRNUM(25),NRROOT(25),NRDEN(25)
C
      COMMON/CGCSPH/NA1UNU,NA1UDE,NA2UNU,NA2UDE,NB1UNU,NB1UDE,
     &              NA1URT,NA1LRT,NA2URT,NA2LRT,NB1URT,NB1LRT
C
      SQPINV=DSQRT(1.0D0/PI)
C 
C     CONSTRUCT LARGE COMPONENT SPINORS
c      write(6,*) 'ILMAKE'
C
      IF(KAPPA.GT.0) THEN
      LQN=KAPPA
      JQN2=2*KAPPA-1
      ICGUN=JQN2+2-MQN2
      ICGUD=2*(JQN2+2)
      ICGLN=JQN2+2+MQN2
      ICGLD=2*(JQN2+2)
      ISGN=-1
      ELSE
      LQN=-KAPPA-1
      JQN2=-2*KAPPA-1
      ICGUN=JQN2+MQN2
      ICGUD=2*JQN2
      ICGLN=JQN2-MQN2
      ICGLD=2*JQN2
      ISGN=1
      ENDIF
C
      MLQNU=(MQN2-1)/2
      MLQNL=(MQN2+1)/2
C
      IADRU=LQN*(LQN+1)+MLQNU+1
      IADRL=LQN*(LQN+1)+MLQNL+1
C
C
      ILEN=((LQN+1)*(LQN+2)*(LQN+3))/6
C
      IF(IABS(MLQNU).LE.LQN) THEN
      DO 10 I=1,ILEN
      NCRU(I)=NCR(I,IADRU)
      NCIU(I)=NCI(I,IADRU)
10    CONTINUE
      ELSE
      DO 11 I=1,ILEN
      NCRU(I)=0
      NCIU(I)=0
11    CONTINUE
      ENDIF
C
      IF(IABS(MLQNL).LE.LQN) THEN
      DO 20 I=1,ILEN
      NCRL(I)=NCR(I,IADRL)
      NCIL(I)=NCI(I,IADRL)
20    CONTINUE
      ELSE
      DO 21 I=1,ILEN
      NCRL(I)=0
      NCIL(I)=0
21    CONTINUE
      ENDIF
C
C
      IF(IABS(MLQNU).LE.LQN) THEN
      NUNUM=ISGN*NRNUM(IADRU)
      NUROOT=ICGUN*ICGUD*NRROOT(IADRU)
      NUDEN=ICGUD*NRDEN(IADRU)
      ELSE
      NUNUM=0
      NUROOT=0
      NUDEN=1
      ENDIF
C
      IF(IABS(MLQNL).LE.LQN) THEN
      NLNUM=NRNUM(IADRL)
      NLROOT=ICGLN*ICGLD*NRROOT(IADRL)
      NLDEN=ICGLD*NRDEN(IADRL)
      ELSE
      NLNUM=0
      NLROOT=0
      NLDEN=1
      ENDIF
C
      RETURN
      END

      SUBROUTINE OEMAKESS(ERSS11,EISS11,ERSS21,EISS21,
     #                   KAPPA,MQN,NFUNS,IALT,IND1,IND2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************C
C     EMAKELL GENERATES BLOCKS OF SPHERICAL SPINOR E-COEFFICIENTS       C
C     BY CONTRACTING ON THEIR CARTESIAN TENSOR COMPONENTS               C
C     (CURRENTLY LIMITED TO LMAX=4)                                     C
C***********************************************************************C
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXDIM=300,NKAPM=7,
     &  NCENTM=6,IL4=24,MAXMV=5,MAXMV2=2*MAXMV)
      PARAMETER(MLL=165,MAXIJ=125,MAXLQN=4)
C      PARAMETER(MABLL=((MAXLQN+1)*(MAXLQN+2)*(MAXLQN+3))/6)
      PARAMETER(MABLL=MLL)
      PARAMETER(SQPINV=0.5641895835477563D0)
C
      COMMON/RNORM/RNLL(MAXB2),RNSL(MAXB2),RNLS(MAXB2),RNSS(MAXB2),
     #EXPA(MAXB2),EXPB(MAXB2),EXPAB(MAXB2)
      COMMON/INDSYS/INABCD(0:4*MAXLQN,0:4*MAXLQN,0:4*MAXLQN),
     &              IVEC(MABLL),JVEC(MABLL),KVEC(MABLL)
C
      COMMON/LOCGEO/XYZ(3,4)
      COMMON/TIMEE/ETIME
C
      COMMON/SPEC/COORD(3,NCENTM),ZNUC(NCENTM),CNUC(NCENTM),
     &  EXPSET(MAXB,NKAPM,NCENTM),IZNUC(NCENTM),ICRGE(NCENTM),
     &  KVALS(NKAPM,NCENTM),NKAPPA(NCENTM),LMAXX(NCENTM),
     &  NFUNCT(NKAPM,NCENTM),NCENT,NDIM,NSHIFT,
     &  NOCC,ITER,IALL,IRUN
      DIMENSION     ERSS11(MAXB2,0:MLL),EISS11(MAXB2,0:MLL),
     #              ERSS21(MAXB2,0:MLL),EISS21(MAXB2,0:MLL)
      DIMENSION NRAU1(84),NIAU1(84),NRAU2(84),NIAU2(84),
     &          NRBU1(84),NIBU1(84),
     &          NRAL1(84),NIAL1(84),NRAL2(84),NIAL2(84),
     &          NRBL1(84),NIBL1(84)
      DIMENSION NRAU1S(84),NIAU1S(84),NRAU2S(84),NIAU2S(84),
     &          NRBU1S(84),NIBU1S(84),
     &          NRAL1S(84),NIAL1S(84),NRAL2S(84),NIAL2S(84),
     &          NRBL1S(84),NIBL1S(84)
      DIMENSION IEXYZ(MAXIJ),IEEXP(MAXIJ),
     &          P2(MAXB2),RKAB(MAXB2)
      DIMENSION IED(5,5) 
      DIMENSION KAPPA(4),MQN(4),NFUNS(4)
C
      DIMENSION NAR11U(MABLL,0:2*MAXLQN),NAR11L(MABLL,0:2*MAXLQN),
     &          NAI11U(MABLL,0:2*MAXLQN),NAI11L(MABLL,0:2*MAXLQN),
     &          NAR21U(MABLL,0:2*MAXLQN),NAR21L(MABLL,0:2*MAXLQN),
     &          NAI21U(MABLL,0:2*MAXLQN),NAI21L(MABLL,0:2*MAXLQN) 
C
      DIMENSION NBR11U(MABLL,0:2*MAXLQN),NBR11L(MABLL,0:2*MAXLQN),
     &          NBI11U(MABLL,0:2*MAXLQN),NBI11L(MABLL,0:2*MAXLQN),
     &          NBR21U(MABLL,0:2*MAXLQN),NBR21L(MABLL,0:2*MAXLQN),
     &          NBI21U(MABLL,0:2*MAXLQN),NBI21L(MABLL,0:2*MAXLQN)
C 
      DIMENSION NCR11U(MABLL,0:2*MAXLQN),NCR11L(MABLL,0:2*MAXLQN),
     &          NCI11U(MABLL,0:2*MAXLQN),NCI11L(MABLL,0:2*MAXLQN),
     &          NCR21U(MABLL,0:2*MAXLQN),NCR21L(MABLL,0:2*MAXLQN),
     &          NCI21U(MABLL,0:2*MAXLQN),NCI21L(MABLL,0:2*MAXLQN) 
C
      DIMENSION NDR11U(MABLL,0:2*MAXLQN),NDR11L(MABLL,0:2*MAXLQN),
     &          NDI11U(MABLL,0:2*MAXLQN),NDI11L(MABLL,0:2*MAXLQN),
     &          NDR21U(MABLL,0:2*MAXLQN),NDR21L(MABLL,0:2*MAXLQN),
     &          NDI21U(MABLL,0:2*MAXLQN),NDI21L(MABLL,0:2*MAXLQN) 
C
      DATA IED/1,16,36,61,91,
     &         2,18,39,65,96,
     &         4,21,43,70,102,
     &         7,25,48,76,109,
     &        11,30,54,83,117/
C
      DATA IEXYZ/1,0,1,1,0,1,0,3,0,1,3,0,6,0,1,0,1,1,0,1,
     &         0,3,0,1,3,0,6,0,1,0,15,0,10,0,1,1,0,1,0,3,
     &      0,1,3,0,6,0,1,0,15,0,10,0,1,15,0,45,0,15,0,1,
     &      0,3,0,1,3,0,6,0,1,0,15,0,10,0,1,15,0,45,0,15,
     &    0,1,0,105,0,105,0,21,0,1,3,0,6,0,1,0,15,0,10,0,
     & 1,15,0,45,0,15,0,1,0,105,0,105,0,21,0,1,105,0,420,0,
     &          210,0,28,0,1/
C
      DATA IEEXP/0,0,1,1,0,2,0,2,0,3,2,0,3,0,4,0,1,1,0,2,
     &          0,2,0,3,2,0,3,0,4,0,3,0,4,0,5,1,0,2,0,2,
     &          0,3,2,0,3,0,4,0,3,0,4,0,5,3,0,4,0,5,0,6,
     &          0,2,0,3,2,0,3,0,4,0,3,0,4,0,5,3,0,4,0,5,
     &          0,6,0,4,0,5,0,6,0,7,2,0,3,0,4,0,3,0,4,0,
     &          5,3,0,4,0,5,0,6,0,4,0,5,0,6,0,7,4,0,5,0,
     &          6,0,7,0,8/
C
C
      KAPPAA=KAPPA(IND1)
      KAPPAB=KAPPA(IND2)
      MJA2=MQN(IND1)
      MJB2=MQN(IND2)
      MAXM=NFUNS(IND1)*NFUNS(IND2)
C
C
C     SET GEOMETRIC PARAMETERS
C    
      NFUNA=NFUNS(IND1)
      NFUNB=NFUNS(IND2)
      IF(KAPPAA.GT.0) THEN
      LA=KAPPAA-1
      NA=1
      AFAC=DFLOAT(2*KAPPAA+1)
      ELSE
      LA=-KAPPAA
      NA=0
      AFAC=0.0D0
      ENDIF
      IF(KAPPAB.GT.0) THEN
      LB=KAPPAB-1
      NB=1
      BFAC=DFLOAT(2*KAPPAB+1)
      ELSE
      LB=-KAPPAB
      NB=0
      BFAC=0.0D0
      ENDIF
C
      NA2=2*NA
      NB2=2*NB
      LEFFA=LA+NA2
      LEFFB=LB+NB2
      LEFFAB=LA+LB+(NA2+NB2)
C
      CXA=XYZ(1,IND1)
      CXB=XYZ(1,IND2)
      CYA=XYZ(2,IND1)
      CYB=XYZ(2,IND2)
      CZA=XYZ(3,IND1)
      CZB=XYZ(3,IND2)
      DXAB=CXB-CXA
      DYAB=CYB-CYA
      DZAB=CZB-CZA
      RSQ=(DXAB*DXAB)+(DYAB*DYAB)+(DZAB*DZAB)
C
      IF(RSQ.EQ.0.0D0) THEN
      ICNT=1
      ELSE
      ICNT=2
      ENDIF
C
      DO 10 M=1,MAXM
      P=EXPA(M)+EXPB(M)
      P2(M)=0.5D0/P
      RKAB(M)=DEXP(-EXPA(M)*EXPB(M)*RSQ/P)
10    CONTINUE
C 
C
C
      LABMAX=LEFFAB
      LAMAB=LEFFAB
C
      ITUVMX=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      DO 300 ITUV=1,ITUVMX
      DO 300 M=1,MAXM
      ERSS11(M,ITUV)=0.0D0
      EISS11(M,ITUV)=0.0D0
      ERSS21(M,ITUV)=0.0D0
      EISS21(M,ITUV)=0.0D0
300   CONTINUE 
C
      DO 255 ITUV=1,ITUVMX
      DO 255 IP=0,8
      NAR11U(ITUV,IP)=0
      NAR11L(ITUV,IP)=0
      NAI11U(ITUV,IP)=0
      NAI11L(ITUV,IP)=0
      NAR21U(ITUV,IP)=0
      NAR21L(ITUV,IP)=0
      NAI21U(ITUV,IP)=0
      NAI21L(ITUV,IP)=0
255   CONTINUE
C
      IF(BFAC.NE.0.0D0) THEN
      DO 355 ITUV=1,ITUVMX
      DO 355 IP=0,8
      NBR11U(ITUV,IP)=0
      NBR11L(ITUV,IP)=0
      NBI11U(ITUV,IP)=0
      NBI11L(ITUV,IP)=0
      NBR21U(ITUV,IP)=0
      NBR21L(ITUV,IP)=0
      NBI21U(ITUV,IP)=0
      NBI21L(ITUV,IP)=0
355   CONTINUE
      ENDIF
C
      IF(AFAC.NE.0.0D0) THEN
      DO 455 ITUV=1,ITUVMX
      DO 455 IP=0,8
      NCR11U(ITUV,IP)=0
      NCR11L(ITUV,IP)=0
      NCI11U(ITUV,IP)=0
      NCI11L(ITUV,IP)=0
      NCR21U(ITUV,IP)=0
      NCR21L(ITUV,IP)=0
      NCI21U(ITUV,IP)=0
      NCI21L(ITUV,IP)=0
455   CONTINUE
      ENDIF
C
      IF(AFAC*BFAC.NE.0.0D0) THEN
      DO 555 ITUV=1,ITUVMX
      DO 555 IP=0,8
      NDR11U(ITUV,IP)=0
      NDR11L(ITUV,IP)=0
      NDI11U(ITUV,IP)=0
      NDI11L(ITUV,IP)=0
      NDR21U(ITUV,IP)=0
      NDR21L(ITUV,IP)=0
      NDI21U(ITUV,IP)=0
      NDI21L(ITUV,IP)=0
555   CONTINUE
      ENDIF
C
C*****************************************************************C
C     TRANSFORM FROM CARTESIAN SCALARS TO SPHERICAL SPINORS       C
C*****************************************************************C
C
      IF(KAPPAA.LT.0) THEN
C     
      NQN=0
c      write(6,*) 'a'
c      write(6,*) KAPPAA,-MJA2,NQN 
      CALL ISMAKE(NRAU1S,NIAU1S,NRAL1S,NIAL1S,
     &            KAPPAA,-MJA2,NQN,
     &            NA1UNS,NA1URS,NA1UDS,NA1LNS,NA1LRS,NA1LDS)
c      write(6,*) 'b'
      CALL ISMAKE(NRAU2S,NIAU2S,NRAL2S,NIAL2S,
     &            KAPPAA,MJA2,NQN,
     &            NA2UNS,NA2URS,NA2UDS,NA2LNS,NA2LRS,NA2LDS)
C
      ELSE
C
      NQN=0
      CALL ISMAKE(NRAU1,NIAU1,NRAL1,NIAL1,
     &            KAPPAA,-MJA2,NQN,
     &            NA1UN,NA1UR,NA1UD,NA1LN,NA1LR,NA1LD)
      CALL ISMAKE(NRAU2,NIAU2,NRAL2,NIAL2,
     &            KAPPAA,MJA2,NQN,
     &            NA2UN,NA2UR,NA2UD,NA2LN,NA2LR,NA2LD)
      NQN=1
      CALL ISMAKE(NRAU1S,NIAU1S,NRAL1S,NIAL1S,
     &            KAPPAA,-MJA2,NQN,
     &            NA1UNS,NA1URS,NA1UDS,NA1LNS,NA1LRS,NA1LDS)
      CALL ISMAKE(NRAU2S,NIAU2S,NRAL2S,NIAL2S,
     &            KAPPAA,MJA2,NQN,
     &            NA2UNS,NA2URS,NA2UDS,NA2LNS,NA2LRS,NA2LDS)
C
      ENDIF
C
      IF(KAPPAB.LT.0) THEN
C
      NQN=0
      CALL ISMAKE(NRBU1S,NIBU1S,NRBL1S,NIBL1S,
     &            KAPPAB,-MJB2,NQN,
     &            NB1UNS,NB1URS,NB1UDS,NB1LNS,NB1LRS,NB1LDS)
C
      ELSE
C
      NQN=0
      CALL ISMAKE(NRBU1,NIBU1,NRBL1,NIBL1,
     &            KAPPAB,-MJB2,NQN,
     &            NB1UN,NB1UR,NB1UD,NB1LN,NB1LR,NB1LD)
C
      NQN=1
      CALL ISMAKE(NRBU1S,NIBU1S,NRBL1S,NIBL1S,
     &            KAPPAB,-MJB2,NQN,
     &            NB1UNS,NB1URS,NB1UDS,NB1LNS,NB1LRS,NB1LDS)
C
      ENDIF
C
      LAMAB=LEFFAB
      NTRI1=((LEFFA+1)*(LEFFA+2)*(LEFFA+3))/6
      NTRI2=((LEFFB+1)*(LEFFB+2)*(LEFFB+3))/6
      NTUV=((LAMAB+1)*(LAMAB+2)*(LAMAB+3))/6
      DO 100 ITRI1S=1,NTRI1
      I1=IVEC(ITRI1S)
      J1=JVEC(ITRI1S)
      K1=KVEC(ITRI1S)
C
      ITRI1=INABCD(I1,J1,K1)
C
      DO 101 ITRI2S=1,NTRI2
      I2=IVEC(ITRI2S)
      J2=JVEC(ITRI2S)
      K2=KVEC(ITRI2S)
C
      ITRI2=INABCD(I2,J2,K2)
C
      ISTRT=IED(I1+1,I2+1)
      JSTRT=IED(J1+1,J2+1)
      KSTRT=IED(K1+1,K2+1)
C
C
C     CONSTRUCT SCALAR PRODUCT OF EXPANSION COEFFICIENTS (COMPONENTWISE)
C
C-----------------------------------------------------------------------C
C
      IF((I1+J1+K1).EQ.LEFFA) THEN
      IF((I2+J2+K2).EQ.LEFFB) THEN
C
      IFR11U=NRAU1S(ITRI1S)*NRBU1S(ITRI2S)+NIAU1S(ITRI1S)*NIBU1S(ITRI2S)
      IFR11L=NRAL1S(ITRI1S)*NRBL1S(ITRI2S)+NIAL1S(ITRI1S)*NIBL1S(ITRI2S)
      IFI11U=NRAU1S(ITRI1S)*NIBU1S(ITRI2S)-NIAU1S(ITRI1S)*NRBU1S(ITRI2S)
      IFI11L=NRAL1S(ITRI1S)*NIBL1S(ITRI2S)-NIAL1S(ITRI1S)*NRBL1S(ITRI2S)
      IFR21U=NRAU2S(ITRI1S)*NRBU1S(ITRI2S)+NIAU2S(ITRI1S)*NIBU1S(ITRI2S)
      IFR21L=NRAL2S(ITRI1S)*NRBL1S(ITRI2S)+NIAL2S(ITRI1S)*NIBL1S(ITRI2S)
      IFI21U=NRAU2S(ITRI1S)*NIBU1S(ITRI2S)-NIAU2S(ITRI1S)*NRBU1S(ITRI2S)
      IFI21L=NRAL2S(ITRI1S)*NIBL1S(ITRI2S)-NIAL2S(ITRI1S)*NRBL1S(ITRI2S)
C
C     ACCUMULATE 3-D CARTESIAN HERMITE INDICES
C
      DO 230 ITAU=0,I1+I2
      IF(IEXYZ(ISTRT+ITAU).EQ.0) GO TO 229
      DO 240 IMU=0,J1+J2
      IF(IEXYZ(JSTRT+IMU).EQ.0)  GO TO 239
      DO 250 INU=0,K1+K2
      IF(IEXYZ(KSTRT+INU).EQ.0) GO TO 249
C
      ITUV=INABCD(ITAU,IMU,INU)
C
      NPOWER=IEEXP(ISTRT+ITAU)+IEEXP(JSTRT+IMU)+IEEXP(KSTRT+INU)
C
      IECT=IEXYZ(ISTRT+ITAU)*IEXYZ(JSTRT+IMU)*IEXYZ(KSTRT+INU)
      NAR11U(ITUV,NPOWER)=NAR11U(ITUV,NPOWER)+IFR11U*IECT
      NAR11L(ITUV,NPOWER)=NAR11L(ITUV,NPOWER)+IFR11L*IECT
      NAI11U(ITUV,NPOWER)=NAI11U(ITUV,NPOWER)+IFI11U*IECT
      NAI11L(ITUV,NPOWER)=NAI11L(ITUV,NPOWER)+IFI11L*IECT
      NAR21U(ITUV,NPOWER)=NAR21U(ITUV,NPOWER)+IFR21U*IECT
      NAR21L(ITUV,NPOWER)=NAR21L(ITUV,NPOWER)+IFR21L*IECT
      NAI21U(ITUV,NPOWER)=NAI21U(ITUV,NPOWER)+IFI21U*IECT
      NAI21L(ITUV,NPOWER)=NAI21L(ITUV,NPOWER)+IFI21L*IECT
C
249   CONTINUE
250   CONTINUE
C 
239   CONTINUE
240   CONTINUE
C 
229   CONTINUE
230   CONTINUE
C
C
      ENDIF
      ENDIF
C
C-----------------------------------------------------------------------C
C
      IF(BFAC.NE.0.0D0) THEN
      IF((I1+J1+K1).EQ.LEFFA) THEN
      IF((I2+J2+K2).EQ.LB) THEN
C
      IFR11U=NRAU1S(ITRI1S)*NRBU1(ITRI2)+NIAU1S(ITRI1S)*NIBU1(ITRI2)
      IFR11L=NRAL1S(ITRI1S)*NRBL1(ITRI2)+NIAL1S(ITRI1S)*NIBL1(ITRI2)
      IFI11U=NRAU1S(ITRI1S)*NIBU1(ITRI2)-NIAU1S(ITRI1S)*NRBU1(ITRI2)
      IFI11L=NRAL1S(ITRI1S)*NIBL1(ITRI2)-NIAL1S(ITRI1S)*NRBL1(ITRI2)
      IFR21U=NRAU2S(ITRI1S)*NRBU1(ITRI2)+NIAU2S(ITRI1S)*NIBU1(ITRI2)
      IFR21L=NRAL2S(ITRI1S)*NRBL1(ITRI2)+NIAL2S(ITRI1S)*NIBL1(ITRI2)
      IFI21U=NRAU2S(ITRI1S)*NIBU1(ITRI2)-NIAU2S(ITRI1S)*NRBU1(ITRI2)
      IFI21L=NRAL2S(ITRI1S)*NIBL1(ITRI2)-NIAL2S(ITRI1S)*NRBL1(ITRI2)
C
C     ACCUMULATE 3-D CARTESIAN HERMITE INDICES
C
      DO 330 ITAU=0,I1+I2
      IF(IEXYZ(ISTRT+ITAU).EQ.0) GO TO 329
      DO 340 IMU=0,J1+J2
      IF(IEXYZ(JSTRT+IMU).EQ.0)  GO TO 339
      DO 350 INU=0,K1+K2
      IF(IEXYZ(KSTRT+INU).EQ.0) GO TO 349 
C
      ITUV=INABCD(ITAU,IMU,INU)
C
      NPOWER=IEEXP(ISTRT+ITAU)+IEEXP(JSTRT+IMU)+IEEXP(KSTRT+INU)
C
      IECT=IEXYZ(ISTRT+ITAU)*IEXYZ(JSTRT+IMU)*IEXYZ(KSTRT+INU)
      NBR11U(ITUV,NPOWER)=NBR11U(ITUV,NPOWER)+IFR11U*IECT
      NBR11L(ITUV,NPOWER)=NBR11L(ITUV,NPOWER)+IFR11L*IECT
      NBI11U(ITUV,NPOWER)=NBI11U(ITUV,NPOWER)+IFI11U*IECT
      NBI11L(ITUV,NPOWER)=NBI11L(ITUV,NPOWER)+IFI11L*IECT
      NBR21U(ITUV,NPOWER)=NBR21U(ITUV,NPOWER)+IFR21U*IECT
      NBR21L(ITUV,NPOWER)=NBR21L(ITUV,NPOWER)+IFR21L*IECT
      NBI21U(ITUV,NPOWER)=NBI21U(ITUV,NPOWER)+IFI21U*IECT
      NBI21L(ITUV,NPOWER)=NBI21L(ITUV,NPOWER)+IFI21L*IECT
C
349   CONTINUE
350   CONTINUE
C
339   CONTINUE
340   CONTINUE
C
329   CONTINUE
330   CONTINUE
C
      ENDIF
      ENDIF
      ENDIF
C
C
C-----------------------------------------------------------------------C
C
      IF(AFAC.NE.0.0D0) THEN
      IF((I1+J1+K1).EQ.LA) THEN
      IF((I2+J2+K2).EQ.LEFFB) THEN
C
      IFR11U=NRAU1(ITRI1)*NRBU1S(ITRI2S)+NIAU1(ITRI1)*NIBU1S(ITRI2S)
      IFR11L=NRAL1(ITRI1)*NRBL1S(ITRI2S)+NIAL1(ITRI1)*NIBL1S(ITRI2S)
      IFI11U=NRAU1(ITRI1)*NIBU1S(ITRI2S)-NIAU1(ITRI1)*NRBU1S(ITRI2S)
      IFI11L=NRAL1(ITRI1)*NIBL1S(ITRI2S)-NIAL1(ITRI1)*NRBL1S(ITRI2S)
      IFR21U=NRAU2(ITRI1)*NRBU1S(ITRI2S)+NIAU2(ITRI1)*NIBU1S(ITRI2S)
      IFR21L=NRAL2(ITRI1)*NRBL1S(ITRI2S)+NIAL2(ITRI1)*NIBL1S(ITRI2S)
      IFI21U=NRAU2(ITRI1)*NIBU1S(ITRI2S)-NIAU2(ITRI1)*NRBU1S(ITRI2S)
      IFI21L=NRAL2(ITRI1)*NIBL1S(ITRI2S)-NIAL2(ITRI1)*NRBL1S(ITRI2S)
C
C     ACCUMULATE 3-D CARTESIAN HERMITE INDICES
C
      DO 430 ITAU=0,I1+I2
      IF(IEXYZ(ISTRT+ITAU).EQ.0) GO TO 429
      DO 440 IMU=0,J1+J2
      IF(IEXYZ(JSTRT+IMU).EQ.0)  GO TO 439
      DO 450 INU=0,K1+K2
      IF(IEXYZ(KSTRT+INU).EQ.0) GO TO 449 
C
      ITUV=INABCD(ITAU,IMU,INU)
C
      NPOWER=IEEXP(ISTRT+ITAU)+IEEXP(JSTRT+IMU)+IEEXP(KSTRT+INU)     
C
      IECT=IEXYZ(ISTRT+ITAU)*IEXYZ(JSTRT+IMU)*IEXYZ(KSTRT+INU)
      NCR11U(ITUV,NPOWER)=NCR11U(ITUV,NPOWER)+IFR11U*IECT
      NCR11L(ITUV,NPOWER)=NCR11L(ITUV,NPOWER)+IFR11L*IECT
      NCI11U(ITUV,NPOWER)=NCI11U(ITUV,NPOWER)+IFI11U*IECT
      NCI11L(ITUV,NPOWER)=NCI11L(ITUV,NPOWER)+IFI11L*IECT
      NCR21U(ITUV,NPOWER)=NCR21U(ITUV,NPOWER)+IFR21U*IECT
      NCR21L(ITUV,NPOWER)=NCR21L(ITUV,NPOWER)+IFR21L*IECT
      NCI21U(ITUV,NPOWER)=NCI21U(ITUV,NPOWER)+IFI21U*IECT
      NCI21L(ITUV,NPOWER)=NCI21L(ITUV,NPOWER)+IFI21L*IECT
C
449   CONTINUE
450   CONTINUE
C
439   CONTINUE
440   CONTINUE
C
429   CONTINUE
430   CONTINUE
C
      ENDIF
      ENDIF
      ENDIF
C
C-----------------------------------------------------------------------C
C
      IF(AFAC*BFAC.NE.0.0D0) THEN
      IF((I1+J1+K1).EQ.LA) THEN
      IF((I2+J2+K2).EQ.LB) THEN
C
      IFR11U=NRAU1(ITRI1)*NRBU1(ITRI2)+NIAU1(ITRI1)*NIBU1(ITRI2)
      IFR11L=NRAL1(ITRI1)*NRBL1(ITRI2)+NIAL1(ITRI1)*NIBL1(ITRI2)
      IFI11U=NRAU1(ITRI1)*NIBU1(ITRI2)-NIAU1(ITRI1)*NRBU1(ITRI2)
      IFI11L=NRAL1(ITRI1)*NIBL1(ITRI2)-NIAL1(ITRI1)*NRBL1(ITRI2)
      IFR21U=NRAU2(ITRI1)*NRBU1(ITRI2)+NIAU2(ITRI1)*NIBU1(ITRI2)
      IFR21L=NRAL2(ITRI1)*NRBL1(ITRI2)+NIAL2(ITRI1)*NIBL1(ITRI2)
      IFI21U=NRAU2(ITRI1)*NIBU1(ITRI2)-NIAU2(ITRI1)*NRBU1(ITRI2)
      IFI21L=NRAL2(ITRI1)*NIBL1(ITRI2)-NIAL2(ITRI1)*NRBL1(ITRI2)
C
C     ACCUMULATE 3-D CARTESIAN HERMITE INDICES
C
      DO 530 ITAU=0,I1+I2
      IF(IEXYZ(ISTRT+ITAU).EQ.0) GO TO 529
      DO 540 IMU=0,J1+J2
      IF(IEXYZ(JSTRT+IMU).EQ.0)  GO TO 539 
      DO 550 INU=0,K1+K2
      IF(IEXYZ(KSTRT+INU).EQ.0) GO TO 549 
C
      ITUV=INABCD(ITAU,IMU,INU)
C
      NPOWER=IEEXP(ISTRT+ITAU)+IEEXP(JSTRT+IMU)+IEEXP(KSTRT+INU)
C
      IECT=IEXYZ(ISTRT+ITAU)*IEXYZ(JSTRT+IMU)*IEXYZ(KSTRT+INU)
      NDR11U(ITUV,NPOWER)=NDR11U(ITUV,NPOWER)+IFR11U*IECT
      NDR11L(ITUV,NPOWER)=NDR11L(ITUV,NPOWER)+IFR11L*IECT
      NDI11U(ITUV,NPOWER)=NDI11U(ITUV,NPOWER)+IFI11U*IECT
      NDI11L(ITUV,NPOWER)=NDI11L(ITUV,NPOWER)+IFI11L*IECT
      NDR21U(ITUV,NPOWER)=NDR21U(ITUV,NPOWER)+IFR21U*IECT
      NDR21L(ITUV,NPOWER)=NDR21L(ITUV,NPOWER)+IFR21L*IECT
      NDI21U(ITUV,NPOWER)=NDI21U(ITUV,NPOWER)+IFI21U*IECT
      NDI21L(ITUV,NPOWER)=NDI21L(ITUV,NPOWER)+IFI21L*IECT
C
549   CONTINUE
550   CONTINUE
C
539   CONTINUE
540   CONTINUE
C
529   CONTINUE
530   CONTINUE
C
      ENDIF
      ENDIF
      ENDIF
C
C-----------------------------------------------------------------------C
98    CONTINUE
101   CONTINUE
99    CONTINUE
100   CONTINUE
C
C**********************************************************************C
C    
C     FIRST SUM IN P2(M) IN HORNER FORM
C
      RA1US=SQPINV*DFLOAT(NA1UNS)*DSQRT(DFLOAT(NA1URS))/DFLOAT(NA1UDS)
      RA1LS=SQPINV*DFLOAT(NA1LNS)*DSQRT(DFLOAT(NA1LRS))/DFLOAT(NA1LDS)
C
      RA2US=SQPINV*DFLOAT(NA2UNS)*DSQRT(DFLOAT(NA2URS))/DFLOAT(NA2UDS)
      RA2LS=SQPINV*DFLOAT(NA2LNS)*DSQRT(DFLOAT(NA2LRS))/DFLOAT(NA2LDS)
C
      RB1US=SQPINV*DFLOAT(NB1UNS)*DSQRT(DFLOAT(NB1URS))/DFLOAT(NB1UDS)
      RB1LS=SQPINV*DFLOAT(NB1LNS)*DSQRT(DFLOAT(NB1LRS))/DFLOAT(NB1LDS)
C
      CG11U=RA1US*RB1US
      CG11L=RA1LS*RB1LS
      CG21U=RA2US*RB1US
      CG21L=RA2LS*RB1LS
C
      MAXPOW=LEFFA+LEFFB
C
      DO 650 ITUV=1,ITUVMX
      DO 660 M=1,MAXM
      AR11=(CG11U*DFLOAT(NAR11U(ITUV,MAXPOW))+
     &      CG11L*DFLOAT(NAR11L(ITUV,MAXPOW)))
      AI11=(CG11U*DFLOAT(NAI11U(ITUV,MAXPOW))+
     &      CG11L*DFLOAT(NAI11L(ITUV,MAXPOW)))
      AR21=(CG21U*DFLOAT(NAR21U(ITUV,MAXPOW))+
     &      CG21L*DFLOAT(NAR21L(ITUV,MAXPOW)))
      AI21=(CG21U*DFLOAT(NAI21U(ITUV,MAXPOW))+
     &      CG21L*DFLOAT(NAI21L(ITUV,MAXPOW)))
      P2M=P2(M)
      DO 670 IPOWER=MAXPOW,1,-1
      AR11=AR11*P2M+(CG11U*DFLOAT(NAR11U(ITUV,IPOWER-1))+
     &               CG11L*DFLOAT(NAR11L(ITUV,IPOWER-1)))
      AI11=AI11*P2M+(CG11U*DFLOAT(NAI11U(ITUV,IPOWER-1))+
     &               CG11L*DFLOAT(NAI11L(ITUV,IPOWER-1)))
      AR21=AR21*P2M+(CG21U*DFLOAT(NAR21U(ITUV,IPOWER-1))+
     &               CG21L*DFLOAT(NAR21L(ITUV,IPOWER-1)))
      AI21=AI21*P2M+(CG21U*DFLOAT(NAI21U(ITUV,IPOWER-1))+
     &               CG21L*DFLOAT(NAI21L(ITUV,IPOWER-1)))
670   CONTINUE
      TMPA=4.0D0*EXPA(M)*EXPB(M)
      ERSS11(M,ITUV)=TMPA*AR11
      EISS11(M,ITUV)=TMPA*AI11
      ERSS21(M,ITUV)=TMPA*AR21
      EISS21(M,ITUV)=TMPA*AI21
660   CONTINUE
650   CONTINUE
C
C**********************************************************************C
C
      IF(BFAC.NE.0.0D0) THEN
C
      RB1U=SQPINV*DFLOAT(NB1UN)*DSQRT(DFLOAT(NB1UR))/DFLOAT(NB1UD)
      RB1L=SQPINV*DFLOAT(NB1LN)*DSQRT(DFLOAT(NB1LR))/DFLOAT(NB1LD)
C
      CG11U=RA1US*RB1U
      CG11L=RA1LS*RB1L
      CG21U=RA2US*RB1U
      CG21L=RA2LS*RB1L
C
      MAXPOW=LEFFA+LB
C
      DO 750 ITUV=1,ITUVMX
      DO 760 M=1,MAXM
      BR11=(CG11U*DFLOAT(NBR11U(ITUV,MAXPOW))+
     &      CG11L*DFLOAT(NBR11L(ITUV,MAXPOW)))
      BI11=(CG11U*DFLOAT(NBI11U(ITUV,MAXPOW))+
     &      CG11L*DFLOAT(NBI11L(ITUV,MAXPOW)))
      BR21=(CG21U*DFLOAT(NBR21U(ITUV,MAXPOW))+
     &      CG21L*DFLOAT(NBR21L(ITUV,MAXPOW)))
      BI21=(CG21U*DFLOAT(NBI21U(ITUV,MAXPOW))+
     &      CG21L*DFLOAT(NBI21L(ITUV,MAXPOW)))
      P2M=P2(M)
      DO 770 IPOWER=MAXPOW,1,-1
      BR11=BR11*P2M+(CG11U*DFLOAT(NBR11U(ITUV,IPOWER-1))+
     &               CG11L*DFLOAT(NBR11L(ITUV,IPOWER-1)))
      BI11=BI11*P2M+(CG11U*DFLOAT(NBI11U(ITUV,IPOWER-1))+
     &               CG11L*DFLOAT(NBI11L(ITUV,IPOWER-1)))
      BR21=BR21*P2M+(CG21U*DFLOAT(NBR21U(ITUV,IPOWER-1))+
     &               CG21L*DFLOAT(NBR21L(ITUV,IPOWER-1)))
      BI21=BI21*P2M+(CG21U*DFLOAT(NBI21U(ITUV,IPOWER-1))+
     &               CG21L*DFLOAT(NBI21L(ITUV,IPOWER-1)))
770   CONTINUE
      TMPB=-2.0D0*EXPA(M)*BFAC
      ERSS11(M,ITUV)=ERSS11(M,ITUV)+TMPB*BR11
      EISS11(M,ITUV)=EISS11(M,ITUV)+TMPB*BI11
      ERSS21(M,ITUV)=ERSS21(M,ITUV)+TMPB*BR21
      EISS21(M,ITUV)=EISS21(M,ITUV)+TMPB*BI21
760   CONTINUE
750   CONTINUE
C
      ENDIF
C
C**********************************************************************C
C
      IF(AFAC.NE.0.0D0) THEN
C
      RA1U=SQPINV*DFLOAT(NA1UN)*DSQRT(DFLOAT(NA1UR))/DFLOAT(NA1UD)
      RA1L=SQPINV*DFLOAT(NA1LN)*DSQRT(DFLOAT(NA1LR))/DFLOAT(NA1LD)
      RA2U=SQPINV*DFLOAT(NA2UN)*DSQRT(DFLOAT(NA2UR))/DFLOAT(NA2UD)
      RA2L=SQPINV*DFLOAT(NA2LN)*DSQRT(DFLOAT(NA2LR))/DFLOAT(NA2LD)
C
      CG11U=RA1U*RB1US
      CG11L=RA1L*RB1LS
      CG21U=RA2U*RB1US
      CG21L=RA2L*RB1LS
C
      MAXPOW=LA+LEFFB
C
      DO 850 ITUV=1,ITUVMX
      DO 860 M=1,MAXM
      CR11=(CG11U*DFLOAT(NCR11U(ITUV,MAXPOW))+
     &      CG11L*DFLOAT(NCR11L(ITUV,MAXPOW)))
      CI11=(CG11U*DFLOAT(NCI11U(ITUV,MAXPOW))+
     &      CG11L*DFLOAT(NCI11L(ITUV,MAXPOW)))
      CR21=(CG21U*DFLOAT(NCR21U(ITUV,MAXPOW))+
     &      CG21L*DFLOAT(NCR21L(ITUV,MAXPOW)))
      CI21=(CG21U*DFLOAT(NCI21U(ITUV,MAXPOW))+
     &      CG21L*DFLOAT(NCI21L(ITUV,MAXPOW)))
      P2M=P2(M)
      DO 870 IPOWER=MAXPOW,1,-1
      CR11=CR11*P2M+(CG11U*DFLOAT(NCR11U(ITUV,IPOWER-1))+
     &               CG11L*DFLOAT(NCR11L(ITUV,IPOWER-1)))
      CI11=CI11*P2M+(CG11U*DFLOAT(NCI11U(ITUV,IPOWER-1))+
     &               CG11L*DFLOAT(NCI11L(ITUV,IPOWER-1)))
      CR21=CR21*P2M+(CG21U*DFLOAT(NCR21U(ITUV,IPOWER-1))+
     &               CG21L*DFLOAT(NCR21L(ITUV,IPOWER-1)))
      CI21=CI21*P2M+(CG21U*DFLOAT(NCI21U(ITUV,IPOWER-1))+
     &               CG21L*DFLOAT(NCI21L(ITUV,IPOWER-1)))
870   CONTINUE
      TMPC=-2.0D0*EXPB(M)*AFAC
      ERSS11(M,ITUV)=ERSS11(M,ITUV)+TMPC*CR11
      EISS11(M,ITUV)=EISS11(M,ITUV)+TMPC*CI11
      ERSS21(M,ITUV)=ERSS21(M,ITUV)+TMPC*CR21
      EISS21(M,ITUV)=EISS21(M,ITUV)+TMPC*CI21
860   CONTINUE
850   CONTINUE
C
      ENDIF
C
C**********************************************************************C
C 
      IF(AFAC*BFAC.NE.0.0D0) THEN
C
      CG11U=RA1U*RB1U
      CG11L=RA1L*RB1L
      CG21U=RA2U*RB1U
      CG21L=RA2L*RB1L
C
      MAXPOW=LA+LB
C
      DO 950 ITUV=1,ITUVMX
      DO 960 M=1,MAXM
      DR11=(CG11U*DFLOAT(NDR11U(ITUV,MAXPOW))+
     &      CG11L*DFLOAT(NDR11L(ITUV,MAXPOW)))
      DI11=(CG11U*DFLOAT(NDI11U(ITUV,MAXPOW))+
     &      CG11L*DFLOAT(NDI11L(ITUV,MAXPOW)))
      DR21=(CG21U*DFLOAT(NDR21U(ITUV,MAXPOW))+
     &      CG21L*DFLOAT(NDR21L(ITUV,MAXPOW)))
      DI21=(CG21U*DFLOAT(NDI21U(ITUV,MAXPOW))+
     &      CG21L*DFLOAT(NDI21L(ITUV,MAXPOW)))
      P2M=P2(M)
      DO 970 IPOWER=MAXPOW,1,-1
      DR11=DR11*P2M+(CG11U*DFLOAT(NDR11U(ITUV,IPOWER-1))+
     &               CG11L*DFLOAT(NDR11L(ITUV,IPOWER-1)))
      DI11=DI11*P2M+(CG11U*DFLOAT(NDI11U(ITUV,IPOWER-1))+
     &               CG11L*DFLOAT(NDI11L(ITUV,IPOWER-1)))
      DR21=DR21*P2M+(CG21U*DFLOAT(NDR21U(ITUV,IPOWER-1))+
     &               CG21L*DFLOAT(NDR21L(ITUV,IPOWER-1)))
      DI21=DI21*P2M+(CG21U*DFLOAT(NDI21U(ITUV,IPOWER-1))+
     &               CG21L*DFLOAT(NDI21L(ITUV,IPOWER-1)))
970   CONTINUE
      TMPD=AFAC*BFAC
      ERSS11(M,ITUV)=ERSS11(M,ITUV)+TMPD*DR11
      EISS11(M,ITUV)=EISS11(M,ITUV)+TMPD*DI11
      ERSS21(M,ITUV)=ERSS21(M,ITUV)+TMPD*DR21
      EISS21(M,ITUV)=EISS21(M,ITUV)+TMPD*DI21
960   CONTINUE
950   CONTINUE
C
      ENDIF
C
C**********************************************************************C
C
      ITUV=0
      DO 400 MTUV=0,LAMAB
      RPH=DFLOAT((IALT)**(MTUV))
      DO 402 MDUM=1,((MTUV+1)*(MTUV+2))/2
      ITUV=ITUV+1
      DO 401 M=1,MAXM
      ERSS11(M,ITUV)=ERSS11(M,ITUV)*RKAB(M)*RNSS(M)*RPH
      EISS11(M,ITUV)=EISS11(M,ITUV)*RKAB(M)*RNSS(M)*RPH
      ERSS21(M,ITUV)=ERSS21(M,ITUV)*RKAB(M)*RNSS(M)*RPH
      EISS21(M,ITUV)=EISS21(M,ITUV)*RKAB(M)*RNSS(M)*RPH
401   CONTINUE
402   CONTINUE
400   CONTINUE
C
C
      RETURN
      END
C
C
C
      SUBROUTINE ISMAKE(NCRU,NCIU,NCRL,NCIL,KAPPA,MQN2,NQN,
     &            NUNUM,NUROOT,NUDEN,NLNUM,NLROOT,NLDEN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(PI=3.1415926535898D0)
C
      DIMENSION NCRU(84),NCIU(84),NCRL(84),NCIL(84)
      COMMON/CONSTS/NCR(84,25),NCI(84,25),MCR(84,25),MCI(84,25),
     &              NRNUM(25),NRROOT(25),NRDEN(25)
C
C
c      SQPINV=DSQRT(1.0D0/PI)
      SQPINV=0.5641895835477563D0
C 
C     CONSTRUCT LARGE COMPONENT SPINORS
c      write(6,*) 'ISMAKE'
C
      IF(KAPPA.GT.0) THEN
      LQN=KAPPA-1
      JQN2=2*KAPPA-1
      ICGUN=JQN2+MQN2
      ICGUD=2*JQN2
      ICGLN=JQN2-MQN2
      ICGLD=2*JQN2
      ISGN=1
c      CGCU= DSQRT(DFLOAT(JQN2+MQN2)/(2.0D0*DFLOAT(JQN2)))
c      CGCL= DSQRT(DFLOAT(JQN2-MQN2)/(2.0D0*DFLOAT(JQN2)))
      ELSE
      LQN=-KAPPA
      JQN2=-2*KAPPA-1
      ICGUN=JQN2+2-MQN2
      ICGUD=2*(JQN2+2)
      ICGLN=JQN2+2+MQN2
      ICGLD=2*(JQN2+2)
      ISGN=-1
c      CGCU= DSQRT(DFLOAT(JQN2+2-MQN2)/(2.0D0*DFLOAT(JQN2+2)))
c      CGCL=-DSQRT(DFLOAT(JQN2+2+MQN2)/(2.0D0*DFLOAT(JQN2+2)))
      ENDIF
C
      MLQNU=(MQN2-1)/2
      MLQNL=(MQN2+1)/2
C
      IADRU=LQN*(LQN+1)+MLQNU+1
      IADRL=LQN*(LQN+1)+MLQNL+1
C
      ILEN=((LQN+3)*(LQN+4)*(LQN+5))/6
C
      IF(NQN.EQ.0) THEN
C
      IF(IABS(MLQNU).LE.LQN) THEN
      DO 10 I=1,ILEN
      NCRU(I)=NCR(I,IADRU)
      NCIU(I)=NCI(I,IADRU)
10    CONTINUE
      ELSE
      DO 11 I=1,ILEN
      NCRU(I)=0
      NCIU(I)=0
11    CONTINUE
      ENDIF
C
      IF(IABS(MLQNL).LE.LQN) THEN
      DO 20 I=1,ILEN
      NCRL(I)=NCR(I,IADRL)
      NCIL(I)=NCI(I,IADRL)
20    CONTINUE
      ELSE
      DO 21 I=1,ILEN
      NCRL(I)=0
      NCIL(I)=0
21    CONTINUE
      ENDIF
C
C
      ELSE
C
      IF(IABS(MLQNU).LE.LQN) THEN
      DO 30 I=1,ILEN
      NCRU(I)=MCR(I,IADRU)
      NCIU(I)=MCI(I,IADRU)
30    CONTINUE
      ELSE
      DO 31 I=1,ILEN
      NCRU(I)=0
      NCIU(I)=0
31    CONTINUE
      ENDIF
C
      IF(IABS(MLQNL).LE.LQN) THEN
      DO 40 I=1,ILEN
      NCRL(I)=MCR(I,IADRL)
      NCIL(I)=MCI(I,IADRL)
40    CONTINUE
      ELSE
      DO 41 I=1,ILEN
      NCRL(I)=0
      NCIL(I)=0
41    CONTINUE
      ENDIF
C
C
      ENDIF
C
C
      IF(IABS(MLQNU).LE.LQN) THEN
      NUNUM=ISGN*NRNUM(IADRU)
      NUROOT=ICGUN*ICGUD*NRROOT(IADRU)
      NUDEN=ICGUD*NRDEN(IADRU)
      ELSE
      NUNUM=0
      NUROOT=0
      NUDEN=1
      ENDIF
C
      IF(IABS(MLQNL).LE.LQN) THEN
      NLNUM=NRNUM(IADRL)
      NLROOT=ICGLN*ICGLD*NRROOT(IADRL)
      NLDEN=ICGLD*NRDEN(IADRL)
      ELSE
      NLNUM=0
      NLROOT=0
      NLDEN=1
      ENDIF
C
      RETURN
      END
C
C
C********************************************************************C
C
C
C
      SUBROUTINE ECART(EQ,PAQ,PBQ,P2,MAXM,IMAX1,IMAX2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXIJ=125)
C*****************************************************************C
C     SUBROUTINE TO GENERATE CARTESIAN E-COEFFICIENTS             C
C*****************************************************************C
      DIMENSION EQ(MAXB2,MAXIJ),PAQ(MAXB2),PBQ(MAXB2),P2(MAXB2)
C
C     INITIALIZE EQ(0,0)
C
      DO 1 M=1,MAXM
      EQ(M,1)=1.0D0
1     CONTINUE
C
      IF(IMAX1.GE.0) THEN
C*****************************************************************C
C     I1=0                                                        C
C*****************************************************************C
      IF(IMAX2.GE.1) THEN
C
C     (0,1)
C
      DO 6 M=1,MAXM
      EQ(M,2)=PBQ(M)
      EQ(M,3)=P2(M)
6     CONTINUE
      ENDIF
      IF(IMAX2.GE.2) THEN
C
C     (0,2)
C
      DO 7 M=1,MAXM
      EQ(M,4)=PBQ(M)*EQ(M,2)+EQ(M,3)
      EQ(M,5)=EQ(M,2)*P2(M)+PBQ(M)*EQ(M,3)
      EQ(M,6)=EQ(M,3)*P2(M)
7     CONTINUE
      ENDIF
      IF(IMAX2.GE.3) THEN
C
C     (0,3)
C
      DO 8 M=1,MAXM
      EQ(M,7)=PBQ(M)*EQ(M,4)+EQ(M,5)
      EQ(M,8)=EQ(M,4)*P2(M)+PBQ(M)*EQ(M,5)+2.0D0*EQ(M,6)
      EQ(M,9)=EQ(M,5)*P2(M)+PBQ(M)*EQ(M,6)
      EQ(M,10)=EQ(M,6)*P2(M)
8     CONTINUE
      ENDIF
      IF(IMAX2.GE.4) THEN
C
C     (0,4)
C
      DO 9 M=1,MAXM
      EQ(M,11)=PBQ(M)*EQ(M,7)+EQ(M,8)
      EQ(M,12)=EQ(M,7)*P2(M)+PBQ(M)*EQ(M,8)+2.0D0*EQ(M,9)
      EQ(M,13)=EQ(M,8)*P2(M)+PBQ(M)*EQ(M,9)+3.0D0*EQ(M,10)
      EQ(M,14)=EQ(M,9)*P2(M)+PBQ(M)*EQ(M,10)
      EQ(M,15)=EQ(M,10)*P2(M)
9     CONTINUE
      ENDIF
      ENDIF
C
      IF(IMAX1.GE.1) THEN
C*****************************************************************C
C     I1=1                                                        C
C*****************************************************************C
C
C     (1,0)
C
      DO 2 M=1,MAXM
      EQ(M,16)=PAQ(M)
      EQ(M,17)=P2(M)
2     CONTINUE
C
      IF(IMAX2.GE.1) THEN
C
C     (1,1)
C
      DO 10 M=1,MAXM
      EQ(M,18)=PBQ(M)*EQ(M,16)+EQ(M,17)
      EQ(M,19)=EQ(M,16)*P2(M)+PBQ(M)*EQ(M,17)
      EQ(M,20)=EQ(M,17)*P2(M)
10    CONTINUE
      ENDIF
      IF(IMAX2.GE.2) THEN
C
C     (1,2)
C
      DO 11 M=1,MAXM
      EQ(M,21)=PBQ(M)*EQ(M,18)+EQ(M,19)
      EQ(M,22)=EQ(M,18)*P2(M)+PBQ(M)*EQ(M,19)+2.0D0*EQ(M,20)
      EQ(M,23)=EQ(M,19)*P2(M)+PBQ(M)*EQ(M,20)
      EQ(M,24)=EQ(M,20)*P2(M)
11    CONTINUE
      ENDIF
      IF(IMAX2.GE.3) THEN
C
C     (1,3)
C
      DO 12 M=1,MAXM
      EQ(M,25)=PBQ(M)*EQ(M,21)+EQ(M,22)
      EQ(M,26)=EQ(M,21)*P2(M)+PBQ(M)*EQ(M,22)+2.0D0*EQ(M,23)
      EQ(M,27)=EQ(M,22)*P2(M)+PBQ(M)*EQ(M,23)+3.0D0*EQ(M,24)
      EQ(M,28)=EQ(M,23)*P2(M)+PBQ(M)*EQ(M,24)
      EQ(M,29)=EQ(M,24)*P2(M)
12    CONTINUE
      ENDIF
      IF(IMAX2.GE.4) THEN
C
C     (1,4)
C
      DO 13 M=1,MAXM
      EQ(M,30)=PBQ(M)*EQ(M,25)+EQ(M,26)
      EQ(M,31)=EQ(M,25)*P2(M)+PBQ(M)*EQ(M,26)+2.0D0*EQ(M,27)
      EQ(M,32)=EQ(M,26)*P2(M)+PBQ(M)*EQ(M,27)+3.0D0*EQ(M,28)
      EQ(M,33)=EQ(M,27)*P2(M)+PBQ(M)*EQ(M,28)+4.0D0*EQ(M,29)
      EQ(M,34)=EQ(M,28)*P2(M)+PBQ(M)*EQ(M,29)
      EQ(M,35)=EQ(M,29)*P2(M)
13    CONTINUE
      ENDIF
      ENDIF
C
      IF(IMAX1.GE.2) THEN
C*****************************************************************C
C     I1=2                                                        C
C*****************************************************************C
C
C     (2,0)
C
      DO 3 M=1,MAXM
      EQ(M,36)=PAQ(M)*EQ(M,16)+EQ(M,17)
      EQ(M,37)=EQ(M,16)*P2(M)+PAQ(M)*EQ(M,17)
      EQ(M,38)=EQ(M,17)*P2(M)
3     CONTINUE
      IF(IMAX2.GE.1) THEN
C
C     (2,1)
C
      DO 15 M=1,MAXM
      EQ(M,39)=PBQ(M)*EQ(M,36)+EQ(M,37)
      EQ(M,40)=EQ(M,36)*P2(M)+PBQ(M)*EQ(M,37)+2.0D0*EQ(M,38)
      EQ(M,41)=EQ(M,37)*P2(M)+PBQ(M)*EQ(M,38)
      EQ(M,42)=EQ(M,38)*P2(M)
15    CONTINUE
      ENDIF
      IF(IMAX2.GE.2) THEN
C
C     (2,2)
C
      DO 16 M=1,MAXM
      EQ(M,43)=PBQ(M)*EQ(M,39)+EQ(M,40)
      EQ(M,44)=EQ(M,39)*P2(M)+PBQ(M)*EQ(M,40)+2.0D0*EQ(M,41)
      EQ(M,45)=EQ(M,40)*P2(M)+PBQ(M)*EQ(M,41)+3.0D0*EQ(M,42)
      EQ(M,46)=EQ(M,41)*P2(M)+PBQ(M)*EQ(M,42)
      EQ(M,47)=EQ(M,42)*P2(M)
16    CONTINUE
      ENDIF
      IF(IMAX2.GE.3) THEN
C
C     (2,3)
C
      DO 17 M=1,MAXM
      EQ(M,48)=PBQ(M)*EQ(M,43)+EQ(M,44)
      EQ(M,49)=EQ(M,43)*P2(M)+PBQ(M)*EQ(M,44)+2.0D0*EQ(M,45)
      EQ(M,50)=EQ(M,44)*P2(M)+PBQ(M)*EQ(M,45)+3.0D0*EQ(M,46)
      EQ(M,51)=EQ(M,45)*P2(M)+PBQ(M)*EQ(M,46)+4.0D0*EQ(M,47)
      EQ(M,52)=EQ(M,46)*P2(M)+PBQ(M)*EQ(M,47)
      EQ(M,53)=EQ(M,47)*P2(M)
17    CONTINUE
      ENDIF
      IF(IMAX2.GE.4) THEN
C
C     (2,4)
C
      DO 18 M=1,MAXM
      EQ(M,54)=PBQ(M)*EQ(M,48)+EQ(M,49)
      EQ(M,55)=EQ(M,48)*P2(M)+PBQ(M)*EQ(M,49)+2.0D0*EQ(M,50)
      EQ(M,56)=EQ(M,49)*P2(M)+PBQ(M)*EQ(M,50)+3.0D0*EQ(M,51)
      EQ(M,57)=EQ(M,50)*P2(M)+PBQ(M)*EQ(M,51)+4.0D0*EQ(M,52)
      EQ(M,58)=EQ(M,51)*P2(M)+PBQ(M)*EQ(M,52)+5.0D0*EQ(M,53)
      EQ(M,59)=EQ(M,52)*P2(M)+PBQ(M)*EQ(M,53)
      EQ(M,60)=EQ(M,53)*P2(M)
18    CONTINUE
      ENDIF
      ENDIF
C
      IF(IMAX1.GE.3) THEN
C*****************************************************************C
C     I1=3                                                        C
C*****************************************************************C
C
C     (3,0)
C      
      DO 4 M=1,MAXM
      EQ(M,61)=PAQ(M)*EQ(M,36)+EQ(M,37)
      EQ(M,62)=EQ(M,36)*P2(M)+PAQ(M)*EQ(M,37)+2.0D0*EQ(M,38)
      EQ(M,63)=EQ(M,37)*P2(M)+PAQ(M)*EQ(M,38)
      EQ(M,64)=EQ(M,38)*P2(M)
4     CONTINUE
      IF(IMAX2.GE.1) THEN
C
C     (3,1)
C
      DO 20 M=1,MAXM
      EQ(M,65)=PBQ(M)*EQ(M,61)+EQ(M,62)
      EQ(M,66)=EQ(M,61)*P2(M)+PBQ(M)*EQ(M,62)+2.0D0*EQ(M,63)
      EQ(M,67)=EQ(M,62)*P2(M)+PBQ(M)*EQ(M,63)+3.0D0*EQ(M,64)
      EQ(M,68)=EQ(M,63)*P2(M)+PBQ(M)*EQ(M,64)
      EQ(M,69)=EQ(M,64)*P2(M)
20    CONTINUE
      ENDIF
      IF(IMAX2.GE.2) THEN
C
C     (3,2)
C
      DO 21 M=1,MAXM
      EQ(M,70)=PBQ(M)*EQ(M,65)+EQ(M,66)
      EQ(M,71)=EQ(M,65)*P2(M)+PBQ(M)*EQ(M,66)+2.0D0*EQ(M,67)
      EQ(M,72)=EQ(M,66)*P2(M)+PBQ(M)*EQ(M,67)+3.0D0*EQ(M,68)
      EQ(M,73)=EQ(M,67)*P2(M)+PBQ(M)*EQ(M,68)+4.0D0*EQ(M,69)
      EQ(M,74)=EQ(M,68)*P2(M)+PBQ(M)*EQ(M,69)
      EQ(M,75)=EQ(M,69)*P2(M)
21    CONTINUE
      ENDIF
      IF(IMAX2.GE.3) THEN
C
C     (3,3)
C
      DO 22 M=1,MAXM
      EQ(M,76)=PBQ(M)*EQ(M,70)+EQ(M,71)
      EQ(M,77)=EQ(M,70)*P2(M)+PBQ(M)*EQ(M,71)+2.0D0*EQ(M,72)
      EQ(M,78)=EQ(M,71)*P2(M)+PBQ(M)*EQ(M,72)+3.0D0*EQ(M,73)
      EQ(M,79)=EQ(M,72)*P2(M)+PBQ(M)*EQ(M,73)+4.0D0*EQ(M,74)
      EQ(M,80)=EQ(M,73)*P2(M)+PBQ(M)*EQ(M,74)+5.0D0*EQ(M,75)
      EQ(M,81)=EQ(M,74)*P2(M)+PBQ(M)*EQ(M,75)
      EQ(M,82)=EQ(M,75)*P2(M)
22    CONTINUE
      ENDIF
      IF(IMAX2.GE.4) THEN
C
C     (3,4)
C
      DO 23 M=1,MAXM
      EQ(M,83)=PBQ(M)*EQ(M,76)+EQ(M,77)
      EQ(M,84)=EQ(M,76)*P2(M)+PBQ(M)*EQ(M,77)+2.0D0*EQ(M,78)
      EQ(M,85)=EQ(M,77)*P2(M)+PBQ(M)*EQ(M,78)+3.0D0*EQ(M,79)
      EQ(M,86)=EQ(M,78)*P2(M)+PBQ(M)*EQ(M,79)+4.0D0*EQ(M,80)
      EQ(M,87)=EQ(M,79)*P2(M)+PBQ(M)*EQ(M,80)+5.0D0*EQ(M,81)
      EQ(M,88)=EQ(M,80)*P2(M)+PBQ(M)*EQ(M,81)+6.0D0*EQ(M,82)
      EQ(M,89)=EQ(M,81)*P2(M)+PBQ(M)*EQ(M,82)
      EQ(M,90)=EQ(M,82)*P2(M)
23    CONTINUE
      ENDIF
      ENDIF
C
      IF(IMAX1.GE.4) THEN
C*****************************************************************C
C     I1=4                                                        C
C*****************************************************************C
C
C     (4,0)
C
      DO 5 M=1,MAXM
      EQ(M,91)=PAQ(M)*EQ(M,61)+EQ(M,62)
      EQ(M,92)=EQ(M,61)*P2(M)+PAQ(M)*EQ(M,62)+2.0D0*EQ(M,63)
      EQ(M,93)=EQ(M,62)*P2(M)+PAQ(M)*EQ(M,63)+3.0D0*EQ(M,64)
      EQ(M,94)=EQ(M,63)*P2(M)+PAQ(M)*EQ(M,64)
      EQ(M,95)=EQ(M,64)*P2(M)
5     CONTINUE
C
      IF(IMAX2.GE.1) THEN
C
C     (4,1)
C
      DO 24 M=1,MAXM
      EQ(M,96)=PBQ(M)*EQ(M,91)+EQ(M,92)
      EQ(M,97)=EQ(M,91)*P2(M)+PBQ(M)*EQ(M,92)+2.0D0*EQ(M,93)
      EQ(M,98)=EQ(M,92)*P2(M)+PBQ(M)*EQ(M,93)+3.0D0*EQ(M,94)
      EQ(M,99)=EQ(M,93)*P2(M)+PBQ(M)*EQ(M,94)+4.0D0*EQ(M,95)
      EQ(M,100)=EQ(M,94)*P2(M)+PBQ(M)*EQ(M,95)
      EQ(M,101)=EQ(M,95)*P2(M)
24    CONTINUE
      ENDIF
      IF(IMAX2.GE.2) THEN
C
C     (4,2)
C
      DO 25 M=1,MAXM
      EQ(M,102)=PBQ(M)*EQ(M,96)+EQ(M,97)
      EQ(M,103)=EQ(M,96)*P2(M)+PBQ(M)*EQ(M,97)+2.0D0*EQ(M,98)
      EQ(M,104)=EQ(M,97)*P2(M)+PBQ(M)*EQ(M,98)+3.0D0*EQ(M,99)
      EQ(M,105)=EQ(M,98)*P2(M)+PBQ(M)*EQ(M,99)+4.0D0*EQ(M,100)
      EQ(M,106)=EQ(M,99)*P2(M)+PBQ(M)*EQ(M,100)+5.0D0*EQ(M,101)
      EQ(M,107)=EQ(M,100)*P2(M)+PBQ(M)*EQ(M,101)
      EQ(M,108)=EQ(M,101)*P2(M)
25    CONTINUE
      ENDIF
      IF(IMAX2.GE.3) THEN
C
C     (4,3)
C
      DO 26 M=1,MAXM
      EQ(M,109)=PBQ(M)*EQ(M,102)+EQ(M,103)
      EQ(M,110)=EQ(M,102)*P2(M)+PBQ(M)*EQ(M,103)+2.0D0*EQ(M,104)
      EQ(M,111)=EQ(M,103)*P2(M)+PBQ(M)*EQ(M,104)+3.0D0*EQ(M,105)
      EQ(M,112)=EQ(M,104)*P2(M)+PBQ(M)*EQ(M,105)+4.0D0*EQ(M,106)
      EQ(M,113)=EQ(M,105)*P2(M)+PBQ(M)*EQ(M,106)+5.0D0*EQ(M,107)
      EQ(M,114)=EQ(M,106)*P2(M)+PBQ(M)*EQ(M,107)+6.0D0*EQ(M,108)
      EQ(M,115)=EQ(M,107)*P2(M)+PBQ(M)*EQ(M,108)
      EQ(M,116)=EQ(M,108)*P2(M)
26    CONTINUE
      ENDIF
      IF(IMAX2.GE.4) THEN
C
C     (4,4)
C
      DO 27 M=1,MAXM
      EQ(M,117)=PBQ(M)*EQ(M,109)+EQ(M,110)
      EQ(M,118)=EQ(M,109)*P2(M)+PBQ(M)*EQ(M,110)+2.0D0*EQ(M,111)
      EQ(M,119)=EQ(M,110)*P2(M)+PBQ(M)*EQ(M,111)+3.0D0*EQ(M,112)
      EQ(M,120)=EQ(M,111)*P2(M)+PBQ(M)*EQ(M,112)+4.0D0*EQ(M,113)
      EQ(M,121)=EQ(M,112)*P2(M)+PBQ(M)*EQ(M,113)+5.0D0*EQ(M,114)
      EQ(M,122)=EQ(M,113)*P2(M)+PBQ(M)*EQ(M,114)+6.0D0*EQ(M,115)
      EQ(M,123)=EQ(M,114)*P2(M)+PBQ(M)*EQ(M,115)+7.0D0*EQ(M,116)
      EQ(M,124)=EQ(M,115)*P2(M)+PBQ(M)*EQ(M,116)
      EQ(M,125)=EQ(M,116)*P2(M)
27    CONTINUE
      ENDIF
      ENDIF
C      
      RETURN
      END
C**********************************************************************C
C
      SUBROUTINE DENS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXB=20,MAXB2=MAXB*MAXB,MAXDIM=300,NKAPM=7,
     &          NCENTM=6,IL4=24,MAXMV=5,MAXMV2=2*MAXMV)
      PARAMETER(TRESH=1.0D-10,MAXIT=30)
C
      COMPLEX*16 FOCK,OVAP,C
      COMPLEX*16 SUM
C
      COMMON/SPEC/COORD(3,NCENTM),ZNUC(NCENTM),CNUC(NCENTM),
     &  EXPSET(MAXB,NKAPM,NCENTM),IZNUC(NCENTM),ICRGE(NCENTM),
     &  KVALS(NKAPM,NCENTM),NKAPPA(NCENTM),LMAXX(NCENTM),
     &  NFUNCT(NKAPM,NCENTM),NCENT,NDIM,NSHIFT,
     &  NOCC,ITER,IALL,IRUN
      COMMON/FMAT/FOCK(MAXDIM,MAXDIM),OVAP(MAXDIM,MAXDIM)
      COMMON/CLABEL/IOCCM0
      COMMON/COEFF/C(MAXDIM,MAXDIM)
C
C     CONSTRUCT THE DENSITY MATIX USING THE FOCK MATRIX AS SCRATCH SPACE
C
C
      DO 10 I=1,NDIM
      DO 10 J=1,NDIM
      SUM=0.0D0
      DO 20 IOCC=1,IOCCM0
      SUM=SUM+DCONJG(C(I,NSHIFT+IOCC))*C(J,NSHIFT+IOCC)
20    CONTINUE
      FOCK(I,J)=SUM
10    CONTINUE
C
C     COPY OVER TO C-MATRIX
C
      DO 30 I=1,NDIM
      DO 30 J=1,NDIM
      C(I,J)=FOCK(I,J)
30    CONTINUE
C
      RETURN
      END
C
C*********************************************************************
C
      BLOCK DATA ECONST 
C
      PARAMETER(MAXLQN=4,MLL=165,MABLL=MLL)
C
      COMMON/CONSTS/NCR(84,25),NCI(84,25),MCR(84,25),MCI(84,25),
     &              NRNUM(25),NRROOT(25),NRDEN(25)
      COMMON/INDSYS/INABCD(0:4*MAXLQN,0:4*MAXLQN,0:4*MAXLQN),
     &              IVEC(MABLL),JVEC(MABLL),KVEC(MABLL)
C
      DATA NCR/
     &1,86*0,1,81*0,2,85*0,-1,86*0,-1,2*0,1,81*0,4,80*0,4,1*0,
     &-2,2*0,-2,81*0,-4,82*0,-1,2*0,1,90*0,-3,2*0,1,76*0,-6,4*0,
     &6,80*0,12,1*0,-3,2*0,-3,74*0,8,1*0,-12,4*0,-12,80*0,-12,
     &1*0,3,2*0,3,76*0,-6,4*0,6,82*0,3,2*0,-1,88*0,1,6*0,-6,2*0,
     &1,76*0,-24,4*0,8,73*0,-24,1*0,4,4*0,24,4*0,-4,74*0,32,1*0,
     &-24,4*0,-24,71*0,16,1*0,-48,1*0,6,4*0,-48,1*0,12,2*0,6,
     &74*0,-32,1*0,24,4*0,24,73*0,-24,1*0,4,4*0,24,4*0,-4,76*0,
     &24,4*0,-8,75*0,1,6*0,-6,2*0,1,49*0/
      DATA NCI/
     &86*0,-1,167*0,-1,89*0,-2,80*0,-4,167*0,-4,86*0,2,88*0,1,
     &4*0,-3,80*0,-12,79*0,-12,1*0,3,4*0,3,160*0,-12,1*0,3,4*0,
     &3,80*0,12,81*0,1,4*0,-3,93*0,4,4*0,-4,73*0,8,6*0,-24,79*0,
     &-48,1*0,8,4*0,8,71*0,-32,1*0,24,6*0,24,158*0,-32,1*0,24,
     &6*0,24,79*0,48,1*0,-8,4*0,-8,73*0,8,6*0,-24,81*0,-4,4*0,
     &4,50*0/
      DATA MCR/
     &4*0,1,1*0,1,2*0,1,88*0,1,1*0,1,2*0,1,74*0,2,1*0,2,4*0,2,
     &80*0,-1,1*0,-1,2*0,-1,86*0,-1,1*0,-1,4*0,1,4*0,1,74*0,4,
     &1*0,4,4*0,4,71*0,4,1*0,2,1*0,-2,4*0,2,1*0,-4,2*0,-2,74*0,
     &-4,1*0,-4,4*0,-4,73*0,-1,1*0,-1,4*0,1,4*0,1,92*0,-3,1*0,
     &-3,4*0,1,1*0,-2,2*0,1,65*0,-6,1*0,-6,6*0,6,6*0,6,71*0,12,
     &1*0,9,1*0,-3,4*0,9,1*0,-6,2*0,-3,63*0,8,1*0,-4,1*0,-12,
     &6*0,-4,1*0,-24,4*0,-12,71*0,-12,1*0,-9,1*0,3,4*0,-9,1*0,
     &6,2*0,3,65*0,-6,1*0,-6,6*0,6,6*0,6,73*0,3,1*0,3,4*0,-1,
     &1*0,2,2*0,-1,88*0,1,1*0,1,8*0,-6,1*0,-5,4*0,1,1*0,-5,2*0,
     &1,65*0,-24,1*0,-24,6*0,8,1*0,-16,4*0,8,60*0,-24,1*0,-20,
     &1*0,4,6*0,24,3*0,4,4*0,20,1*0,-4,2*0,-4,63*0,32,1*0,8,1*0,
     &-24,6*0,8,1*0,-48,4*0,-24,58*0,16,1*0,-32,1*0,-42,1*0,6,
     &6*0,-32,1*0,-84,1*0,18,4*0,-42,1*0,18,2*0,6,63*0,-32,1*0,
     &-8,1*0,24,6*0,-8,1*0,48,4*0,24,60*0,-24,1*0,-20,1*0,4,6*0,
     &24,3*0,4,4*0,20,1*0,-4,2*0,-4,65*0,24,1*0,24,6*0,-8,1*0,
     &16,4*0,-8,62*0,1,1*0,1,8*0,-6,1*0,-5,4*0,1,1*0,-5,2*0,1/
      DATA MCI/
     &95*0,-1,1*0,-1,4*0,-1,160*0,-1,1*0,-1,4*0,-1,91*0,-2,1*0,
     &-2,4*0,-2,71*0,-4,1*0,-4,6*0,-4,158*0,-4,1*0,-4,6*0,-4,
     &79*0,2,1*0,2,4*0,2,88*0,1,1*0,1,6*0,-3,1*0,-2,4*0,-3,71*0,
     &-12,1*0,-12,6*0,-12,68*0,-12,1*0,-9,1*0,3,6*0,-9,1*0,6,
     &4*0,3,149*0,-12,1*0,-9,1*0,3,6*0,-9,1*0,6,4*0,3,71*0,12,
     &1*0,12,6*0,12,70*0,1,1*0,1,6*0,-3,1*0,-2,4*0,-3,95*0,4,
     &1*0,4,6*0,-4,6*0,-4,60*0,8,1*0,8,8*0,-24,1*0,-16,6*0,-24,
     &68*0,-48,1*0,-40,1*0,8,6*0,-40,1*0,16,4*0,8,58*0,-32,1*0,
     &-8,1*0,24,8*0,-8,1*0,48,6*0,24,145*0,-32,1*0,-8,1*0,24,
     &8*0,-8,1*0,48,6*0,24,68*0,48,1*0,40,1*0,-8,6*0,40,1*0,-16,
     &4*0,-8,60*0,8,1*0,8,8*0,-24,1*0,-16,6*0,-24,70*0,-4,1*0,
     &-4,6*0,4,6*0,4,1*0/
      DATA NRNUM/
     &1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3/
      DATA NRROOT/
     &1,6,3,6,30,30,5,30,30,35,210,21,7,21,210,35,70,35,10,5,
     &1,5,10,35,70/
      DATA NRDEN/
     &2,4,4,4,8,16,8,16,8,8,48,24,16,24,48,8,32,64,64,64,32,64,
     &64,64,32/
C
      DATA IVEC/
     &3*0,1,3*0,1,1,2,4*0,1,1,1,2,2,3,5*0,1,1,1,1,2,2,2,3,3,4,
     &6*0,1,1,1,1,1,2,2,2,2,3,3,3,4,4,5,7*0,1,1,1,1,1,1,2,2,2,
     &2,2,3,3,3,3,4,4,4,5,5,6,8*0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,
     &3,3,3,3,4,4,4,4,5,5,5,6,6,7,9*0,1,1,1,1,1,1,1,1,2,2,2,2,
     &2,2,2,3,3,3,3,3,3,4,4,4,4,4,5,5,5,5,6,6,6,7,7,8/
      DATA JVEC/
     &2*0,1,2*0,1,2,1*0,1,2*0,1,2,3,1*0,1,2,1*0,1,2*0,1,2,3,4,
     &1*0,1,2,3,1*0,1,2,1*0,1,2*0,1,2,3,4,5,1*0,1,2,3,4,1*0,1,
     &2,3,1*0,1,2,1*0,1,2*0,1,2,3,4,5,6,1*0,1,2,3,4,5,1*0,1,2,
     &3,4,1*0,1,2,3,1*0,1,2,1*0,1,2*0,1,2,3,4,5,6,7,1*0,1,2,3,
     &4,5,6,1*0,1,2,3,4,5,1*0,1,2,3,4,1*0,1,2,3,1*0,1,2,1*0,1,
     &2*0,1,2,3,4,5,6,7,8,1*0,1,2,3,4,5,6,7,1*0,1,2,3,4,5,6,1*0,
     &1,2,3,4,5,1*0,1,2,3,4,1*0,1,2,3,1*0,1,2,1*0,1,1*0/
      DATA KVEC/
     &1*0,1,2*0,2,1,1*0,1,2*0,3,2,1,1*0,2,1,1*0,1,2*0,4,3,2,1,
     &1*0,3,2,1,1*0,2,1,1*0,1,2*0,5,4,3,2,1,1*0,4,3,2,1,1*0,3,
     &2,1,1*0,2,1,1*0,1,2*0,6,5,4,3,2,1,1*0,5,4,3,2,1,1*0,4,3,
     &2,1,1*0,3,2,1,1*0,2,1,1*0,1,2*0,7,6,5,4,3,2,1,1*0,6,5,4,
     &3,2,1,1*0,5,4,3,2,1,1*0,4,3,2,1,1*0,3,2,1,1*0,2,1,1*0,1,
     &2*0,8,7,6,5,4,3,2,1,1*0,7,6,5,4,3,2,1,1*0,6,5,4,3,2,1,1*0,
     &5,4,3,2,1,1*0,4,3,2,1,1*0,3,2,1,1*0,2,1,1*0,1,2*0/
      DATA INABCD/
     &1,4,10,20,35,56,84,120,165,220,286,364,455,560,680,816,
     &969,3,9,19,34,55,83,119,164,219,285,363,454,559,679,815,
     &968,1*0,7,17,32,53,81,117,162,217,283,361,452,557,677,813,
     &966,2*0,14,29,50,78,114,159,214,280,358,449,554,674,810,
     &963,3*0,25,46,74,110,155,210,276,354,445,550,670,806,959,
     &4*0,41,69,105,150,205,271,349,440,545,665,801,954,5*0,63,
     &99,144,199,265,343,434,539,659,795,948,6*0,92,137,192,258,
     &336,427,532,652,788,941,7*0,129,184,250,328,419,524,644,
     &780,933,8*0,175,241,319,410,515,635,771,924,9*0,231,309,
     &400,505,625,761,914,10*0,298,389,494,614,750,903,11*0,377,
     &482,602,738,891,12*0,469,589,725,878,13*0,575,711,864,14*0,
     &696,849,15*0,833,16*0,2,8,18,33,54,82,118,163,218,284,362,
     &453,558,678,814,967,1*0,6,16,31,52,80,116,161,216,282,360,
     &451,556,676,812,965,2*0,13,28,49,77,113,158,213,279,357,
     &448,553,673,809,962,3*0,24,45,73,109,154,209,275,353,444,
     &549,669,805,958,4*0,40,68,104,149,204,270,348,439,544,664,
     &800,953,5*0,62,98,143,198,264,342,433,538,658,794,947,6*0,
     &91,136,191,257,335,426,531,651,787,940,7*0,128,183,249,
     &327,418,523,643,779,932,8*0,174,240,318,409,514,634,770,
     &923,9*0,230,308,399,504,624,760,913,10*0,297,388,493,613,
     &749,902,11*0,376,481,601,737,890,12*0,468,588,724,877,13*0,
     &574,710,863,14*0,695,848,15*0,832,33*0,5,15,30,51,79,115,
     &160,215,281,359,450,555,675,811,964,2*0,12,27,48,76,112,
     &157,212,278,356,447,552,672,808,961,3*0,23,44,72,108,153,
     &208,274,352,443,548,668,804,957,4*0,39,67,103,148,203,269,
     &347,438,543,663,799,952,5*0,61,97,142,197,263,341,432,537,
     &657,793,946,6*0,90,135,190,256,334,425,530,650,786,939,
     &7*0,127,182,248,326,417,522,642,778,931,8*0,173,239,317,
     &408,513,633,769,922,9*0,229,307,398,503,623,759,912,10*0,
     &296,387,492,612,748,901,11*0,375,480,600,736,889,12*0,467,
     &587,723,876,13*0,573,709,862,14*0,694,847,15*0,831,50*0,
     &11,26,47,75,111,156,211,277,355,446,551,671,807,960,3*0,
     &22,43,71,107,152,207,273,351,442,547,667,803,956,4*0,38,
     &66,102,147,202,268,346,437,542,662,798,951,5*0,60,96,141,
     &196,262,340,431,536,656,792,945,6*0,89,134,189,255,333,
     &424,529,649,785,938,7*0,126,181,247,325,416,521,641,777,
     &930,8*0,172,238,316,407,512,632,768,921,9*0,228,306,397,
     &502,622,758,911,10*0,295,386,491,611,747,900,11*0,374,479,
     &599,735,888,12*0,466,586,722,875,13*0,572,708,861,14*0,
     &693,846,15*0,830,67*0,21,42,70,106,151,206,272,350,441,
     &546,666,802,955,4*0,37,65,101,146,201,267,345,436,541,661,
     &797,950,5*0,59,95,140,195,261,339,430,535,655,791,944,6*0,
     &88,133,188,254,332,423,528,648,784,937,7*0,125,180,246,
     &324,415,520,640,776,929,8*0,171,237,315,406,511,631,767,
     &920,9*0,227,305,396,501,621,757,910,10*0,294,385,490,610,
     &746,899,11*0,373,478,598,734,887,12*0,465,585,721,874,13*0,
     &571,707,860,14*0,692,845,15*0,829,84*0,36,64,100,145,200,
     &266,344,435,540,660,796,949,5*0,58,94,139,194,260,338,429,
     &534,654,790,943,6*0,87,132,187,253,331,422,527,647,783,
     &936,7*0,124,179,245,323,414,519,639,775,928,8*0,170,236,
     &314,405,510,630,766,919,9*0,226,304,395,500,620,756,909,
     &10*0,293,384,489,609,745,898,11*0,372,477,597,733,886,12*0,
     &464,584,720,873,13*0,570,706,859,14*0,691,844,15*0,828,
     &101*0,57,93,138,193,259,337,428,533,653,789,942,6*0,86,
     &131,186,252,330,421,526,646,782,935,7*0,123,178,244,322,
     &413,518,638,774,927,8*0,169,235,313,404,509,629,765,918,
     &9*0,225,303,394,499,619,755,908,10*0,292,383,488,608,744,
     &897,11*0,371,476,596,732,885,12*0,463,583,719,872,13*0,
     &569,705,858,14*0,690,843,15*0,827,118*0,85,130,185,251,
     &329,420,525,645,781,934,7*0,122,177,243,321,412,517,637,
     &773,926,8*0,168,234,312,403,508,628,764,917,9*0,224,302,
     &393,498,618,754,907,10*0,291,382,487,607,743,896,11*0,370,
     &475,595,731,884,12*0,462,582,718,871,13*0,568,704,857,14*0,
     &689,842,15*0,826,135*0,121,176,242,320,411,516,636,772,
     &925,8*0,167,233,311,402,507,627,763,916,9*0,223,301,392,
     &497,617,753,906,10*0,290,381,486,606,742,895,11*0,369,474,
     &594,730,883,12*0,461,581,717,870,13*0,567,703,856,14*0,
     &688,841,15*0,825,152*0,166,232,310,401,506,626,762,915,
     &9*0,222,300,391,496,616,752,905,10*0,289,380,485,605,741,
     &894,11*0,368,473,593,729,882,12*0,460,580,716,869,13*0,
     &566,702,855,14*0,687,840,15*0,824,169*0,221,299,390,495,
     &615,751,904,10*0,288,379,484,604,740,893,11*0,367,472,592,
     &728,881,12*0,459,579,715,868,13*0,565,701,854,14*0,686,
     &839,15*0,823,186*0,287,378,483,603,739,892,11*0,366,471,
     &591,727,880,12*0,458,578,714,867,13*0,564,700,853,14*0,
     &685,838,15*0,822,203*0,365,470,590,726,879,12*0,457,577,
     &713,866,13*0,563,699,852,14*0,684,837,15*0,821,220*0,456,
     &576,712,865,13*0,562,698,851,14*0,683,836,15*0,820,237*0,
     &561,697,850,14*0,682,835,15*0,819,254*0,681,834,15*0,818,
     &271*0,817,288*0/
C
      END

