IMPLICIT NONE

INTEGER :: I,J,K,L,ISO,idum,M
INTEGER :: IC,JC,KC,LINES,VER
INTEGER :: ICM,JCM,KCM,LVC,LA,LC,LCM,NW,NWM,NAVG
!{ 140317, JHLEE
CHARACTER(LEN=3) :: CARDNUM
INTEGER :: ISCO, NDM, LDM, NODE, ELEMENT
INTEGER,ALLOCATABLE  :: NN2(:)
REAL,ALLOCATABLE :: VAR(:), VAR1(:,:)
!}
!{ 141010, JHLEE
CHARACTER(LEN=3),ALLOCATABLE :: CH(:)
CHARACTER(LEN=3),ALLOCATABLE :: LH
CHARACTER(LEN=9) FNTSR
CHARACTER(LEN=10) FNTSR2
CHARACTER(LEN=10) FNTSRA
CHARACTER(LEN=4) FNHOR
INTEGER :: FORLEN
! }

!OPEN(1) - INP FILE
INTEGER :: IOFILE,IHOR,NTSR,ISBEDSTR,IWRSP,ISWAVE,ISBDLDBC,NASER
! {JHLEE, 20141010
INTEGER :: IWQOUT, IPO4OUT
! }
REAL :: SDAY,EDAY,BELADJ,BELCVRT,TBEDIT,DTWQ,dum,R
INTEGER,ALLOCATABLE :: ITSR(:),JTSR(:)

!CELL.INP
INTEGER,ALLOCATABLE :: IJCT(:,:)

!LXLY.INP
INTEGER,ALLOCATABLE :: IL(:),JL(:),LNC(:),LIJ(:,:)
REAL,ALLOCATABLE :: DLON(:),DLAT(:),STCUV(:)
REAL,ALLOCATABLE :: CUE(:),CVE(:),CUN(:),CVN(:)

!DXDY.INP
REAL,ALLOCATABLE :: BELV(:)
REAL,ALLOCATABLE :: BELV1(:)

!{ 140317, JHLEE
!WQ3DWC.INP
REAL :: rC1, rN1, rP1, rC2, rN2, rP2, rC3, rN3, rP3
REAL :: KeTSS, KeChl, CChlc, CChld, CChlg
REAL :: rdum, CPprm1, CPprm2, CPprm3
REAL :: ANCc, ANCd, ANCg, ANCm, KPO4P, KHS
REAL :: PMC, PMD, PMG
!}

!{ 140317, JHLEE
!Convert_Tecplot3D.INP
CHARACTER(LEN=5) :: VARNAME
CHARACTER(LEN=100) :: OUTFILE
INTEGER :: ITFILE, IWQOUT1, ITIME, EC, VC
REAL :: TEXTX, TEXTY, TEXTH
!}

!{ 140317, JHLEE
!EVENT_TOX2.INP
INTEGER :: YY, IM, ID, IH
!}

!{ 140317, JHLEE
!CORNERS.INP
INTEGER :: NC
REAL,ALLOCATABLE :: XX1(:,:),YY1(:,:)
!}

!OUTPUT FILE
INTEGER :: N
REAL :: TIME
REAL,ALLOCATABLE :: VELE(:),VELN(:),SPD(:),ANG(:)
REAL :: AVEVELE,AVEVELN,AVESPD,AVEANG
REAL,ALLOCATABLE :: HP(:)
REAL :: SURFEL

!######################################################
!# READ MAIN CONTROL FILE (POST_EFDC_EE.INP) ---- START
!######################################################
OPEN(1,FILE='POST_EFDC_EE.INP',STATUS='OLD')

DO I=1,19
   READ(1,*)
ENDDO
READ(1,*)IOFILE,SDAY,EDAY,IHOR,NTSR,BELADJ,BELCVRT,ISBEDSTR,IWRSP,ISWAVE,TBEDIT,ISBDLDBC,DTWQ

! {JHLEE, 20141010
DO I=1,5
   READ(1,*)
ENDDO
READ(1,*) IWQOUT, IPO4OUT
! }
IF( NTSR.GT.0 ) THEN
  ALLOCATE( ITSR(NTSR),JTSR(NTSR) )
  DO I=1,5
     READ(1,*)
  ENDDO
  DO I=1,NTSR
     READ(1,*)ITSR(I),JTSR(I)
  ENDDO
ENDIF
CLOSE(1)
!######################################################
!# READ MAIN CONTROL FILE (POST_EFDC_EE.INP) ---- END
!######################################################

!######################################################
!# READ EFDC.INP ---- START
!######################################################
OPEN(1,FILE='../EFDC.INP',STATUS='UNKNOWN')
!{140317, JHLEE
IF(IOFILE.EQ.11) THEN
CALL SEEK('C9')
READ(1,*,IOSTAT=ISO) KC, IC, JC, LC, LVC, ISCO, NDM, LDM  
  ELEMENT=LDM*KC
  NODE=ELEMENT*8
  ALLOCATE( NN2(NODE) )
  ALLOCATE( XX1(LDM,4), YY1(LDM,4) )
ENDIF
!}
CALL SEEK('C11')  
READ(1,*,IOSTAT=ISO) dum,dum,dum,idum,dum,dum,dum,dum,dum,dum,dum,BELADJ,BELCVRT
CALL SEEK('C14')  
READ(1,*,IOSTAT=ISO) idum,idum,NASER,idum,idum,ISWAVE
CALL SEEK('C36A')  
READ(1,*,IOSTAT=ISO)ISBEDSTR
CALL SEEK('C40')  
READ(1,*,IOSTAT=ISO)IWRSP
ISBDLDBC=0
CALL SEEK('C42A')  
READ(1,*,IOSTAT=ISO)ISBDLDBC
CLOSE(1)
!######################################################
!# READ EFDC.INP ---- END
!######################################################

!{ 140317, JHLEE
!######################################################
!# READ Convert_Tecplot3D.INP ---- START
!######################################################
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
IF(IOFILE.EQ.11) THEN
  OPEN(1,FILE='Convert_Tecplot3D.INP',STATUS='OLD')
  DO I=1,4
    READ(1,*)
  ENDDO
  READ(1,*) ITIME
  DO I=1,28
    READ(1,*)
  ENDDO
  READ(1,*) ITFILE, IWQOUT1
  DO I=1,6
    READ(1,*)
  ENDDO
  READ(1,*) VARNAME, TEXTX, TEXTY, TEXTH
!  DO I=1,4
!    READ(1,*)
!  ENDDO
!  READ(1,*) OUTFILE
ENDIF      
CLOSE(1)
!######################################################
!# READ Convert_Tecplot3D.INP ---- END
!######################################################

!######################################################
!# READ WQ3DWC.INP ---- START
!######################################################
! IF(IOFILE.EQ.11.AND.ITFILE.GE.10.OR.IOFILE.EQ.10) THEN
IF(IOFILE.GE.10) THEN
OPEN(1,FILE='../WQ3DWC.INP',STATUS='OLD')
2001  READ(1,*,END=2009) CARDNUM
        IF(CARDNUM.EQ.'C08') THEN
          DO I=1,14
          	READ(1,*)
          ENDDO		
          READ(1,*) dum, dum, dum, dum, dum, dum, dum, dum, KHS, dum
        ENDIF  
        IF(CARDNUM.EQ.'C09') THEN
          DO I=1,17
          	READ(1,*)
          ENDDO		
          READ(1,*) KeTSS, KeChl, CChlc, CChld, CChlg
        ENDIF  
        IF(CARDNUM.EQ.'C20') THEN
          DO I=1,16
          	READ(1,*)
          ENDDO			
          READ(1,*) dum, dum, dum, dum, dum, dum, dum, dum, KPO4P
        ENDIF	
        IF(CARDNUM.EQ.'C21') THEN
          DO I=1,11
          	READ(1,*)
          ENDDO		
          READ(1,*) dum, dum, dum, dum, dum, dum, CPprm1, CPprm2, CPprm3
        ENDIF  
        IF(CARDNUM.EQ.'C24') THEN
          DO I=1,15
          	READ(1,*)
          ENDDO		
          READ(1,*) dum, dum, dum, dum, dum, dum, dum, dum, ANCc, ANCd, ANCg, ANCm
        ENDIF  
        IF(CARDNUM.EQ.'C45') THEN
          DO I=1,17
          	READ(1,*)
          ENDDO		
          READ(1,*) PMC, PMD, PMG, dum, dum, dum, dum, dum, dum, dum, dum, dum, dum
        ENDIF  
      GOTO 2001
2009  CONTINUE  
      rC1=CChlc
      rC2=CChld
      rC3=CChlg
      rP1=CChlc/CPprm1
      rP2=CChld/CPprm1
      rP3=CChlg/CPprm1
      rN1=rP1*(ANCc*CPprm1)
      rN2=rP2*(ANCd*CPprm1)
      rN3=rP3*(ANCg*CPprm1)
ENDIF
CLOSE(1)
!######################################################
!# READ WQ3DWC.INP ---- END
!######################################################

!######################################################
!# READ EVENT_TOX2.INP ---- START
!######################################################
IF(IOFILE.EQ.11) THEN
OPEN(1,FILE='../EVENT_TOX2.INP',STATUS='OLD')
  READ(1,*) YY, IM, ID, IH 
ENDIF
CLOSE(1)
!######################################################
!# READ EVENT_TOX2.INP ---- END
!######################################################

!######################################################
!# READ CORNERS.INP ---- START
!######################################################
IF(IOFILE.EQ.11) THEN
OPEN(1,FILE='../CORNERS.INP',STATUS='OLD')
  READ(1,*)
  READ(1,*)
  DO L=2,LDM+1
    READ(1,*) idum, idum, ((XX1(L,J), YY1(L,J)), J=1,4)
  ENDDO
ENDIF
CLOSE(1)
!######################################################
!# READ CORNERS.INP ---- END
!######################################################
!}

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CHECK ASER.INP
IF (NASER.gt.0) then
      OPEN(1,FILE='../ASER.INP',STATUS='OLD')  
      DO N=1,NASER  
   10   READ(1,*,ERR=10,END=40)M,R,R,I,R,R,R,R  
        READ(1,*,END=40)I,R,R,R,R,R,R,TBEDIT,R,R  
!        NDASER=MAX(NDASER,M)  
        DO I=1,M  
          READ(1,*,END=40)R,R,R,R,R,R,R,R  
        ENDDO  
      ENDDO  

   40 CONTINUE
      CLOSE(1)  
endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CHECK ASER.INP

!######################################################
!# INITAILIZATION                            ---- START
!######################################################
OPEN(6,FILE='../EE_VEL.OUT',STATUS='OLD',FORM='BINARY')
READ(6)VER,IC,JC,KC,LINES; CLOSE(6)

ICM=IC+1; JCM=JC+1; KCM=KC+1
LVC=LINES; LA=LINES+1
LC=LVC+2; LCM=LVC+3

ALLOCATE( IJCT(ICM,JCM) )
ALLOCATE( IL(LCM),JL(LCM),LNC(LCM),LIJ(ICM,JCM) )
ALLOCATE( DLON(LCM),DLAT(LCM),STCUV(LCM) )
ALLOCATE( CUE(LCM),CVE(LCM),CUN(LCM),CVN(LCM) )
ALLOCATE( BELV(LCM),HP(LCM),BELV1(LCM) )
ALLOCATE( VELE(KCM),VELN(KCM),SPD(KCM),ANG(KCM) )

CALL ALLOCATE_VAR_INITIALIZE

CALL READ_CELL_INP

CALL READ_LXLY_INP

CALL READ_DXDY_INP
!######################################################
!# INITAILIZATION                            ---- END
!######################################################


SELECT CASE(IOFILE)
  CASE(1)
    WRITE(*,*) 'VELOCITY'
    CALL VELOCITY

  CASE(2)
    WRITE(*,*) 'ELEVATION'
    CALL ELEVATION

  CASE(3)
    WRITE(*,*) 'SALINITY'
    CALL CONC

  CASE(4)
    WRITE(*,*) 'TEMPERATURE'
    CALL CONC

  CASE(5)
    WRITE(*,*) 'DYE'
    CALL CONC

  CASE(6)
    WRITE(*,*) 'SFL'
    CALL CONC

  CASE(7)
    WRITE(*,*) 'TOX'
    CALL CONC

  CASE(8)
    WRITE(*,*) 'SED'
    CALL CONC

  CASE(9)
    WRITE(*,*) 'SND'
    CALL CONC

  CASE(10)
    WRITE(*,*) 'Water Quality'
    CALL WAQ
    
!{ 140317, JHLEE
  CASE(11)
    WRITE(*,*) 'CONVERT TECPLOT'
    CALL TECPLOT
!}    
!{ 141010, JHLEE
  CASE(12)
    WRITE(*,*) 'Limitation'
    CALL LIM
!}    

ENDSELECT

CONTAINS

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE ALLOCATE_VAR_INITIALIZE
IMPLICIT NONE

 IJCT=0;
 IL=0; JL=0; LNC=0; LIJ=0
 DLON=0.; DLAT=0.; STCUV=1.
 CUE=0.; CVE=0.; CUN=0.; CVN=0.
 BELV=0.; HP=0.
 VELE=0.; VELN=0.; SPD=0.; ANG=0.;
! {JHLEE, 20141010
  NAVG=0 
! }
ENDSUBROUTINE

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE READ_CELL_INP
IMPLICIT NONE
 
 INTEGER :: IACROSS,IT,IFIRST,ILAST

 OPEN(2,FILE='../CELL.INP',STATUS='OLD')
 
 DO K=1,4
    READ(2,*)
 ENDDO
 IF( IC.GT.120 ) THEN
   IACROSS=120
   DO IT=1,IC,IACROSS
      IFIRST=IT
	  ILAST=IT+IACROSS-1
	  IF( ILAST.GT.IC ) ILAST=IC

	  DO J=JC,1,-1
	     READ(2,'(5X,120I1)',IOSTAT=ISO) ( IJCT(I,J),I=IFIRST,ILAST )
         IF( ISO.GT.0 ) THEN
		   PRINT *,"READ ERROR FOR FILE CELL.INP"
		   STOP
		 ENDIF
	  ENDDO 
   ENDDO !-IT LOOP
 ELSE
   IFIRST=1
   ILAST=IC
   DO J=JC,1,-1
      READ(2,'(5X,120I1)',IOSTAT=ISO) ( IJCT(I,J),I=IFIRST,ILAST )
	  IF( ISO.GT.0 ) THEN
	    PRINT *,"READ ERROR FOR FILE CELL.INP"
	    STOP
	  ENDIF
   ENDDO !-J LOOP
 ENDIF

 CLOSE(2)

ENDSUBROUTINE !SUBROUTINE READ_CELL_INP

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE READ_LXLY_INP
IMPLICIT NONE
  
  INTEGER,ALLOCATABLE :: LCT(:)
  
  REAL :: XLNUTME,YLTUTMN,CCUE,CCVE,CCUN,CCVN,TMPVAL
  REAL :: ANG1,ANG2,ANG,TA1,TA2

  ALLOCATE( LCT(LCM) ); LCT=0

  OPEN(3,FILE='../LXLY.INP',STATUS='OLD')
  
  DO K=1,4
     READ(3,*)
  ENDDO
  L=2
  DO J=1,JC
     DO I=1,IC
        IF( IJCT(I,J).GT.0 .AND. IJCT(I,J).LT.9 ) THEN
          IL(L)=I
		  JL(L)=J
		  LCT(L)=IJCT(I,J)
		  LIJ(I,J)=L
		  L=L+1
		ENDIF
     ENDDO
  ENDDO

!  DO WHILE( .NOT. EOF(3) )
!     READ(3,*,IOSTAT=ISO)I,J,XLNUTME,YLTUTMN,CCUE,CCVE,CCUN,CCVN,TMPVAL
301   READ(3,*,IOSTAT=ISO,END=309)I,J,XLNUTME,YLTUTMN,CCUE,CCVE,CCUN,CCVN,TMPVAL
	 IF( ISO.GT.0 ) THEN
       PRINT *,"READ ERROR FOR FILE LXLY.INP"
       STOP
     ENDIF
	 L=LIJ(I,J)
	 DLON(L)=XLNUTME
	 DLAT(L)=YLTUTMN

	 ANG1=ATAN2(CCUN,CCUE)
	 ANG2=ATAN2(-CCVE,CCVN)
	 ANG=0.5*(ANG1+ANG2)

	 TA1=SIGN(1.,ANG1)
	 TA2=SIGN(1.,ANG2)
	 IF( SIGN(1.,ANG1).NE.SIGN(1.,ANG2) ) THEN
       IF(ABS(ANG1).GT.(1.57).OR.ABS(ANG2).GT.(1.57)) THEN
	     ANG = ANG + ACOS(-1.0)
       ENDIF
     ENDIF
     CUE(L)=COS(ANG)
     CVE(L)=-SIN(ANG)
     CUN(L)=SIN(ANG)
     CVN(L)=COS(ANG)
     goto 301
309  continue
!  ENDDO
  CLOSE(3)

  LNC(1)=LC; LNC(LC)=1
  DO L=2,LA
     IF( LCT(L).EQ.1 .OR. LCT(L).EQ.2 .OR. &
       LCT(L).EQ.3 .OR. LCT(L).EQ.4 ) THEN
       STCUV(L)=0.
     ENDIF

     I=IL(L)
     J=JL(L)
     IF( IJCT(I,J+1).EQ.9 ) THEN
       LNC(L)=LC
     ELSE
       LNC(L)=LIJ(I,J+1)
     ENDIF
  ENDDO

ENDSUBROUTINE !SUBROUTINE READ_LXLY_INP

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE READ_DXDY_INP
IMPLICIT NONE

  REAL :: DXIJ,DYIJ,HIJ,BELVIJ

  OPEN(4,FILE='../DXDY.INP',STATUS='OLD')
  DO I=1,4
     READ(4,*)
  ENDDO
!  DO WHILE( .NOT. EOF(4) )
!     READ(4,*,IOSTAT=ISO) I,J,DXIJ,DYIJ,HIJ,BELVIJ
401  READ(4,*,IOSTAT=ISO,end=409) I,J,DXIJ,DYIJ,HIJ,BELVIJ
     IF( ISO.GT.0 ) THEN
       PRINT *,"READ ERROR FOR FILE DXDY.INP"
       STOP
     ENDIF
	 L=LIJ(I,J)
	 BELV(L)=BELADJ + BELCVRT*BELVIJ
     BELV1(L)=BELADJ + BELCVRT*BELVIJ 
     goto 401
409  continue
!  ENDDO
  CLOSE(4)
ENDSUBROUTINE !SUBROUTINE READ_DXDY_INP

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE VELOCITY
IMPLICIT NONE

  INTEGER :: CLENG,LN,Nper,IAVG
  REAL :: DELT
  REAL,ALLOCATABLE :: RSSBCE(:),RSSBCW(:),RSSBCS(:),RSSBCN(:)
  REAL,ALLOCATABLE :: U(:,:),V(:,:),W(:,:)
  REAL,ALLOCATABLE :: MAXSPD(:,:),MAXDIR(:,:),MAXASPD(:),MAXADIR(:)
  REAL,ALLOCATABLE :: AVGU(:,:),AVGV(:,:),RSPD(:,:),RDIR(:,:)
  CHARACTER(LEN=100) :: TFNAME,FNAME
  REAL :: UTMPS,VTMPS,UTMPB,VTMPB
  REAL :: SUMVELE,SUMVELN

  ALLOCATE( RSSBCE(LCM),RSSBCW(LCM),RSSBCS(LCM),RSSBCN(LCM) )
  ALLOCATE( U(LCM,KCM),V(LCM,KCM),W(LCM,0:KCM) )
  ALLOCATE( MAXSPD(LCM,KCM),MAXDIR(LCM,KCM),MAXASPD(LCM),MAXADIR(LCM) )
  ALLOCATE( AVGU(LCM,KCM),AVGV(LCM,KCM),RSPD(LCM,KCM),RDIR(LCM,KCM) )
  RSSBCE=0.; RSSBCW=0.; RSSBCS=0.; RSSBCN=0. 
  U=0.; V=0.; W=0.
  MAXSPD=-9999.; MAXDIR=0.; MAXASPD=-9999.; MAXADIR=0.
  AVGU=0.; AVGV=0.
  IAVG=0

  OPEN(6,FILE='../EE_VEL.OUT',STATUS='OLD',FORM='BINARY')
  READ(6)VER,IC,JC,KC,LINES
  READ(6)RSSBCE,RSSBCW,RSSBCS,RSSBCN

  IF( NTSR.GT.0 ) THEN
    DO I=1,NTSR

       IF (IJCT(ITSR(I),JTSR(I)).ne.5) then
         WRITE(*,*) 'NOT WATER CELL:',ITSR(I),JTSR(I)
         STOP
       ENDIF

	   WRITE(TFNAME,"('TIME_VEL',I5.5,'_',I5.5,'.DAT')")ITSR(I),JTSR(I)
 	   OPEN(100+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
	   PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
	   IF( KC.GT.1 ) THEN
         WRITE(100+I,9101)  &
		               'TIME','N',('SPD',K,'DIR',K,K=KC,1,-1),'SPDA','DIRA'  &
                       ,('VE',K,'VN',K,K=KC,1,-1),'VEA','VNA'
       ELSEIF( KC.EQ.1 ) THEN
	     WRITE(100+I,'(3X,A4,9X,A1,6X,A5,3X,A3,5X,A4,5X,A4)')'TIME','N','SPEED','DIR','VELE','VELN'
       ENDIF
	ENDDO
  ENDIF
9101 FORMAT(3X,A4,9X,A1,6X,<KC*2>(A3,I2.2,3X),1X,A4,4X,A4,6X,<KC*2>(A2,I2.2,4X),2(A3,6X))
  PRINT *," "

  PRINT '(3X,A4,1X,A10)',"TIME","FILENAME :"
  PRINT '(3X,A5)',"[DAY]"
!  DO WHILE( .NOT. EOF(6))

     Nper=N     
!	 READ(6)N,TIME,DELT
601	 READ(6,end=609)N,TIME,DELT
     IF (TIME.GT.EDAY) goto 609 !STOP !EXIT

     IAVG=IAVG+1

     DO L=2,LA
        READ(6) ( U(L,K),V(L,K),W(L,K),K=1,KC )
     ENDDO
     	 
     IF( TIME.GE.SDAY.AND.TIME.LE.EDAY ) THEN
	   WRITE(*,*)N,TIME,DELT
       IF (IHOR.EQ.1) THEN
         WRITE(FNAME,"('V',I8.8,'.DAT')")N
   	     OPEN(21,FILE=TRIM(FNAME),STATUS='UNKNOWN')
	     CLENG=LEN(TRIM(FNAME))
!	     PRINT '(F7.2,1X,A<CLENG>)',TIME,TRIM(FNAME)
	   
  	     IF( KC.GT.1 ) THEN
           WRITE(21,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,<kc*2>(a4,i2.2,a1,2x),A7,2X,A7)')  &
		           'I','J','X','Y', &
				   ('SPD(',K,')','DIR(',K,')',K=KC,1,-1), &
				   'SPD(AV)','DIR(AV)'   
	     ELSEIF( KC.EQ.1 ) THEN	   
	       WRITE(21,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,A5,3X,A3)')'I','J','X','Y','SPEED','DIR'
	     ENDIF
       ENDIF

	   DO L=2,LA
          LN=LNC(L)

		  SUMVELE=0.; SUMVELN=0.
		  DO K=1,KC
             UTMPS=50.*STCUV(L)*( RSSBCE(L)*U(L+1,K)+RSSBCW(L)*U(L,K) )
		     VTMPS=50.*STCUV(L)*( RSSBCN(L)*V(LN ,K)+RSSBCS(L)*V(L,K) )

		     VELE(K)=CUE(L)*UTMPS + CVE(L)*VTMPS
		     VELN(K)=CUN(L)*UTMPS + CVN(L)*VTMPS

		     CALL VELOCITY_TO_SPEED( VELE(K),VELN(K),SPD(K),ANG(K) )

             IF ( SPD(K).GE.MAXSPD(L,K) ) THEN
		       MAXSPD(L,K)=SPD(K)
			   MAXDIR(L,K)=ANG(K)
		     ENDIF

		     SUMVELE=SUMVELE+VELE(K)
		     SUMVELN=SUMVELN+VELN(K)
          
		     AVGU(L,K)=AVGU(L,K)+VELE(K)
			 AVGV(L,K)=AVGV(L,K)+VELN(K)

		  ENDDO
		  AVEVELE=SUMVELE/REAL(KC)
		  AVEVELN=SUMVELN/REAL(KC)
		  CALL VELOCITY_TO_SPEED( AVEVELE,AVEVELN,AVESPD,AVEANG )

          IF ( AVESPD.GE.MAXASPD(L) ) THEN
		     MAXASPD(L)=AVESPD
		     MAXADIR(L)=AVEANG
          ENDIF

		  IF (IHOR.EQ.1) CALL HORIZONTAL_VEL_OUT

		  IF( NTSR.GT.0 ) THEN
		    CALL TIMESERIES_VEL_OUT
  		  ENDIF
	   ENDDO !DO L=2,LA
     ENDIF !     IF( TIME.GE.SDAY.AND.TIME.LE.EDAY .AND. IHOR.EQ.1 ) THEN
	 CLOSE(21)

     Nper=N
     goto 601
609  continue
!  ENDDO
  CLOSE(6);

  WRITE(FNAME,'(A,I8.8,A)')'MAXVEL',Nper,'.DAT'
  WRITE(*,*) TRIM(FNAME)
  OPEN(1001,FILE=TRIM(FNAME),STATUS='UNKNOWN')
  IF( KC.GT.1 ) THEN
    WRITE(1001,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,<kc*2>(a4,i2.2,a1,2x),A7,2X,A7)')  &
		           'I','J','X','Y', &
				   ('SPD(',K,')','DIR(',K,')',K=KC,1,-1), &
				   'SPD(AV)','DIR(AV)'   
  ELSEIF( KC.EQ.1 ) THEN	   
    WRITE(1001,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,A5,3X,A3)')'I','J','X','Y','SPEED','DIR'
  ENDIF

  DO L=2,LA
    IF( KC.GT.1 ) THEN  
	  WRITE(1001,'(2I5,2F11.2,1x,<KC*2+2>(F9.2))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                  (MAXSPD(L,K),MAXDIR(L,K),K=KC,1,-1), &
												  MAXASPD(L),MAXADIR(L)
    ELSEIF( KC.EQ.1 ) THEN
      WRITE(1001,'(2I5,1X,2(F10.2,1X),2(F7.2,1X))') IL(L),JL(L),DLON(L),DLAT(L),MAXSPD(L,KC),MAXDIR(L,KC)
    ENDIF
  ENDDO
  CLOSE(1001)

  WRITE(FNAME,'(A,I8.8,A)')'AVGVEL',Nper,'.DAT'
  WRITE(*,*) TRIM(FNAME)
  OPEN(1001,FILE=TRIM(FNAME),STATUS='UNKNOWN')
  WRITE(1001,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,<KC*2>(a4,i2.2,a1,2x))')  &
		           'I','J','X','Y', &
				   ('AVU(',K,')','AVV(',K,')',K=KC,1,-1)
  
  WRITE(FNAME,'(A,I8.8,A)')'AVGSPD',Nper,'.DAT'
  WRITE(*,*) TRIM(FNAME)
  OPEN(1002,FILE=TRIM(FNAME),STATUS='UNKNOWN')
  WRITE(1002,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,<KC*2>(a4,i2.2,a1,2x))')  &
		           'I','J','X','Y', &
				   ('SPD(',K,')','DIR(',K,')',K=KC,1,-1)

  DO L=2,LA
    DO K=1,KC
      AVGU(L,K)=AVGU(L,K)/FLOAT(IAVG)
      AVGV(L,K)=AVGV(L,K)/FLOAT(IAVG)
      RSPD(L,K)=SQRT(AVGU(L,K)*AVGU(L,K) + AVGV(L,K)*AVGV(L,K))
      RDIR(L,K)=ATAN2(AVGV(L,K),AVGU(L,K))*180./3.14159
    ENDDO
    WRITE(1001,'(2I5,2F11.2,1x,<KC*2>(F9.2))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                  (AVGU(L,K),AVGV(L,K),K=KC,1,-1)
    WRITE(1002,'(2I5,2F11.2,1x,<KC*2>(F9.2))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                  (RSPD(L,K),RDIR(L,K),K=KC,1,-1)
  ENDDO
  CLOSE(1001)
  CLOSE(1002)

ENDSUBROUTINE !SUBROUTINE VELOCITY

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE HORIZONTAL_VEL_OUT
IMPLICIT NONE
  
  IF( TIME.GE.SDAY.AND.TIME.LE.EDAY ) THEN
	IF( KC.GT.1 ) THEN  
	  WRITE(21,'(2I5,2F11.2,1x,<KC*2+2>(F9.2))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                  (SPD(K),ANG(K),K=KC,1,-1), &
												  AVESPD,AVEANG
    ELSEIF( KC.EQ.1 ) THEN
      WRITE(21,'(2I5,1X,2(F10.2,1X),2(F7.2,1X))') IL(L),JL(L),DLON(L),DLAT(L),SPD(1),ANG(1)
    ENDIF

  ENDIF
  
ENDSUBROUTINE !SUBROUTINE HORIZONTAL_VEL_OUT

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE TIMESERIES_VEL_OUT
IMPLICIT NONE

  DO I=1,NTSR
     IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
	     
       WRITE(100+I,9201)  &
	                   TIME,N,(SPD(K),ANG(K),K=KC,1,-1),AVESPD,AVEANG,(VELE(K),VELN(K),K=KC,1,-1),AVEVELE,AVEVELN

     ENDIF
  ENDDO

9201 FORMAT(F11.7,1X,I8,1X,<KC*2+2>(F7.2,1X),2X,<KC*2+2>(F7.2,1X))

ENDSUBROUTINE !SUBROUTINE TIMESERIES_VEL_OUT

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE VELOCITY_TO_SPEED(VELE,VELN,SPD,ANG)
IMPLICIT NONE

  REAL,INTENT(IN) :: VELE,VELN
  REAL,INTENT(IN OUT) :: SPD,ANG

  SPD= SQRT( VELE*VELE + VELN*VELN )
  IF( ABS(VELE).LE.1.0E-16 .AND. ABS(VELN).LE.1.0E-16 ) THEN
    ANG=0.
  ELSE
    ANG=ATAN2(VELN,VELE)/3.141592*180.
  ENDIF

ENDSUBROUTINE !SUBROUTINE VELOCITY_TO_SPEED(VELE,VELN,SPD,ANG)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE ELEVATION
IMPLICIT NONE
  
  REAL :: DELT
  CHARACTER(LEN=100) :: TFNAME,FNAME
  INTEGER :: CLENG, Nper
  REAL,ALLOCATABLE :: MAXEL(:),MINEL(:)
  ALLOCATE( MAXEL(LCM),MINEL(LCM) )
  MAXEL=-9999.; MINEL=9999.


  OPEN(7,FILE='../EE_WS.OUT',STATUS='OLD',FORM='BINARY')

  IF( NTSR.GT.0 ) THEN
    DO I=1,NTSR
       
	   IF (IJCT(ITSR(I),JTSR(I)).ne.5) then
         WRITE(*,*) 'NOT WATER CELL:',ITSR(I),JTSR(I)
         STOP
       ENDIF
	   
	   WRITE(TFNAME,'("TIME_EL",I5.5,"_",I5.5,".DAT")')ITSR(I),JTSR(I)
 	   OPEN(200+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
	   PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
	   WRITE(200+I,'(3X,A4,8X,A5,6X,A1)')'TIME','EL(M)','N'
	ENDDO
  ENDIF
  PRINT *," "

  PRINT '(3X,A4,1X,A10)',"TIME","FILENAME :"
  PRINT '(3X,A5)',"[DAY]"

  READ(7)VER,IC,JC,LINES
!  DO WHILE( .NOT. EOF(7) )

     Nper=N
!    READ(7)N,TIME,DELT
701  READ(7,end=709)N,TIME,DELT

     IF (TIME.GT.EDAY) goto 709 !STOP !EXIT

	 DO L=2,LA
	    READ(7)HP(L)
	 ENDDO

     IF( TIME.GE.SDAY.AND.TIME.LE.EDAY ) THEN
       
	   WRITE(*,*)N,TIME,DELT
	   
	   IF (IHOR.EQ.1) THEN
	     WRITE(FNAME,'("ELE",I8.8,".DAT")')N
   	     OPEN(22,FILE=TRIM(FNAME),STATUS='UNKNOWN')
	     CLENG=LEN(TRIM(FNAME))
!	     PRINT '(F7.2,1X,A<CLENG>)',TIME,TRIM(FNAME)
	     WRITE(22,'(3X,A1,3X,A1,7X,A1,13X,A1,9X,A2)')"I","J","X","Y","EL"
       ENDIF
     ENDIF
     
     IF( TIME.GE.SDAY.AND.TIME.LE.EDAY ) THEN
       DO L=2,LA
	     SURFEL=BELV(L)+HP(L)
         IF (SURFEL.GE.MAXEL(L)) MAXEL(L)=SURFEL
         IF (SURFEL.LE.MINEL(L)) MINEL(L)=SURFEL

	 	IF (IHOR.EQ.1) CALL ELE_OUT
		
		 IF( NTSR.GT.0 ) THEN
		   CALL TIMESERIES_ELE_OUT
		 ENDIF
	   ENDDO
	 ENDIF		
	 CLOSE(22)

     Nper=N
     goto 701
709  continue
!  ENDDO
  CLOSE(7);

  WRITE(FNAME,'(A,I8.8,A)')'MAXELE',Nper,'.DAT'
  WRITE(*,*) TRIM(FNAME)
  OPEN(1001,FILE=TRIM(FNAME),STATUS='UNKNOWN')
  WRITE(1001,'(3X,A1,3X,A1,7X,A1,13X,A1,6X,A5,4X,A5)')'I','J','X','Y','MAXEL','MINEL'
  DO L=2,LA
    WRITE(1001,'(2I4,2F12.2,2F9.3)')IL(L),JL(L),DLON(L),DLAT(L),MAXEL(L),MINEL(L)
  ENDDO
  CLOSE(1001)

ENDSUBROUTINE !SUBROUTINE ELEVATION

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE ELE_OUT
IMPLICIT NONE
  
  IF( TIME.GE.SDAY.AND.TIME.LE.EDAY ) THEN
  	WRITE(22,'(2I4,2(F13.2,1X),F8.3)')IL(L),JL(L),DLON(L),DLAT(L),SURFEL
  ENDIF

ENDSUBROUTINE !SUBROUTINE ELE_OUT

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE TIMESERIES_ELE_OUT
IMPLICIT NONE
  
  DO I=1,NTSR
     IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN

	   IF( KC.GT.1 ) THEN  
	     WRITE(200+I,'(F11.7,1X,F8.3,1X,I8)')TIME,SURFEL,N
	   ELSEIF( KC.EQ.1 ) THEN
	     WRITE(200+I,'(F11.7,1X,F8.3,1X,I8)')TIME,SURFEL,N
	   ENDIF

	 ENDIF
  ENDDO

ENDSUBROUTINE !SUBROUTINE TIMESERIES_ELE_OUT

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE CONC
IMPLICIT NONE

  INTEGER :: ISTRAN(7),NSED,NSND,KB,KC,NTOX,NSXD,NS,KBM,NT,NX
  INTEGER :: NSCM,NSNM,NTXM,NSTM
  INTEGER :: NACTIVE,CLENG,LN,Nper
  INTEGER :: N1,NSEDFLUME,ICONC
  CHARACTER(LEN=100) :: FNAME
  REAL :: EETIME,SHEAR,DELT
  REAL,ALLOCATABLE :: KBT(:),SEDDIA(:),SAL(:,:),TEM(:,:),TEMB(:)
  REAL,ALLOCATABLE :: TAUBSED(:),TAUBSND(:),TAUB(:),WVWHA(:),WVFRQL(:),WACCWE(:)
  REAL,ALLOCATABLE :: DYE(:,:),SFL(:,:),TOXB(:,:,:),TOX(:,:,:)


  REAL,ALLOCATABLE :: BELV(:),HBED(:,:),BDENBED(:,:),PORBED(:,:)
  REAL,ALLOCATABLE :: SEDB(:,:,:),VFRBED(:,:,:),SED(:,:,:)
  REAL,ALLOCATABLE :: SNDB(:,:,:),SND(:,:,:)
! { 20141111, JHLEE, TSS print
  REAL,ALLOCATABLE :: SEDT(:,:)
! }  
  REAL,ALLOCATABLE :: CQBEDLOADX(:,:),CQBEDLOADY(:,:)


  REAL,ALLOCATABLE :: MAXSAL(:,:),MINSAL(:,:),MAXTEM(:,:),MINTEM(:,:),ASAL(:,:),ATEM(:,:)
  REAL,ALLOCATABLE :: RSSBCE(:),RSSBCW(:),RSSBCS(:),RSSBCN(:)

  ALLOCATE( RSSBCE(LCM),RSSBCW(LCM),RSSBCS(LCM),RSSBCN(LCM) )
  ALLOCATE( SAL(LCM,KCM),TEM(LCM,KCM),KBT(LCM),TEMB(LCM) )
  ALLOCATE( TAUBSED(LCM),TAUBSND(LCM),TAUB(LCM),WVWHA(LCM),WVFRQL(LCM),WACCWE(LCM) )
  ALLOCATE( DYE(LCM,KCM),SFL(LCM,KCM) )
  ALLOCATE( BELV(LCM) )
  ALLOCATE( MAXSAL(LCM,KCM),MINSAL(LCM,KCM),MAXTEM(LCM,KCM),MINTEM(LCM,KCM),ASAL(LCM,KCM),ATEM(LCM,KCM) )

  MAXSAL=-9999.;MINSAL=9999.;MAXTEM=-9999.;MINTEM=9999.;ASAL=0.;ICONC=0

  OPEN(6,FILE='../EE_WS.OUT',STATUS='OLD',FORM='BINARY')
  READ(6)VER,IC,JC,LINES
  READ(6)N,TIME,DELT
  CLOSE(6)
  N=0

  OPEN(6,FILE='../EE_WC.OUT',STATUS='OLD',FORM='BINARY')
  READ(6)VER
  READ(6)ISTRAN(1),ISTRAN(2),ISTRAN(3),ISTRAN(4)
  READ(6)ISTRAN(5),ISTRAN(6),ISTRAN(7)
  READ(6)NSED,NSND,KB,KC,NTOX

  IF (ISTRAN(IOFILE-2).NE.1) THEN
    WRITE(*,*) 'There is not exist result file'
    STOP
  ENDIF

  NSXD=NSED+NSND
  KBM=MAX(1,KB)
  NSCM=MAX(1,NSED)
  NSNM=MAX(1,NSND)
  NTXM=MAX(1,NTOX)
  NSTM=MAX(3,NSCM+NSNM+NTXM)

  ALLOCATE( SEDDIA(NSTM) )
  ALLOCATE( TOXB(LCM,KBM,NTXM),TOX(LCM,KCM,NTXM) )
  ALLOCATE( HBED(LCM,KBM),BDENBED(LCM,KBM),PORBED(LCM,KBM) )
  ALLOCATE( SEDB(LCM,KBM,NSCM),VFRBED(LCM,KBM,NSTM),SED(LCM,KCM,NSCM) )
  ALLOCATE( SNDB(LCM,KBM,NSNM),SND(LCM,KCM,NSNM) )
  ALLOCATE( CQBEDLOADX(LCM,NSNM),CQBEDLOADY(LCM,NSNM) )
! { 20141111, JHLEE, TSS print
  ALLOCATE( SEDT(LCM,KCM) )
! }


  DO NS=1,NSXD
    READ(6)SEDDIA(NS)
  ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Ready timeseries file
  IF( NTSR.GT.0 ) THEN
    DO I=1,NTSR

       IF (IJCT(ITSR(I),JTSR(I)).ne.5) then
         WRITE(*,*) 'NOT WATER CELL:',ITSR(I),JTSR(I)
         STOP
       ENDIF

       CALL CONC_TMSR_READY(NTOX,NSED,NSND)

	ENDDO
  ENDIF !IF( NTSR.GT.0 ) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Ready timeseries file
  PRINT *," "

!  DO WHILE( .NOT. EOF(6))

    Nper=N
!    READ(6)EETIME,NACTIVE
610  READ(6,end=619)EETIME,NACTIVE
    IF (EETIME.GT.EDAY) goto 619 !STOP !EXIT
      ICONC=ICONC+1

    N=INT(86400./DELT*EETIME)

    IF( EETIME.GE.SDAY.AND.EETIME.LE.EDAY ) THEN
      WRITE(*,*)EETIME,N,NACTIVE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Ready horizontal output
        IF (IHOR.EQ.1) THEN
          CALL CONC_HORI_READY(NTOX,NSED,NSND)
	    ENDIF !IF (IHOR.EQ.1) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Ready horizontal output

      NSEDFLUME=0
      IF (IWRSP.EQ.98) THEN
	    NSEDFLUME=1
      ELSEIF (IWRSP.EQ.99) THEN
        NSEDFLUME=2
      ENDIF

	  DO L=2,LA
        N1=KBT(L)

        IF(ISTRAN(6).GT.0.OR.ISTRAN(7).GT.0)THEN
          IF(ISBEDSTR.GE.1.AND.NSEDFLUME.EQ.0)THEN
            READ(6)TAUBSED(L)
            IF(ISBEDSTR.EQ.1)THEN
              READ(6)TAUBSND(L)
            ENDIF
          ELSE
            READ(6)TAUB(L)
          ENDIF
        ELSE
          READ(6)SHEAR
        ENDIF

        IF(ISWAVE.GE.1)THEN
          READ(6)SHEAR
          IF(ISWAVE.EQ.3)THEN
            READ(6)WVWHA(L),WVFRQL(L),WACCWE(L)
          ENDIF
        ENDIF

        IF(ISTRAN(1).EQ.1) READ(6)(SAL(L,K),K=1,KC)
        IF(ISTRAN(2).EQ.1) THEN
          READ(6)(TEM(L,K),K=1,KC)
          IF(TBEDIT.GT.0.)READ(6)TEMB(L)
        ENDIF
        IF(ISTRAN(3).EQ.1)READ(6)(DYE(L,K),K=1,KC)
        IF(ISTRAN(4).EQ.1)READ(6)(SFL(L,K),K=1,KC)
        IF(ISTRAN(5).EQ.1)THEN
          READ(6)(TOXB(L,N1,NT),NT=1,NTOX)
          READ(6)((TOX(L,K,NT),K=1,KC),NT=1,NTOX)
        ENDIF
        IF(ISTRAN(6).EQ.1.OR.ISTRAN(7).GE.1)THEN
          READ(6)N1,BELV(L),HBED(L,N1),BDENBED(L,N1),PORBED(L,N1)
          IF(ISTRAN(6).EQ.1)THEN
            READ(6)(SEDB(L,N1,NS),VFRBED(L,N1,NS),NS=1,NSED)
            READ(6)((SED(L,K,NS),K=1,KC),NS=1,NSED)
          ENDIF
          IF(ISTRAN(7).EQ.1)THEN
            READ(6)(SNDB(L,N1,NX),VFRBED(L,N1,NX+NSED),NX=1,NSND)
            READ(6)((SND(L,K,NX),K=1,KC),NX=1,NSND)
            IF(ISBDLDBC.GT.0)THEN
              READ(6)(CQBEDLOADX(L,NX),CQBEDLOADY(L,NX),NX=1,NSND)
            ENDIF
          ENDIF
        ENDIF

! { 20141111, JHLEE, TSS print
      IF(IOFILE.EQ.8) THEN
         DO K=1,KC
          SEDT(L,K)=0.
         ENDDO

       IF(NSED.GE.1) THEN  
        DO K=1,KC
         DO NS=1,NSED
          SEDT(L,K)=SEDT(L,K)+SED(L,K,NS)
         ENDDO
        ENDDO         
       ENDIF

       IF(NSND.GE.1) THEN           
        DO K=1,KC
         DO NX=1,NSND
          SEDT(L,K)=SEDT(L,K)+SND(L,K,NX)
         ENDDO
        ENDDO           
       ENDIF 
      ENDIF     
! } 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Write horizontal distribution
        IF (IHOR.EQ.1) THEN
          SELECT CASE(IOFILE)
            CASE(3)
              WRITE(21,'(2I5,2F11.2,1x,<KC>(F9.3))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                    (SAL(L,K),K=KC,1,-1)
            CASE(4)
              WRITE(21,'(2I5,2F11.2,1x,<KC>(F9.3))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                    (TEM(L,K),K=KC,1,-1)
            CASE(5)
              WRITE(21,'(2I5,2F11.2,1x,<KC>(F9.3))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                    (DYE(L,K),K=KC,1,-1)
            CASE(6)
              WRITE(21,'(2I5,2F11.2,1x,<KC>(F9.3))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                    (SFL(L,K),K=KC,1,-1)
            CASE(7)
              WRITE(21,'(2I5,2F11.2,1x,<KC>(F10.3))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                     ((TOX(L,K,NT),NT=1,NTOX),K=KC,1,-1)
            CASE(8)
              WRITE(21,'(2I5,2F11.2,1x,<KC>(F10.3))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                     ((SED(L,K,NS),NS=1,NSED),K=KC,1,-1)
! { 20141111, JHLEE, TSS print
              WRITE(22,'(2I5,2F11.2,1x,<KC>(F10.3))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                     (SEDT(L,K),K=KC,1,-1)                                                     
! }                                                     
            CASE(9)
              WRITE(21,'(2I5,2F11.2,1x,<KC>(F10.3))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                     ((SND(L,K,NX),NX=1,NSND),K=KC,1,-1)
          ENDSELECT
	    ENDIF !IF (IHOR.EQ.1) THEN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Write timeseries
        IF (NTSR.GT.0) THEN
          SELECT CASE(IOFILE)
            CASE(3)
              DO I=1,NTSR
                IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
                  WRITE(100+I,'(F11.7,1X,I8,<KC>(F9.3))')  &
	                       EETIME,N,(SAL(L,K),K=KC,1,-1)
                ENDIF
              ENDDO
            CASE(4)
              DO I=1,NTSR
                IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
                  WRITE(100+I,'(F11.7,1X,I8,<KC>(F9.3))')  &
	                       EETIME,N,(TEM(L,K),K=KC,1,-1)
                ENDIF
              ENDDO
            CASE(5)
              DO I=1,NTSR
                IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
                  WRITE(100+I,'(F11.7,1X,I8,<KC>(F9.3))')  &
	                       EETIME,N,(DYE(L,K),K=KC,1,-1)
                ENDIF
              ENDDO
            CASE(6)
              DO I=1,NTSR
                IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
                  WRITE(100+I,'(F11.7,1X,I8,<KC>(F9.3))')  &
	                       EETIME,N,(SFL(L,K),K=KC,1,-1)
                ENDIF
              ENDDO
            CASE(7)
              DO I=1,NTSR
                IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
                  WRITE(100+I,'(F11.7,1X,I8,<KC>(F10.3))')  &
	                       EETIME,N,((TOX(L,K,NT),NT=1,NTOX),K=KC,1,-1)
                ENDIF
              ENDDO
            CASE(8)
              DO I=1,NTSR
                IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
                  WRITE(100+I,'(F11.7,1X,I8,<KC>(F10.3))')  &
	                       EETIME,N,((SED(L,K,NS),NS=1,NSED),K=KC,1,-1)
! { 20141111, JHLEE, TSS print
	              WRITE(200+I,'(F11.7,1X,I8,<KC>(F10.3))')  &
	                       EETIME,N,(SEDT(L,K),K=KC,1,-1)
! }	                       
                ENDIF
              ENDDO
            CASE(9)
              DO I=1,NTSR
                IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
                  WRITE(100+I,'(F11.7,1X,I8,<KC>(F10.3))')  &
	                       EETIME,N,((SND(L,K,NX),NX=1,NSND),K=KC,1,-1)
                ENDIF
              ENDDO
          ENDSELECT
		ENDIF !IF (NTSR.GT.0) THEN

      ENDDO !DO L=2,LA
    ENDIF !IF( EETIME.GE.SDAY.AND.EETIME.LE.EDAY ) THEN
    CLOSE(21)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MAX & MIN CALCILATION (YSSONG, 110916)
    IF(ISTRAN(1).EQ.1)THEN
      DO L=2,LA
        DO K=1,KC
          IF (SAL(L,K).LE.MINSAL(L,K)) MINSAL(L,K)=SAL(L,K)
          IF (SAL(L,K).GE.MAXSAL(L,K)) MAXSAL(L,K)=SAL(L,K)
          ASAL(L,K)=ASAL(L,K)+SAL(L,K)
        ENDDO
      ENDDO
    ENDIF

    IF(ISTRAN(2).GE.1)THEN
      DO L=2,LA
        DO K=1,KC
          IF (TEM(L,K).LE.MINTEM(L,K)) MINTEM(L,K)=TEM(L,K)
          IF (TEM(L,K).GE.MAXTEM(L,K)) MAXTEM(L,K)=TEM(L,K)
          ATEM(L,K)=ATEM(L,K)+TEM(L,K)
        ENDDO
      ENDDO
    ENDIF

    Nper=N
    goto 610
619 continue
!  ENDDO !DO WHILE( .NOT. EOF(6))

  CLOSE(6);

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! MAX & MIN PRINT
  IF(ISTRAN(1).EQ.1)THEN
    WRITE(FNAME,'(A,I8.8,A)')'MAXSAL',Nper,'.DAT'
    WRITE(*,*) TRIM(FNAME)
    OPEN(1001,FILE=TRIM(FNAME),STATUS='UNKNOWN')
    WRITE(1001,'(3X,A1,3X,A1,7X,A1,13X,A1,6X,A5,4X,A5)')'I','J','X','Y','MAXSAL, K=KC,1,-1'
    DO L=2,LA
      WRITE(1001,'(2I4,2F12.2,11F9.3)')IL(L),JL(L),DLON(L),DLAT(L),(MAXSAL(L,K),K=KC,1,-1)
    ENDDO
    CLOSE(1001)

    WRITE(FNAME,'(A,I8.8,A)')'MINSAL',Nper,'.DAT'
    WRITE(*,*) TRIM(FNAME)
    OPEN(1001,FILE=TRIM(FNAME),STATUS='UNKNOWN')
    WRITE(1001,'(3X,A1,3X,A1,7X,A1,13X,A1,6X,A5,4X,A5)')'I','J','X','Y','MINSAL, K=KC,1,-1'
    DO L=2,LA
      WRITE(1001,'(2I4,2F12.2,11F9.3)')IL(L),JL(L),DLON(L),DLAT(L),(MINSAL(L,K),K=KC,1,-1)
    ENDDO
    CLOSE(1001)

    WRITE(FNAME,'(A,I8.8,A)')'AVGSAL',Nper,'.DAT'
    WRITE(*,*) TRIM(FNAME)
    OPEN(1001,FILE=TRIM(FNAME),STATUS='UNKNOWN')
    WRITE(1001,'(3X,A1,3X,A1,7X,A1,13X,A1,6X,A5,4X,A5)')'I','J','X','Y','AVGSAL, K=KC,1,-1'

    DO L=2,LA
      DO K=1,KC
        ASAL(L,K)=ASAL(L,K)/FLOAT(ICONC)
      ENDDO
      WRITE(1001,'(2I4,2F12.2,11F9.3)')IL(L),JL(L),DLON(L),DLAT(L),(ASAL(L,K),K=KC,1,-1)
    ENDDO
    CLOSE(1001)
  ENDIF

  IF(ISTRAN(2).GE.1)THEN
    WRITE(FNAME,'(A,I8.8,A)')'MAXTEM',Nper,'.DAT'
    WRITE(*,*) TRIM(FNAME)
    OPEN(1001,FILE=TRIM(FNAME),STATUS='UNKNOWN')
    WRITE(1001,'(3X,A1,3X,A1,7X,A1,13X,A1,6X,A5,4X,A5)')'I','J','X','Y','MAXTEM, K=KC,1,-1'
    DO L=2,LA
      WRITE(1001,'(2I4,2F12.2,11F9.3)')IL(L),JL(L),DLON(L),DLAT(L),(MAXTEM(L,K),K=KC,1,-1)
    ENDDO
    CLOSE(1001)

    WRITE(FNAME,'(A,I8.8,A)')'MINTEM',Nper,'.DAT'
    WRITE(*,*) TRIM(FNAME)
    OPEN(1001,FILE=TRIM(FNAME),STATUS='UNKNOWN')
    WRITE(1001,'(3X,A1,3X,A1,7X,A1,13X,A1,6X,A5,4X,A5)')'I','J','X','Y','MINTEM, K=KC,1,-1'
    DO L=2,LA
      WRITE(1001,'(2I4,2F12.2,11F9.3)')IL(L),JL(L),DLON(L),DLAT(L),(MINTEM(L,K),K=KC,1,-1)
    ENDDO
    CLOSE(1001)

    WRITE(FNAME,'(A,I8.8,A)')'AVGTEM',Nper,'.DAT'
    WRITE(*,*) TRIM(FNAME)
    OPEN(1001,FILE=TRIM(FNAME),STATUS='UNKNOWN')
    WRITE(1001,'(3X,A1,3X,A1,7X,A1,13X,A1,6X,A5,4X,A5)')'I','J','X','Y','AVGTEM, K=KC,1,-1'

    DO L=2,LA
      DO K=1,KC
        ATEM(L,K)=ATEM(L,K)/FLOAT(ICONC)
      ENDDO
      WRITE(1001,'(2I4,2F12.2,11F9.3)')IL(L),JL(L),DLON(L),DLAT(L),(ATEM(L,K),K=KC,1,-1)
    ENDDO
    CLOSE(1001)
  ENDIF



ENDSUBROUTINE !SUBROUTINE CONC

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE CONC_TMSR_READY(NTOX,NSED,NSND)
IMPLICIT NONE

  CHARACTER(LEN=100) :: FHEAD,TFNAME
! { 20141111, JHLEE, TSS print
  CHARACTER(LEN=100) :: FHEAD1,TFNAME1  
! }  
  INTEGER NTOX,NSED,NSND
  INTEGER NT,NS,NX

  SELECT CASE(IOFILE)
    CASE(3)
      FHEAD='SAL'
    CASE(4)
      FHEAD='TEM'
    CASE(5)
      FHEAD='DYE'
    CASE(6)
      FHEAD='SFL'
    CASE(7)
      FHEAD='TOX'
    CASE(8)
      FHEAD='SED'
      FHEAD1='TSS'
    CASE(9)
      FHEAD='SND'
  ENDSELECT

  WRITE(TFNAME,'(A,A3,I5.5,A,I5.5,A)')'TIME_',TRIM(FHEAD),ITSR(I),'_',JTSR(I),'.DAT'
! { 20141111, JHLEE, TSS print
  WRITE(TFNAME1,'(A,A3,I5.5,A,I5.5,A)')'TIME_',TRIM(FHEAD1),ITSR(I),'_',JTSR(I),'.DAT'
  IF(IOFILE.EQ.8) THEN
  	OPEN(200+I,FILE=TRIM(TFNAME1),STATUS='UNKNOWN')
  ENDIF
! }  
  OPEN(100+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
  PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)

  IF (IOFILE.LE.6) THEN
    IF( KC.GT.1 ) THEN
      WRITE(100+I,'(3X,A4,9X,A1,5X,<KC>(A3,A1,I2.2,A1,2X))')  &
                  'TIME','N',(TRIM(FHEAD),'(',K,')',K=KC,1,-1)
    ELSEIF( KC.EQ.1 ) THEN
      WRITE(100+I,'(3X,A4,9X,A1,5X,A3)')'TIME','N',TRIM(FHEAD)
    ENDIF
  ELSEIF (IOFILE.GE.7 .AND. IOFILE.LE.9) THEN ! IF (IOFILE.LE.6) THEN
    SELECT CASE(IOFILE)
      CASE(7)
        IF( KC.GT.1 ) THEN
          WRITE(100+I,'(3X,A4,9X,A1,5X,<KC>(A3,I2.2,A1,I2.2,2X))')  &
                      'TIME','N',((TRIM(FHEAD),K,'_',NT=1,NTOX),K=KC,1,-1)
        ELSEIF( KC.EQ.1 ) THEN
          WRITE(100+I,'(3X,A4,9X,A1,5X,A3)')'TIME','N',TRIM(FHEAD)
        ENDIF
      CASE(8)
        IF( KC.GT.1 ) THEN
          WRITE(100+I,'(3X,A4,9X,A1,5X,<KC>(A3,I2.2,A1,I2.2,2X))')  &
                      'TIME','N',((TRIM(FHEAD),K,'_',NS,NS=1,NSED),K=KC,1,-1)
! { 20141111, JHLEE, TSS print
          WRITE(200+I,'(3X,A4,9X,A1,5X,<KC>(A3,A1,I2.2,A1,2X))')  &
                      'TIME','N',(TRIM(FHEAD1),'(',K,')',K=KC,1,-1)
! }                      
        ELSEIF( KC.EQ.1 ) THEN
          WRITE(100+I,'(3X,A4,9X,A1,5X,A3)')'TIME','N',TRIM(FHEAD)
! { 20141111, JHLEE, TSS print
          WRITE(200+I,'(3X,A4,9X,A1,5X,A3)')'TIME','N',TRIM(FHEAD1)
! }                    
        ENDIF
      CASE(9)
        IF( KC.GT.1 ) THEN
          WRITE(100+I,'(3X,A4,9X,A1,5X,<KC>(A3,I2.2,A1,I2.2,2X))')  &
                      'TIME','N',((TRIM(FHEAD),K,'_',NX,NX=1,NSND),K=KC,1,-1)
        ELSEIF( KC.EQ.1 ) THEN
          WRITE(100+I,'(3X,A4,9X,A1,5X,A3)')'TIME','N',TRIM(FHEAD)
        ENDIF
    ENDSELECT
  ENDIF !IF (IOFILE.LE.6) THEN

ENDSUBROUTINE !SUBROUTINE CONC_TMSR_READY

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE CONC_HORI_READY(NTOX,NSED,NSND)
IMPLICIT NONE

  CHARACTER(LEN=100) :: FHEAD,FNAME
! { 20141111, JHLEE, TSS print
  CHARACTER(LEN=100) :: FHEAD1,FNAME1
! }  
  INTEGER NTOX,NSED,NSND
  INTEGER NT,NS,NX

  SELECT CASE(IOFILE)
    CASE(3)
      FHEAD='SAL'
    CASE(4)
      FHEAD='TEM'
    CASE(5)
      FHEAD='DYE'
    CASE(6)
      FHEAD='SFL'
    CASE(7)
      FHEAD='TOX'
    CASE(8)
      FHEAD='SED'
      FHEAD1='TSS'
    CASE(9)
      FHEAD='SND'
  ENDSELECT

  WRITE(FNAME,'(A3,I8.8,A)') TRIM(FHEAD),N,'.DAT'
! {  
  WRITE(FNAME1,'(A3,I8.8,A)') TRIM(FHEAD1),N,'.DAT'
  IF(IOFILE.EQ.8) THEN
    OPEN(22,FILE=TRIM(FNAME1),STATUS='UNKNOWN')
  ENDIF
! }  
  OPEN(21,FILE=TRIM(FNAME),STATUS='UNKNOWN')

  IF (IOFILE.LE.6) THEN
    IF (KC.GT.1) THEN
      WRITE(21,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,<KC>(A3,A1,I2.2,A1,2X))')  &
	           'I','J','X','Y',(TRIM(FHEAD),'(',K,')',K=KC,1,-1)
    ELSEIF (KC.EQ.1) THEN
      WRITE(21,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,A3)')'I','J','X','Y',TRIM(FHEAD)
    ENDIF !IF (KC.GT.1) THEN
  ELSEIF (IOFILE.GE.7 .AND. IOFILE.LE.9) THEN !IF (IOFILE.LE.6) THEN
    SELECT CASE(IOFILE)
      CASE(7)
        IF (KC.GT.1) THEN
          WRITE(21,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,<KC>(A3,I2.2,A1,I2.2,2X))')  &
	               'I','J','X','Y',((TRIM(FHEAD),K,'_',NT,NT=1,NTOX),K=KC,1,-1)
        ELSEIF (KC.EQ.1) THEN
          WRITE(21,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,A3)')'I','J','X','Y',TRIM(FHEAD)
        ENDIF !IF (KC.GT.1) THEN
      CASE(8)
        IF (KC.GT.1) THEN
          WRITE(21,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,<KC>(A3,I2.2,A1,I2.2,2X))')  &
	               'I','J','X','Y',((TRIM(FHEAD),K,'_',NS,NS=1,NSED),K=KC,1,-1)
! { 20141111, JHLEE, TSS print
	      WRITE(22,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,<KC>(A3,A1,I2.2,A1,2X))')  &
	               'I','J','X','Y',(TRIM(FHEAD1),'(',K,')',K=KC,1,-1)
! }	               
        ELSEIF (KC.EQ.1) THEN
          WRITE(21,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,A3)')'I','J','X','Y',TRIM(FHEAD)
! { 20141111, JHLEE, TSS print
          WRITE(22,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,A3)')'I','J','X','Y',TRIM(FHEAD1)
! }          
        ENDIF !IF (KC.GT.1) THEN
      CASE(9)
        IF (KC.GT.1) THEN
          WRITE(21,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,<KC>(A3,I2.2,A1,I2.2,2X))')  &
	               'I','J','X','Y',((TRIM(FHEAD),K,'_',NX,NX=1,NSND),K=KC,1,-1)
        ELSEIF (KC.EQ.1) THEN
          WRITE(21,'(4X,A1,4X,A1,5X,A1,10X,A1,8X,A3)')'I','J','X','Y',TRIM(FHEAD)
        ENDIF !IF (KC.GT.1) THEN
    ENDSELECT
  ENDIF !IF (IOFILE.LE.6) THEN

ENDSUBROUTINE !SUBROUTINE CONC_HORI_READY

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE WAQ
IMPLICIT NONE

  CHARACTER(LEN=100) :: TFNAME,FNAME
  INTEGER :: NWQV,NW,Nper,FORLEN,itmp
! {JHLEE, 20141010
  INTEGER :: ISTRAN(7),NSED,NSND,KB,NTOX,NSXD,NS,KBM,NT,NX
  INTEGER :: NSCM,NSNM,NTXM,NSTM
  INTEGER :: NACTIVE,CLENG,LN
  INTEGER :: N1,NSEDFLUME
  INTEGER :: KCNSED,KCNSND
  REAL :: SHEAR
  REAL,ALLOCATABLE :: SED(:,:,:),SND(:,:,:),SEDT(:,:),AVG_SEDT(:,:)
  REAL,ALLOCATABLE :: BELV(:),HBED(:,:),BDENBED(:,:),PORBED(:,:)
  REAL,ALLOCATABLE :: SEDB(:,:,:),VFRBED(:,:,:)
  REAL,ALLOCATABLE :: SNDB(:,:,:)
  REAL,ALLOCATABLE :: CQBEDLOADX(:,:),CQBEDLOADY(:,:)
  REAL,ALLOCATABLE :: KBT(:),SEDDIA(:)  
  REAL,ALLOCATABLE :: TAUBSED(:),TAUBSND(:),TAUB(:),WVWHA(:),WVFRQL(:),WACCWE(:)
  REAL,ALLOCATABLE :: DYE(:,:),SFL(:,:),TOXB(:,:,:),TOX(:,:,:)
  REAL,ALLOCATABLE :: SAL(:,:),TEM(:,:),TEMB(:)
! }
  REAL :: EETIME,DELT
  REAL*4 WQ
  INTEGER,ALLOCATABLE :: ISTRWQ(:),IWQ(:)
  REAL,ALLOCATABLE :: WQV(:,:,:),WQFIVE(:,:,:)
  REAL,ALLOCATABLE :: RSSBCE(:),RSSBCW(:),RSSBCS(:),RSSBCN(:)
  ALLOCATE( RSSBCE(LCM),RSSBCW(LCM),RSSBCS(LCM),RSSBCN(LCM) )


  OPEN(6,FILE='../EE_WS.OUT',STATUS='OLD',FORM='BINARY')
  READ(6)VER,IC,JC,LINES
  READ(6)N,TIME,DELT
  CLOSE(6)
  N=0
  DELT=DTWQ

  OPEN(6,FILE='../EE_WQ.OUT',STATUS='OLD',FORM='BINARY')
  READ(6)VER
  READ(6)NWQV
  ALLOCATE( ISTRWQ(NWQV),IWQ(NWQV),WQV(LCM,KCM,NWQV) )
! {JHLEE, 20141010
  ALLOCATE( WQFIVE(LCM,KCM,9) ) ! 1=TOC, 2=CHL, 3=T-N, 4=T-P, 5=DO, 6=CYANO, 7=DIATOM, 8=Others  , 9=DIN
  ALLOCATE( SEDT(LCM,KCM) )
! }  
  READ(6)(ISTRWQ(NW),NW=1,NWQV)
  READ(6)(IWQ(NW),NW=1,NWQV)

  FORLEN=KC*NWQV

! {JHLEE, 20141010
  OPEN(7,FILE='../EE_WC.OUT',STATUS='OLD',FORM='BINARY')
  READ(7)idum
  READ(7)ISTRAN(1),ISTRAN(2),ISTRAN(3),ISTRAN(4)
  READ(7)ISTRAN(5),ISTRAN(6),ISTRAN(7)
  READ(7)NSED,NSND,KB,idum,NTOX
  
  NSXD=NSED+NSND
  KBM=MAX(1,KB)
  NSCM=MAX(1,NSED)
  NSNM=MAX(1,NSND)
  NTXM=MAX(1,NTOX)
  NSTM=MAX(3,NSCM+NSNM+NTXM)

  ALLOCATE( KBT(LCM), BELV(LCM), SEDDIA(NSTM) )
  ALLOCATE( HBED(LCM,KBM),BDENBED(LCM,KBM),PORBED(LCM,KBM) )
  ALLOCATE( SEDB(LCM,KBM,NSCM),VFRBED(LCM,KBM,NSTM),SED(LCM,KCM,NSCM) )
  ALLOCATE( SNDB(LCM,KBM,NSNM),SND(LCM,KCM,NSNM) )
  ALLOCATE( CQBEDLOADX(LCM,NSNM),CQBEDLOADY(LCM,NSNM) )
  ALLOCATE( TAUBSED(LCM),TAUBSND(LCM),TAUB(LCM),WVWHA(LCM),WVFRQL(LCM),WACCWE(LCM) )
  ALLOCATE( SAL(LCM,KCM),TEM(LCM,KCM),TEMB(LCM) )
  ALLOCATE( DYE(LCM,KCM),SFL(LCM,KCM) )

  DO NS=1,NSXD
    READ(7) SEDDIA(NS)
  ENDDO
! }

! {JHLEE, 20141010
!  IF( NTSR.GT.0 ) THEN
!    DO I=1,NTSR
! 
!      IF (IJCT(ITSR(I),JTSR(I)).ne.5) then
!        WRITE(*,*) 'NOT WATER CELL:',ITSR(I),JTSR(I)
!        STOP
!      ENDIF
! 
! 	  WRITE(TFNAME,'(A,I5.5,A,I5.5,A)')'TIME_WQ',ITSR(I),'_',JTSR(I),'.DAT'
! 	  OPEN(100+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
! 	  PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
! 	  IF( KC.GT.1 ) THEN
!        WRITE(100+I,'(3X,A4,9X,A1,7X,<FORLEN>(A2,I2.2,A1,I2.2,4X))')  &
! 		               'TIME','N',(('WQ',K,'_',NW,NW=1,NWQV),K=KC,1,-1)
!      ELSEIF( KC.EQ.1 ) THEN
!         WRITE(100+I,'(3X,A4,9X,A1,7X,<NWQV>(A2,I2.2,7X))')  &
! 		               'TIME','N',('WQ',NW,NW=1,NWQV)
!      ENDIF
! 	ENDDO
!  ENDIF !IF( NTSR.GT.0 ) THEN
  IF( NTSR.GT.0 ) THEN
    IF (IWQOUT.eq.0) then
      
      DO I=1,NTSR
        IF (IJCT(ITSR(I),JTSR(I)).ne.5) then
          WRITE(*,*) 'NOT WATER CELL:',ITSR(I),JTSR(I)
          STOP
        ENDIF
      
       WRITE(TFNAME,'(A,I5.5,A,I5.5,A)')'TIME_WQ',ITSR(I),'_',JTSR(I),'.DAT'
        OPEN(100+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
       PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
       IF( KC.GT.1 ) THEN
          WRITE(100+I,'(3X,A4,9X,A1,7X,<FORLEN>(A2,I2.2,A1,I2.2,4X))')  &
                   'TIME','N',(('WQ',K,'_',NW,NW=1,NWQV),K=KC,1,-1)
        ELSEIF( KC.EQ.1 ) THEN
          WRITE(100+I,'(3X,A4,9X,A1,7X,<NWQV>(A2,I2.2,7X))')  &
                   'TIME','N',('WQ',NW,NW=1,NWQV)
        ENDIF
     ENDDO ! DO I=1,NTSR
  
    ELSEIF (IWQOUT.eq.1) then  ! IF (IWQOUT.eq.0) then
  
      DO I=1,NTSR
        IF (IJCT(ITSR(I),JTSR(I)).ne.5) then
          WRITE(*,*) 'NOT WATER CELL:',ITSR(I),JTSR(I)
          STOP
        ENDIF
  
        call MAKEWQTSFILES
        
     ENDDO ! DO I=1,NTSR
  
  
    ENDIF ! IF (IWQOUT.eq.0) then
  ENDIF ! IF( NTSR.GT.0 ) THEN     
! }  	
  PRINT *," "

  itmp=0
!  DO WHILE( .NOT. EOF(6))
    itmp=itmp+1

    WQV=0.

    Nper=N
!    READ(6)EETIME
620    READ(6,end=629)EETIME
! {JHLEE, 20141010
    READ(7) dum,NACTIVE
! }
    IF (EETIME.GT.EDAY) goto 629 ! STOP !EXIT
	
    N=INT(86400./DELT*EETIME)

! {JHLEE, 20141010
    NSEDFLUME=0
      IF (IWRSP.EQ.98) THEN
	    NSEDFLUME=1
      ELSEIF (IWRSP.EQ.99) THEN
        NSEDFLUME=2
      ENDIF
      
    DO L=2,LA
    N1=KBT(L)
    
    IF(ISTRAN(6).GT.0.OR.ISTRAN(7).GT.0)THEN
      IF(ISBEDSTR.GE.1.AND.NSEDFLUME.EQ.0)THEN
        READ(7)TAUBSED(L)
          IF(ISBEDSTR.EQ.1)THEN
            READ(7)TAUBSND(L)
          ENDIF
      ELSE
        READ(7)TAUB(L)
      ENDIF
    ELSE
      READ(7)SHEAR
    ENDIF
    
        IF(ISWAVE.GE.1)THEN
          READ(7)SHEAR
          IF(ISWAVE.EQ.3)THEN
            READ(7)WVWHA(L),WVFRQL(L),WACCWE(L)
          ENDIF
        ENDIF

        IF(ISTRAN(1).EQ.1) READ(7)(SAL(L,K),K=1,KC)
        IF(ISTRAN(2).EQ.1) THEN
          READ(7)(TEM(L,K),K=1,KC)
          IF(TBEDIT.GT.0.)READ(7)TEMB(L)
        ENDIF
        IF(ISTRAN(3).EQ.1)READ(7)(DYE(L,K),K=1,KC)
        IF(ISTRAN(4).EQ.1)READ(7)(SFL(L,K),K=1,KC)
        IF(ISTRAN(5).EQ.1)THEN
          READ(7)(TOXB(L,N1,NT),NT=1,NTOX)
          READ(7)((TOX(L,K,NT),K=1,KC),NT=1,NTOX)
        ENDIF    
    
    IF(ISTRAN(6).EQ.1.OR.ISTRAN(7).GE.1)THEN
      READ(7)N1,BELV(L),HBED(L,N1),BDENBED(L,N1),PORBED(L,N1)
        IF(ISTRAN(6).EQ.1)THEN
          READ(7)(SEDB(L,N1,NS),VFRBED(L,N1,NS),NS=1,NSED)
          READ(7)((SED(L,K,NS),K=1,KC),NS=1,NSED)
        ENDIF
        IF(ISTRAN(7).EQ.1)THEN
          READ(7)(SNDB(L,N1,NX),VFRBED(L,N1,NX+NSED),NX=1,NSND)
          READ(7)((SND(L,K,NX),K=1,KC),NX=1,NSND)
          IF(ISBDLDBC.GT.0)THEN
            READ(7)(CQBEDLOADX(L,NX),CQBEDLOADY(L,NX),NX=1,NSND)
          ENDIF
        ENDIF
    ENDIF
    
      DO K=1,KC
        SEDT(L,K)=0.
      ENDDO

      IF(NSED.GE.1) THEN  
        DO K=1,KC
          DO NS=1,NSED
            SEDT(L,K)=SEDT(L,K)+SED(L,K,NS)
          ENDDO
        ENDDO         
      ENDIF

      IF(NSND.GE.1) THEN           
        DO K=1,KC
          DO NX=1,NSND
            SEDT(L,K)=SEDT(L,K)+SND(L,K,NX)
          ENDDO
        ENDDO           
      ENDIF    
    ENDDO
! }
	
    IF ( EETIME.GE.SDAY.AND.EETIME.LE.EDAY ) THEN
	  WRITE(*,*) EETIME,N

	  IF (IHOR.EQ.1) THEN
	  	IF(IWQOUT.EQ.0) THEN
        WRITE(FNAME,'(A,I8.8,A)')'WQ',N,'.DAT'
        OPEN(21,FILE=TRIM(FNAME),STATUS='UNKNOWN')
          IF (KC.GT.1) THEN
            WRITE(21,'(4X,A1,4X,A1,5X,A1,10X,A1,10X,<FORLEN>(A2,I2.2,A1,I2.2,4X))')  &
		               'I','J','X','Y',(('WQ',K,'_',NW,NW=1,NWQV),K=KC,1,-1)
          ELSEIF (KC.EQ.1) THEN
            WRITE(21,'(4X,A1,4X,A1,5X,A1,10X,A1,10X,<NWQV>(A2,I2.2,7X))')  &
		               'I','J','X','Y',('WQ',NW,NW=1,NWQV)
          ENDIF !IF (KC.GT.1) THEN
        ELSEIF(IWQOUT.EQ.1) THEN
!          WRITE(999,'(F11.4,2X,A3,I8.8,A4,2X,I8.8)') EETIME, 'TOC',N,'.DAT', N    
!          WRITE(998,'(F11.4,2X,A3,I8.8,A4,2X,I8.8)') EETIME, 'CHL',N,'.DAT', N    
!          WRITE(997,'(F11.4,2X,A3,I8.8,A4,2X,I8.8)') EETIME, 'T-N',N,'.DAT', N    
!          WRITE(996,'(F11.4,2X,A3,I8.8,A4,2X,I8.8)') EETIME, 'T-P',N,'.DAT', N    
!          WRITE(995,'(F11.4,2X,A3,I8.8,A4,2X,I8.8)') EETIME, 'DOX',N,'.DAT', N   
!          WRITE(994,'(F11.4,2X,A3,I8.8,A4,2X,I8.8)') EETIME, 'CHC',N,'.DAT', N
!          WRITE(993,'(F11.4,2X,A3,I8.8,A4,2X,I8.8)') EETIME, 'CHD',N,'.DAT', N
!          WRITE(992,'(F11.4,2X,A3,I8.8,A4,2X,I8.8)') EETIME, 'CHG',N,'.DAT', N
!          WRITE(991,'(F11.4,2X,A3,I8.8,A4,2X,I8.8)') EETIME, 'DIN',N,'.DAT', N
          
          CALL MAKEWQHORFILES
          
        ENDIF ! IF(IWQOUT.EQ.0) THEN
      ENDIF   ! IF (IHOR.EQ.1) THEN
	  
      DO L=2,LA
        DO K=1,KC
          DO NW=1,NWQV
            IF(IWQ(NW).GT.0)THEN
              READ(6)WQ
              WQV(L,K,NW)=WQ
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      IF (IPO4OUT.EQ.1) THEN
        DO L=2,LA
        	DO K=1,KC
            WQV(L,K,10)=(1./(1+KPO4P*SEDT(L,K)))*WQV(L,K,10)
          ENDDO
        ENDDO
      ENDIF ! IF (IPO4OUT.EQ.1) THEN

	  DO L=2,LA
        IF (IHOR.EQ.1) THEN
          IF(IWQOUT.EQ.0) THEN
            IF (KC.GT.1) THEN
	          WRITE(21,'(2I5,2F11.2,1x,<FORLEN>(E11.3))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                    ((WQV(L,K,NW),NW=1,NWQV),K=KC,1,-1)
            ELSEIF (KC.EQ.1) THEN
              WRITE(21,'(2I5,2F11.2,1x,<NWQV>(E11.3))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                    (WQV(L,KC,NW),NW=1,NWQV)
		    ENDIF
		  ELSEIF(IWQOUT.EQ.1) THEN  ! IF(IWQOUT.EQ.0) THEN
            DO I=1,9
              WRITE(210+I,'(2I5,2F11.2,1x,<KC>(E15.7))') IL(L),JL(L),DLON(L),DLAT(L),  &
                                                         (WQFIVE(L,K,I),K=KC,1,-1)
            ENDDO	
		  ENDIF  ! IF(IWQOUT.EQ.0) THEN
        ENDIF !IF (IHOR.EQ.1) THEN
      ENDDO
      
! {JHLEE, 20141010
!        IF (NTSR.GT.0) THEN
!          DO I=1,NTSR
!            IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
!              IF (KC.GT.1) THEN
!                WRITE(100+I,'(F11.7,1X,I8,<FORLEN>(E11.3))')  &
! 	                   EETIME,N,((WQV(L,K,NW),NW=1,NWQV),K=KC,1,-1)
!              ELSEIF (KC.EQ.1) THEN
!                WRITE(100+I,'(F11.7,1X,I8,<NWQV>(E11.3))')  &
! 	                   EETIME,N,(WQV(L,KC,NW),NW=1,NWQV)
!              ENDIF
!            ENDIF
! 		  ENDDO
!        ENDIF !IF (NTSR.GT.0) THEN

    IF (IWQOUT.EQ.1) THEN
      DO L=2,LA
        DO K=1,KC
	      !TOC
	      WQFIVE(L,K,1)=WQV(L,K,1)+WQV(L,K,2)+WQV(L,K,3)+WQV(L,K,4)+WQV(L,K,5)+WQV(L,K,6)
	      !CHL
	      WQFIVE(L,K,2)=((WQV(L,K,1)*(1./rC1)) + (WQV(L,K,2)*(1./rC2)) + (WQV(L,K,3)*(1./rC3)))
	      !T-N
	      WQFIVE(L,K,3)=WQV(L,K,11)+WQV(L,K,12)+WQV(L,K,13)+WQV(L,K,14)+WQV(L,K,15) &
	                    + (WQV(L,K,1)*(rN1/rC1))+(WQV(L,K,2)*(rN2/rC2))+(WQV(L,K,3)*(rN3/rC3))
	      !T-P
	      WQFIVE(L,K,4)=WQV(L,K,7)+WQV(L,K,8)+WQV(L,K,9)+WQV(L,K,10) &
	                    + (WQV(L,K,1)*(rP1/rC1))+(WQV(L,K,2)*(rP2/rC2))+(WQV(L,K,3)*(rP3/rC3))
	      WQFIVE(L,K,5)=WQV(L,K,19)
             !Cyano
	      WQFIVE(L,K,6)=((WQV(L,K,1)*(1./rC1)))
             !Diatom
	      WQFIVE(L,K,7)=((WQV(L,K,2)*(1./rC2)))
             !Others
	      WQFIVE(L,K,8)=((WQV(L,K,3)*(1./rC3)))
	         !DIN
	      WQFIVE(L,K,9)=WQV(L,K,14)+WQV(L,K,15)
	    ENDDO
	  ENDDO
    ENDIF ! IF (IWQOUT.eq.1) THEN
    
    DO L=2,LA
      IF (NTSR.GT.0) THEN
        if (IWQOUT.eq.0) then
        
          DO I=1,NTSR
            IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
              IF (KC.GT.1) THEN  
                WRITE(100+I,'(F11.7,1X,I8,<FORLEN>(E11.3))')  &
                    EETIME,N,((WQV(L,K,NW),NW=1,NWQV),K=KC,1,-1)
              ELSEIF (KC.EQ.1) THEN
                WRITE(100+I,'(F11.7,1X,I8,<NWQV>(E11.3))')  &
                    EETIME,N,(WQV(L,KC,NW),NW=1,NWQV)
              ENDIF
            ENDIF
        ENDDO
      
      ELSEIF (IWQOUT.EQ.1) THEN ! if (IWQOUT.eq.0) then
      
        DO I=1,NTSR
          IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
          	WRITE(1100+I,'(F11.7,1X,I8,<KC>(E15.7))')  &
                    EETIME,N,(WQFIVE(L,K,1),K=KC,1,-1)
          	WRITE(1200+I,'(F11.7,1X,I8,<KC>(E15.7))')  &
                    EETIME,N,(WQFIVE(L,K,2),K=KC,1,-1)
          	WRITE(1300+I,'(F11.7,1X,I8,<KC>(E15.7))')  &
                    EETIME,N,(WQFIVE(L,K,3),K=KC,1,-1)
          	WRITE(1400+I,'(F11.7,1X,I8,<KC>(E15.7))')  &
                    EETIME,N,(WQFIVE(L,K,4),K=KC,1,-1)
          	WRITE(1500+I,'(F11.7,1X,I8,<KC>(E15.7))')  &
                    EETIME,N,(WQFIVE(L,K,5),K=KC,1,-1)
            WRITE(1600+I,'(F11.7,1X,I8,<KC>(E15.7))')  &
                    EETIME,N,(WQFIVE(L,K,6),K=KC,1,-1)
            WRITE(1700+I,'(F11.7,1X,I8,<KC>(E15.7))')  &
                    EETIME,N,(WQFIVE(L,K,7),K=KC,1,-1)
            WRITE(1800+I,'(F11.7,1X,I8,<KC>(E15.7))')  &
                    EETIME,N,(WQFIVE(L,K,8),K=KC,1,-1)
            WRITE(1900+I,'(F11.7,1X,I8,<KC>(E15.7))')  &
                    EETIME,N,(WQFIVE(L,K,9),K=KC,1,-1)        
          ENDIF ! IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
        ENDDO
      
      endif
      ENDIF !IF (NTSR.GT.0) THEN    
    ENDDO
! }

      IF (IHOR.EQ.1) THEN
        CLOSE(21)
      ENDIF

    ENDIF !IF ( EETIME.GE.SDAY.AND.EETIME.LE.EDAY ) THEN


    itmp=itmp+1
    WQV=0.
    Nper=N
    goto 620
629 continue
!  ENDDO !  DO WHILE( .NOT. EOF(6))

  CLOSE(6);

ENDSUBROUTINE !SUBROUTINE WAQ

!{ 140317, JHLEE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE TECPLOT
IMPLICIT NONE
  
  INTEGER :: Nper
  INTEGER :: ISTRAN(7),NSED,NSND,KB,NTOX,NSXD,NS,KBM,NT,NX,ZN
  INTEGER :: NSCM,NSNM,NTXM,NSTM
  INTEGER :: NACTIVE,LN
  INTEGER :: N1,NSEDFLUME,ICONC
  INTEGER :: NWQV,NW,FORLEN,itmp,NN1,NN
  INTEGER :: KCNSED,KCNSND
  INTEGER, dimension(12) :: day      
  REAL :: DELT
  REAL :: EETIME,SHEAR,DAY1
! {JHLEE, 141103  
!  REAL :: UTMPS,VTMPS,UTMPB,VTMPB
  REAL :: UTMPB,VTMPB
! }  
  REAL :: SUMVELE,SUMVELN  
  REAL :: TIME1
  REAL :: TIMTMP
  REAL*4 WQ

  CHARACTER(LEN=100) :: TFNAME,FNAME,TECNAME  
  CHARACTER(LEN=75) :: LINE1
  CHARACTER(LEN=60) :: LINE2
  CHARACTER(LEN=40) :: LINE3
  CHARACTER(LEN=40) :: LINE4
  
  INTEGER,ALLOCATABLE :: ISTRWQ(:),IWQ(:)
  
  REAL,ALLOCATABLE :: KBT(:),SEDDIA(:),SAL(:,:),TEM(:,:),TEMB(:)
  REAL,ALLOCATABLE :: TAUBSED(:),TAUBSND(:),TAUB(:),WVWHA(:),WVFRQL(:),WACCWE(:)
  REAL,ALLOCATABLE :: DYE(:,:),SFL(:,:),TOXB(:,:,:),TOX(:,:,:)
  REAL,ALLOCATABLE :: BELV(:),HBED(:,:),BDENBED(:,:),PORBED(:,:)
  REAL,ALLOCATABLE :: SEDB(:,:,:),VFRBED(:,:,:),SED(:,:,:)
  REAL,ALLOCATABLE :: TSED(:,:), TSND(:,:), SEDT(:,:)
  REAL,ALLOCATABLE :: SNDB(:,:,:),SND(:,:,:)
  REAL,ALLOCATABLE :: CQBEDLOADX(:,:),CQBEDLOADY(:,:)
  REAL,ALLOCATABLE :: RSSBCE(:),RSSBCW(:),RSSBCS(:),RSSBCN(:)
  REAL,ALLOCATABLE :: WQV(:,:,:), TCHL(:,:), TOC(:,:), TN(:,:), TP(:,:), VAR(:,:)
  REAL,ALLOCATABLE :: CELLDEP(:)
  REAL,ALLOCATABLE :: U(:,:),V(:,:),W(:,:)
! {JHLEE, 141103    
  REAL,ALLOCATABLE :: TUTMPS(:,:),TVTMPS(:,:)
  REAL,ALLOCATABLE :: TVELE(:,:),TVELN(:,:)
  REAL,ALLOCATABLE :: PC(:,:), PD(:,:), PG(:,:)
  REAL,ALLOCATABLE :: XX2(:), YY2(:), ZZ2(:), UU2(:), VV2(:), WW2(:)
! }  
  REAL,ALLOCATABLE :: WQLIM(:,:,:), WQLIM2(:,:), WQLIMB(:,:,:)

  ALLOCATE( RSSBCE(LCM),RSSBCW(LCM),RSSBCS(LCM),RSSBCN(LCM) )
  ALLOCATE( XX2(NODE), YY2(NODE), ZZ2(NODE), UU2(ELEMENT), VV2(ELEMENT), WW2(ELEMENT) )
    
    IF(ITFILE.EQ.1) THEN
    	OPEN(501,FILE='Tec3D_Sets_VEL.DAT',STATUS='UNKNOWN')                        !! Outputfile
    	WRITE(501,'(A)') 'TITLE = Tecplot3D'
    	WRITE(501,'(A)') 'VARIABLES = "X", "Y", "Z", "U", "V", "W"'
    ELSEIF(ITFILE.EQ.3) THEN
      OPEN(500,FILE='Tec3D_Sets_SAL.dat',STATUS='UNKNOWN')                          !! Outputfile
      WRITE(500,'(A)') 'TITLE = Tecplot3D'
      WRITE(500,'(A)') 'VARIABLES = "X", "Y", "Z", "SAL"'	
    ELSEIF(ITFILE.EQ.4) THEN
      OPEN(500,FILE='Tec3D_Sets_TEM.dat',STATUS='UNKNOWN')                          !! Outputfile
      WRITE(500,'(A)') 'TITLE = Tecplot3D'
      WRITE(500,'(A)') 'VARIABLES = "X", "Y", "Z", "TEM"'	
    ELSEIF(ITFILE.EQ.5) THEN
      OPEN(500,FILE='Tec3D_Sets_DYE.dat',STATUS='UNKNOWN')                          !! Outputfile
      WRITE(500,'(A)') 'TITLE = Tecplot3D'
      WRITE(500,'(A)') 'VARIABLES = "X", "Y", "Z", "DYE"'	  
    ELSEIF(ITFILE.EQ.6) THEN
      OPEN(500,FILE='Tec3D_Sets_SFL.dat',STATUS='UNKNOWN')                          !! Outputfile
      WRITE(500,'(A)') 'TITLE = Tecplot3D'
      WRITE(500,'(A)') 'VARIABLES = "X", "Y", "Z", "SFL"'	  
    ELSEIF(ITFILE.EQ.7) THEN
      OPEN(500,FILE='Tec3D_Sets_TOX.dat',STATUS='UNKNOWN')                          !! Outputfile
      WRITE(500,'(A)') 'TITLE = Tecplot3D'
!      WRITE(500,'(A)') 'VARIABLES = "X", "Y", "Z", "TOX"'
      WRITE(500,'(A,<NTOX>(A,I1,A))') 'VARIABLES = "X", "Y", "Z",', (('"TOX',(I),'"'),I=1,NTOX)
    ELSEIF(ITFILE.EQ.8) THEN
      OPEN(500,FILE='Tec3D_Sets_SSC.dat',STATUS='UNKNOWN')                          !! Outputfile
      WRITE(500,'(A)') 'TITLE = Tecplot3D'
      WRITE(500,'(A)') 'VARIABLES = "X", "Y", "Z", "SED", "SND", "TSS"'
    ELSEIF(ITFILE.EQ.10) THEN
!      CALL WQ21_Tec_FILENAME
      WRITE(TECNAME,'(A,A3,A)')'Tec3D_Sets_',TRIM(VARNAME),'.DAT'
      OPEN(500,FILE=TRIM(TECNAME),STATUS='UNKNOWN')                          !! Outputfile
      WRITE(500,'(A)') 'TITLE = Tecplot3D'
      WRITE(500,'(A,A,A)') 'VARIABLES = "X", "Y", "Z", "', TRIM(VARNAME),'"' 
    ELSEIF(ITFILE.EQ.11) THEN
      OPEN(500,FILE='Tec3D_Sets_CHL.dat',STATUS='UNKNOWN')                          !! Outputfile
      WRITE(500,'(A)') 'TITLE = Tecplot3D'
      WRITE(500,'(A)') 'VARIABLES = "X", "Y", "Z", "CHL"'
    ELSEIF(ITFILE.EQ.12) THEN
      OPEN(500,FILE='Tec3D_Sets_TOC.dat',STATUS='UNKNOWN')                          !! Outputfile
      WRITE(500,'(A)') 'TITLE = Tecplot3D'
      WRITE(500,'(A)') 'VARIABLES = "X", "Y", "Z", "TOC"'
    ELSEIF(ITFILE.EQ.13) THEN
      OPEN(500,FILE='Tec3D_Sets_T-N.dat',STATUS='UNKNOWN')                          !! Outputfile
      WRITE(500,'(A)') 'TITLE = Tecplot3D'
      WRITE(500,'(A)') 'VARIABLES = "X", "Y", "Z", "TN"'  
    ELSEIF(ITFILE.EQ.14) THEN
      OPEN(500,FILE='Tec3D_Sets_T-P.dat',STATUS='UNKNOWN')                          !! Outputfile
      WRITE(500,'(A)') 'TITLE = Tecplot3D'
      WRITE(500,'(A)') 'VARIABLES = "X", "Y", "Z", "TP"'    
    ELSEIF(ITFILE.EQ.15) THEN
  	OPEN(501,FILE='Tec3D_Sets_LIM_Cyano.dat',STATUS='UNKNOWN')                          !! Outputfile
  	OPEN(502,FILE='Tec3D_Sets_LIM_Diatom.dat',STATUS='UNKNOWN')                          !! Outputfile
  	OPEN(503,FILE='Tec3D_Sets_LIM_Green.dat',STATUS='UNKNOWN')                          !! Outputfile
  	WRITE(501,'(A)') 'TITLE = Tecplot3D'
  	WRITE(502,'(A)') 'TITLE = Tecplot3D'
  	WRITE(503,'(A)') 'TITLE = Tecplot3D'
  	WRITE(501,'(A)') 'VARIABLES = "X", "Y", "Z", "PC", "N", "P", "L", "T"'
  	WRITE(502,'(A)') 'VARIABLES = "X", "Y", "Z", "PD", "N", "P", "Si", "L", "T"'
  	WRITE(503,'(A)') 'VARIABLES = "X", "Y", "Z", "PG", "N", "P", "L", "T"'
    ENDIF    
  ZN=0    
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Water Conc.
      IF(ITFILE.GE.1.AND.ITFILE.LT.10) THEN
      	N=0
        DELT=DTWQ
        OPEN(6,FILE='../EE_WC.OUT',STATUS='OLD',FORM='BINARY')
        OPEN(7,FILE='../EE_WS.OUT',STATUS='OLD',FORM='BINARY')
        OPEN(8,FILE='../EE_VEL.OUT',STATUS='OLD',FORM='BINARY')
  
        READ(6)VER
        READ(6)ISTRAN(1),ISTRAN(2),ISTRAN(3),ISTRAN(4)
        READ(6)ISTRAN(5),ISTRAN(6),ISTRAN(7)
        READ(6)NSED,NSND,KB,KC,NTOX
        
        NSXD=NSED+NSND
        KBM=MAX(1,KB)
        NSCM=MAX(1,NSED)
        NSNM=MAX(1,NSND)
        NTXM=MAX(1,NTOX)
        NSTM=MAX(3,NSCM+NSNM+NTXM)
        
        ALLOCATE( SEDDIA(NSTM) )
        ALLOCATE( SAL(LCM,KCM),TEM(LCM,KCM),KBT(LCM),TEMB(LCM) )
        ALLOCATE( TAUBSED(LCM),TAUBSND(LCM),TAUB(LCM),WVWHA(LCM),WVFRQL(LCM),WACCWE(LCM) )
        ALLOCATE( DYE(LCM,KCM),SFL(LCM,KCM),VAR(LCM,KCM) )
        ALLOCATE( U(LCM,KCM),V(LCM,KCM),W(LCM,KCM) )
        ALLOCATE( TSED(LCM,KCM),TSND(LCM,KCM),SEDT(LCM,KCM) )
        ALLOCATE( CELLDEP(LCM) )
        ALLOCATE( BELV(LCM) )  
        ALLOCATE( HBED(LCM,KBM),BDENBED(LCM,KBM),PORBED(LCM,KBM) )    
        ALLOCATE( SEDB(LCM,KBM,NSCM),VFRBED(LCM,KBM,NSTM),SED(LCM,KCM,NSCM) )
        ALLOCATE( SNDB(LCM,KBM,NSNM),SND(LCM,KCM,NSNM) )
        ALLOCATE( CQBEDLOADX(LCM,NSNM),CQBEDLOADY(LCM,NSNM) )
                
! {JHLEE, 141103      
        ALLOCATE( TUTMPS(LCM,KCM), TVTMPS(LCM,KCM), TVELE(LCM,KCM), TVELN(LCM,KCM) )
        RSSBCE=0.; RSSBCW=0.; RSSBCS=0.; RSSBCN=0. 
        U=0.; V=0.; W=0.
        TVELE=0.; TVELN=0.
! }
        DO NS=1,NSXD
          READ(6)SEDDIA(NS)
        ENDDO
         
        READ(7)idum,idum,idum,idum
        READ(8)VER,IC,JC,KC,LINES
        READ(8)RSSBCE,RSSBCW,RSSBCS,RSSBCN
        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Data Skip
        READ(6)EETIME,NACTIVE
 	      DO L=2,LA
           N1=KBT(L)
           
           IF(ISTRAN(6).GT.0.OR.ISTRAN(7).GT.0)THEN
             IF(ISBEDSTR.GE.1.AND.NSEDFLUME.EQ.0)THEN
               READ(6)TAUBSED(L)
               IF(ISBEDSTR.EQ.1)THEN
                 READ(6)TAUBSND(L)
               ENDIF
             ELSE
               READ(6)TAUB(L)
             ENDIF
           ELSE
             READ(6)SHEAR
           ENDIF
    
           IF(ISWAVE.GE.1)THEN
             READ(6)SHEAR
             IF(ISWAVE.EQ.3)THEN
               READ(6)WVWHA(L),WVFRQL(L),WACCWE(L)
             ENDIF
           ENDIF
    
           IF(ISTRAN(1).EQ.1) READ(6)(SAL(L,K),K=1,KC)
           IF(ISTRAN(2).EQ.1) THEN
             READ(6)(TEM(L,K),K=1,KC)
             IF(TBEDIT.GT.0.)READ(6)TEMB(L)
           ENDIF
           
           IF(ISTRAN(3).EQ.1)READ(6)(DYE(L,K),K=1,KC)
           IF(ISTRAN(4).EQ.1)READ(6)(SFL(L,K),K=1,KC)
           IF(ISTRAN(5).EQ.1)THEN
             READ(6)(TOXB(L,N1,NT),NT=1,NTOX)
             READ(6)((TOX(L,K,NT),K=1,KC),NT=1,NTOX)
           ENDIF
           IF(ISTRAN(6).EQ.1.OR.ISTRAN(7).GE.1)THEN
             READ(6)N1,BELV(L),HBED(L,N1),BDENBED(L,N1),PORBED(L,N1)
             IF(ISTRAN(6).EQ.1)THEN
               READ(6)(SEDB(L,N1,NS),VFRBED(L,N1,NS),NS=1,NSED)
               READ(6)((SED(L,K,NS),K=1,KC),NS=1,NSED)
             ENDIF
             IF(ISTRAN(7).EQ.1)THEN
               READ(6)(SNDB(L,N1,NX),VFRBED(L,N1,NX+NSED),NX=1,NSND)
               READ(6)((SND(L,K,NX),K=1,KC),NX=1,NSND)
               IF(ISBDLDBC.GT.0)THEN
                 READ(6)(CQBEDLOADX(L,NX),CQBEDLOADY(L,NX),NX=1,NSND)
               ENDIF
             ENDIF
           ENDIF	 
         ENDDO        
            
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Data Skip

 610    READ(6,end=619)EETIME,NACTIVE

        IF (EETIME.GT.EDAY) goto 619 ! STOP !EXIT
        ICONC=ICONC+1
        
        N=INT(86400./DELT*EETIME)
	
        IF ( EETIME.GE.SDAY.AND.EETIME.LE.EDAY ) THEN

         NSEDFLUME=0
         IF (IWRSP.EQ.98) THEN
	        NSEDFLUME=1
         ELSEIF (IWRSP.EQ.99) THEN
           NSEDFLUME=2
         ENDIF
    
	      DO L=2,LA
           N1=KBT(L)
    
           IF(ISTRAN(6).GT.0.OR.ISTRAN(7).GT.0)THEN
             IF(ISBEDSTR.GE.1.AND.NSEDFLUME.EQ.0)THEN
               READ(6)TAUBSED(L)
               IF(ISBEDSTR.EQ.1)THEN
                 READ(6)TAUBSND(L)
               ENDIF
             ELSE
               READ(6)TAUB(L)
             ENDIF
           ELSE
             READ(6)SHEAR
           ENDIF
    
           IF(ISWAVE.GE.1)THEN
             READ(6)SHEAR
             IF(ISWAVE.EQ.3)THEN
               READ(6)WVWHA(L),WVFRQL(L),WACCWE(L)
             ENDIF
           ENDIF
    
           IF(ISTRAN(1).EQ.1) READ(6)(SAL(L,K),K=1,KC)
           IF(ISTRAN(2).EQ.1) THEN
             READ(6)(TEM(L,K),K=1,KC)
             IF(TBEDIT.GT.0.)READ(6)TEMB(L)
           ENDIF
           IF(ISTRAN(3).EQ.1)READ(6)(DYE(L,K),K=1,KC)
           IF(ISTRAN(4).EQ.1)READ(6)(SFL(L,K),K=1,KC)
           IF(ISTRAN(5).EQ.1)THEN
             READ(6)(TOXB(L,N1,NT),NT=1,NTOX)
             READ(6)((TOX(L,K,NT),K=1,KC),NT=1,NTOX)
           ENDIF
           IF(ISTRAN(6).EQ.1.OR.ISTRAN(7).GE.1)THEN
             READ(6)N1,BELV(L),HBED(L,N1),BDENBED(L,N1),PORBED(L,N1)
             IF(ISTRAN(6).EQ.1)THEN
               READ(6)(SEDB(L,N1,NS),VFRBED(L,N1,NS),NS=1,NSED)
               READ(6)((SED(L,K,NS),K=1,KC),NS=1,NSED)
             ENDIF
             IF(ISTRAN(7).EQ.1)THEN
               READ(6)(SNDB(L,N1,NX),VFRBED(L,N1,NX+NSED),NX=1,NSND)
               READ(6)((SND(L,K,NX),K=1,KC),NX=1,NSND)
               IF(ISBDLDBC.GT.0)THEN
                 READ(6)(CQBEDLOADX(L,NX),CQBEDLOADY(L,NX),NX=1,NSND)
               ENDIF
             ENDIF
           ENDIF	 
         ENDDO
                  
! 702    READ(7,end=708)idum,TIME,dum
        READ(7)idum,TIME,dum
        DO L=2,LA
          READ(7)HP(L)	
        ENDDO  
        
!        IF(TIME.EQ.EETIME) THEN
  	      WRITE(*,*) EETIME,N
!  	      GOTO 708
!  	    ENDIF
  	    
!  	    GOTO 702
! 708    CONTINUE
        
! 703    READ(8,end=707)idum,TIME1,dum
        READ(8)idum,TIME1,dum
          
          DO L=2,LA
            READ(8) ( U(L,K),V(L,K),W(L,K),K=1,KC )
          ENDDO
          DO L=2,LA
          	LN=LNC(L)
            DO K=1,KC
              TUTMPS(L,K)=50.*STCUV(L)*( RSSBCE(L)*U(L+1,K)+RSSBCW(L)*U(L,K) )
		      TVTMPS(L,K)=50.*STCUV(L)*( RSSBCN(L)*V(LN ,K)+RSSBCS(L)*V(L,K) )
            
		      TVELE(L,K)=CUE(L)*TUTMPS(L,K) + CVE(L)*TVTMPS(L,K)
		      TVELN(L,K)=CUN(L)*TUTMPS(L,K) + CVN(L)*TVTMPS(L,K)
		    ENDDO
		  ENDDO
          
!          IF(TIME1.EQ.EETIME) THEN
!  	        GOTO 707
!  	      ENDIF          
!        GOTO 703
! 707    CONTINUE
        
        DO L=2,LA
          CELLDEP(L)=HP(L)/KC
          IF(ITFILE.EQ.8) THEN
             DO K=1,KC
              TSED(L,K)=0.
              TSND(L,K)=0.
              SEDT(L,K)=0.
             ENDDO
      
           IF(NSED.GE.1) THEN  
            DO K=1,KC
             DO NS=1,NSED
              TSED(L,K)=TSED(L,K)+SED(L,K,NS)
             ENDDO
            ENDDO         
           ENDIF
      
           IF(NSND.GE.1) THEN           
            DO K=1,KC
             DO NX=1,NSND
              TSND(L,K)=TSND(L,K)+SND(L,K,NX)
             ENDDO
            ENDDO           
           ENDIF 
          ENDIF   
          DO K=1,KC
            SEDT(L,K)=TSED(L,K)+TSND(L,K)
          ENDDO         
        ENDDO   
        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Julianday
      DATA DAY/31,28,31,30,31,30,31,31,30,31,30,31/
      DAY(2)=28
      IF(mod(YY,4).EQ.0) THEN
        DAY(2)=29
         IF(mod(YY,100).EQ.0) THEN
          DAY(2)=28
           IF(mod(YY,400).EQ.0) THEN
             DAY(2)=29 
           ENDIF
         ENDIF
      ENDIF
      
      DAY1=EETIME
      DO J=1,12
        IF(DAY1.GT.0.AND.DAY1.LE.DAY(J)) THEN
          IM=J
          ID=INT(DAY1)
          IH=(DAY1-ID+0.0001)*24.
          IH=INT(IH)
          GOTO 1002
        ELSE
          DAY1=DAY1-DAY(J) 
        ENDIF
      ENDDO            
 1002 CONTINUE  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Calculation        
  	    IF(ITFILE.EQ.1) THEN     !! SALINITY
  	      ZN=ZN+1
  	      CALL TEC3D_TEXT(ZN, EETIME)
  	      DO L=2,LA
  	        DO K=1,KC
  	          IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	      	  	TVELE(L,K)=-999.9
  	      	  	TVELN(L,K)=-999.9
  	      	  	W(L,K)=-999.9
  	      	  ENDIF		
  	        ENDDO

  	        DO K=1,KC
  	          DO J=1,4
  	            WRITE(501,'(2(F8.1,1x),F5.1,1x,3(F7.2,1x))') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K-1), TVELE(L,K), TVELN(L,K), W(L,K)
  	          ENDDO
 
  	          DO J=1,4
  	            WRITE(501,'(2(F8.1,1x),F5.1,1x,3(F7.2,1x))') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K), TVELE(L,K), TVELN(L,K), W(L,K)
  	          ENDDO
  	        ENDDO
 	      ENDDO  
  	    	
  	    ELSEIF(ITFILE.EQ.3) THEN     !! SALINITY
  	      ZN=ZN+1
          CALL TEC3D_TEXT(ZN, EETIME)
  	      DO L=2,LA
  	        DO K=1,KC
  	          IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	      	  	VAR(L,K)=-999.9
  	      	  ELSE
  	      	  	VAR(L,K)=SAL(L,K)
  	      	  ENDIF		
  	        ENDDO
  	        DO K=1,KC
  	          DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K-1), VAR(L,K)
  	          ENDDO

  	          DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K), VAR(L,K)
  	          ENDDO
  	        ENDDO
  	      ENDDO  
  	      
  	    ELSEIF(ITFILE.EQ.4) THEN     !! TEMPERATURE
  	      ZN=ZN+1
          CALL TEC3D_TEXT(ZN, EETIME)
  	      DO L=2,LA
  	      	DO K=1,KC	
  	      	  IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	      	  	VAR(L,K)=-999.9
  	      	  ELSE
  	      	  	VAR(L,K)=TEM(L,K)			
  	      	  ENDIF	
  	        ENDDO
  	        DO K=1,KC
  	      	  DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K-1), VAR(L,K)
  	          ENDDO

  	          DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K), VAR(L,K)
  	          ENDDO
  	        ENDDO
  	      ENDDO
  	    
  	    ELSEIF(ITFILE.EQ.5) THEN     !! DYE
  	      ZN=ZN+1
          CALL TEC3D_TEXT(ZN, EETIME)
  	      DO L=2,LA
  	      	DO K=1,KC	
  	          IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	      	  	VAR(L,K)=-999.9
  	      	  ELSE
  	      	  	VAR(L,K)=DYE(L,K)
  	      	  ENDIF	
  	        ENDDO
  	        DO K=1,KC
  	      	  DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K-1), VAR(L,K)
  	          ENDDO

  	          DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K), VAR(L,K)
  	          ENDDO
  	        ENDDO
  	      ENDDO  	
  	    
  	    ELSEIF(ITFILE.EQ.6) THEN     !! SFL
  	      ZN=ZN+1  
          CALL TEC3D_TEXT(ZN, EETIME)
  	      DO L=2,LA
  	      	DO K=1,KC	
  	          IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	      	  	VAR(L,K)=-999.9
  	      	  ELSE
  	      	  	VAR(L,K)=SFL(L,K)
  	      	  ENDIF	
  	        ENDDO
  	        DO K=1,KC
  	      	  DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K-1), VAR(L,K)
  	          ENDDO

  	          DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K), VAR(L,K)
  	          ENDDO
  	        ENDDO
  	      ENDDO  
  	      
  	    ELSEIF(ITFILE.EQ.7) THEN     !! TOX
  	      ZN=ZN+1  
          CALL TEC3D_TEXT(ZN, EETIME)
  	      DO L=2,LA
  	      	DO K=1,KC	
  	          IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	      	  	VAR(L,K)=-999.9
  	      	  ELSE
  	      	  	VAR(L,K)=TOX(L,K,NT)
  	      	  ENDIF	
  	        ENDDO
  	        DO K=1,KC
  	      	  DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K-1), VAR(L,K)
  	          ENDDO

  	          DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K), VAR(L,K)
  	          ENDDO
  	        ENDDO
  	      ENDDO  	  
  	      
  	    ELSEIF(ITFILE.EQ.8) THEN     !! SED
  	      ZN=ZN+1  
          CALL TEC3D_TEXT(ZN, EETIME)
  	      DO L=2,LA
  	      	DO K=1,KC	
  	          IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	      	  	TSED(L,K)=-999.9
  	      	  	TSND(L,K)=-999.9
  	      	  	SEDT(L,K)=-999.9
  	      	  ELSE
  	      	  	TSED(L,K)=TSED(L,K)
  	      	  	TSND(L,K)=TSND(L,K)
  	      	  	SEDT(L,K)=SEDT(L,K)
  	      	  ENDIF	
  	        ENDDO
  	        DO K=1,KC
  	      	  DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,3(F6.1,1x))') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K-1), TSED(L,K), TSND(L,K), SEDT(L,K)
  	          ENDDO

  	          DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,3(F6.1,1x))') XX1(L,J), YY1(L,J), BELV(L)+CELLDEP(L)*(K), TSED(L,K), TSND(L,K), SEDT(L,K)
  	          ENDDO
  	        ENDDO
  	      ENDDO   	          
  	      
  	    ENDIF   ! IF(ITFILE.EQ.11)
  	    NN1=0
        DO J=1,NODE
          NN1=NN1+1
          NN2(J)=NN1
        ENDDO
        IF(ITFILE.EQ.1) THEN
          WRITE(501,'(8(I8))') (NN2(J),J=1,NODE)
        ELSE  
          WRITE(500,'(8(I8))') (NN2(J),J=1,NODE)
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Calculation          	    
  	    
        ENDIF   ! IF ( EETIME.GE.SDAY.AND.EETIME.LE.EDAY ) THEN

        Nper=N
     GOTO 610
 619 CONTINUE
     CLOSE(6);
     CLOSE(7);
     CLOSE(8);
     CLOSE(500)
     CLOSE(501)
     CLOSE(502)

     ENDIF     ! IF(ITFILE.GE.10) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Water Conc.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Water Quality
      IF(ITFILE.GE.10.AND.ITFILE.LE.14) THEN
      	N=0
        DELT=DTWQ
        
        OPEN(6,FILE='../EE_WQ.OUT',STATUS='OLD',FORM='BINARY')
        OPEN(7,FILE='../EE_WS.OUT',STATUS='OLD',FORM='BINARY')
        OPEN(8,FILE='../EE_VEL.OUT',STATUS='OLD',FORM='BINARY')
        READ(6)VER
        READ(6)NWQV
        
        ALLOCATE( ISTRWQ(NWQV),IWQ(NWQV),WQV(LCM,KCM,NWQV) )
        ALLOCATE( TCHL(LCM,KCM), TOC(LCM,KCM), TN(LCM,KCM), TP(LCM,KCM) )
        ALLOCATE( U(LCM,KCM),V(LCM,KCM),W(LCM,KCM) )
        ALLOCATE( CELLDEP(LCM) )
        ALLOCATE( VAR(LCM,KCM) )
        ALLOCATE( BELV(LCM) )          
        READ(6)(ISTRWQ(NW),NW=1,NWQV)
        READ(6)(IWQ(NW),NW=1,NWQV)
        READ(7)VER,IC,JC,LINES
        READ(8)idum,idum,idum,idum,idum
        READ(8)RSSBCE,RSSBCW,RSSBCS,RSSBCN
        FORLEN=KC*NWQV
        itmp=0
        itmp=itmp+1
        WQV=0.
        Nper=N

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Data Skip
       READ(6)EETIME
         DO L=2,LA
           DO K=1,KC
             DO NW=1,NWQV
               IF(IWQ(NW).GT.0)THEN
                 READ(6)WQ
                 WQV(L,K,NW)=WQ
               ENDIF
             ENDDO
           ENDDO
         ENDDO       
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Data Skip
 620    READ(6,end=629)EETIME

        IF (EETIME.GT.EDAY) goto 629 ! STOP !EXIT
        N=INT(86400./DELT*EETIME)
	
        IF ( EETIME.GE.SDAY.AND.EETIME.LE.EDAY ) THEN

          DO L=2,LA
            DO K=1,KC
              DO NW=1,NWQV
                IF(IWQ(NW).GT.0)THEN
                  READ(6)WQ
                  WQV(L,K,NW)=WQ
                ENDIF
              ENDDO
            ENDDO
          ENDDO
                  
! 701    READ(7,end=709)idum,TIME,dum
        READ(7)idum,TIME,dum
        DO L=2,LA
          READ(7)HP(L)	
        ENDDO  
        
!        IF(TIME.EQ.EETIME) THEN
  	      WRITE(*,*) EETIME,N
!  	      GOTO 709
!  	    ENDIF
  	    
!  	    GOTO 701
! 709    CONTINUE
! 704    READ(8,end=706)idum,TIME1,dum
        READ(8)idum,TIME1,dum

          DO L=2,LA
            READ(8) ( U(L,K),V(L,K),W(L,K),K=1,KC )
          ENDDO

!          IF(TIME1.EQ.EETIME) THEN
!  	        GOTO 706
!  	      ENDIF          
!        GOTO 704
! 706    CONTINUE      

        DO L=2,LA
          CELLDEP(L)=HP(L)/KC
        ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Julianday
      DATA DAY/31,28,31,30,31,30,31,31,30,31,30,31/
      DAY(2)=28
      IF(mod(YY,4).EQ.0) THEN
        DAY(2)=29
         IF(mod(YY,100).EQ.0) THEN
          DAY(2)=28
           IF(mod(YY,400).EQ.0) THEN
             DAY(2)=29 
           ENDIF
         ENDIF
      ENDIF

      DAY1=EETIME
      DO J=1,12
        IF(DAY1.GT.0.AND.DAY1.LE.DAY(J)) THEN
          IM=J
          ID=INT(DAY1)
          IH=(DAY1-ID+0.0001)*24.
          IH=INT(IH)
          GOTO 1003
        ELSE
          DAY1=DAY1-DAY(J) 
        ENDIF
      ENDDO            
1003  CONTINUE  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Calculation        
  	    IF(ITFILE.EQ.10) THEN     !! 21 Variable
           ZN=ZN+1
           
           CALL TEC3D_TEXT(ZN, EETIME)
          J=IWQOUT1
          DO L=2,LA
          	DO K=1,KC
  	          IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	      	  	VAR(L,K)=-999.9
  	      	  ELSE
  	      	  	VAR(L,K)=WQV(L,K,J)
  	      	  ENDIF	
  	        ENDDO
  	        DO K=1,KC
  	        	DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K-1), VAR(L,K)
  	          ENDDO

  	          DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K), VAR(L,K)
  	          ENDDO
  	        ENDDO
  	      ENDDO
  	    
        ELSEIF(ITFILE.EQ.11) THEN     !! TCHL
  	      ZN=ZN+1  
          CALL TEC3D_TEXT(ZN, EETIME)
  	      DO L=2,LA
  	        DO K=1,KC
  	          IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	      	  	TCHL(L,K)=-999.9
  	      	  ELSE
  	      	  	TCHL(L,K)=((WQV(L,K,1)*(1./rC1)) + (WQV(L,K,2)*(1./rC2)) + (WQV(L,K,3)*(1./rC3)))
  	      	  ENDIF	
  	        ENDDO
  	        DO K=1,KC
  	        	DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K-1), TCHL(L,K)
  	          ENDDO

  	          DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K), TCHL(L,K)
  	          ENDDO
  	        ENDDO
  	      ENDDO  
  	    
  	    ELSEIF(ITFILE.EQ.12) THEN     !! TOC
  	      ZN=ZN+1  
          CALL TEC3D_TEXT(ZN, EETIME)
  	      DO L=2,LA
  	      	DO K=1,KC	
  	      	  IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	      	  	TOC(L,K)=-999.9
  	      	  ELSE
  	      	  	TOC(L,K)=WQV(L,K,1)+WQV(L,K,2)+WQV(L,K,3)+WQV(L,K,4)+WQV(L,K,5)+WQV(L,K,6)
  	      	  ENDIF	
  	        ENDDO
  	        DO K=1,KC
  	      	  DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K-1), TOC(L,K)
  	          ENDDO

  	          DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K), TOC(L,K)
  	          ENDDO
  	        ENDDO
  	      ENDDO  
  	    
  	    ELSEIF(ITFILE.EQ.13) THEN     !! T-N
  	      ZN=ZN+1  
          CALL TEC3D_TEXT(ZN, EETIME)
  	      DO L=2,LA
  	      	DO K=1,KC	
  	          IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	      	  	TN(L,K)=-999.9
  	      	  ELSE
  	      	  	TN(L,K)=WQV(L,K,11)+WQV(L,K,12)+WQV(L,K,13)+WQV(L,K,14)+WQV(L,K,15) &
		               + (WQV(L,K,1)*(rN1/rC1))+(WQV(L,K,2)*(rN2/rC2))+(WQV(L,K,3)*(rN3/rC3))
  	      	  ENDIF
  	        ENDDO
  	        DO K=1,KC
  	      	  DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K-1), TN(L,K)
  	          ENDDO

  	          DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K), TN(L,K)
  	          ENDDO
  	        ENDDO
  	      ENDDO  	
  	    
  	    ELSEIF(ITFILE.EQ.14) THEN     !! T-P
  	      ZN=ZN+1  
          CALL TEC3D_TEXT(ZN, EETIME)
  	      DO L=2,LA
  	      	DO K=1,KC	
  	          IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	      	  	TP(L,K)=-999.9
  	      	  ELSE
  	      	  	TP(L,K)=WQV(L,K,7)+WQV(L,K,8)+WQV(L,K,9)+WQV(L,K,10) &
		                    + (WQV(L,K,1)*(rP1/rC1))+(WQV(L,K,2)*(rP2/rC2))+(WQV(L,K,3)*(rP3/rC3))
  	      	  ENDIF
  	        ENDDO
  	        DO K=1,KC
  	      	  DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K-1), TP(L,K)
  	          ENDDO

  	          DO J=1,4
  	            WRITE(500,'(2(F8.1,1x),F5.1,1x,F6.1)') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K), TP(L,K)
  	          ENDDO
  	        ENDDO
  	      ENDDO  
  	      
  	    ENDIF   ! IF(ITFILE.EQ.11)
  	    NN1=0
        DO J=1,NODE
          NN1=NN1+1
          NN2(J)=NN1
        ENDDO
        WRITE(500,'(8(I8))') (NN2(J),J=1,NODE)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Calculation          	    
  	    
        ENDIF   ! IF ( EETIME.GE.SDAY.AND.EETIME.LE.EDAY ) THEN

        itmp=itmp+1
        WQV=0.
        Nper=N
     GOTO 620
 629 CONTINUE
     CLOSE(6);
     CLOSE(7);
     CLOSE(8);

     ENDIF     ! IF(ITFILE.EQ.11) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Water Quality  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Limiting Factor
     IF(ITFILE.EQ.15) THEN

     OPEN(7,FILE='../EE_WQ.OUT',STATUS='OLD',FORM='BINARY')
     READ(7)VER
     READ(7)NWQV
     ALLOCATE( ISTRWQ(NWQV),IWQ(NWQV),WQV(LCM,KCM,NWQV) )
     READ(7)(ISTRWQ(NW),NW=1,NWQV)
     READ(7)(IWQ(NW),NW=1,NWQV)

     OPEN(8,FILE='../EE_VEL.OUT',STATUS='OLD',FORM='BINARY')
     READ(8)idum,idum,idum,idum,idum
     READ(8)RSSBCE,RSSBCW,RSSBCS,RSSBCN
          
     OPEN(9,FILE='../EE_WS.OUT',STATUS='OLD',FORM='BINARY')
     READ(9)VER,IC,JC,LINES
     
     N=0
     DELT=DTWQ
   
       NWM=18
       OPEN(6,FILE='../WQDOCOMP.BIN',FORM='UNFORMATTED',STATUS='UNKNOWN')
       
!       ALLOCATE( CH(NWM) )
       ALLOCATE( WQLIM(LCM,KCM,NWM),WQLIMB(LCM,KCM,NWM))
       ALLOCATE( WQLIM2(LCM,KCM), PC(LCM,KCM), PD(LCM,KCM), PG(LCM,KCM))
       ALLOCATE( CELLDEP(LCM) )
       ALLOCATE( U(LCM,KCM),V(LCM,KCM),W(LCM,KCM) )
       
     
       DO L=2,LA 
        READ(6) IL(L)
       ENDDO
       DO L=2,LA 
        READ(6) JL(L)
       ENDDO
       DO L=2,LA 
        READ(6) DLON(L)
       ENDDO
       DO L=2,LA 
        READ(6) DLAT(L)
       ENDDO

! DUMMY DATA READ
       READ(6,END=639) N,TIMTMP
       
       DO L=2,LA
         DO K=1,KC    
           READ(6) (WQLIM(L,K,NW),NW=1,NWM)  
         ENDDO
       ENDDO  
       
       READ(7) EETIME
       
       DO L=2,LA
         DO K=1,KC
           DO NW=1,NWQV
             IF(IWQ(NW).GT.0)THEN
               READ(7)WQ
               WQV(L,K,NW)=WQ
             ENDIF
           ENDDO
         ENDDO
       ENDDO

       WQV=0.

! DATA READ
630    READ(6,END=639) N,TIMTMP
       READ(7) EETIME

! 801    READ(9,end=809)idum,TIME,dum
        READ(9)idum,TIME,dum
        DO L=2,LA
          READ(9)HP(L)	
        ENDDO  
        
!        IF(TIME.EQ.EETIME) THEN
!  	      WRITE(*,*) EETIME,N
!  	      GOTO 809
!  	    ENDIF
  	    
!  	    GOTO 801
! 809    CONTINUE
       
! 804    READ(8,end=806)idum,TIME1,dum
        READ(8)idum,TIME1,dum
          
          DO L=2,LA
            READ(8) ( U(L,K),V(L,K),W(L,K),K=1,KC )
          ENDDO
          
!          IF(TIME1.EQ.EETIME) THEN
!  	        GOTO 806
!  	      ENDIF          
!        GOTO 804
! 806    CONTINUE     
        
        DO L=2,LA
          CELLDEP(L)=HP(L)/KC
        ENDDO
        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Julianday
      DATA DAY/31,28,31,30,31,30,31,31,30,31,30,31/
      DAY(2)=28
      IF(mod(YY,4).EQ.0) THEN
        DAY(2)=29
         IF(mod(YY,100).EQ.0) THEN
          DAY(2)=28
           IF(mod(YY,400).EQ.0) THEN
             DAY(2)=29 
           ENDIF
         ENDIF
      ENDIF
      
      DAY1=EETIME
      DO J=1,12
        IF(DAY1.GT.0.AND.DAY1.LE.DAY(J)) THEN
          IM=J
          ID=INT(DAY1)
          IH=(DAY1-ID+0.0001)*24.
          IH=INT(IH)
          GOTO 805
        ELSE
          DAY1=DAY1-DAY(J) 
        ENDIF
      ENDDO            
 805  CONTINUE  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Calculation                
        
       IF (EETIME.GT.EDAY) goto 639 ! STOP !EXIT

       IF ( EETIME.GE.SDAY.AND.EETIME.LE.EDAY ) THEN
          
       N=INT(86400./DELT*EETIME)
       
       WRITE(*,*) EETIME,N
          
       DO L=2,LA
         DO K=1,KC    
           READ(6) (WQLIM(L,K,NW),NW=1,NWM)  
         ENDDO
       ENDDO  
       
       DO L=2,LA
         DO K=1,KC
           DO NW=1,NWQV
             IF(IWQ(NW).GT.0)THEN
               READ(7)WQ
               WQV(L,K,NW)=WQ
             ENDIF
           ENDDO
           IF(KHS.EQ.0) THEN
             WQLIM2(L,K)=0.
           ELSE
             WQLIM2(L,K)=WQV(L,K,17)/(KHS+WQV(L,K,17))
           ENDIF
           PC(L,K)=PMC * min(WQLIM(L,K,1), WQLIM(L,K,5)) * WQLIM(L,K,9) * WQLIM(L,K,13)
           PD(L,K)=PMD * min(WQLIM(L,K,2), WQLIM(L,K,6), WQLIM2(L,K)) * WQLIM(L,K,10) * WQLIM(L,K,14)
           PG(L,K)=PMG * min(WQLIM(L,K,3), WQLIM(L,K,7)) * WQLIM(L,K,11) * WQLIM(L,K,15)           
         ENDDO
       ENDDO
       
       ZN=ZN+1
           
       CALL TEC3D_LIM_TEXT(ZN, EETIME)
       
!       J=IWQOUT1
       DO L=2,LA
       	DO K=1,KC
  	       IF(U(L,K).EQ.0.AND.V(L,K).EQ.0) THEN
  	   	  	PC(L,K)=-999.9
  	   	  	PD(L,K)=-999.9
  	   	  	PG(L,K)=-999.9
  	   	  	WQLIM(L,K,1)=-999.9
  	   	  	WQLIM(L,K,2)=-999.9
  	   	  	WQLIM(L,K,3)=-999.9
  	   	  	WQLIM(L,K,5)=-999.9
  	   	  	WQLIM(L,K,6)=-999.9
  	   	  	WQLIM(L,K,7)=-999.9
  	   	  	WQLIM(L,K,9)=-999.9
  	   	  	WQLIM(L,K,10)=-999.9
  	   	  	WQLIM(L,K,11)=-999.9
  	   	  	WQLIM(L,K,13)=-999.9
  	   	  	WQLIM(L,K,14)=-999.9
  	   	  	WQLIM(L,K,15)=-999.9
  	   	  	WQLIM2(L,K)=-999.9
  	   	  ENDIF	
  	     ENDDO
  	     DO K=1,KC
  	     	DO J=1,4
  	         WRITE(501,'(2(F8.1,1x),F5.1,1x,5(F8.3,1x))') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K-1), &
  	                                               PC(L,K), WQLIM(L,K,1),WQLIM(L,K,5),WQLIM(L,K,9),WQLIM(L,K,13)
  	                                               
  	         WRITE(502,'(2(F8.1,1x),F5.1,1x,6(F8.3,1x))') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K-1), &
  	                                               PD(L,K), WQLIM(L,K,2),WQLIM(L,K,6), WQLIM2(L,K),WQLIM(L,K,10),WQLIM(L,K,14)
  	                                               
  	         WRITE(503,'(2(F8.1,1x),F5.1,1x,5(F8.3,1x))') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K-1), &
  	                                               PG(L,K), WQLIM(L,K,3),WQLIM(L,K,7),WQLIM(L,K,11),WQLIM(L,K,15)  	                                                 	                                               
  	       ENDDO

  	       DO J=1,4
  	         WRITE(501,'(2(F8.1,1x),F5.1,1x,5(F8.3,1x))') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K), &
  	                                               PC(L,K), WQLIM(L,K,1),WQLIM(L,K,5),WQLIM(L,K,9),WQLIM(L,K,13)
  	                                               
  	         WRITE(502,'(2(F8.1,1x),F5.1,1x,6(F8.3,1x))') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K), &
  	                                               PD(L,K), WQLIM(L,K,2),WQLIM(L,K,6), WQLIM2(L,K),WQLIM(L,K,10),WQLIM(L,K,14)
  	                                               
  	         WRITE(503,'(2(F8.1,1x),F5.1,1x,5(F8.3,1x))') XX1(L,J), YY1(L,J), BELV1(L)+CELLDEP(L)*(K), &
  	                                               PG(L,K), WQLIM(L,K,3),WQLIM(L,K,7),WQLIM(L,K,11),WQLIM(L,K,15)  	                                                 	                                               
  	       ENDDO
  	     ENDDO
  	   ENDDO
  	   
  	   NN1=0
        DO J=1,NODE
          NN1=NN1+1
          NN2(J)=NN1
        ENDDO
        WRITE(501,'(8(I8))') (NN2(J),J=1,NODE)
        WRITE(502,'(8(I8))') (NN2(J),J=1,NODE)
        WRITE(503,'(8(I8))') (NN2(J),J=1,NODE)
       	
       ENDIF	!! IF ( EETIME.GE.SDAY.AND.EETIME.LE.EDAY ) THEN

       goto 630
639    continue
       CLOSE(6);
       CLOSE(7);
       CLOSE(8);
       CLOSE(9);

     ENDIF  !! IF(ITFILE.EQ.15) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Limiting Factor

 
ENDSUBROUTINE !SUBROUTINE TECPLOT
!}

!{ 141010, JHLEE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE LIM
IMPLICIT NONE
  CHARACTER(LEN=100) :: TFNAME,TFANAME,FNAME,AFNAME,TFNAME2
  INTEGER :: NWQV,Nper,itmp,ND,NDUM,ISDAY,IEDAY,IPDAY
  REAL :: TIMTMP,TIMTMPB,ATIMTMP
  INTEGER :: NREC, IWQTSDT, NPARM, NCELLS, NDATA
  REAL :: TBEGAN, TEND, DT
  REAL,ALLOCATABLE :: WQLIM(:,:,:), WQLIM2(:,:), WQLIMB(:,:,:)
  CHARACTER*20 WQNAME(30)  
  CHARACTER*10 WQUNITS(30)  
  CHARACTER*3  WQCODE(30) 
  
  INTEGER :: NW
  REAL :: EETIME,DELT
  REAL*4 WQ
  INTEGER,ALLOCATABLE :: ISTRWQ(:),IWQ(:)
  REAL,ALLOCATABLE :: WQV(:,:,:)
  REAL,ALLOCATABLE :: RSSBCE(:),RSSBCW(:),RSSBCS(:),RSSBCN(:)
  REAL,ALLOCATABLE :: PC(:,:), PD(:,:), PG(:,:)
  ALLOCATE( RSSBCE(LCM),RSSBCW(LCM),RSSBCS(LCM),RSSBCN(LCM) )

  OPEN(7,FILE='../EE_WQ.OUT',STATUS='OLD',FORM='BINARY')
  READ(7)VER
  READ(7)NWQV
  ALLOCATE( ISTRWQ(NWQV),IWQ(NWQV),WQV(LCM,KCM,NWQV) )
  READ(7)(ISTRWQ(NW),NW=1,NWQV)
  READ(7)(IWQ(NW),NW=1,NWQV)
  FNTSR2='TIME_LMT2_'
  
  N=0
  DELT=DTWQ

    NWM=18
    OPEN(6,FILE='../WQDOCOMP.BIN',FORM='UNFORMATTED',STATUS='UNKNOWN')
    FNTSR='TIME_LMT_'
    FNTSRA='TIME_aLMT_'
    FNHOR='LMT_'
    
    ALLOCATE( CH(22) )
    ALLOCATE( WQLIM(LCM,KCM,NWM),WQLIMB(LCM,KCM,NWM))
    ALLOCATE( WQLIM2(LCM,KCM))
    ALLOCATE( PC(LCM,KCM), PD(LCM,KCM), PG(LCM,KCM))
    
    CALL HEADER
    
    DO L=2,LA 
     READ(6) IL(L)
    ENDDO
    DO L=2,LA 
     READ(6) JL(L)
    ENDDO
    DO L=2,LA 
     READ(6) DLON(L)
    ENDDO
    DO L=2,LA 
     READ(6) DLAT(L)
    ENDDO
        
! TIMESERIES FILE OPEN AND HEADER INFORMATION WRITE
  IF( NTSR.GT.0 ) THEN  
    DO I=1,NTSR
      IF (IJCT(ITSR(I),JTSR(I)).ne.5) then
        WRITE(*,*) 'NOT WATER CELL:',ITSR(I),JTSR(I)
        STOP
      ENDIF
	  WRITE(TFNAME,'(A,I5.5,A,I5.5,A)') FNTSR,ITSR(I),'_',JTSR(I),'.DAT'
	  WRITE(TFNAME2,'(A,I5.5,A,I5.5,A)') FNTSR2,ITSR(I),'_',JTSR(I),'.DAT'
 	  OPEN(100+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
	  PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)

	  IF( KC.GT.1 ) THEN
         WRITE(100+I,9827)  &
		               'TIME','N',((CH(NW),'_',K,NW=1,NWM+4),K=KC,1,-1)
! 		 WRITE(200+I,9835)  'TIME','N',(('LSD_',K),K=KC,1,-1)              
 9827    FORMAT(3X,A4,9X,A1,7X,<FORLEN>(A3,A1,I2.2,5X))
 9835    FORMAT(3X,A4,9X,A1,7X,<KC>(A4,I2.2,5X))

      ELSEIF( KC.EQ.1 ) THEN
        WRITE(100+I,9828)  &
		               'TIME','N',(CH(NW),NW=1,NWM+4)
! 		WRITE(200+I,9836)  &
! 		               'TIME','N',('LSD_')
 9828   FORMAT(3X,A4,9X,A1,7X,<FORLEN>(A3,8X))
 9836   FORMAT(3X,A4,9X,A1,7X,(A3,8X))
      ENDIF
	ENDDO
  ENDIF !IF( NTSR.GT.0 ) THEN
  PRINT *," "   
  
! DUMMY DATA READ
  READ(6,END=629) N,TIMTMP
  
  DO L=2,LA
    DO K=1,KC    
      READ(6) (WQLIM(L,K,NW),NW=1,NWM)  
    ENDDO
  ENDDO  
  
  READ(7) EETIME
  
  DO L=2,LA
    DO K=1,KC
      DO NW=1,NWQV
        IF(IWQ(NW).GT.0)THEN
          READ(7)WQ
          WQV(L,K,NW)=WQ
        ENDIF
      ENDDO
    ENDDO
  ENDDO
 
  WQV=0.

! DATA READ
 620 READ(6,END=629) N,TIMTMP
     READ(7) EETIME
     
     IF (EETIME.GT.EDAY) goto 629 ! STOP !EXIT
     
     N=INT(86400./DELT*EETIME)
     
  DO L=2,LA
    DO K=1,KC    
      READ(6) (WQLIM(L,K,NW),NW=1,NWM)  
    ENDDO
  ENDDO  
  
  DO L=2,LA
    DO K=1,KC
      DO NW=1,NWQV
        IF(IWQ(NW).GT.0)THEN
          READ(7)WQ
          WQV(L,K,NW)=WQ
        ENDIF
      ENDDO
      IF(KHS.EQ.0) THEN
        WQLIM2(L,K)=0.
      ELSE
        WQLIM2(L,K)=WQV(L,K,17)/(KHS+WQV(L,K,17))
      ENDIF
      PC(L,K)=PMC * min(WQLIM(L,K,1), WQLIM(L,K,5)) * WQLIM(L,K,9) * WQLIM(L,K,13)
      PD(L,K)=PMD * min(WQLIM(L,K,2), WQLIM(L,K,6), WQLIM2(L,K)) * WQLIM(L,K,10) * WQLIM(L,K,14)
      PG(L,K)=PMG * min(WQLIM(L,K,3), WQLIM(L,K,7)) * WQLIM(L,K,11) * WQLIM(L,K,15)
    ENDDO
  ENDDO

! DATA PRINT
    IF (TIMTMP.GT.EDAY) GOTO 629 !EXIT
		
    IF ( TIMTMP.GE.SDAY.AND.TIMTMP.LE.EDAY.OR.EETIME.GE.SDAY.AND.EETIME.LE.EDAY ) THEN   	
   	
      NDATA=NDATA+1

 8999 FORMAT(F8.2,A,I9,A,I5,A)
      NAVG=NAVG+1
      IF(NAVG.EQ.2)THEN
	   ATIMTMP=TIMTMP+TIMTMPB
      ELSEIF(NAVG.GT.2)THEN
       ATIMTMP=ATIMTMP+TIMTMP
      ENDIF

	  WRITE(*,*) EETIME, ' DAYS'

	  IF (IHOR.EQ.1) THEN 
        WRITE(FNAME,'(A,I8.8,A)')FNHOR,N,'.DAT'
        OPEN(21,FILE=TRIM(FNAME),STATUS='UNKNOWN')
          IF (KC.GT.1) THEN
            WRITE(21,9829)  &
 		               'I','J','X','Y',((CH(NW),'_',K,NW=1,NWM+4),K=KC,1,-1)
 9829       FORMAT(4X,A1,4X,A1,5X,A1,10X,A1,10X,<FORLEN>(A3,A1,I2.2,5X))
          ELSEIF (KC.EQ.1) THEN
            WRITE(21,9830)  & 
 		               'I','J','X','Y',(CH(NW),NW=1,NWM+4)
 9830       FORMAT(4X,A1,4X,A1,5X,A1,10X,A1,10X,<FORLEN>(A3,8X))
          ENDIF !IF (KC.GT.1) THEN
      ENDIF
	  DO L=2,LA
          
!        IF (IHOR.EQ.0) THEN       ! Only Timeseries
          IF (NTSR.GT.0) THEN
            DO I=1,NTSR
              IF( L.EQ.LIJ(ITSR(I),JTSR(I)) ) THEN
                WRITE(100+I,9833) EETIME,N,((PC(L,K),PD(L,K),PG(L,K),(WQLIM(L,K,NW),NW=1,NWM),WQLIM2(L,K)),K=KC,1,-1)
!                WRITE(200+I,9833) EETIME,N,((WQLIM2(L,K)),K=KC,1,-1)
 9833           FORMAT(F11.7,1X,I8,<FORLEN>(E11.3))                
 9834           FORMAT(F11.7,1X,I8,<KC>(E11.3))                
              ENDIF
		    ENDDO
          ENDIF !IF (NTSR.GT.0) THEN
!        ENDIF !   
          
        IF (IHOR.EQ.1) THEN       ! INSTAENTANEOUS FIELD PRINT
	        WRITE(21,9831) IL(L),JL(L),DLON(L),DLAT(L),((PC(L,K),PD(L,K),PG(L,K),(WQLIM(L,K,NW),NW=1,NWM),WQLIM2(L,K)),K=KC,1,-1)
 9831       FORMAT(2I5,2F11.2,1x,<FORLEN>(E11.3))
        ENDIF ! 
      ENDDO   ! LA
      CLOSE(21)
      
! 
    ENDIF !IF ( TIMTMP.GE.SDAY.AND.TIMTMP.LE.EDAY ) THEN
    DO L=2,LA
      DO K=1,KC 
        DO NW=1,NWM
          WQLIMB(L,K,NW)=WQLIM(L,K,NW)
        ENDDO
      ENDDO
    ENDDO

    TIMTMPB=TIMTMP

    goto 620
629 continue

!!!
  CLOSE(6);       
  CLOSE(7);       
ENDSUBROUTINE !SUBROUTINE LIM
!}

!###################################### SUBROUTINE SEEK(TAG)  
      SUBROUTINE SEEK(TAG)  

      CHARACTER TAG*(*)  
      CHARACTER*80 TEXT  

      L=LEN(TAG)  
      DO I=1,L  
        J=ICHAR(TAG(I:I))  
        IF(97.LE.J.AND.J.LE.122)THEN  
          TAG(I:I)=CHAR(J-32)  
        ENDIF  
      ENDDO  
!      WRITE(7,'(A,A)')'SEEKING GROUP: ',TAG  
      DO K=1,2  
   10   READ(1,'(A)',END=20)TEXT  
        M=MAX(1,LEN_TRIM(TEXT))  
!        WRITE(7,'(A)')TEXT(1:M)  
        DO WHILE(M.GT.L.AND.TEXT(1:1).EQ.'')  
          TEXT(1:M-1)=TEXT(2:M)  
          TEXT(M:M)=' '  
          M=M-1  
        ENDDO  
        IF(M.LT.L)GO TO 10  
        DO I=1,M  
          J=ICHAR(TEXT(I:I))  
          IF(97.LE.J.AND.J.LE.122)THEN  
            TEXT(I:I)=CHAR(J-32)  
          ENDIF  
        ENDDO  
        IF(TEXT(1:L).NE.TAG)GO TO 10  
        IF(TEXT(L+1:L+1).NE.' ')GO TO 10  
      ENDDO  
      RETURN  

   20 WRITE(*,'(A,A,A)')'GROUP: ',TAG,' NOT FOUND BEFORE END OF FILE'  
      STOP  

      ENDSUBROUTINE
!###################################### END SUBROUTINE SEEK(TAG)  

!###################################### SUBROUTINE MAKEWQHORFILES
      SUBROUTINE MAKEWQHORFILES
        CHARACTER(LEN=100) :: FNAME

	      !!! TOC {
	      WRITE(FNAME,'(A,I8.8,A)')'TOC',N,'.DAT'
	      OPEN(211,FILE=TRIM(FNAME),STATUS='UNKNOWN')
	      WRITE(211,'(4X,A1,4X,A1,5X,A1,10X,A1,10X,<KC>(A3,I2.2,4X))')  &
                  'I','J','X','Y',('TOC',K,K=KC,1,-1)
        !!! TOC }

        !!! CHL {
        WRITE(FNAME,'(A,I8.8,A)')'CHL',N,'.DAT'
	      OPEN(212,FILE=TRIM(FNAME),STATUS='UNKNOWN')
	      WRITE(212,'(4X,A1,4X,A1,5X,A1,10X,A1,10X,<KC>(A3,I2.2,4X))')  &
		               'I','J','X','Y',('CHL',K,K=KC,1,-1)	      
        !!! CHL }
        
        !!! T-N {
        WRITE(FNAME,'(A,I8.8,A)')'T-N',N,'.DAT'
	      OPEN(213,FILE=TRIM(FNAME),STATUS='UNKNOWN')
	      WRITE(213,'(4X,A1,4X,A1,5X,A1,10X,A1,10X,<KC>(A3,I2.2,4X))')  &
		               'I','J','X','Y',('T-N',K,K=KC,1,-1)
        !!! T-N }
        
        !!! T-P {
        WRITE(FNAME,'(A,I8.8,A)')'T-P',N,'.DAT'
	      OPEN(214,FILE=TRIM(FNAME),STATUS='UNKNOWN')
	      WRITE(214,'(4X,A1,4X,A1,5X,A1,10X,A1,10X,<KC>(A3,I2.2,4X))')  &
		               'I','J','X','Y',('T-P',K,K=KC,1,-1)
        !!! T-P }
        
        !!! DO {
        WRITE(FNAME,'(A,I8.8,A)')'DOX',N,'.DAT'
	      OPEN(215,FILE=TRIM(FNAME),STATUS='UNKNOWN')
	      WRITE(215,'(4X,A1,4X,A1,5X,A1,10X,A1,10X,<KC>(A3,I2.2,4X))')  &
		               'I','J','X','Y',('DOX',K,K=KC,1,-1)
        !!! DO }
        
        !!! Cyano {
        WRITE(FNAME,'(A,I8.8,A)')'CHC',N,'.DAT'
	      OPEN(216,FILE=TRIM(FNAME),STATUS='UNKNOWN')
	      WRITE(216,'(4X,A1,4X,A1,5X,A1,10X,A1,10X,<KC>(A3,I2.2,4X))')  &
		               'I','J','X','Y',('CHC',K,K=KC,1,-1)
        !!! Cyano }
        
        !!! Diatom {
        WRITE(FNAME,'(A,I8.8,A)')'CHD',N,'.DAT'
	      OPEN(217,FILE=TRIM(FNAME),STATUS='UNKNOWN')
	      WRITE(217,'(4X,A1,4X,A1,5X,A1,10X,A1,10X,<KC>(A3,I2.2,4X))')  &
		               'I','J','X','Y',('CHD',K,K=KC,1,-1)
        !!! Diatom }
        
        !!! Others {
        WRITE(FNAME,'(A,I8.8,A)')'CHG',N,'.DAT'
	      OPEN(218,FILE=TRIM(FNAME),STATUS='UNKNOWN')
	      WRITE(218,'(4X,A1,4X,A1,5X,A1,10X,A1,10X,<KC>(A3,I2.2,4X))')  &
		               'I','J','X','Y',('CHG',K,K=KC,1,-1)
        !!! Others }
        
        !!! DIN {
        WRITE(FNAME,'(A,I8.8,A)')'DIN',N,'.DAT'
	      OPEN(219,FILE=TRIM(FNAME),STATUS='UNKNOWN')
	      WRITE(219,'(4X,A1,4X,A1,5X,A1,10X,A1,10X,<KC>(A3,I2.2,4X))')  &
		               'I','J','X','Y',('DIN',K,K=KC,1,-1)
        !!! DIN }

      RETURN
      ENDSUBROUTINE
!###################################### END SUBROUTINE MAKEWQHORFILES

!{ 140317, JHLEE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE TEC3D_TEXT(ZN, EETIME)
IMPLICIT NONE
  INTEGER :: ZN
  REAL :: EETIME
  
  IF(ITIME.EQ.0) THEN
  	IF(ITFILE.EQ.1) THEN
      WRITE(501,'(A,F5.2,1x,A,F5.2,1x,A,F5.2,1x,A,I3,A,I4,A,I2.2,A,I2.2,A)') 'TEXT X=',TEXTX,'y=',TEXTY, 'H=',TEXTH, 'ZN=', ZN,', & 
                                      C=BLACK, T="DATE:',YY,'/',IM,'/',ID,'"'                                  
    ELSE                                  
      WRITE(500,'(A,F5.2,1x,A,F5.2,1x,A,F5.2,1x,A,I3,A,I4,A,I2.2,A,I2.2,A)') 'TEXT X=',TEXTX,'y=',TEXTY, 'H=',TEXTH, 'ZN=', ZN,', & 
                                      C=BLACK, T="DATE:',YY,'/',IM,'/',ID,'"'                                    
    ENDIF                                      
  ELSEIF(ITIME.EQ.1) THEN
  	IF(ITFILE.EQ.1) THEN
      WRITE(501,'(A,F5.2,1x,A,F5.2,1x,A,F5.2,1x,A,I3,A,I4,A,I2.2,A,I2.2,1x,I2.2,A,A)') 'TEXT X=',TEXTX,'y=',TEXTY, 'H=',TEXTH, 'ZN=', ZN,', & 
                                      C=BLACK, T="DATE:',YY,'/',IM,'/',ID,IH,':00','"'
    ELSE
      WRITE(500,'(A,F5.2,1x,A,F5.2,1x,A,F5.2,1x,A,I3,A,I4,A,I2.2,A,I2.2,1x,I2.2,A,A)') 'TEXT X=',TEXTX,'y=',TEXTY, 'H=',TEXTH, 'ZN=', ZN,', & 
                                      C=BLACK, T="DATE:',YY,'/',IM,'/',ID,IH,':00','"'
    ENDIF                                           	                                      
  ENDIF     
  IF(ITFILE.EQ.1) THEN
!  	WRITE(501,'((A,I8),(A,I6),(A,F9.5))') 'ZONE N = ', NODE, ', E = ', ELEMENT, ', SolutionTime = ', EETIME
!  	WRITE(501,'(A)') 'DATAPACKING=BLOCK, ZONETYPE=Ordered'
!  	WRITE(501,'(A)') 'VARLOCATION=([4,5,6]=CELLCENTERED)'
   WRITE(501,'((A,I8),(A,I6),(A,F9.5),A)') 'ZONE N = ', NODE, ', E = ', ELEMENT, ', SolutionTime = ', EETIME , ', DATAPACKING=POINT, ZONETYPE=FEBRICK'
  ELSE	
    WRITE(500,'((A,I8),(A,I6),(A,F9.5),A)') 'ZONE N = ', NODE, ', E = ', ELEMENT, ', SolutionTime = ', EETIME , ', DATAPACKING=POINT, ZONETYPE=FEBRICK'
  ENDIF
  
ENDSUBROUTINE
!###################################### END SUBROUTINE TEC3D_TEXT

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE TEC3D_LIM_TEXT(ZN, EETIME)
IMPLICIT NONE
  INTEGER :: ZN
  REAL :: EETIME
  
  IF(ITIME.EQ.0) THEN
    WRITE(501,'(A,F5.2,1x,A,F5.2,1x,A,F5.2,1x,A,I3,A,I4,A,I2.2,A,I2.2,A)') 'TEXT X=',TEXTX,'y=',TEXTY, 'H=',TEXTH, 'ZN=', ZN,', & 
                                    C=BLACK, T="DATE:',YY,'/',IM,'/',ID,'"'
    WRITE(502,'(A,F5.2,1x,A,F5.2,1x,A,F5.2,1x,A,I3,A,I4,A,I2.2,A,I2.2,A)') 'TEXT X=',TEXTX,'y=',TEXTY, 'H=',TEXTH, 'ZN=', ZN,', & 
                                    C=BLACK, T="DATE:',YY,'/',IM,'/',ID,'"'
    WRITE(503,'(A,F5.2,1x,A,F5.2,1x,A,F5.2,1x,A,I3,A,I4,A,I2.2,A,I2.2,A)') 'TEXT X=',TEXTX,'y=',TEXTY, 'H=',TEXTH, 'ZN=', ZN,', & 
                                    C=BLACK, T="DATE:',YY,'/',IM,'/',ID,'"'                                                                        
  ELSEIF(ITIME.EQ.1) THEN
    WRITE(501,'(A,F5.2,1x,A,F5.2,1x,A,F5.2,1x,A,I3,A,I4,A,I2.2,A,I2.2,1x,I2.2,A,A)') 'TEXT X=',TEXTX,'y=',TEXTY, 'H=',TEXTH, 'ZN=', ZN,', & 
                                    C=BLACK, T="DATE:',YY,'/',IM,'/',ID,IH,':00','"'
    WRITE(502,'(A,F5.2,1x,A,F5.2,1x,A,F5.2,1x,A,I3,A,I4,A,I2.2,A,I2.2,1x,I2.2,A,A)') 'TEXT X=',TEXTX,'y=',TEXTY, 'H=',TEXTH, 'ZN=', ZN,', & 
                                    C=BLACK, T="DATE:',YY,'/',IM,'/',ID,IH,':00','"'
    WRITE(503,'(A,F5.2,1x,A,F5.2,1x,A,F5.2,1x,A,I3,A,I4,A,I2.2,A,I2.2,1x,I2.2,A,A)') 'TEXT X=',TEXTX,'y=',TEXTY, 'H=',TEXTH, 'ZN=', ZN,', & 
                                    C=BLACK, T="DATE:',YY,'/',IM,'/',ID,IH,':00','"'                                                                        
  ENDIF     
  WRITE(501,'((A,I8),(A,I6),(A,F9.5),A)') 'ZONE N = ', NODE, ', E = ', ELEMENT, ', SolutionTime = ', EETIME , ', DATAPACKING=POINT, ZONETYPE=FEBRICK'
  WRITE(502,'((A,I8),(A,I6),(A,F9.5),A)') 'ZONE N = ', NODE, ', E = ', ELEMENT, ', SolutionTime = ', EETIME , ', DATAPACKING=POINT, ZONETYPE=FEBRICK'
  WRITE(503,'((A,I8),(A,I6),(A,F9.5),A)') 'ZONE N = ', NODE, ', E = ', ELEMENT, ', SolutionTime = ', EETIME , ', DATAPACKING=POINT, ZONETYPE=FEBRICK'
  
ENDSUBROUTINE
!###################################### END SUBROUTINE TEC3D_LIM_TEXT

! {JHLEE, 20141010
!###################################### SUBROUTINE MAKEWQTSFILES
      SUBROUTINE MAKEWQTSFILES
        CHARACTER(LEN=100) :: TFNAME
	      !!! TOC {
	      WRITE(TFNAME,'(A,I5.5,A,I5.5,A)')'TIME_TOC',ITSR(I),'_',JTSR(I),'.DAT'
 	      OPEN(1100+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
	      PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
        WRITE(1100+I,'(3X,A4,9X,A1,7X,<KC>(A3,I2.2,4X))')  &
		                 'TIME','N',('TOC',K,K=KC,1,-1)
        !!! TOC }
        
        !!! CHL {
	      WRITE(TFNAME,'(A,I5.5,A,I5.5,A)')'TIME_CHL',ITSR(I),'_',JTSR(I),'.DAT'
 	      OPEN(1200+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
	      PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
        WRITE(1200+I,'(3X,A4,9X,A1,7X,<KC>(A3,I2.2,4X))')  &
		                 'TIME','N',('CHL',K,K=KC,1,-1)
        !!! CHL }
        
        !!! T-N {
	      WRITE(TFNAME,'(A,I5.5,A,I5.5,A)')'TIME_T-N',ITSR(I),'_',JTSR(I),'.DAT'
 	      OPEN(1300+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
	      PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
        WRITE(1300+I,'(3X,A4,9X,A1,7X,<KC>(A3,I2.2,4X))')  &
		                 'TIME','N',('T-N',K,K=KC,1,-1)
        !!! T-N }
        
        !!! T-P {
	      WRITE(TFNAME,'(A,I5.5,A,I5.5,A)')'TIME_T-P',ITSR(I),'_',JTSR(I),'.DAT'
 	      OPEN(1400+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
	      PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
        WRITE(1400+I,'(3X,A4,9X,A1,7X,<KC>(A3,I2.2,4X))')  &
		                 'TIME','N',('T-P',K,K=KC,1,-1)
        !!! T-P }
        
        !!! DO {
	      WRITE(TFNAME,'(A,I5.5,A,I5.5,A)')'TIME_DOX',ITSR(I),'_',JTSR(I),'.DAT'
 	      OPEN(1500+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
	      PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
        WRITE(1500+I,'(3X,A4,9X,A1,7X,<KC>(A3,I2.2,4X))')  &
		                 'TIME','N',('DOX',K,K=KC,1,-1)
        !!! DO }
        
        !!! Cyano {
	      WRITE(TFNAME,'(A,I5.5,A,I5.5,A)')'TIME_CHC',ITSR(I),'_',JTSR(I),'.DAT'
 	      OPEN(1600+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
	      PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
        WRITE(1600+I,'(3X,A4,9X,A1,7X,<KC>(A3,I2.2,4X))')  &
		                 'TIME','N',('CHC',K,K=KC,1,-1)
        !!! Cyano }
        
        !!! Diatom {
	      WRITE(TFNAME,'(A,I5.5,A,I5.5,A)')'TIME_CHD',ITSR(I),'_',JTSR(I),'.DAT'
 	      OPEN(1700+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
	      PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
        WRITE(1700+I,'(3X,A4,9X,A1,7X,<KC>(A3,I2.2,4X))')  &
		                 'TIME','N',('CHD',K,K=KC,1,-1)
        !!! Diatom }
        
        !!! Others {
	      WRITE(TFNAME,'(A,I5.5,A,I5.5,A)')'TIME_CHG',ITSR(I),'_',JTSR(I),'.DAT'
 	      OPEN(1800+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
	      PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
        WRITE(1800+I,'(3X,A4,9X,A1,7X,<KC>(A3,I2.2,4X))')  &
		                 'TIME','N',('CHG',K,K=KC,1,-1)
        !!! Others }
        
        !!! DIN {
	      WRITE(TFNAME,'(A,I5.5,A,I5.5,A)')'TIME_DIN',ITSR(I),'_',JTSR(I),'.DAT'
 	      OPEN(1900+I,FILE=TRIM(TFNAME),STATUS='UNKNOWN')
	      PRINT *,"TIMESERIES FILE : ",TRIM(TFNAME)
        WRITE(1900+I,'(3X,A4,9X,A1,7X,<KC>(A3,I2.2,4X))')  &
		                 'TIME','N',('DIN',K,K=KC,1,-1)
        !!! Others }

      RETURN
      ENDSUBROUTINE
!###################################### END SUBROUTINE MAKEWQTSFILES
! }

! {JHLEE, 20141010
!###################################### SUBROUTINE MAKEWQTSFILES
SUBROUTINE HEADER
IMPLICIT NONE

FORLEN=KC*(NWM+4)

  CH(1)='PC_'
  CH(2)='PD_'
  CH(3)='PG_'
  CH(4)='LNC'
  CH(5)='LND'
  CH(6)='LNG'   
  CH(7)='LNM'   
  CH(8)='LPC'   
  CH(9)='LPD'
  CH(10)='LPG'
  CH(11)='LPM'
  CH(12)='LIC'
  CH(13)='LID'
  CH(14)='LIG'
  CH(15)='LIM'
  CH(16)='LTC'
  CH(17)='LTD'
  CH(18)='LTG'
  CH(19)='LTM'
  CH(20)='LVM'
  CH(21)='LDM'
  CH(22)='LSD'

  
ENDSUBROUTINE
!###################################### END HEADER

!###################################### SUBROUTINE WQ21_Tec_FILENAME
SUBROUTINE WQ21_Tec_FILENAME
IMPLICIT NONE

  CHARACTER(LEN=3) :: VARNAME
  
  SELECT CASE(IWQOUT)
    CASE(1)
      VARNAME='CHC'
    CASE(2)
      VARNAME='CHG'
    CASE(3)
      VARNAME='CHD'
    CASE(4)
      VARNAME='ROC'
    CASE(5)
      VARNAME='LOC'
    CASE(6)
      VARNAME='DOC'
    CASE(7)
      VARNAME='ROP'
    CASE(8)
      VARNAME='LOP'
    CASE(9)
      VARNAME='DOP'
    CASE(10)
      VARNAME='P4D'
    CASE(11)
      VARNAME='RON'
    CASE(12)
      VARNAME='LON'
    CASE(13)
      VARNAME='DON'
    CASE(14)
      VARNAME='NHX'
    CASE(15)
      VARNAME='NOX'
    CASE(16)
      VARNAME='SUU'
    CASE(17)
      VARNAME='SAA'     
    CASE(18)
      VARNAME='COD'            
    CASE(19)
      VARNAME='DOX'            
    CASE(20)
      VARNAME='TAM'            
    CASE(21)
      VARNAME='FCB'                  
  ENDSELECT
ENDSUBROUTINE
!###################################### SUBROUTINE WQ21_Tec_FILENAME
  
END
