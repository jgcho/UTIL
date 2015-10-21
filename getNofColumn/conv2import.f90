!234567

      INTEGER     :: NVAL
      REAL,ALLOCATABLE:: A(:,:)
      CHARACTER(1)   :: $CH
      CHARACTER(1)   :: $DELIM =CHAR(44) ! 9 if tab, or ',' ' ' etc
      OPEN(5, FILE='201507161600_EFDC_Yeongsan_UpdateStates_DLL1.csv')
!  Read first record one character at a time and count delimiters.
      NVAL = 1 ! First value requires no delimiter
      DO
        read (5,'(A)', advance='no', eor=10) $ch
        if ($ch == $delim) nval = nval + 1
      END DO
   10 CONTINUE
! According to delimiters the first line contains NVALS values.
! Read first record to verify (not shown)
! Read rest of file (without saving values) to get a record count.
      REWIND (5)
      NREC = 0
      DO
        READ (5, *, END=20)
        NREC = NREC + 1
      END DO
   20 CONTINUE
! Read the data for real now that size requirments are known.
      write(*,*) NREC,NVAL
      ALLOCATE (A(NREC, NVAL))
      REWIND(5)
      
      DO IREC = 1, NREC
        READ (5, *, IOSTAT = IERR) (A(IREC, IVAL), IVAL = 1, NVAL)
        write(*,*) A(IREC,2)
        ! ierr trap not shown for clarity.
      END DO
      write(*,*) A(1,423)
      DEALLOCATE(A)! Not needed but good practice

      stop
      end