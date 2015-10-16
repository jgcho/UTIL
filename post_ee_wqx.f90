!234567
      integer VER
      
      open(51,file='15_07.dat')
      
      open(11,file='../EE_WQX.OUT',STATUS='OLD',FORM='BINARY')
      read(11) VER
      read(11) NXSP,LA,KC
      write(*,*) VER,NXSP,LA,KC
      
  111 read(11,end=119) EETIME,N
      write(*,*) EETIME,N
      do nsp=1,NXSP
        do K=1,KC
          do L=2,LA
            READ(11)WQ
            if (L.eq.134 .and. K.eq.10) WQ10=WQ
            if (L.eq.134 .and. K.eq.1) WQ1=WQ
          ENDDO
        ENDDO
      ENDDO
      write(51,5101) EETIME,WQ10,WQ1
      goto 111
  119 continue
      close(11)

 5101 format(f11.7,2e11.3)
   
      stop
      end