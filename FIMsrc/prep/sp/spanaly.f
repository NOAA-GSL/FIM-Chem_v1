C-----------------------------------------------------------------------
      SUBROUTINE SPANALY(I,M,IM,IX,NC,NCTOP,KM,WGT,CLAT,PLN,PLNTOP,MP,
     &                   F,SPC,SPCTOP)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPANALY     ANALYZE SPECTRAL FROM FOURIER
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: ANALYZES SPECTRAL COEFFICIENTS FROM FOURIER COEFFICIENTS
C           FOR A LATITUDE PAIR (NORTHERN AND SOUTHERN HEMISPHERES).
C           VECTOR COMPONENTS ARE MULTIPLIED BY COSINE OF LATITUDE.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C   94-08-01  MARK IREDELL   MOVED ZONAL WAVENUMBER LOOP INSIDE
C 1998-12-15  IREDELL  OPENMP DIRECTIVES INSERTED
C
C USAGE:    CALL SPANALY(I,M,IM,IX,NC,NCTOP,KM,WGT,CLAT,PLN,PLNTOP,MP,
C    &                   F,SPC,SPCTOP)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     IM       - INTEGER EVEN NUMBER OF FOURIER COEFFICIENTS
C     IX       - INTEGER DIMENSION OF FOURIER COEFFICIENTS (IX>=IM+2)
C     NC       - INTEGER DIMENSION OF SPECTRAL COEFFICIENTS
C                (NC>=(M+1)*((I+1)*M+2))
C     NCTOP    - INTEGER DIMENSION OF SPECTRAL COEFFICIENTS OVER TOP
C                (NCTOP>=2*(M+1))
C     KM       - INTEGER NUMBER OF FIELDS
C     WGT      - REAL GAUSSIAN WEIGHT
C     CLAT     - REAL COSINE OF LATITUDE
C     PLN      - REAL ((M+1)*((I+1)*M+2)/2) LEGENDRE POLYNOMIALS
C     PLNTOP   - REAL (M+1) LEGENDRE POLYNOMIAL OVER TOP
C     MP       - INTEGER (KM) IDENTIFIERS (0 FOR SCALAR, 1 FOR VECTOR)
C     F        - REAL (IX,2,KM) FOURIER COEFFICIENTS COMBINED
C     SPC      - REAL (NC,KM) SPECTRAL COEFFICIENTS
C     SPCTOP   - REAL (NCTOP,KM) SPECTRAL COEFFICIENTS OVER TOP
C
C   OUTPUT ARGUMENT LIST:
C     SPC      - REAL (NC,KM) SPECTRAL COEFFICIENTS
C     SPCTOP   - REAL (NCTOP,KM) SPECTRAL COEFFICIENTS OVER TOP
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      INTEGER MP(KM)
      REAL PLN((M+1)*((I+1)*M+2)/2),PLNTOP(M+1)
      REAL F(IX,2,KM)
      REAL SPC(NC,KM),SPCTOP(NCTOP,KM)
      REAL FW(2,2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FOR EACH ZONAL WAVENUMBER, ANALYZE TERMS OVER TOTAL WAVENUMBER.
C  ANALYZE EVEN AND ODD POLYNOMIALS SEPARATELY.
      LX=MIN(M,IM/2)
!JR Mods to get omp to work
!C$OMP PARALLEL DO PRIVATE(L,NT,KS,KP,FW,n)
      DO K=1,KM
        DO L=0,LX
          NT=MOD(M+1+(I-1)*L,2)+1
          KS=L*(2*M+(I-1)*(L-1))
          KP=KS/2+1
          IF(MP(K).EQ.0) THEN
            FW(1,1)=WGT*(F(2*L+1,1,K)+F(2*L+1,2,K))
            FW(2,1)=WGT*(F(2*L+2,1,K)+F(2*L+2,2,K))
            FW(1,2)=WGT*(F(2*L+1,1,K)-F(2*L+1,2,K))
            FW(2,2)=WGT*(F(2*L+2,1,K)-F(2*L+2,2,K))
          ELSE
            FW(1,1)=WGT*CLAT*(F(2*L+1,1,K)+F(2*L+1,2,K))
            FW(2,1)=WGT*CLAT*(F(2*L+2,1,K)+F(2*L+2,2,K))
            FW(1,2)=WGT*CLAT*(F(2*L+1,1,K)-F(2*L+1,2,K))
            FW(2,2)=WGT*CLAT*(F(2*L+2,1,K)-F(2*L+2,2,K))
            SPCTOP(2*L+1,K)=SPCTOP(2*L+1,K)+PLNTOP(L+1)*FW(1,NT)
            SPCTOP(2*L+2,K)=SPCTOP(2*L+2,K)+PLNTOP(L+1)*FW(2,NT)
          ENDIF
          DO N=L,I*L+M,2
            SPC(KS+2*N+1,K)=SPC(KS+2*N+1,K)+PLN(KP+N)*FW(1,1)
            SPC(KS+2*N+2,K)=SPC(KS+2*N+2,K)+PLN(KP+N)*FW(2,1)
          ENDDO
          DO N=L+1,I*L+M,2
            SPC(KS+2*N+1,K)=SPC(KS+2*N+1,K)+PLN(KP+N)*FW(1,2)
            SPC(KS+2*N+2,K)=SPC(KS+2*N+2,K)+PLN(KP+N)*FW(2,2)
          ENDDO
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
