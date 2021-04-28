        SUBROUTINE AEA (IA, IE, NC )
C$$$    SUBPROGRAM DOCUMENTATION  BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    AEA         ASCII TO EBCDIC, OR EBCDIC TO ASCII
C   PRGMMR: DESMARAIS        ORG: W342       DATE: 82-11-29
C
C ABSTRACT: CONVERT ASCII TO EBCDIC, OR EBCDIC TO ASCII BY CHARACTER.
C   THIS SUBROUTINE CAN BE REPLACED BY CRAY UTILITY SUBROUTINES
C   USCCTC AND USCCTT. SEE MANUAL SR-2079 PAGE 3-15. CRAY UTILITY TR
C   CAN ALSO BE USED FOR ASCII, EBCDIC CONVERSION. SEE MANUAL SR-2079
C   PAGE 9-35.
C
C PROGRAM HISTORY LOG:
C   82-11-29  DESMARAIS
C   88-03-31  R.E.JONES  CHANGE LOGIC SO IT WORKS LIKE A
C                        IBM370 TRANSLATE INSTRUCTION.
C   88-08-22  R.E.JONES  CHANGES FOR MICROSOFT FORTRAN 4.10
C   88-09-04  R.E.JONES  CHANGE TABLES TO 128 CHARACTER SET
C   90-01-31  R.E.JONES  CONVERT TO CRAY CFT77 FORTRAN
C                        CRAY DOES NOT ALLOW CHAR*1 TO BE SET TO HEX
C   98-12-21  Gilbert    Replaced Function ICHAR with mova2i.
C
C USAGE:    CALL AEA (IA, IE, NC)
C   INPUT ARGUMENT LIST:
C     IA       - CHARACTER*1 ARRAY OF ASCII DATA  IF NC < 0
C     IE       - CHARACTER*1 ARRAY OF EBCDIC DATA IF NC > 0
C     NC       - INTEGER,  CONTAINS CHARACTER COUNT TO CONVERT....
C                IF NC .LT. 0,  CONVERT ASCII TO EBCDIC
C                IF NC .GT. 0,  CONVERT EBCDIC TO ASCII
C
C   OUTPUT ARGUMENT LIST:
C     IA       - CHARACTER*1 ARRAY OF ASCII  DATA IF NC > 0
C     IE       - CHARACTER*1 ARRAY OF EBCDIC DATA IF NC < 0
C
C REMARKS: SOFTWARE VERSION OF IBM370 TRANSLATE INSTRUCTION, BY
C   CHANGING THE TWO TABLES WE COULD DO A  64, 96, 128  ASCII
C   CHARACTER SET, CHANGE LOWER CASE TO UPPER, ETC.
C   AEA CONVERTS DATA AT A RATE OF 1.5 MILLION CHARACTERS PER SEC.
C   CRAY UTILITY USCCTI CONVERT IBM EBCDIC TO ASCII
C   CRAY UTILITY USCCTC CONVERT ASCII TO IBM EBCDIC
C   THEY CONVERT DATA AT A RATE OF 2.1 MILLION CHARACTERS PER SEC.
C   CRAY UTILITY TR WILL ALSO DO A ASCII, EBCDIC CONVERSION.
C   TR CONVERT DATA AT A RATE OF 5.4 MILLION CHARACTERS PER SEC.
C   TR IS IN LIBRARY  /USR/LIB/LIBCOS.A   ADD TO SEGLDR CARD.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C***   ASCII  CONTAINS ASCII CHARACTERS, AS PUNCHED ON IBM029
C
      INTEGER(8)      IASCII(32)
      INTEGER(8)      IEBCDC(32)
C
      CHARACTER*1  IA(*)
      CHARACTER*1  IE(*)
      CHARACTER*1  ASCII(0:255)
      CHARACTER*1  EBCDIC(0:255)
C
      EQUIVALENCE  (IASCII(1),ASCII(0))
      EQUIVALENCE  (IEBCDC(1),EBCDIC(0))
C
       DATA  IASCII/
     & Z'000102030009007F',Z'0000000B0C0D0E0F',
     & Z'1011120000000000',Z'1819000000000000',
     & Z'00001C000A001700',Z'0000000000050607',
     & Z'00001600001E0004',Z'000000001415001A',
     & Z'2000600000000000',Z'0000602E3C282B00',
     & Z'2600000000000000',Z'000021242A293B5E',
     & Z'2D2F000000000000',Z'00007C2C255F3E3F',
     & Z'0000000000000000',Z'00603A2340273D22',
     & Z'2061626364656667',Z'6869202020202020',
     & Z'206A6B6C6D6E6F70',Z'7172202020202020',
     & Z'207E737475767778',Z'797A2020205B2020',
     & Z'0000000000000000',Z'00000000005D0000',
     & Z'7B41424344454647',Z'4849202020202020',
     & Z'7D4A4B4C4D4E4F50',Z'5152202020202020',
     & Z'5C20535455565758',Z'595A202020202020',
     & Z'3031323334353637',Z'3839202020202020'/
C
C***  EBCDIC CONTAINS HEX. REPRESENTATION OF EBCDIC CHARACTERS
C
       DATA  IEBCDC/
     & Z'00010203372D2E2F',Z'1605250B0C0D0E0F',
     & Z'101112003C3D3226',Z'18193F2722003500',
     & Z'405A7F7B5B6C507D',Z'4D5D5C4E6B604B61',
     & Z'F0F1F2F3F4F5F6F7',Z'F8F97A5E4C7E6E6F',
     & Z'7CC1C2C3C4C5C6C7',Z'C8C9D1D2D3D4D5D6',
     & Z'D7D8D9E2E3E4E5E6',Z'E7E8E9ADE0BD5F6D',
     & Z'7981828384858687',Z'8889919293949596',
     & Z'979899A2A3A4A5A6',Z'A7A8A9C06AD0A107',
     & 16*Z'4040404040404040'/
C
      NUM = IABS(NC)
C
      IF (NC .EQ. 0)   RETURN
C
      IF (NC .GT. 0)   THEN
C
C***  CONVERT STRING ...  EBCDIC TO ASCII,   NUM CHARACTERS
C
        DO  10  J = 1, NUM
          IA(J) = ASCII(mova2i(IE(J)))
 10     CONTINUE
C
      ELSE
C
C***  CONVERT STRING ...  ASCII TO EBCDIC,   NUM CHARACTERS
C
        DO  20  J = 1, NUM
          IE(J) = EBCDIC(mova2i(IA(J)))
 20     CONTINUE
      END IF
C
      RETURN
      END
