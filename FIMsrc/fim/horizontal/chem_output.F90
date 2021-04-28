module module_chem_output

  real, parameter :: g = 9.80796
  REAL, PARAMETER :: airmw = 28.97
contains

  subroutine chem_output(its,nts,aod2d,exch,p10,pm25,emid1st,emid2st,emid3st,emid4st,emid5st,ero1,ero2,ero3,pr3d,ph3d,tk3d,tr,trfall,&  
    phys2dwrf,tr1_tavg,d1st_ave,d2st_ave,d3st_ave,d4st_ave,d5st_ave,emid1_ave,emid2_ave,emid3_ave,emid4_ave,emid5_ave,aod2d_ave,&
    oh_bg,h2o2_bg,no3_bg,ebu_oc,wet_dep)

    use module_constants,only:rd
    use module_control,  only:ArchvStep,dt,nip,ntra,ntrb,nvlp1
    use fimnamelist,     only:ArchvTimeUnit,nvl
    use icosio_wrapper,  only: maybe_write
    use module_initial_chem_namelists
    use module_wrf_control,only:num_chem,num_moist,nvl_gocart,nvl_gocart,numgas
    USE module_chemvars, only:num_emis_dust 
    use module_chem_variables,only: emiss_ab1,emiss_abu,dm0
    USE module_variables, only:ext_cof
!    USE module_chem_variables, only:emid1_ave,emid2_ave,emid3_ave,emid4_ave,emid5_ave,&
!       d1st_ave,d2st_ave,d3st_ave,d4st_ave,d5st_ave 
    integer,intent(in)::its,nts
!sms$distribute (dh,1) begin
    real,intent(inout)::emid1_ave(nip),emid2_ave(nip),emid3_ave(nip),emid4_ave(nip),emid5_ave(nip)
    real,intent(inout)::aod2d(nip), aod2d_ave(nip)
    real,intent(in)::trfall(nip,num_chem),wet_dep(nip,num_chem)
    real::intaer(nip),intash(nip),intbc(nip),intdust(nip),intoc(nip),&
      intsulf(nip),o3du(nip),o3dg(nip)
    real,intent(in)::phys2dwrf(:,:) ! (nip,:)
    real,intent(in)::emid1st(nip),emid2st(nip),emid3st(nip),emid4st(nip),emid5st(nip) 
    real,intent(in)::ero1(nip),ero2(nip),ero3(nip) 
!sms$distribute end
!sms$distribute(dh,2) begin
    real,intent(inout):: tr1_tavg(nvl,nip)
    real,intent(inout)::d1st_ave(nvl,nip),d2st_ave(nvl,nip),d3st_ave(nvl,nip),d4st_ave(nvl,nip),d5st_ave(nvl,nip)
    real,intent(in)::oh_bg(nvl,nip),h2o2_bg(nvl,nip),no3_bg(nvl,nip),ebu_oc(nvl,nip)
    real,intent(in)::exch(nvl,nip),p10(nvl,nip),pm25(nvl,nip),pr3d(nvlp1,nip),ph3d(nvlp1,nip),tk3d(nvl,nip),&
      tr(nvl,nip,ntra+ntrb)
    real::d1st(nvl,nip),d2st(nvl,nip),d3st(nvl,nip),d4st(nvl,nip),d5st(nvl,nip),&
      dms1(nvl,nip),qcct(nvl,nip),qict(nvl,nip),qrct(nvl,nip),qsct(nvl,nip),&
      rho_phys(nvl,nip),sea1(nvl,nip),sea2(nvl,nip),sea3(nvl,nip),sea4(nvl,nip),&
      trco(nvl,nip)
!sms$distribute end
!      data (chem_names301(n), n=1,49) /'pso2','sulf','pno2','ppno','ppo3',             &
      character (len=4), parameter :: chem_names301(49) = (/ &
                           'pso2','sulf','pno2','ppno','ppo3',        &
                          'hno3','h2o2','pald','hcho','pop1','pop2',  &
                          'ppaa','ora1','ora2','pnh3','n2o5','pno3',  &
                          'ppan','phc3','phc5','phc8','peth','ppco',  &
                          'pete','polt','poli','ptol','pxyl','aco3',  &
                          'tpan','hono','hno4','pket','pgly','mgly',  &
                          'pdcb','onit','pcsl','piso','pco2','pch4',  &
                          'pudd','hket','papi','plim','dien','macr',  &
                          'ppho','pho2'/)
      character (len=4), parameter :: chem_names108(103) = (/ &
                          'pso2','sulf','pno2','ppno','ppo3',         &
                          'hno3','h2o2','pald','hcho','pop1','pop2',  &
                          'ppaa','ora1','ora2','pnh3','n2o5','pno3',  &
                          'ppan','phc3','phc5','phc8','peth','ppco',  &
                          'pete','polt','poli','ptol','pxyl','aco3',  &
                          'tpan','hono','hno4','pket','pgly','mgly',  &
                          'pdcb','onit','pcsl','piso','pco2','pch4',  &
                          'pudd','hket','papi','plim','dien','macr',  &
                          'hace','ishp','ison','mahp','mpan','nald',  &
                          'sesq','pmbo','cva1','cva2','cva3','cva4',  &
                          'cvb1','cvb2','cvb3','cvb4','ppho','pho2',  &
                          'soaj','soai','nhaj','nhai','n3aj','n3ai',  &
                          'naaj','naai','claj','clai','as1j','as1i',  &
                          'as2j','as2i','as3j','as3i','as4j','as4i',  &
                          'bs1j','bs1i','bs2j','bs2i','bs3j','bs3i',  &
                          'bs4j','bs4i','opaj','opai','pecj','peci',  &
                          'p25j','p25i','atha','seas','sila','pnu0',  &
                          'pac0','corn'/)

    integer::ichem_start,imoist_start,j,k
    real::dpsum
    logical, parameter:: flux_avg=.false.        ! T: write fluxes averaged over "ArchvIntvl" !lzhang
! time average value


   ichem_start=ntra
if (chem_opt >= 300) then
   d1st(:,:) = tr(:,:,ichem_start+p_dust_1)
   d2st(:,:) = tr(:,:,ichem_start+p_dust_2)
   d3st(:,:) = tr(:,:,ichem_start+p_dust_3)
   d4st(:,:) = tr(:,:,ichem_start+p_dust_4)
   d5st(:,:) = tr(:,:,ichem_start+p_dust_5)
   if (its .eq. 0) then
       d1st_ave=0.     
       d2st_ave=0.
       d3st_ave=0.
       d4st_ave=0.
       d5st_ave=0.
    endif

endif ! chem_opt>=300
       emid1_ave=0.
       emid2_ave=0.
       emid3_ave=0.
       emid4_ave=0.
       emid5_ave=0.
       aod2d_ave=0.
!      print *, 'zlzl1',its,ArchvStep,minval(d1st_ave),minval(emid1_ave)
!      print *, 'zlzl2',minval(d1st),minval(emid1st) 
if (its.gt.0) then
!SMS$PARALLEL(dh, j) BEGIN
    do j=1,nip
       exttsum=0.
       do k=1,nvl
            rho_phys(k,j)=.5*(pr3d(k,j)+pr3d(k+1,j))&
              /(RD*tk3d(k,j)) !*(1.+.608*qv3d(k,j))
if (chem_opt >= 300) then
            d1st_ave(k,j) = d1st_ave(k,j)+d1st(k,j)
            d2st_ave(k,j) = d2st_ave(k,j)+d2st(k,j)
            d3st_ave(k,j) = d3st_ave(k,j)+d3st(k,j)
            d4st_ave(k,j) = d4st_ave(k,j)+d4st(k,j)
            d5st_ave(k,j) = d5st_ave(k,j)+d5st(k,j)
endif ! chem_opt>=300
            if (aer_ra_feedback == 0 )then
            exttsum=exttsum+ext_cof(k,j,p_extcof55)
            endif
       enddo
            if (aer_ra_feedback == 0 )then
            aod2d(j)=exttsum
            endif
       emid1_ave(j) = emid1_ave(j)+emid1st(j)
       emid2_ave(j) = emid2_ave(j)+emid2st(j)
       emid3_ave(j) = emid3_ave(j)+emid3st(j)
       emid4_ave(j) = emid4_ave(j)+emid4st(j)
       emid5_ave(j) = emid5_ave(j)+emid5st(j)
       aod2d_ave(j) = aod2d_ave(j)+aod2d(j)
    enddo
!SMS$PARALLEL END
!      print *, 'zlzl3',its,ArchvStep,minval(d1st_ave),minval(d1st),minval(emid1st),minval(emid1_ave)

endif ! its>1



    if (mod(its,ArchvStep)==0.or.(its==nts.and.ArchvTimeUnit.eq.'ts')) then

      ichem_start=ntra
      if (chem_opt >= 300 .and. chem_opt < 500) then
        dms1(:,:) = tr(:,:,ichem_start+p_dms)
        sea1(:,:) = tr(:,:,ichem_start+p_seas_1)!*rho_phys(:,:)
        sea2(:,:) = tr(:,:,ichem_start+p_seas_2)!*rho_phys(:,:)
        sea3(:,:) = tr(:,:,ichem_start+p_seas_3)!*rho_phys(:,:)
        sea4(:,:) = tr(:,:,ichem_start+p_seas_4)!*rho_phys(:,:)
      endif !chem_opt >= 300 .and. chem_opt < 500
!
! the floowing requires some type of GOCART or volcanic ash option
! will only work for gocart type mass variables (such as p_bc, .....)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if ((chem_opt >= 300 .and. chem_opt < 500).or.chem_opt==108) then
!SMS$PARALLEL(dh, j) BEGIN
        do j=1,nip
          dpsum=0.
          intash(j)=0.
          intaer(j)=0.
          intbc(j)=0.
          intoc(j)=0.
          intsulf(j)=0.
          intdust(j)=0.
          o3du(j)=0.
          o3dg(j)=0.
          do k=1,nvl
            dpsum=dpsum+(pr3d(k,j)-pr3d(k+1,j))
            rho_phys(k,j)=.5*(pr3d(k,j)+pr3d(k+1,j))&
              /(RD*tk3d(k,j)) !*(1.+.608*qv3d(k,j))
        if (chem_opt >= 300 .and. chem_opt < 500) then
            intaer(j)=intaer(j)+1e-6*tr(k,j,ichem_start+p_p25)*(pr3d(k,j)-pr3d(k+1,j))/g
            intbc(j)=intbc(j)+(tr(k,j,ichem_start+p_bc1)&
              +tr(k,j,ichem_start+p_bc2))*1e-6*(pr3d(k,j)-pr3d(k+1,j))/g
            intoc(j)=intoc(j)+(tr(k,j,ichem_start+p_oc1)&
              +tr(k,j,ichem_start+p_oc2))*1e-6*(pr3d(k,j)-pr3d(k+1,j))/g
             intdust(j)=intdust(j)+(d1st(k,j)&
              +.286*d2st(k,j))*1e-6*(pr3d(k,j)-pr3d(k+1,j))/g
            intsulf(j)=intsulf(j)+1e-6*tr(k,j,ichem_start+p_sulf)*(pr3d(k,j)&
              -pr3d(k+1,j))/g
        endif !chem_opt >= 300 .and. chem_opt < 500

        if (chem_opt == 108) then
            intaer(j)=intaer(j)+(tr(k,j,ichem_start+p_p25j)+&
              tr(k,j,ichem_start+p_p25i))*1e-6*(pr3d(k,j)-pr3d(k+1,j))/g
            intbc(j)=intbc(j)+(tr(k,j,ichem_start+p_ecj)&
              +tr(k,j,ichem_start+p_eci))*1e-6*(pr3d(k,j)-pr3d(k+1,j))/g
            intoc(j)=intoc(j)+(tr(k,j,ichem_start+p_orgpaj)&
              +tr(k,j,ichem_start+p_orgpai))*1e-6*(pr3d(k,j)-pr3d(k+1,j))/g
             intdust(j)=intdust(j)+(tr(k,j,ichem_start+p_soila)&
              )*1e-6*(pr3d(k,j)-pr3d(k+1,j))/g
            intsulf(j)=intsulf(j)+(tr(k,j,ichem_start+p_so4aj)+&
            tr(k,j,ichem_start+p_so4ai))*1e-6*(pr3d(k,j)&
              -pr3d(k+1,j))/g
        endif !chem_opt == 108

            !if (chem_opt >= 300)then 
             o3du(j)=o3du(j)+(0.001*tr(k,j,ichem_start+p_o3)* (ph3d(k+1,j)-ph3d(k,j))&
             *rho_phys(k,j)*6.022*1e23/(48.*9.8*2.69*1e20))
             o3dg(j)=o3dg(j)+(1e6*0.001*tr(k,j,4)*airmw/48.*(ph3d(k+1,j)-ph3d(k,j))&
             *rho_phys(k,j)*6.022*1e23/(48.*9.8*2.69*1e20)) 
             !endif

            if (chem_opt==304.or.chem_opt==316.or.chem_opt==317) then
              intdust(j)=intdust(j)+d1st(k,j)*1e-6*(pr3d(k,j)-pr3d(k+1,j))&
                /g
            endif
!

            if (chem_opt == 316) then
              intash(j)=intash(j)+(tr(k,j,ichem_start+p_vash_1)       &
                + tr(k,j,ichem_start+p_vash_2)       &
                + tr(k,j,ichem_start+p_vash_3)       &
                + tr(k,j,ichem_start+p_vash_4)       &
                + tr(k,j,ichem_start+p_vash_5)       &
                + tr(k,j,ichem_start+p_vash_6)       &
                + tr(k,j,ichem_start+p_vash_7)       &
                + tr(k,j,ichem_start+p_vash_8)       &
                + tr(k,j,ichem_start+p_vash_9)       &
                + tr(k,j,ichem_start+p_vash_10))     &
                *1e-6*(pr3d(k,j)-pr3d(k+1,j))/g
            endif
            if (chem_opt == 317 ) then
              intash(j)=intash(j)+(tr(k,j,ichem_start+p_vash_1)       &
                + tr(k,j,ichem_start+p_vash_2)       &
                + tr(k,j,ichem_start+p_vash_3)       &
                + tr(k,j,ichem_start+p_vash_4))      &
                *1e-6*(pr3d(k,j)-pr3d(k+1,j))/g
            endif
          enddo
          if (chem_opt == 316 .or. chem_opt == 317 ) intash(j)=intash(j)
          intaer(j)=intaer(j)
          intbc(j)=intbc(j)
          intoc(j)=intoc(j)
          intsulf(j)=intsulf(j)
          intdust(j)=intdust(j)
        enddo
!SMS$PARALLEL END
      endif ! chem_opt >= 300 .and. chem_opt < 500 .or. chem_opt=108
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (chem_opt == 502) then
!SMS$PARALLEL(dh, j) BEGIN
        do j=1,nip
          dpsum=0.
          intash(j)=0.
            do k=1,nvl
              dpsum=dpsum+(pr3d(k,j)-pr3d(k+1,j))
              rho_phys(k,j)=.5*(pr3d(k,j)+pr3d(k+1,j))&
              /(RD*tk3d(k,j)) !*(1.+.608*qv3d(k,j))
              intash(j)=intash(j)+(tr(k,j,ichem_start+p_vash_1)       &
                + tr(k,j,ichem_start+p_vash_2)       &
                + tr(k,j,ichem_start+p_vash_3)       &
                + tr(k,j,ichem_start+p_vash_4))      &
                *1e-6*(pr3d(k,j)-pr3d(k+1,j))/g
          enddo
          intash(j)=intash(j)
        enddo
!SMS$PARALLEL END
      endif ! chem_opt = 502
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! --- accumulate fields averaged over "ArchvIntvl"


!SMS$PARALLEL(dh, j) BEGIN
    do j=1,nip
    do k=1,nvl
if (chem_opt >= 300) then
    d1st_ave(k,j) = d1st_ave(k,j)/float(ArchvStep)
    d2st_ave(k,j) = d2st_ave(k,j)/float(ArchvStep)
    d3st_ave(k,j) = d3st_ave(k,j)/float(ArchvStep)
    d4st_ave(k,j) = d4st_ave(k,j)/float(ArchvStep)
    d5st_ave(k,j) = d5st_ave(k,j)/float(ArchvStep)
endif !chem_opt=300
    enddo
    aod2d_ave(j) = aod2d_ave(j)/float(ArchvStep)
    enddo
!SMS$PARALLEL END
!     if (mp_physics.eq.2) then
!       call maybe_write(its,'qcct',qcct,nvl)
!       call maybe_write(its,'qrct',qrct,nvl)
!       call maybe_write(its,'qict',qict,nvl)
!       call maybe_write(its,'qsct',qsct,nvl)
!     endif
      if ((mp_physics.ne.0).or.(cu_physics.ne.0)) then
        call maybe_write(its,'trco',trco,nvl)
      endif
      if (chem_opt == 500) then
        if(its.gt.1)tr1_tavg(:,:) = tr1_tavg(:,:)/float(ArchvStep-1)
        call maybe_write(its,'c13D',d1st,nvl)
        call maybe_write(its,'c23D',tr1_tavg,nvl)
        tr1_tavg(:,:) = 0.
      endif 
      if (chem_opt.ge.300 .and. chem_opt.lt.500) then
        call maybe_write(its,'ex3D',exch,nvl)
        call maybe_write(its,'pm25',pm25,nvl)
        call maybe_write(its,'pm10',p10,nvl)
        call maybe_write(its,'dms1',dms1,nvl)
        call maybe_write(its,'s1ea',sea1,nvl)
        call maybe_write(its,'s2ea',sea2,nvl)
        call maybe_write(its,'s3ea',sea3,nvl)
        call maybe_write(its,'s4ea',sea4,nvl)
          if (flux_avg) then
             call maybe_write(its,'d1st',d1st_ave,nvl)
             call maybe_write(its,'d2st',d2st_ave,nvl)
             call maybe_write(its,'d3st',d3st_ave,nvl)
             call maybe_write(its,'d4st',d4st_ave,nvl)
             call maybe_write(its,'d5st',d5st_ave,nvl)          
             call maybe_write(its,'emd1',emid1_ave, 1, twodfile=.true.)
             call maybe_write(its,'emd2',emid2_ave, 1, twodfile=.true.)
             call maybe_write(its,'emd3',emid3_ave, 1, twodfile=.true.)
             call maybe_write(its,'emd4',emid4_ave, 1, twodfile=.true.)
             call maybe_write(its,'emd5',emid5_ave, 1, twodfile=.true.)
             call maybe_write(its,'ao2D',aod2d_ave ,1, twodfile=.true.)
             d1st_ave=0.
             d2st_ave=0.
             d3st_ave=0.
             d4st_ave=0.
             d5st_ave=0.
             emid1_ave=0.
             emid2_ave=0.
             emid3_ave=0.
             emid4_ave=0.
             emid5_ave=0.
             aod2d_ave=0.
          else
             call maybe_write(its,'d1st',d1st,nvl)
             call maybe_write(its,'d2st',d2st,nvl)
             call maybe_write(its,'d3st',d3st,nvl)
             call maybe_write(its,'d4st',d4st,nvl)
             call maybe_write(its,'d5st',d5st,nvl)          
          
! output dust emis and source
             call maybe_write(its,'emd1',emid1st, 1, twodfile=.true.)
             call maybe_write(its,'emd2',emid2st, 1, twodfile=.true.)
             call maybe_write(its,'emd3',emid3st, 1, twodfile=.true.)
             call maybe_write(its,'emd4',emid4st, 1, twodfile=.true.)
             call maybe_write(its,'emd5',emid5st, 1, twodfile=.true.)
             call maybe_write(its,'ao2D',aod2d,1, twodfile=.true.)
          endif
          call maybe_write(its,'ero1',ero1, 1, twodfile=.true.)
          call maybe_write(its,'ero2',ero2, 1, twodfile=.true.)
          call maybe_write(its,'ero3',ero3, 1, twodfile=.true.)
          call maybe_write(its,'dms0',dm0, 1, twodfile=.true.)
!output aeroosol wetdepostion
          !call maybe_write(its,'wbc1',wet_dep(:,p_bc1), 1, twodfile=.true.)
          call maybe_write(its,'wbc2',wet_dep(:,p_bc2), 1, twodfile=.true.)
          !call maybe_write(its,'woc1',wet_dep(:,p_oc1), 1, twodfile=.true.)
          call maybe_write(its,'woc2',wet_dep(:,p_oc2), 1, twodfile=.true.)
          call maybe_write(its,'wp25',wet_dep(:,p_p25), 1, twodfile=.true.)
          call maybe_write(its,'wp10',wet_dep(:,p_p10), 1, twodfile=.true.)
          call maybe_write(its,'wso4',wet_dep(:,p_sulf), 1, twodfile=.true.)
          call maybe_write(its,'wdt1',wet_dep(:,p_dust_1), 1, twodfile=.true.)
          call maybe_write(its,'wdt2',wet_dep(:,p_dust_2), 1, twodfile=.true.)
          call maybe_write(its,'wdt3',wet_dep(:,p_dust_3), 1, twodfile=.true.)
          call maybe_write(its,'wdt4',wet_dep(:,p_dust_4), 1, twodfile=.true.)
          call maybe_write(its,'wdt5',wet_dep(:,p_dust_5), 1, twodfile=.true.)
          call maybe_write(its,'wse1',wet_dep(:,p_seas_1), 1, twodfile=.true.)
          call maybe_write(its,'wse2',wet_dep(:,p_seas_2), 1, twodfile=.true.)
          call maybe_write(its,'wse3',wet_dep(:,p_seas_3), 1, twodfile=.true.)
          call maybe_write(its,'wse4',wet_dep(:,p_seas_4), 1, twodfile=.true.)
          call maybe_write(its,'aiso',emiss_ab1(:,p_e_iso), 1, twodfile=.true.)
          call maybe_write(its,'aso2',emiss_ab1(:,p_e_so2), 1, twodfile=.true.)
          call maybe_write(its,'ano2',emiss_ab1(:,p_e_no2), 1, twodfile=.true.)
          call maybe_write(its,'fiso',emiss_abu(:,p_e_iso), 1, twodfile=.true.)
!          call maybe_write(its,'aeth',emiss_ab(:,p_e_eth), 1, twodfile=.true.)
!          call maybe_write(its,'feth',emiss_abu(:,p_e_eth), 1, twodfile=.true.)
          if(chem_opt == 300) then
          call maybe_write(its,'ohbg',oh_bg, nvl)
          call maybe_write(its,'hobg',h2o2_bg, nvl)
          call maybe_write(its,'no3b',no3_bg, nvl)
          call maybe_write(its,'ocbb',ebu_oc, nvl)
          endif
! output air density
          call maybe_write(its,'aird',rho_phys, nvl)
          call maybe_write(its,'extt',ext_cof,nvl)! out_put ext_cof  
                    
! output GOCART aerosols variables
!
!  change unit from ug/kg to ug/m3, need to multiply rho_phys 
!
          dms1(:,:) = tr(:,:,ichem_start+p_bc1)!*rho_phys(:,:)
          call maybe_write(its,'pbc1',dms1,nvl)
          dms1(:,:) = tr(:,:,ichem_start+p_bc2)!*rho_phys(:,:)
          call maybe_write(its,'pbc2',dms1,nvl)
          dms1(:,:) = tr(:,:,ichem_start+p_oc1)!*rho_phys(:,:)
          call maybe_write(its,'obc1',dms1,nvl)
          dms1(:,:) = tr(:,:,ichem_start+p_oc2)!*rho_phys(:,:)
          call maybe_write(its,'obc2',dms1,nvl)
          dms1(:,:) = tr(:,:,ichem_start+p_sulf)
          if(chem_opt == 300)call maybe_write(its,'sulf',dms1,nvl)
          dms1(:,:) = tr(:,:,ichem_start+p_so2)
          if(chem_opt == 300)call maybe_write(its,'pso2',dms1,nvl)
          dms1(:,:) = tr(:,:,ichem_start+p_msa)
          call maybe_write(its,'pmsa',dms1,nvl)
          dms1(:,:) = tr(:,:,ichem_start+p_p25)!*rho_phys(:,:)
          call maybe_write(its,'pp25',dms1,nvl)
          dms1(:,:) = tr(:,:,ichem_start+p_p10)!*rho_phys(:,:)
          call maybe_write(its,'pp10',dms1,nvl)
! output gas phase chemistry
          if (chem_opt.eq.301) then
            call maybe_write(its,'o3du',o3du,1, twodfile=.true.)
            call maybe_write(its,'o3dg',o3dg,1, twodfile=.true.)
            do nv=1,49
              dms1(:,:) = tr(:,:,ichem_start+nv)
              call maybe_write(its,chem_names301(nv),dms1,nvl)
            enddo
          endif
!
! output additional gas phase chemistry
!
  
          if (chem_opt.eq.316.or.chem_opt.eq.317) then
            print *,'p_vash_1,p_vash_4 = ',p_vash_1,p_vash_4
            dms1(:,:) = tr(:,:,ichem_start+p_vash_1)
            call maybe_write(its,'ash1',dms1,nvl)
            dms1(:,:) = tr(:,:,ichem_start+p_vash_2)
            call maybe_write(its,'ash2',dms1,nvl)
            dms1(:,:) = tr(:,:,ichem_start+p_vash_3)
            call maybe_write(its,'ash3',dms1,nvl)
            dms1(:,:) = tr(:,:,ichem_start+p_vash_4)
            call maybe_write(its,'ash4',dms1,nvl)
            if (chem_opt.eq.316) then
              dms1(:,:) = tr(:,:,ichem_start+p_vash_5)
              call maybe_write(its,'ash5',dms1,nvl)
              dms1(:,:) = tr(:,:,ichem_start+p_vash_6)
              call maybe_write(its,'ash6',dms1,nvl)
              dms1(:,:) = tr(:,:,ichem_start+p_vash_7)
              call maybe_write(its,'ash7',dms1,nvl)
              dms1(:,:) = tr(:,:,ichem_start+p_vash_8)
              call maybe_write(its,'ash8',dms1,nvl)
              dms1(:,:) = tr(:,:,ichem_start+p_vash_9)
              call maybe_write(its,'ash9',dms1,nvl)
              dms1(:,:) = tr(:,:,ichem_start+p_vash_10)
              call maybe_write(its,'ash0',dms1,nvl)
            endif !chem_opt=316
          endif !chem_opt=316 or chem_opt=317
        endif !chem_opt.ge.300 .and. chem_opt.lt.500
        if (chem_opt == 500) then
          call maybe_write(its,'fl2D',intaer,1, twodfile=.true.)
        endif
! output for volcanic ash only
      if (chem_opt.eq.502) then
          call maybe_write(its,'iash',intash,1, twodfile=.true.)
          print *,'p_vash_1,p_vash_4 = ',p_vash_1,p_vash_4
          dms1(:,:) = tr(:,:,ichem_start+p_vash_1)
          !call maybe_write(its,'ash1',dms1,nvl)
          dms1(:,:) = tr(:,:,ichem_start+p_vash_2)
          !call maybe_write(its,'ash2',dms1,nvl)
          dms1(:,:) = tr(:,:,ichem_start+p_vash_3)
          !call maybe_write(its,'ash3',dms1,nvl)
          dms1(:,:) = tr(:,:,ichem_start+p_vash_4)
          !call maybe_write(its,'ash4',dms1,nvl)
      endif
      if (chem_opt.ge.300 .and. chem_opt.lt.500) then
        call maybe_write(its,'ia2D',intaer,1, twodfile=.true.)
        call maybe_write(its,'ib2D',intbc,1, twodfile=.true.)
        call maybe_write(its,'io2D',intoc,1, twodfile=.true.)
        call maybe_write(its,'is2D',intsulf,1, twodfile=.true.)
        call maybe_write(its,'id2D',intdust,1, twodfile=.true.)
        if (chem_opt.eq.316.or.chem_opt.eq.317) then
          call maybe_write(its,'iash',intash,1, twodfile=.true.)
        endif
      endif !chem_opt.ge.300 .and. chem_opt.lt.500
! output racm soa vbs chemistry
          if (chem_opt.eq.108) then
            if (flux_avg) then
         call maybe_write(its,'ao2D',aod2d_ave ,1, twodfile=.true.)
             aod2d_ave=0.0
            else
         call maybe_write(its,'ao2D',aod2d,1, twodfile=.true.)
            endif

        call maybe_write(its,'ia2D',intaer,1, twodfile=.true.)
        call maybe_write(its,'ib2D',intbc,1, twodfile=.true.)
        call maybe_write(its,'io2D',intoc,1, twodfile=.true.)
        call maybe_write(its,'is2D',intsulf,1, twodfile=.true.)
        call maybe_write(its,'id2D',intdust,1, twodfile=.true.)
            call maybe_write(its,'aiso',emiss_ab1(:,p_e_iso), 1,twodfile=.true.)
            call maybe_write(its,'aso2',emiss_ab1(:,p_e_so2), 1,twodfile=.true.)
            call maybe_write(its,'ano2',emiss_ab1(:,p_e_no2), 1,twodfile=.true.)
            call maybe_write(its,'fiso',emiss_abu(:,p_e_iso), 1,twodfile=.true.)
            call maybe_write(its,'o3du',o3du,1, twodfile=.true.)
            call maybe_write(its,'o3dg',o3dg,1, twodfile=.true.)
            call maybe_write(its,'pm25',pm25,nvl)
            call maybe_write(its,'pm10',p10,nvl)
            call maybe_write(its,'aird',rho_phys, nvl)
            call maybe_write(its,'extt',ext_cof,nvl)! out_put ext_cof  
            do nv=1,103
              dms1(:,:) = tr(:,:,ichem_start+nv)
              call maybe_write(its,chem_names108(nv),dms1,nvl)
            enddo
          endif !chem_opt==108
    endif 

  end subroutine chem_output

end module module_chem_output
