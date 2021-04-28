module Library

  # REQUIRED METHODS (CALLED BY DRIVER)

  def lib_build(env,prepkit)

    # When running inside a suite, env.qmon will already point to an initialized
    # Qmon object. If not, this is a single-run invocation: Create a Qmon object
    # and make note that we need to shut it down later.

    unless env.qmon
      env.qmon=Qmon.new(60)
      env.must_stop_qmon=true
    end

    logd "*   (Also see #{env.build.log})"
    cmd="cd #{env.build.builddir} && qsub #{env.build.script}"
    queued=false
    output=[]

    # Try twice to queue the build job, a sad but necessary response to issues
    # with a flaky qsub (on jet, IIRC) that randomly fails.

    2.times do |n|
      props={:msg=>"Job submission failed, retrying...",:die=>false}
      output,status=Thread.exclusive { ext(cmd,props) }
      if status==0
        queued=true
        break
      end
    end

    # Make a 3rd (and final) attempt to queue the job, if necessary.

    unless queued
      props={:msg=>"Job submission failed, see #{logfile}",:die=>true}
      output,status=Thread.exclusive { ext(cmd,props) }
    end

    # Collect and verify the job id reported by qsub.

    re=Regexp.new('^(\d+)\..*')
    jobid=nil
    output.each do |e|
      m=re.match(e)
      jobid=m[1] if m
    end
    die "Job ID not found in batch-submit output" unless jobid

    # Register this job id for deletion in case of a test-suite failure.

    job_activate(jobid,self)

    # Monitor the job until completion.

    jobstatus=lib_wait_for_job(env,jobid)

    # De-register the job from the list of active jobs.

    job_deactivate(jobid)

    # Check for job success and return result.

    if jobstatus and not jobstatus==0
      die "Build #{env.build.name} failed. "+
        "See #{env.build.log} (if it exists) for details."
    end

    File.join(env.build.builddir,"FIMrun#{lib_suffix(env)}")
  end

  def lib_build_post(env,postkit)
    postkit
  end

  def lib_build_prep_jet(env)
    lib_build_prep_std(env)
    script=<<SCRIPT
#!/bin/sh
#PBS -A gsd-fv3-dev
#PBS -d #{env.build.builddir}
#PBS -j oe
#PBS -l procs=16
#PBS -l partition=vjet
#PBS -l walltime=00:20:00
#PBS -o #{env.build.log}
#PBS -q batch
#PBS -W umask=022
#{env.build.copy_src_cmd}
#{env.build.copy_run_cmd}
cd #{env.build.src}
./makefim #{env.build.args}
SCRIPT
    File.open(env.build.script,'w') { |f| f.write(script) }
    FileUtils.chmod(0755,env.build.script)
    logd "Wrote batch-build script #{env.build.script}"
    nil
  end

  def lib_build_prep_std(env)

    # Some setup.

    makefimargs=env.build.args.squeeze
    buildname=makefimargs.gsub(/  */,'_')
    env.build.name=buildname
    logd "Set build name: #{buildname}"

    # Create build location.

    builddir=(env.build.builddir=env.build.ddts_root)
    unless Dir.exist?(builddir)
      FileUtils.mkdir_p(builddir)
      logd "Made directory: #{builddir}"
    end

    # Use knowledge of this app's relationship to model directory hierarchy.

    topdir=File.expand_path("..")

    # Prepare copy commands.

    src='FIMsrc'
    srcdir=valid_dir(File.join(topdir,src))
    dstdir=builddir
    env.build.copy_src_cmd="rsync -a #{srcdir} #{dstdir}"

    run='FIMrun'
    srcdir=valid_dir(File.join(topdir,run))
    dstdir=File.join(builddir,run)
    env.build.copy_run_cmd="rsync -a --exclude '*/' #{srcdir}/ #{dstdir}/"

    # More setup.

    env.build.log=File.join(builddir,"build.log")
    env.build.script=File.join(builddir,"build.#{buildname}")
    env.build.src=File.join(builddir,src)

    nil
  end

  def lib_build_prep_theia(env)
    lib_build_prep_std(env)
    script=<<SCRIPT
#!/bin/sh
#PBS -A gsd-fv3-test
#PBS -d #{env.build.builddir}
#PBS -j oe
#PBS -l procs=24
#PBS -l walltime=00:20:00
#PBS -o #{env.build.log}
#PBS -q batch
#PBS -W umask=022
#{env.build.copy_src_cmd}
#{env.build.copy_run_cmd}
cd #{env.build.src}
./makefim #{env.build.args}
SCRIPT
    File.open(env.build.script,'w') { |f| f.write(script) }
    FileUtils.chmod(0755,env.build.script)
    logd "Wrote batch-build script #{env.build.script}"
    nil
  end

  def lib_data_jet(env)
    lib_link_data("/lfs2/projects/fim/test-suite-data/2015-07-12")
  end

  def lib_data_theia(env)
    lib_link_data("/scratch3/BMC/fim/test-suite-data/2015-07-12")
  end

  def lib_outfiles(env,path)

    # See README for a description of what this method returns.

    restrs=['(.*/)(fim/fim_out_.*)','(.*/)(post/[0-9]+)']
    res=restrs.map { |e| Regexp.new(e) }
    outs=[]
    cmd="find -L #{path} -type f"
    props={:msg=>"Error executing: #{cmd}",:out=>false}
    output,status=ext(cmd,props)
    allfiles=output
    allfiles.each do |e|
      res.each do |re|
        m=re.match(e)
        unless m.nil?
          outs << [m[1],m[2]]
          break
        end
      end
    end
    outs
  end

  def lib_queue_del_cmd(env)
    'qdel'
  end

  def lib_run_check(env,postkit)
    fim_stdout,rundir=postkit
    fim_ok=job_check(fim_stdout,lib_re_str_success)
    logi "Run failed: fim stdout is #{fim_stdout}" unless fim_ok
    pop_ok=true
    unless env.run.ignore_pop
      fimdir=File.dirname(fim_stdout)
      pop_stdout=File.expand_path(File.join(fimdir,"..","post","stdout"))
      pop_ok=job_check(pop_stdout,"pop completed successfully")
      logi "Run failed: pop stdout is #{pop_stdout}" unless pop_ok
    end
    (fim_ok and pop_ok)?(rundir):(nil)
  end

  def lib_run_check_rocoto(env,postkit)
    logfile,subdir=postkit
    fim_ok=job_check(logfile,"This cycle is complete: Success")
    logi "Run failed: Rocoto log is #{logfile}" unless fim_ok
    (fim_ok)?(subdir):(nil)
  end

  def lib_run_compare_var(env,prepkit)

    # Compare_Var runs are the same as standard runs, except that their output
    # directory name is modified by the external run automation. Reproduce and
    # return that directory name here.

    rundir=prepkit
    lib_run_std(env,rundir)
    nls=env.run.namelists
    g=nls.cntlnamelist.glvl
    k=nls.cntlnamelist.nvl
    pes1=env.run.pes1
    pes2=env.run.pes2
    stdout=File.join(rundir,"fim#{g}_#{k}_cv.#{pes1}.vs.#{pes2}","fim","stdout")
    [stdout,rundir]
  end

  def lib_run_post(env,runkit)
    env.qmon.stop if env.must_stop_qmon
    runkit
  end

  def lib_run_prep_compare_var(env)

    # In addition to doing the standard run prep, set up SMSnamelist for
    # Compare_Var operation.

    rundir=lib_run_prep_std(env)
    totalpes=env.run.namelists.queuenamelist.computetasks.to_i
    pes1=1
    pes2=totalpes-pes1
    smsenv=OpenStruct.new
    smsenv.smsnamelist=OpenStruct.new
    smsnl=smsenv.smsnamelist
    smsnl.compare_var_on=YAML_Unquoted.new('.true.')
    smsnl.compare_var_ntasks_1=pes1
    smsnl.compare_var_ntasks_2=pes2
    smsnlfile=valid_file(File.join(rundir,"SMSnamelist"))
    lib_mod_namelist_file(env,smsnlfile,smsenv)
    env.run.pes1=pes1
    env.run.pes2=pes2
    rundir
  end

  def lib_run_prep_enkf(env)

    # Like a restart run, the EnKF run happens in two parts. Do the standard run
    # prep first, then set up a second namelist file for the second part of the
    # EnKF run.

    rundir=lib_run_prep_std(env)
    nlfile1=File.join(rundir,"FIMnamelist")
    nlfile2=File.join(rundir,"fimenkf_second_run")
    die "ERROR: #{nlfile2} already exists" if File.exist?(nlfile2)
    FileUtils.cp(nlfile1,nlfile2)
    logd "Copied #{nlfile1} to #{nlfile2}"
    datadir=valid_dir(File.join(tmp_dir,"data"))
    nl=env.run.namelists.modelnamelist
    nl.enkfio_in=YAML_Unquoted.new('.true.')
    nl.enkfio_out=YAML_Unquoted.new('.false.')
    nl.incr_fname=File.join(datadir,'gincr.b')
    nl=env.run.namelists.outputnamelist
    lib_mod_namelist_file(env,nlfile2,env.run.namelists)
    rundir
  end

  def lib_run_prep_rocoto(env)

    # Do the standard run prep first.

    rundir=lib_run_prep_std(env)

    # Copy FIMwfm to run directory.

    rocoto_srcdir=valid_dir(File.join(File.expand_path(".."),"FIMwfm"))
    rocoto_rundir=File.join(rundir,"..","FIMwfm")
    logd "Copying #{rocoto_srcdir} -> #{rocoto_rundir}"
    FileUtils.cp_r(rocoto_srcdir,rocoto_rundir)

    # Prepare for XML modifications.

    require 'rexml/document'
    rocoto_xmldir=File.join(rocoto_rundir,"rocoto_xml")
    env.run.rocoto_xml=valid_dir(rocoto_xmldir)

    # Modify prep XML file: Disable retries.
    # Set maxtries=2 to resubmit the job if there are timing issues

    prepxml=valid_file(File.join(rocoto_xmldir,"FIM_Prep.xml"))
    prepdoc=REXML::Document.new(File.new(prepxml),{:raw=>:all})
    prepdoc.delete_element("//task/dependency")
    REXML::XPath.first(prepdoc,"//task").add_attribute("maxtries","2")
    File.open(prepxml,"w") { |f| f.puts prepdoc }
    logd "Modified prep XML file #{prepxml}"

    # Modify fim XML file: Disable retries, and remove dependency on the
    # "spectral" task, which normally copied initialization data for the model,
    # but which we do not run here because the test suite's initializatino data
    # is already available.

    fimxml=valid_file(File.join(rocoto_xmldir,"FIM_FIM.xml"))
    fimdoc=REXML::Document.new(File.new(fimxml),{:raw=>:all})
    REXML::XPath.first(fimdoc,"//task").add_attribute("maxtries","2")
    File.open(fimxml,"w") { |f| f.puts fimdoc }
    logd "Modified fim XML file #{fimxml}"

    # Modify post XML file: Disable retries, and set the GRID_NAMES and
    # GRID_SPECS environment-variable settings to use the values defined in the
    # workflow XML file.

    postxml=valid_file(File.join(rocoto_xmldir,"FIM_PostFimoutTrue.xml"))
    postdoc=REXML::Document.new(File.new(postxml),{:raw=>:all})
    REXML::XPath.first(postdoc,"//task").add_attribute("maxtries","2")
    REXML::XPath.match(postdoc,"//task/envar").each do |e|
      e[1].text="&GRID_NAMES;" if e[0].text=="GRID_NAMES"
      e[1].text="&GRID_SPECS;" if e[0].text=="GRID_SPECS"
    end
    File.open(postxml,"w") { |f| f.puts postdoc }
    logd "Modified post XML file #{postxml}"

    # Modify workflow XML file: Set some environment-variable values as
    # appropriate to this run.

    wfxml=valid_file(File.join(rocoto_xmldir,env.run.wfxml))
    wfdoc=REXML::Document.new(File.new(wfxml))
    datadir=valid_dir(File.join(tmp_dir,"data"))
    entity_map={
      "DATADIR"=>datadir,
      "DATADR2"=>datadir,
      "FIM_HOME"=>File.expand_path(File.join(rundir,"..")),
      "FIM_RUN"=>rundir
    }
    wfdoc.doctype.each do |e|
      if e.is_a?(REXML::Entity) and entity_map.has_key?(e.name)
        e.replace_with(REXML::Entity.new(e.name,entity_map[e.name]))
      end
    end
    File.open(wfxml,"w") { |f| f.puts wfdoc }
    logd "Modified workflow XML file #{wfxml}"

    # Symlink "fim" and "post" directories so that output can be found in a
    # directory structure similar to that created for non-Rocoto runs.

    g=env.run.namelists.cntlnamelist.glvl
    k=env.run.namelists.cntlnamelist.nvl
    p=env.run.namelists.queuenamelist.computetasks
    t=env.run.namelists.timenamelist.yyyymmddhhmm
    subdir=File.expand_path(File.join(rundir,"fim_#{g}_#{k}_#{p}_#{t}"))
    FileUtils.mkdir_p(subdir)
    fimdir_target=File.join(subdir,"fim_C")
    fimdir_linkname=File.join(subdir,"fim")
    postdir_target=File.join(subdir,"post_C","228","NAT","grib1")
    postdir_linkname=File.join(subdir,"post")
    FileUtils.ln_s(fimdir_target,fimdir_linkname)
    logd "Linked #{fimdir_linkname} -> #{fimdir_target}"
    FileUtils.ln_s(postdir_target,postdir_linkname)
    logd "Linked #{postdir_linkname} -> #{postdir_target}"

    # Remember useful paths for later.

    env.run.subdir=subdir
    env.run.wfxml=wfxml

    # Return the path to the unique run directory.

    rocoto_rundir
  end

  def lib_run_prep_std(env)
    rundir=env.run.ddts_root
    runfiles=env.build.ddts_result
    logd "Copying #{runfiles} -> #{rundir}"
    FileUtils.cp_r(runfiles,rundir)
    rundir=File.join(rundir,File.basename(runfiles))
    datadir=valid_dir(File.join(tmp_dir,"data"))
    nl=env.run.namelists.queuenamelist
    nl.chem_datadir=File.join(datadir,"chem")
    nl.datadir=datadir
    nl.datadr2=datadir
    unless (nl=env.run.namelists.landnamelist)
      nl=(env.run.namelists.landnamelist=OpenStruct.new)
    end
    nl.landdatdir=datadir
    unless (nl=env.run.namelists.toponamelist)
      nl=(env.run.namelists.toponamelist=OpenStruct.new)
    end
    nl.topodatfile=File.join(datadir,"wrf5mintopo.dat")
    nlfile=File.join(rundir,"FIMnamelist")
    lib_mod_namelist_file(env,nlfile,env.run.namelists)
    rundir
  end

  def lib_run_restart(env,prepkit)

    # Run once, check the run, then set 'restart' flag and run again.

    fim_stdout,rundir=lib_run_std(env,prepkit)
    return nil if fim_stdout.nil?
    unless job_check(fim_stdout,lib_re_str_success)
      die "Restart first half failed"
    end
    env.run.restart=true
    lib_run_std(env,prepkit)
  end

  def lib_run_rocoto(env,prepkit)

    # Do some setup.

    rocoto_rundir=prepkit
    ts=env.run.namelists.timenamelist.yyyymmddhhmm[0..-3]
    storefile=File.join(rocoto_rundir,"fimts.store")
    logfile=File.join(rocoto_rundir,"log","workflow","workflow_#{ts}.log")
    FileUtils.touch(logfile)
    logd "Rocoto log file is #{logfile}"
    logd "Rocoto store file is #{storefile}"
    logd "Rocoto workflow file is #{env.run.wfxml}"

    # Iterate Rocoto. For unknown reasons, ext() hangs when running this shell
    # command (IO::read appears to be the source of the hang), so run it with
    # IO.popen directly instead.

    cmd="module purge && module load rocoto && rocotorun -d #{storefile} -w #{env.run.wfxml}"
    logd "Iterating Rocto with command: #{cmd}"
    loop do
      IO.popen(cmd)
      str1="This cycle is complete" # All workflow tasks completed
      str2="in state DEAD"          # Some workflow task failed
      break if job_check(logfile,str1) or job_check(logfile,str2)
      sleep 60
    end
    [logfile,env.run.subdir]
  end

  def lib_run_std(env,prepkit)
    rundir=prepkit
    jobid=nil
    subdir=nil
    re1=Regexp.new(lib_re_str_job_id)
    re2=Regexp.new(lib_re_str_run_dir)
    ss='subfim'
    mach_name=invoke(:lib_submit_script,:run,env)
    restart=env.run.restart
    ss+=".restart" if restart
    cmd="#{File.join(rundir,ss)} #{mach_name} #{rundir}"
    logd "Submitting job with command: #{cmd}"
    output,status=Thread.exclusive { ext(cmd,{:msg=>"Job submission failed"}) }
    output.each do |e|
      e.chomp!
      logd e unless e=~/^\s*$/
      jobid=e.gsub(re1,'\1') if re1.match(e)
      subdir=e.gsub(re2,'\1') if re2.match(e)
    end
    if jobid.nil?
      logi "ERROR: Job ID not found in #{ss} output"
      return nil
    end
    job_activate(jobid,self)
    if subdir.nil? and not restart
      logi "ERROR: Run directory not found in #{ss} output"
      return nil
    end
    rundir.replace(File.join(rundir,subdir)) unless restart
    qs="Queued with job ID #{jobid}"
    qs+=" (restart)" if restart
    logi qs
    lib_wait_for_job(env,jobid)
    job_deactivate(jobid)
    nls=env.run.namelists
    g=nls.cntlnamelist.glvl
    k=nls.cntlnamelist.nvl
    p=nls.queuenamelist.computetasks.delete('"')
    stdout=File.join(rundir,"fim#{g}_#{k}_#{p}","fim","stdout")
    [stdout,rundir]
  end

  def lib_suite_post(env)
    env.qmon.stop
  end

  def lib_suite_prep(env)
    env.qmon=Qmon.new(60)
  end

  # CUSTOM METHODS (NOT CALLED BY DRIVER)

  def lib_link_data(target)
    linkname=File.join(tmp_dir,"data")
    FileUtils.rm_f(linkname)
    FileUtils.ln_s(target,linkname)
    lib_validate_data(valid_dir(linkname))
  end

  def lib_mod_namelist_file(env,nlfile,nlenv)
    h=convert_o2h(nlenv)
    sets=h.reduce([]) do |m0,(n,kv)|
      inner=kv.reduce([]) do |m1,(k,v)|
        v="\"#{quote_string(v)}\""
        logd "Set namelist #{n}:#{k}=#{v}"
        m1.push("-s #{n}:#{k}=#{v}")
      end
      m0.concat(inner)
    end
    nml=valid_file(File.join(env.build.ddts_result,"nml"))
    cmd="#{nml} -i #{nlfile} -o #{nlfile} #{sets.join(" ")}"
    Thread.exclusive { ext(cmd,{:msg=>"Failed to edit #{nlfile}"}) }
  end

  def lib_re_str_job_id
    'The job (\d+).* has been submitted.'
  end

  def lib_re_str_run_dir
    'Made directory (.*)'
  end

  def lib_re_str_success
    '(Program exited normally)|(PROGRAM nems *HAS ENDED)'
  end

  def lib_submit_script_jet(env)
    'jet'
  end

  def lib_submit_script_theia(env)
    'theia'
  end

  def lib_suffix(env)
    p_or_s=(env.build.args.include?("serial"))?("s"):("p")
    omp=(env.build.args.include?("omp"))?("_omp"):("")

    # Check for debug and debugprint in build arguments, debug suffix is not
    # added for debugprint 
    if env.build.args.scan(/(?=debug)/).count == 2
      debug="_debug"             # debug occurs twice, both debug and debugprint
    elsif env.build.args.scan(/(?=debug)/).count == 1
      if env.build.args.include?("debugprint")
        debug=""                 # debug occurs once in debugprint
      else
        debug="_debug"          # debug occurs once as debug
      end
    else
      debug=""
    end
    "_"+env.build.args.sub(/^\s+/,'').sub(/\s+.*$/,'')+"_#{p_or_s}#{omp}#{debug}"
  end

  def lib_validate_data(dir)
    logd "Validating data..."
    expected={
      '110010000.gfs.t00z.sanl'             => 'f0f0438321bae6bed3a1ac5fef6f5bb2',
      '110010000.gfs.t00z.sfcanl'           => '767938b50aed1e69ce63fb946f67b553',
      '142460000.gfs.t00z.sanl'             => '289a94ab0db93c6065d58b6b312cd546',
      '142460000.gfs.t00z.sfcanl'           => 'e6a39b2d6ad7c77fb58482b8ace9f7e8',
      'chem/anthro_binary'                  => '1667e3c46700f8016fabc7e89e66bb4d',
      'chem/chemltln.dat'                   => '1c9b7062dbab9600a02b600da57a9237',
      'chem/clay.dat'                       => 'cad8638300235fb7ec8c3fa6c482b275',
      'chem/current.dat'                    => '5c0015327f4229236a3c742ab798c2a1',
      'chem/dm0_binary'                     => 'f08eaf8cdcc128356b211a015f7fc8c8',
      'chem/erod1.dat'                      => 'f0fa3242b4ac05433fcd1cea0c1eb121',
      'chem/erod2.dat'                      => 'd192acb0d08e0030e77490b65049d4bb',
      'chem/erod3.dat'                      => 'd192acb0d08e0030e77490b65049d4bb',
      'chem/erod_binary'                    => '4c32c20c9b77084f6c55c102627e14ab',
      'chem/glvl.dat'                       => '495390a8b5fbeb75bacc7636439b3bbe',
      'chem/gocart_backgd_littlee'          => 'aa4b0410959c1c803ad67d1ea599371e',
      'chem/htapLL_to_fimN.bin'             => '8795156fc810ce076e3c1ba32e8876ca',
      'chem/icos_grid_info_level.dat'       => '7dd067526b7066f6ce50ddd998dce2ac',
      'chem/icos_grid_level.dat'            => 'c1e3d48ae2ab20c8c48a33d3368c78dc',
      'chem/MODIS55N155W'                   => 'fcb9e86f1ba0f58af3eb4425e8123ed7',
      'chem/MODIS60N150W'                   => '4112914e2f7042d87e32f9fc3fe28b40',
      'chem/MODISHEADER'                    => '47470b2a003eb72723aa6861c777936b',
      'chem/OGE55N155W'                     => '7b0483a4c42a7a5c7a5f0d87c4feeb2d',
      'chem/OGE60N150W'                     => '00a1c0722687c1e808c3fe238fd06aa2',
      'chem/OGEHEADER'                      => '47470b2a003eb72723aa6861c777936b',
      'chem/p_gocart.dat'                   => '822c5d8334cbb1af1f2f90b9e81dc0e5',
      'chem/prep_chem_sources'              => '97e23b7025ea774784e55577ad13cc50',
      'chem/prep_chem_sources.lahey'        => 'c0b1fccb47a2da9ff9b20ff1b21eeb68',
      'chem/prep_chem_sources_template.inp' => 'c5eb804dceaad453f76ebe4e243d7f89',
      'chem/sand.dat'                       => '5ba4368a80d93a6210a683d314c5133d',
      'chem/volcanic.dat'                   => '486a1b8d37795669d71c62602dc2eb9a',
      'chem/01/e_ald.dat'                   => 'f3813187db8092bf382849566d9745ce',
      'chem/01/e_ash.dat'                   => '440c703c37ffea56745538793c804d3a',
      'chem/01/e_bc.dat'                    => '5067737f3bdc56b50f1456cb22bb14f4',
      'chem/01/e_co.dat'                    => '65fc3117b80c4dc8504d2457996df7ae',
      'chem/01/e_csl.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/01/e_dms.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/01/e_eth.dat'                   => '4ed3c72ebf88848abd101d6a225a867d',
      'chem/01/e_hc3.dat'                   => '1420face0436d7db275c1d732c417488',
      'chem/01/e_hc5.dat'                   => '178c17f620df79188a7e619ba376dcb1',
      'chem/01/e_hc8.dat'                   => 'cb4cfb1846e426e7fba2b79a239c80f0',
      'chem/01/e_hcho.dat'                  => '76a2827c9bd349a93a84c08763485163',
      'chem/01/e_iso.dat'                   => '9d1d2a3eba62e0f906f4743a7d25cc59',
      'chem/01/e_ket.dat'                   => '42aa37b652e8b6735859623be71c3c90',
      'chem/01/e_nh3.dat'                   => '2b51f1c8b741e8e5b041c876cd0dd530',
      'chem/01/e_no2.dat'                   => '6f705ab61646776a50a61f9b277e3a5b',
      'chem/01/e_no.dat'                    => 'c95d82c3f67303b0d9b328b656de1707',
      'chem/01/e_oc.dat'                    => 'f89d4778d943c11320a8f4360d9ed5b7',
      'chem/01/e_oli.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/01/e_olt.dat'                   => 'fe9c00b3d7d4b0dda90020c131fafa8e',
      'chem/01/e_ora2.dat'                  => 'd41f5339b9d5033598d90b118b32438f',
      'chem/01/e_so2.dat'                   => '57264e13a5d3e98bc50626ad4f71817d',
      'chem/01/e_sulf.dat'                  => 'daf38549f678e5498cc05ef832df6802',
      'chem/01/e_tol.dat'                   => 'fd827fbea391df69c1018ae4663f2d63',
      'chem/01/e_pm_25.dat'                 => '39b6f28dd3c46bc766ab818fc0c95ede',
      'chem/01/e_pm_10.dat'                 => '517ada36f3e8e5346b887de18eeadadb',
      'chem/01/e_xyl.dat'                   => '3a8bf84ed8bcc74380d176049c95c30a',
      'chem/01/dm0.dat'                     => 'e33486dc58139f4f0d5ee80b313fee0c',
      'chem/01/oh.dat'                      => '57a4f03a119ecc32db74204e32ce2229',
      'chem/01/h2o2.dat'                    => '15021075c2324614a496e1f20e4c5dd6',
      'chem/01/no3.dat'                     => 'c29a9c4d2ddd74ff62bc3ed71615c459',
      'chem/02/e_ald.dat'                   => '9f0005a7ada2e2c27fcbe58a8fbe7f99',
      'chem/02/e_ash.dat'                   => '440c703c37ffea56745538793c804d3a',
      'chem/02/e_bc.dat'                    => 'cf06f73b06ba2f44d431faf13c036047',
      'chem/02/e_co.dat'                    => 'a02663cc83d7217dc81d804d4c6e5a3c',
      'chem/02/e_csl.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/02/e_dms.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/02/e_eth.dat'                   => '894abaf917195eef980ff4837a98b011',
      'chem/02/e_hc3.dat'                   => 'e4a7723c372c4277228c604dd070d647',
      'chem/02/e_hc5.dat'                   => '557241b0b621388269967c793c6697ef',
      'chem/02/e_hc8.dat'                   => 'fbf8bd331b5c48986dcb844b55ff4026',
      'chem/02/e_hcho.dat'                  => '42da9b431ae62ade855185f67abd2c7e',
      'chem/02/e_iso.dat'                   => '1a6a03f48b1a71d4a68dc68c72de24ff',
      'chem/02/e_ket.dat'                   => '9d326fd025ff55828c3a6afe345d9baf',
      'chem/02/e_nh3.dat'                   => 'f2f9bd0667c4f63faad04784b23bd3dc',
      'chem/02/e_no2.dat'                   => '8345ecc2e5a709f7b5f554e12eb14fed',
      'chem/02/e_no.dat'                    => '82f48e11ec642897d52eea12a596ac7e',
      'chem/02/e_oc.dat'                    => 'bb4d28457834ef31aa99d1540788fd86',
      'chem/02/e_oli.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/02/e_olt.dat'                   => '52403ac50186d51dd5fe0d347ee9a447',
      'chem/02/e_ora2.dat'                  => 'd06ddff2ba6f45b9d74e88be5f4bfd71',
      'chem/02/e_so2.dat'                   => '81fb6bed3a6fbc09211ee361b9fb2ade',
      'chem/02/e_sulf.dat'                  => 'daf38549f678e5498cc05ef832df6802',
      'chem/02/e_tol.dat'                   => 'afa1f83b778529f09e926de24e7b0c6c',
      'chem/02/e_pm_25.dat'                 => '592a5538ec543449af296517677bd88d',
      'chem/02/e_pm_10.dat'                 => '658e08fbee0ae196a4285d329bb15856',
      'chem/02/e_xyl.dat'                   => '8e2a3d69bb7044ca437c9ef3d3c24e97',
      'chem/02/dm0.dat'                     => '1bcef8f63b6c38ea54ef6195c0b84144',
      'chem/02/oh.dat'                      => '3ff943a06265a6361a85a43a5e986edd',
      'chem/02/h2o2.dat'                    => '8c29955bca90988e86d81c7fabfa12c1',
      'chem/02/no3.dat'                     => 'd3e8c2d6ef45273f6fc44d564179773d',
      'chem/03/e_ald.dat'                   => 'd96d50781d9b76959de60604aeb293b3',
      'chem/03/e_ash.dat'                   => '440c703c37ffea56745538793c804d3a',
      'chem/03/e_bc.dat'                    => 'c983b0a7b0ed1126eb5fbe78bec58588',
      'chem/03/e_co.dat'                    => 'b4c454a1c1c631ba8273fa8bf9495096',
      'chem/03/e_csl.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/03/e_dms.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/03/e_eth.dat'                   => '526f4bf813ab8a90feddc5b8bbe014c2',
      'chem/03/e_hc3.dat'                   => '2abfde60bcd9f4a475c68d689b579ed2',
      'chem/03/e_hc5.dat'                   => '8bd8a684c5870dd58b9fcba6ef410ce5',
      'chem/03/e_hc8.dat'                   => 'a1646e5f518675f6affa142b7b36f99e',
      'chem/03/e_hcho.dat'                  => 'fa7a810334f9dfbeb926018ad2cb7381',
      'chem/03/e_iso.dat'                   => '11761bc7d85a27dce2916718bce224e8',
      'chem/03/e_ket.dat'                   => 'f9363eb82d6d90eb7e460524a007b529',
      'chem/03/e_nh3.dat'                   => '9d44ef3e35bf1ca28c4fb55a31bee2a5',
      'chem/03/e_no2.dat'                   => '9599755f081cf0bcc90c7bee97991aff',
      'chem/03/e_no.dat'                    => '5a085210f0c7224d911026f69d27fc29',
      'chem/03/e_oc.dat'                    => 'cb216156bae0c7641d8e8c48eb0cc703',
      'chem/03/e_oli.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/03/e_olt.dat'                   => 'bca00f09718e50c79374e5a36ccf8f0a',
      'chem/03/e_ora2.dat'                  => '7c58e800baaa47cd1355c1a0ab578d38',
      'chem/03/e_so2.dat'                   => '3880edb6f9a1d7ebe2641f306ad322a2',
      'chem/03/e_sulf.dat'                  => 'daf38549f678e5498cc05ef832df6802',
      'chem/03/e_tol.dat'                   => '2d7905fa1cda00c28936611e89f55e72',
      'chem/03/e_pm_25.dat'                 => '0b6d3f0f1d1cbcdcab4da38c3c2fbcfc',
      'chem/03/e_pm_10.dat'                 => '0eb4cafff32e079b408d00b41b486c24',
      'chem/03/e_xyl.dat'                   => '1c6d7ca120a82427add68e947c6da168',
      'chem/03/dm0.dat'                     => 'fc7d0d54fdf23c8170bba1b715ee5909',
      'chem/03/oh.dat'                      => 'ecf81b45ae39ff6ed3d35f268146ae60',
      'chem/03/h2o2.dat'                    => 'e50ca39f23592ffd405e4cc461c1d5da',
      'chem/03/no3.dat'                     => 'b10439315047231212b45c1123674cd2',
      'chem/04/e_ald.dat'                   => '12c78fbcfcecccf4dbdd3f179c4aa571',
      'chem/04/e_ash.dat'                   => '440c703c37ffea56745538793c804d3a',
      'chem/04/e_bc.dat'                    => '3ca801a50ed88e1162da0ffaa9091ead',
      'chem/04/e_co.dat'                    => 'ea74a416d3c4add8c2dbf997bbb1e795',
      'chem/04/e_csl.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/04/e_dms.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/04/e_eth.dat'                   => 'fa6e1672205df9ab837019740eaade0a',
      'chem/04/e_hc3.dat'                   => '6c001699d01dab2399267a9edd97f6b7',
      'chem/04/e_hc5.dat'                   => '715dbc15f9f487c5623c49b213905126',
      'chem/04/e_hc8.dat'                   => '71371d2f3f850b2c116b18f41a810fca',
      'chem/04/e_hcho.dat'                  => '0666017dec7c05e0cb6725ed8b76ebe2',
      'chem/04/e_iso.dat'                   => 'ac90f0d532438521cf041e23b1669e31',
      'chem/04/e_ket.dat'                   => 'c5093ac6c1e663b6102457c65c77e18f',
      'chem/04/e_nh3.dat'                   => 'dcfb9dd8ee157a384994aecaf1fb2d14',
      'chem/04/e_no2.dat'                   => '70eaaaf8bacc06441b530776a61e60db',
      'chem/04/e_no.dat'                    => '1134bc7fc4ed497a1a14871ad24d5068',
      'chem/04/e_oc.dat'                    => '4ae585082cd94d93382f55b419e738ef',
      'chem/04/e_oli.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/04/e_olt.dat'                   => '8ad16c42d706570c46f33e9414b98668',
      'chem/04/e_ora2.dat'                  => '4815b062ceaef2417108a61c0a753b5e',
      'chem/04/e_so2.dat'                   => '0ddca9449888866f01fc47a1a2366453',
      'chem/04/e_sulf.dat'                  => 'daf38549f678e5498cc05ef832df6802',
      'chem/04/e_tol.dat'                   => 'a62e2ec482a61cc5be25c80624073dc0',
      'chem/04/e_pm_25.dat'                 => '94e87360ca40d9790d7e092571226bad',
      'chem/04/e_pm_10.dat'                 => '64e13dda3df46b81405a5803c638ef6b',
      'chem/04/e_xyl.dat'                   => '9ad26b37b9cb992917a434f73456a7d9',
      'chem/04/dm0.dat'                     => '74af16eb168fee9190852833c4fcc8a0',
      'chem/04/oh.dat'                      => '38932a2b4ac44b58b49d4304448e75b4',
      'chem/04/h2o2.dat'                    => 'ea91ffe3690321dc2c7df6d683e578b3',
      'chem/04/no3.dat'                     => '13dcb5de4057ea082387ae13df6d3b22',
      'chem/05/e_ald.dat'                   => 'c3670f4ac3a80ba6661237df140e4e7f',
      'chem/05/e_ash.dat'                   => '440c703c37ffea56745538793c804d3a',
      'chem/05/e_bc.dat'                    => '148d9f0b8a11d4327eb1c8ad56fb9cc7',
      'chem/05/e_co.dat'                    => 'c2b8b7ea42635a3e55f0dbe58726dd38',
      'chem/05/e_csl.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/05/e_dms.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/05/e_eth.dat'                   => '96935b92dc2e074c6b8655da51594449',
      'chem/05/e_hc3.dat'                   => 'ec45f6dd08cdfc39a982b4850bd63a2b',
      'chem/05/e_hc5.dat'                   => '71e6deacbdce5bb12ca674f5b20c5eff',
      'chem/05/e_hc8.dat'                   => '8460b9dad707f79efe834a82b817bd4e',
      'chem/05/e_hcho.dat'                  => 'b7f68cd49a9cdb1f1c8232e1df3fe309',
      'chem/05/e_iso.dat'                   => '07370428d9780c925d8db88c6bca5e2d',
      'chem/05/e_ket.dat'                   => '9d888ad353dd3ec79cb2760936f68910',
      'chem/05/e_nh3.dat'                   => 'c92973ce235fd58c62ceb4dddcfd2236',
      'chem/05/e_no2.dat'                   => 'aa04c039493293a3d56c96885c805dd2',
      'chem/05/e_no.dat'                    => '5b2af588196848733d78687eae4370f9',
      'chem/05/e_oc.dat'                    => 'aadc956ab2ce2d178f8152e89774f950',
      'chem/05/e_oli.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/05/e_olt.dat'                   => 'cf4688684b64cf5f005e15e36b00b1e5',
      'chem/05/e_ora2.dat'                  => '0dbeb1f18d24ab3c2e8d8c2f5e1a5fc5',
      'chem/05/e_so2.dat'                   => '5444844a3172710b3105da365d701e9d',
      'chem/05/e_sulf.dat'                  => 'daf38549f678e5498cc05ef832df6802',
      'chem/05/e_tol.dat'                   => '389e5aff83a5c8bf56f4bc1eb447e805',
      'chem/05/e_pm_25.dat'                 => 'd79647ca9aaac76d3045e4569ef6acaf',
      'chem/05/e_pm_10.dat'                 => '05b295789d3a41ab99cc1c639a42a7b7',
      'chem/05/e_xyl.dat'                   => '87352505d96f2c9699fe761ac52886cd',
      'chem/05/dm0.dat'                     => '1a88b510216df71d998a0e81872c043a',
      'chem/05/oh.dat'                      => '4f0d5d48bdc17b5c88d0631f6e44ae72',
      'chem/05/h2o2.dat'                    => '69ab5d63774772a10bfa302830b59bb2',
      'chem/05/no3.dat'                     => '927ad908898dc46958c9754d042d25a3',
      'chem/06/e_ald.dat'                   => 'fc152389b1847773abe61b6c8c714c2d',
      'chem/06/e_ash.dat'                   => '440c703c37ffea56745538793c804d3a',
      'chem/06/e_bc.dat'                    => 'bd5206e4964cd9cd7f4c9e1e6c1645df',
      'chem/06/e_co.dat'                    => '15287c979d50ca8e7f27914d1a79dab6',
      'chem/06/e_csl.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/06/e_dms.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/06/e_eth.dat'                   => 'eb98d88a24890efcfef1cc0fe590309f',
      'chem/06/e_hc3.dat'                   => 'c6927ef67e2d7b6f89996edf8097adeb',
      'chem/06/e_hc5.dat'                   => '42c0f2299e512b6a5844765156683bf5',
      'chem/06/e_hc8.dat'                   => '55649ff3b8e55613c66a3a6488e63422',
      'chem/06/e_hcho.dat'                  => 'b5d14a813a1841e2e73e5089d121339c',
      'chem/06/e_iso.dat'                   => 'f238eb91c07b7eab3c119994cb4e29e9',
      'chem/06/e_ket.dat'                   => '6690a911a36df73ee8dae1a15241d711',
      'chem/06/e_nh3.dat'                   => '9a5ffa3546807d464ed8bff834e18bb4',
      'chem/06/e_no2.dat'                   => '560ef323549c6f9433de0bb975595999',
      'chem/06/e_no.dat'                    => '0b5205d655ae3a8b118fdaa0928562db',
      'chem/06/e_oc.dat'                    => '4ad4efd23308831dc0377ab62b759148',
      'chem/06/e_oli.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/06/e_olt.dat'                   => 'f3b5fd377da817e422e7d665cce5c1a5',
      'chem/06/e_ora2.dat'                  => '52d26bd1abdfed9641792c188ab296be',
      'chem/06/e_so2.dat'                   => '3f7f24578507870dcb92e3862ea3af3b',
      'chem/06/e_sulf.dat'                  => 'daf38549f678e5498cc05ef832df6802',
      'chem/06/e_tol.dat'                   => 'e405557b843d6e8aa66faef9bc05ed23',
      'chem/06/e_pm_25.dat'                 => '1c94853c5a4ebd74a0fd1a9af52b4bcb',
      'chem/06/e_pm_10.dat'                 => 'fe6a91a846e4ec1b6965dceb6eb42411',
      'chem/06/e_xyl.dat'                   => '8cc8a89f9f4dc120e001c14ce9e9df34',
      'chem/06/dm0.dat'                     => '653f016109a72649cf01e6190ddebe09',
      'chem/06/oh.dat'                      => '5adb9b5cfc188b29bc590c920e90ffe0',
      'chem/06/h2o2.dat'                    => '22b8062b3371672d93dfce7b4bf43862',
      'chem/06/no3.dat'                     => '04852ad8daf53c90a2db61e5e4573463',
      'chem/07/e_ald.dat'                   => '6aca541af9d3dd520c7a8753a156335a',
      'chem/07/e_ash.dat'                   => '440c703c37ffea56745538793c804d3a',
      'chem/07/e_bc.dat'                    => '89048c42203cdce67aae70f943b7e794',
      'chem/07/e_co.dat'                    => 'bdf4a64fbfc37a358f66f3c53d327864',
      'chem/07/e_csl.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/07/e_dms.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/07/e_eth.dat'                   => '6e5c9fc66e38d5053e9107358c959250',
      'chem/07/e_hc3.dat'                   => 'dd0457d04d7584ea928b427b96821401',
      'chem/07/e_hc5.dat'                   => 'f7c813d051cc605e7f2b94670b53ea2d',
      'chem/07/e_hc8.dat'                   => '37e8397d6a7d648898551eb4c98cdbc1',
      'chem/07/e_hcho.dat'                  => 'd9a79b4584f720287234f5a7aef7352c',
      'chem/07/e_iso.dat'                   => '1bf586f3f92341917a41b4bdd06ce60b',
      'chem/07/e_ket.dat'                   => 'cc090795ec583ca67fa7439ab29b4cc6',
      'chem/07/e_nh3.dat'                   => '48a31568af80a6dbfd49b25ce23ab805',
      'chem/07/e_no2.dat'                   => 'ddb13170878f63bcbf44076efe8fc157',
      'chem/07/e_no.dat'                    => 'a3ac2ddff056cee044c001cfae421b0f',
      'chem/07/e_oc.dat'                    => '9cedd041fd13611f4037e538cc1f0174',
      'chem/07/e_oli.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/07/e_olt.dat'                   => 'c937644eee022f1f7d852316ac2735c6',
      'chem/07/e_ora2.dat'                  => 'aede4a3beea03f9bd5a982c97108fbed',
      'chem/07/e_so2.dat'                   => '0b5d57ce7c4988111e38a2167ce676b8',
      'chem/07/e_sulf.dat'                  => 'daf38549f678e5498cc05ef832df6802',
      'chem/07/e_tol.dat'                   => 'ea3dc2409be5978ace80e761be96a1c8',
      'chem/07/e_pm_25.dat'                 => '4246a91c7691de64d787e16e7518fc40',
      'chem/07/e_pm_10.dat'                 => '3abb872be29201d98dfcba7eafa9b039',
      'chem/07/e_xyl.dat'                   => '113c6c5f26acbd0ffaa9fa1691700df0',
      'chem/07/dm0.dat'                     => '58c32b5fe4de6f93506cab049bc8f540',
      'chem/07/oh.dat'                      => '75980f4b1f2df4d20badd0fc39f62ff3',
      'chem/07/h2o2.dat'                    => 'f0a57d06e6125bf184f113d47b06dc09',
      'chem/07/no3.dat'                     => '61c6ce8980a37b884425931184c99f65',
      'chem/08/e_ald.dat'                   => '68d71705effeab422afa55e190e2b29d',
      'chem/08/e_ash.dat'                   => '440c703c37ffea56745538793c804d3a',
      'chem/08/e_bc.dat'                    => '0b374b46a29273ec899bee93ec1cc1c6',
      'chem/08/e_co.dat'                    => '91a2b8fe82748dc6314dd6f9e231ba9c',
      'chem/08/e_csl.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/08/e_dms.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/08/e_eth.dat'                   => '160ca3ef12ee52a639b6f523974f7db1',
      'chem/08/e_hc3.dat'                   => '6b6e66a1f694e107842a248af40c2dab',
      'chem/08/e_hc5.dat'                   => 'a4025fc249ee408494c1af70aab525e9',
      'chem/08/e_hc8.dat'                   => 'a177ed8c2084fb5b387bdcc0b2970263',
      'chem/08/e_hcho.dat'                  => '745f8f4615f3af5759db3118ac8f7d92',
      'chem/08/e_iso.dat'                   => '8bf11e0afb2c22f649c99110689535d4',
      'chem/08/e_ket.dat'                   => 'cc564d76580c0ae322eb44442c035f04',
      'chem/08/e_nh3.dat'                   => 'f3f006d4ae6371ffc1c77116a0e37d1d',
      'chem/08/e_no2.dat'                   => '3263da757e2ccc30cc5f4f4239491421',
      'chem/08/e_no.dat'                    => 'd8645b6f766dfc767f4dff15e32201dc',
      'chem/08/e_oc.dat'                    => '8caa52c8e03bd86f5dfd35542c9438cc',
      'chem/08/e_oli.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/08/e_olt.dat'                   => '594d3db7877710d5466fbe29445c454d',
      'chem/08/e_ora2.dat'                  => 'abd83f44b9561ddfda65360fd605742b',
      'chem/08/e_so2.dat'                   => '180c12d576c38fdb6b5c5401d3878fb5',
      'chem/08/e_sulf.dat'                  => 'daf38549f678e5498cc05ef832df6802',
      'chem/08/e_tol.dat'                   => '1e7d2da1ca955c4e7c4dda754ef9e35e',
      'chem/08/e_pm_25.dat'                 => 'adb3aca4a9e53a43c5882b526c5cf4b4',
      'chem/08/e_pm_10.dat'                 => 'cb4f07905b984572328bb2ba9eb18161',
      'chem/08/e_xyl.dat'                   => 'c57492d00ef259a6d7a5d7c79b793f1b',
      'chem/08/dm0.dat'                     => 'f0d63510aeb5b9a8b7183f1eed620784',
      'chem/08/oh.dat'                      => '37fa3adea37bc0a6127295ed09d09388',
      'chem/08/h2o2.dat'                    => '3708996337f0ad1f1fcde0210ac8d55c',
      'chem/08/no3.dat'                     => '49cd8039843e817ec6449d0814d6f605',
      'chem/09/e_ald.dat'                   => '3fdecd39ac27b6b5cc7d7c16911a4a0d',
      'chem/09/e_ash.dat'                   => '440c703c37ffea56745538793c804d3a',
      'chem/09/e_bc.dat'                    => '5b5f033a3072a73a0ebe60484f48fd82',
      'chem/09/e_co.dat'                    => '35abad53b41cedda2956cfa72d726306',
      'chem/09/e_csl.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/09/e_dms.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/09/e_eth.dat'                   => '1be77dcdd5b234d189bf1d5ceffb8386',
      'chem/09/e_hc3.dat'                   => '1aa44876072392bd6b2d608ce4c77344',
      'chem/09/e_hc5.dat'                   => '8c806881d82611b5e920bdd2009994bd',
      'chem/09/e_hc8.dat'                   => '74112f0cb4c3fed8b2e724591c575137',
      'chem/09/e_hcho.dat'                  => '6825eae3553781b17254ea3aae693e02',
      'chem/09/e_iso.dat'                   => 'caf8adf8fd859a4b40d39718bb7dacbf',
      'chem/09/e_ket.dat'                   => '82444cc62e853f3b7e8a7170cc16ed62',
      'chem/09/e_nh3.dat'                   => 'c367ac785bd32a4638dcf00abfd5d641',
      'chem/09/e_no2.dat'                   => '816eae5fe1d267322b6d642f0c8dace3',
      'chem/09/e_no.dat'                    => '7531058c3a95e0e0430a8210e32ff96d',
      'chem/09/e_oc.dat'                    => '381787efce68530752cb3e2cbdc7f352',
      'chem/09/e_oli.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/09/e_olt.dat'                   => 'b0635d1d2127df13914e231df059dc95',
      'chem/09/e_ora2.dat'                  => 'b8751617f9e478ecdaf77fc142a9bbce',
      'chem/09/e_so2.dat'                   => '19230b4392b4a3d29e7d7910ce79f1ed',
      'chem/09/e_sulf.dat'                  => 'daf38549f678e5498cc05ef832df6802',
      'chem/09/e_tol.dat'                   => 'f285b65f256794e39918231a1cc93534',
      'chem/09/e_pm_25.dat'                 => '6fa2b47fb30e3953ac625d381f530c96',
      'chem/09/e_pm_10.dat'                 => 'fffee5e86a43abff5f2c39b8625d1ed2',
      'chem/09/e_xyl.dat'                   => '45c7ed2e9ffb15836531329d8079df56',
      'chem/09/dm0.dat'                     => '8568d0144ac6b9dc9a89f9fb7d8f131e',
      'chem/09/oh.dat'                      => 'd417cc821387ee44bebf9451cad6ee56',
      'chem/09/h2o2.dat'                    => '8ab9f5842da03413de8f98aa223c7d75',
      'chem/09/no3.dat'                     => '48de167f2289b96da7c87e2e14e8df04',
      'chem/10/e_ald.dat'                   => 'e298f49a35b579980ca42fd21abd6397',
      'chem/10/e_ash.dat'                   => '440c703c37ffea56745538793c804d3a',
      'chem/10/e_bc.dat'                    => '07926be5b5f5e385c46c110e230dc68c',
      'chem/10/e_co.dat'                    => '76f53e92380c2339caa6eff9fda415bb',
      'chem/10/e_csl.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/10/e_dms.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/10/e_eth.dat'                   => '2f42c9c8089a3cb22f7f6c8930ecac3b',
      'chem/10/e_hc3.dat'                   => '03fb7fb3bd8a87ce33fc23351aae36cf',
      'chem/10/e_hc5.dat'                   => '6010e286e01092720590f955549f4784',
      'chem/10/e_hc8.dat'                   => '70577141a16b15d67b8e5ef31932af21',
      'chem/10/e_hcho.dat'                  => '4e2d4da80958cf9940a25da1248dd5b0',
      'chem/10/e_iso.dat'                   => '7b9cc243f913c2a7c6afb794a2e588c2',
      'chem/10/e_ket.dat'                   => '977aacf7b4d8c576b9c4f98d669e58d8',
      'chem/10/e_nh3.dat'                   => '3ab32f498d899e3d07ebe5081370bc96',
      'chem/10/e_no2.dat'                   => '9836119301ad5d8dd85b9a28bc7a8bdb',
      'chem/10/e_no.dat'                    => 'a1a7b539874bbe75993808dd82476894',
      'chem/10/e_oc.dat'                    => 'f806a3c97af259dc4b48d6580c7513c8',
      'chem/10/e_oli.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/10/e_olt.dat'                   => 'dff4ff1085ae55a94fb3c68ef1b68d52',
      'chem/10/e_ora2.dat'                  => '55ab041f2b35da56106a0ea690cd0a4e',
      'chem/10/e_so2.dat'                   => '0f7ec529c8a8073f81a5e159de73819a',
      'chem/10/e_sulf.dat'                  => 'daf38549f678e5498cc05ef832df6802',
      'chem/10/e_tol.dat'                   => '3feaff8ade4202b037d70da7c4b6c414',
      'chem/10/e_pm_25.dat'                 => '83691395651f6844a62c15b2851ad133',
      'chem/10/e_pm_10.dat'                 => 'eb24455c74dece7d0d9ae459bd5a35bd',
      'chem/10/e_xyl.dat'                   => 'ce24ae340787bb1e7282b1aea2ccef0f',
      'chem/10/dm0.dat'                     => 'a6d876dbbf68db611fb3119d03fed5d2',
      'chem/10/oh.dat'                      => '015c33f425b3e56a0b70468b276f72ad',
      'chem/10/h2o2.dat'                    => '2c3d5c03b7054a5a2793bdfbe1c2cbcc',
      'chem/10/no3.dat'                     => '33cf0f9cee274cff1f1435533600b38f',
      'chem/11/e_ald.dat'                   => 'b2ba32d534d2fcd08f1d82261cf2dbb5',
      'chem/11/e_ash.dat'                   => '440c703c37ffea56745538793c804d3a',
      'chem/11/e_bc.dat'                    => '393420239350c928c4911ea1f7cde65f',
      'chem/11/e_co.dat'                    => '7aecb0cf08469432c161782427fc4332',
      'chem/11/e_csl.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/11/e_dms.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/11/e_eth.dat'                   => '6f3ffe9f6290289539a8018442906636',
      'chem/11/e_hc3.dat'                   => '00957fcdaf3761bb8abadddf82fbcd33',
      'chem/11/e_hc5.dat'                   => '1eba92ad7664ffa68498c8c1a71ae5d4',
      'chem/11/e_hc8.dat'                   => '768b91db5deaae482fde603f127e3d39',
      'chem/11/e_hcho.dat'                  => '0013ec5e26983ac3957a5eaa480b285f',
      'chem/11/e_iso.dat'                   => '8460e81bfc2b4087f5df13e3cabaa1f7',
      'chem/11/e_ket.dat'                   => '1d99c9393ffd9e8c31b4bc77713a13fd',
      'chem/11/e_nh3.dat'                   => 'eaff40245e19f04391d50fc83f259225',
      'chem/11/e_no2.dat'                   => 'c0b8349de8ff8bb7ffdc103bafdd1b7a',
      'chem/11/e_no.dat'                    => '24b14e409c5be83818240e84c7c8d249',
      'chem/11/e_oc.dat'                    => '9aafd3c1f1f90646012de6cfe40a28bb',
      'chem/11/e_oli.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/11/e_olt.dat'                   => '1c0a8f7bf9daf9c003c6d15078a70df4',
      'chem/11/e_ora2.dat'                  => '6620e50e6d2eeeb8c9ffe9365ff4af3f',
      'chem/11/e_so2.dat'                   => '47a1a11648a81ce1fcfc4e6f1ab39fd3',
      'chem/11/e_sulf.dat'                  => 'daf38549f678e5498cc05ef832df6802',
      'chem/11/e_tol.dat'                   => '04bfb6127774a81b725c8f9c7a1f33a0',
      'chem/11/e_pm_25.dat'                 => '37f197ea9eac5814b957d47d89581201',
      'chem/11/e_pm_10.dat'                 => '4db026f8913c6e58daf7e60e3e4d38ac',
      'chem/11/e_xyl.dat'                   => 'a364742577705dd2372f423aa2714f9c',
      'chem/11/dm0.dat'                     => '4adc21a5a7b28aa9336c15d6d392a148',
      'chem/11/oh.dat'                      => 'c8819c436ad2a9af1a7ac2765250d6f8',
      'chem/11/h2o2.dat'                    => '2d2783196c8e4cac8e0feb7babe25a7b',
      'chem/11/no3.dat'                     => '39e5785acc8df7f7082f755b7a914256',
      'chem/12/e_ald.dat'                   => 'de2d97a5c1f75020a6ec87e3a68eb283',
      'chem/12/e_ash.dat'                   => '440c703c37ffea56745538793c804d3a',
      'chem/12/e_bc.dat'                    => 'bcce5410fc57908d9fd51800490ed689',
      'chem/12/e_co.dat'                    => '7279f8f42da62bd30af8fc1335d4ecf6',
      'chem/12/e_csl.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/12/e_dms.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/12/e_eth.dat'                   => '2433eacf93cbecf6eb659c82f934936f',
      'chem/12/e_hc3.dat'                   => 'f10a2b25644789912565ffd3f897a397',
      'chem/12/e_hc5.dat'                   => '95387e6d25d08bfdbf1de0af899af16c',
      'chem/12/e_hc8.dat'                   => 'f79eed3f58f05c667b6c9e8aecdb810c',
      'chem/12/e_hcho.dat'                  => '1676a795ad089857dc665e9b6e90273d',
      'chem/12/e_iso.dat'                   => '3b95ab24f7b660e34d634b23c095452a',
      'chem/12/e_ket.dat'                   => 'bbecdb6fa08650752d6e227155d6d3d1',
      'chem/12/e_nh3.dat'                   => '51bc3def03ea260887fb3ce1586b3ab5',
      'chem/12/e_no2.dat'                   => 'ff5a924b91ecdf57eeaeb49cb1980bf0',
      'chem/12/e_no.dat'                    => '97cd1323127bfd79419e92682ec48aec',
      'chem/12/e_oc.dat'                    => 'f2b5366259b03158611496ea0442d36d',
      'chem/12/e_oli.dat'                   => 'daf38549f678e5498cc05ef832df6802',
      'chem/12/e_olt.dat'                   => 'aa7ee23d52baeceecccc4951ad4d1c63',
      'chem/12/e_ora2.dat'                  => '0fc773b3aaee2cfb13b062afcf2e8476',
      'chem/12/e_so2.dat'                   => 'b064c2afabf15a5bf102044ac8e59f61',
      'chem/12/e_sulf.dat'                  => 'daf38549f678e5498cc05ef832df6802',
      'chem/12/e_tol.dat'                   => 'e2df940ee218a05c346b11ea2fe644e8',
      'chem/12/e_pm_25.dat'                 => 'c01b716d56e7c289c12974d634e4c5e6',
      'chem/12/e_pm_10.dat'                 => 'c8b55e3066ab49080f918f50fdad4cf9',
      'chem/12/e_xyl.dat'                   => '87db721bc703b0f96b4fabf196bf03e8',
      'chem/12/dm0.dat'                     => '7f4e9ab44284c659a8b62b34a0831a07',
      'chem/12/oh.dat'                      => '60b8010d6b9982a6cc02320edb040fe6',
      'chem/12/h2o2.dat'                    => '0885c8e0e56e7c8470d078b5d2483ec6',
      'chem/12/no3.dat'                     => '999587211759b22ae8413e34420d240a',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_othKetones_2000_0.5x0.5.nc'    => 'a0a616bb94c2529fa4d17316feaf1e86',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_propane_2000_0.5x0.5.nc'       => 'd9f06e9a3a55b0430bbf46e85982ca3c',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_formaldehyde_2000_0.5x0.5.nc'  => '1a5e4a2a07dab1fffcff2685589b598b',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_acetaldehyde_2000_0.5x0.5.nc'  => 'a8a2b21de956e4995627a56f8d5ee22e',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_isoprene_2000_0.5x0.5.nc'      => 'f8c25658296c3dade084be2b98791dbe',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_ethane_2000_0.5x0.5.nc'        => '82f317e917688eab8323e66f995405a1',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_ethene_2000_0.5x0.5.nc'        => '6c687fb626c50837b050a006882227e8',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_toluene_2000_0.5x0.5.nc'       => '4456e888d6a8f244ac0d96cd64a7455b',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_monoterpenes_2000_0.5x0.5.nc'  => 'de4a14f3cbbcb53beec124d410c45bfd',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_sesquiterpenes_2000_0.5x0.5.nc'=> 'c6e343f62db140cbc5df7c933d929185',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_acetone_2000_0.5x0.5.nc'       => 'db6838bbdf5bcd2e03349dc5ed26cefc',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_methane_2000_0.5x0.5.nc'       => '4de52171a340f8e1f61fcee95495a0d2',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_CO_2000_0.5x0.5.nc'            => 'cb9d489c2885741588021dbc92f23cb7',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_propene_2000_0.5x0.5.nc'       => '51a261a585350eecdd209b59b29c2fbd',
      'chem/EMISSION_DATA/MEGAN/2000/MEGAN_nat_methanol_2000_0.5x0.5.nc'      => '3267412b78fd78b44c2d48a669625c57',
      'chem/EMISSION_DATA/GOCART/gocart_bg/gmi_200601.nc'                     => 'c6937fee89d588622886d6bafe1ab49d',
      'chem/EMISSION_DATA/GOCART/dms_data/dms_1x1.25.nc'                      => '40c672d9b1c78d8c796d6f74a7df59a7',
      'chem/EMISSION_DATA/GOCART/erod/GAO_source_3cl.nc'                      => '40ca48bb44e60c340720a23447e538e1',
      'chem/EMISSION_DATA/OLSON2/OLSON.h5'  => '0e3a478cd768684e32e93b23d598d39b',
      'climaeropac_global.txt'              => '0056ee94cc520ff9ba759e9660362042',
      'co2historicaldata_2007.txt'          => '4dbae00d7c84d4158659c7bb42d5ce52',
      'co2historicaldata_2008.txt'          => '2706b78df491d9311b93dbb35193e2d1',
      'co2historicaldata_2009.txt'          => 'ee945c7df4e3e9d61adac649a9df5aea',
      'co2historicaldata_2010.txt'          => '51c88e0bb297f00e7e0db1ae08e85c9f',
      'co2historicaldata_2011.txt'          => '29f21c1480e41a598eb999d48fdb0062',
      'co2historicaldata_2012.txt'          => '4f58a15d5d3096ef0af6aae28141ef99',
      'co2historicaldata_glob.txt'          => 'd0b1e1403f7726c5fc5745f9312c9ac3',
      'dm0_binary'                          => 'f08eaf8cdcc128356b211a015f7fc8c8',
      'geo_em.d01.nc'                       => '95af13ee9fa08e684d3b7b3187e83852',
      'gfsltln_t1534.dat'                   => '0f99cf12ec9adcbed091f652fe24c17a',
      'gfsltln_t574.dat'                    => '348172e9f4d23a590482f0d501ff11c9',
      'gincr.b'                             => '7b70a0f9cf28bfc3bf2543aa751eeade',
      'global_mtnvar.t1534'                 => '49152f2339d0fa3c5582940c3c292422',
      'global_mtnvar.t574'                  => '7b34cdde6e35dc56c93e71f9a3dec716',
      'global_o3prdlos.f77'                 => '79fc6d61a66eb2512b5b95e84198fdb2',
      'HADISST_MONTHLY.1991-present'        => 'cb0abf1ee3f99fd40b8cec1ccbfe702c',
      'ocean_bcs_ltln.360x180.dat'          => '079b9bd8513429c467c68570030dbdf2',
      'rucgrid'                             => '521d14c526e6500fdfeac1b413ca0945',
      'sfc_emissivity_idx.txt'              => '8f64bd9e63cc049909ec287decec1c44',
      'solarconstant_noaa_an.txt'           => '1f065539ad66484fd1be236132167760',
      'wrf5mintopo.dat'                     => 'ab846b2154b57c003c0dceb8e0c6d5ee'
    }
# Don't validate the executable prep_chem_sources, since they differ between hardware
# Note: this will not validate any files with prep_chem_sources in the name
    actual=Dir.glob("#{dir}/**/*").reject{ |f| f['prep_chem_sources'] }
    actual=actual.delete_if { |e| File.directory?(e) }
    actual=actual.reduce({}) do
      |m,e| m.merge!({e.sub(/#{dir}\/?/,'')=>Digest::MD5.file(e).to_s})
    end
    actual.keys.sort.each do |k|
      die "Unexpected data file: #{k}" unless expected[k]
      unless actual[k]==expected[k]
        logd "Checksum validation failed for #{dir}/#{k}"
        logd "  Expected #{expected[k]}"
        logd "    Actual #{actual[k]}"
        die "Error validating test-suite data, see #{logfile}"
      end
      logd "  #{k}: OK"
    end
    logd "Validating data: OK"
  end

  def lib_wait_for_job(env,jobid)
    live=%w[E H Q R T W S]
    interval=env.qmon.interval
    tolerance=interval*5
    job_update_time=nil
    job_clock=nil
    while true
      sleep interval*1.25
      result=env.qmon.query(jobid)
      if result[:query_status]==0
        if (state=result[:job_state])
          job_update_time=Time.now
          break unless live.include?(state)
        else
          if job_update_time
            if (age=Time.now-job_update_time) > tolerance
              logd "No news on job #{jobid} in #{age} seconds, assuming complete"
              break
            end
          else
            job_clock||=Time.now # clock is ticking...
            if (age=Time.now-job_clock) > tolerance
              die "Job #{jobid} unknown after #{age} seconds, aborting"
            end
          end
        end
      else
        logd "Batch-system query error:"
        result[:error].split("\n").each { |e| logd e }
        if (age=Time.now-result[:updated]) > tolerance
          die "No response from batch system in #{age} seconds, aborting"
        end
      end
    end
    result[:exit_status]
  end

  class Qmon

    attr_reader :interval

    def initialize(interval)
      require "rexml/document"
      require "thread"
      @error=nil
      @interval=interval
      @jobs={}
      @lock=Mutex.new
      @running=false
      @thread=nil
      start
    end

    def query(jobid)
      @lock.synchronize do
        {
          :error=>@error,
          :exit_status=>((x=@jobs[jobid])?(x[:exit_status]):(nil)),
          :job_state=>((x=@jobs[jobid])?(x[:job_state]):(nil)),
          :query_status=>@status,
          :updated=>@updated
        }
      end
    end

    def start
      return if @running
      update
      @updated=Time.now
      @running=true
      sleep @interval
      @thread=Thread.new do
        while @running
          t0=Time.now
          update
          elapsed=Time.now-t0
          if (adjusted_interval=@interval-elapsed) > 0
            sleep adjusted_interval
          end
        end
      end
    end

    def stop
      return unless @running
      @running=false
      @thread.join if @thread
      @thread=nil
    end

    private

    def update
      jobs={}
      output=IO.popen("qstat -x 2>&1") { |io| io.read }
      status=$?.exitstatus
      if status==0
        error=nil
        doc=REXML::Document.new(output)
        jobs=REXML::XPath.each(doc,"//Data/Job").reduce({}) do |m,e|
          jobid=REXML::XPath.first(e,"Job_Id").text.sub(/^([0-9]+).*/,'\1')
          job_state=REXML::XPath.first(e,"job_state").text
          exit_status=(x=REXML::XPath.first(e,"exit_status"))?(x.text.to_i):(nil)
          m[jobid]={:job_state=>job_state,:exit_status=>exit_status}
          m
        end
      else
        error=output
      end
      @lock.synchronize do
        @error=error
        @status=status
        unless error
          @jobs=jobs
          @updated=Time.now
        end
      end
    end

  end # class Qmon

end
