#!/bin/ksh

# Generic functions

function fail {
  test -n "$1" && print "ERROR: $1" >&2
  exit 1
}

# Platform functions

function jet_common {
  print module load szip/2.1
  print module load hdf5/1.8.9
  print module load netcdf4/4.2.1.1
  print module load wgrib/1.8.1.0b
  print module load wgrib2/0.1.9.6a
  print module load cnvgrib/1.4.0
  print module load imagemagick/6.2.8
}

function jet_modsetup {
  print . /apps/lmod/lmod/init/ksh
  print module purge
}

function theia_common {
  print module load impi/5.1.1.109
  print module load grads/2.0.2
  print module load ncl/6.3.0
  print module load imagemagick/7.0.5
  print module load cnvgrib/1.4.0
  print module load hdf5
}

function theia_modsetup {
  print . /apps/lmod/lmod/init/ksh
  print module purge
}

function stampede_common {
  print module load impi/4.1.3.049
  print module load ncl_ncarg
}

function stampede_modsetup {
  print . /opt/apps/lmod/lmod/init/ksh
  print module purge
}

function stampedexeonphi_common {
  print module load impi/4.1.3.049
}

function stampedexeonphi_modsetup {
  print . /opt/apps/lmod/lmod/init/ksh
  print module purge
}

# Build functions

function endeavor {
  print export NETCDF=/panfs/users/Xjrosin/netcdf/3.6.3-intel
}

function jetnag {
  jet_modsetup
  print module load nag/6.0
  print module load ncl/6.3.0
  print module load netcdf/3.6.3
  print module load wgrib/1.8.1.0b
  print module load wgrib2/0.1.9.6a
  print module load cnvgrib/1.4.0
  print module load imagemagick/6.2.8
}

function lahey {
  jet_modsetup
  print module load lahey/8.10b
  jet_common
}

function linuxpcgnu {
  print export NETCDF=/export/rosinski/netcdf-3.6.3
}

function macgnu {
  print export NETCDF=/Users/julie.schramm/netcdf-3.6.3-gfortran
}

function mvapich {
  jet_modsetup
  print module load intel/12.1.4
  print module load ncl/6.3.0
  jet_common
}

function nems {
  jet_modsetup
  print module load intel/12.1.4
  print module load ncl/6.3.0
  jet_common
  print export FIM_ESMF_INSTALL_LIBDIR_ABSPATH=/home/hpc/ESMF/esmf-3.1.0rp3_intel-12.1.4_mvapich2-1.8/lib/libO/Linux.intel.64.mpich2.default
}

function openmpi {
  jet_modsetup
  print module load intel/12.1.4
  print module load ncl/6.3.0
  jet_common
}

function stampede {
  stampede_modsetup
  print module load intel/14.0.1.106
  print module load netcdf
  stampede_common
}

function stampedexeonphi {
  stampedexeonphi_modsetup
  print module load intel/14.0.1.106
  print module load netcdf
  stampedexeonphi_common
}

function theiaintel {
  theia_modsetup
  print module load intel/14.0.2
  print module load netcdf/3.6.3
  theia_common
}

function theianems {
  theia_modsetup
  print module load intel/14.0.2
  print module load netcdf/3.6.3
  theia_common
# Switch to the version of impi used in the ESMF library build
  print module switch impi/5.1.1.109 impi/4.1.3.048
  print export FIM_ESMF_INSTALL_LIBDIR_ABSPATH=/apps/esmf/3.1.0rp5/intel/intelmpi/lib/libO/Linux.intel.64.intelmpi.default
}

# Set configuration from command line (or use default)

build_config="$1"

test -z "$build_config" && build_config=openmpi

print set -e

# Make per-config settings

case "$build_config" in

  "endeavor_p")
    endeavor
    print export FC=mpiifort
    ;;

  "endeavor_s")
    endeavor
    print export FC=ifort
    ;;

  "gpu")
    print module load cuda
    print export FC=mpif90
    print module list
    ;;

  "jetnag_p")
    jetnag
    print module load mvapich2/1.8
    print export FC=mpif90
    print module list
    ;;

  "jetnag_s")
    jetnag
    print export FC=nagfor
    print module list
    ;;

  "lahey_p")
    lahey
    print module load mvapich2/1.8
    print export FC=mpif90
    print module list
    ;;

  "lahey_s")
    lahey
    print export FC=lf95
    print module list
    ;;

  "linuxpcgnu_p")
    linuxpcgnu
    print export FC=mpif90
    ;;

  "linuxpcgnu_s")
    linuxpcgnu
    print export FC=gfortran
    ;;

  "macgnu_p")
    macgnu
    print export FC=mpif90
    ;;

  "macgnu_s")
    macgnu
    print export FC=gfortran
    ;;

  "mvapich_p")
    mvapich
    print module load mvapich2/1.8
    print export FC=mpif90
    print module list
    ;;

  "mvapich_s")
    mvapich
    print export FC=ifort
    print module list
    ;;

  "nems_p")
    nems
    print module load mvapich2/1.8
    print export FC=mpif90
    print module list
    ;;

  "nems_s")
    nems
    print export FC=ifort
    print module list
    ;;

  "openmpi_p")
    openmpi
    print module load openmpi/1.6.3
    print export FC=mpif90
    print module list
    ;;

  "openmpi_s")
    openmpi
    print export FC=ifort
    print module list
    ;;

  "stampede_p")
    stampede
    print export FC=mpiifort
    print module list
    ;;

  "stampede_s")
    stampede
    print export FC=ifort
    print module list
    ;;

  "stampedexeonphi_p")
    stampedexeonpli
    print export FC=mpiifort
    print module list
    ;;

  "stampedexeonphi_s")
    stampedexeonpli
    print export FC=ifort
    print module list
    ;;

  "theiaintel_p")
    theiaintel
    print export FC=mpiifort
    print module list
    ;;

  "theiaintel_s")
    theiaintel
    print export FC=ifort
    print module list
    ;;

  "theianems_p")
    theianems
    print export FC=mpiifort
    print module list
    ;;

  "theianems_s")
    theianems
    print export FC=ifort
    print module list
    ;;

  *)
    fail "Unrecognized build config '$build_config'"
    ;;

esac

# Are we threading only for SS2ICOS, for all of FIM, or not at all?

mm=macros.make.$(print $build_config | sed 's/_.*//')

grep -qE "OPENMP *= *yes"         $mm && print export THREAD_FIM=yes
grep -qE "OPENMP_SS2ICOS *= *yes" $mm && print export THREAD_SS2ICOS=yes

print set +e
