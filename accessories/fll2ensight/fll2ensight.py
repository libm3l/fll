#!/usr/bin/python
# 
#
#  this is a python script
#
import os
import sys
from subprocess import Popen, PIPE
import unicodedata
import ast

#Definitions

def run(file,filebc,fmt,ofile,endian,fm, bconly):
#
#  execute 
#
    print_header()
#
    path = os.path.dirname(os.path.abspath(__file__))
    cwd = os.getcwd()

    path = check_path(path=path)
    cwd = check_path(path=cwd)

    executable = path+"fll2ensight.x"

    if not os.path.isfile(file):
      print("  ")
      print("\033[031mERROR:\033[039m specified file \033[032m"+file+"\033[039m does not exist, terminating .... ") 
      sys.exit()

    print(" ")  
    print("\033[039m Specified input file is:  \033[032m"+file+"\033[039m")
    if fmt == 'b'  or fmt == 'B':
      print("\033[039m Specified input file format is: \033[032mbinary\033[039m") 
    else:
      print("\033[039m Specified input file format is: \033[032mASCII \033[039m")

    if fm == 'ugrid':
       print("\033[039m Grid format file is: \033[032mUGRID \033[039m")  
       if fmt == 'b'  or fmt == 'B':
         if(endian == 'b' or endian == 'B'):
           print("\033[039m Specified endian of input file is: \033[032mbig\033[039m")  
         else:
           print("\033[039m Specified endian of input file is: \033[032msmall\033[039m")  
    else:
       print("\033[039m Grid format file is: \033[032mFLL \033[039m")  

    print(" ")  
    print("\033[039m Specified ensight file name is:  \033[032m"+ofile+"\033[039m") 

    if bconly == 'y':
      print("\033[039m Exporting only boundaries \033[039m") 

    if sys.version_info < (3,0):
      p = Popen([executable], stdin=PIPE) #NOTE: no shell=True here
      p.communicate(os.linesep.join([file,fm,filebc,fmt,endian,ofile,bconly]))
    else:
      p = Popen([executable], stdin=PIPE,universal_newlines=True) #NOTE: no shell=True here
      p.communicate(os.linesep.join( [file,fm,filebc,fmt,endian,ofile,bconly]))

def print_header():
     print("  ")
     print ("\033[031m************************************************************************************ \033[039m")
     print ("\033[031m                                                                                   \033[039m")
     print ("\033[031m               \033[039m              fll2ensight   - v1.1       \033[031m                          \033[039m")
     print ("\033[031m                                                                                   \033[039m")
     print ("\033[031m             \033[039m           export to ensight format utility  \033[031m                    \033[039m")
     print ("\033[031m                                                                                  \033[039m")
     print ("\033[031m************************************************************************************ \033[039m")


def check_path(path):
    if not(path.endswith("/")):
        path=path + "/"

    return path



#Script
if __name__ == "__main__":
    import argparse

    # Add command line arguments
    parser = argparse.ArgumentParser(description='FLL configure script')
    parser.add_argument('-i','--gridfile',nargs=1,help='Input file')
    parser.add_argument('-b','--bcfile',nargs=1,help='Input file')
    parser.add_argument('-o','--output_file',nargs=1,help='Output file')
    parser.add_argument('-fi','--format_i',nargs=1,help='Format of the input file - ASCII, binary')
    parser.add_argument('-fm','--mesh_format',nargs=1,help='Format of the input file - ugrid or fll')
    parser.add_argument('-e','--endian',nargs=1,help='endian fo input file')
    parser.add_argument('-B','--bconly',action='store_true',help='export boundaries only',required=False)

    # Parse the command line arguments
    args = parser.parse_args()

    file = args.gridfile[0]   if args.gridfile else None
    filebc = args.bcfile[0]   if args.bcfile else ""
    format_i = args.format_i[0] if args.format_i else None
    output = args.output_file[0] if args.output_file else None
    endi    = args.endian[0] if args.endian else None
    fm    = args.mesh_format[0] if args.mesh_format else None
    be = args.bconly


    if not len(sys.argv) > 1:
        print("\nfll_convert - converts files\n")
        print("usage: fll_convert.py [-h] [-i GRID_FILE] [-b BC_FILE]  [-o OUTPUT_FILE] [-fi FORMAT_I] \n")
        sys.exit()

    if not file:
        print ("\033[031mError: \033[039m missing name of input mesh file, option \033[031m -i \033[039m")
        sys.exit()

    if not fm:
        print ("\033[031mError: \033[039m format of input mesh file \033[031m -fm \033[039m")
        sys.exit()

    if fm == 'ugrid':
      if not filebc:
        print ("\033[031mError: \033[039m missing name of input boundary condition file, option \033[031m -b \033[039m")
        sys.exit()

    if not format_i:
        print ("\033[031mError: \033[039m missing input file format, option\033[031m -f \033[039m")
        print ("\033[031m       \033[039m available options are: \033[032m a - ASCII\033[039m")
        print ("\033[031m       \033[039m                        \033[032m b - binary format\033[039m")
        sys.exit()

    bconly = 'n'
    if be:
        bconly='y'

    if not endi:
        endi='b'

    if not output:
        print ("\033[031mError: \033[039m missing output file, option \033[031m -o \033[039m")
        sys.exit()


    run(file=file,filebc=filebc, fmt=format_i, ofile=output, endian=endi, fm = fm, bconly = bconly)
