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

def run(file,fmtg,fmts,ofile, bconly,solyes, solfile):
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
    print("\033[039m Specified grid file is:  \033[032m"+file+"\033[039m")
    if fmtg == 'b'  or fmtg == 'B':
      print("\033[039m Specified file format is: \033[032mbinary\033[039m") 
    else:
      print("\033[039m Specified file format is: \033[032mASCII \033[039m")

      
    if solyes == 'y':
       print("\033[039m Specified solution file is:  \033[032m"+solfile+"\033[039m")
       if not os.path.isfile(solfile):
         print("  ")
         print("\033[031mERROR:\033[039m specified solution file \033[032m"+solfile+"\033[039m does not exist, terminating .... ") 
         sys.exit()

       if fmts == 'b'  or fmts == 'B':
         print("\033[039m Specified file format is: \033[032mbinary\033[039m") 
       else:
         print("\033[039m Specified file format is: \033[032mASCII \033[039m")


    print(" ")  
    print("\033[039m Specified ensight file name is:  \033[032m"+ofile+"\033[039m") 

    if bconly == 'y':
      print("\033[039m Exporting only boundaries \033[039m") 

    if sys.version_info < (3,0):
      p = Popen([executable], stdin=PIPE) #NOTE: no shell=True here
      p.communicate(os.linesep.join([file,fmtg,fmts,ofile,bconly,solyes,solfile]))
    else:
      p = Popen([executable], stdin=PIPE,universal_newlines=True) #NOTE: no shell=True here
      p.communicate(os.linesep.join( [file,fmtg,fmts,ofile,bconly,solyes,solfile]))

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
    parser.add_argument('-g','--grid',nargs=1,help='Input grid file')
    parser.add_argument('-s','--solution',nargs=1,help='Input solution file')
    parser.add_argument('-o','--output_file',nargs=1,help='Output file')
    parser.add_argument('-fg','--format_grid',nargs=1,help='Format of mesh input file - ASCII, binary')
    parser.add_argument('-fs','--format_sol',nargs=1,help='Format of solution input file - ASCII, binary')
    parser.add_argument('-f','--format',nargs=1,help='Format of the input files - ASCII, binary')
    parser.add_argument('-B','--bconly',action='store_true',help='export boundaries only',required=False)

    # Parse the command line arguments
    args = parser.parse_args()

    file = args.grid[0]   if args.grid else None
    solfile = args.solution[0]   if args.solution else None
    format_grid = args.format_grid[0] if args.format_grid else None
    format_sol = args.format_sol[0] if args.format_sol else None
    format = args.format[0] if args.format else None
    output = args.output_file[0] if args.output_file else None
    be = args.bconly


    if not len(sys.argv) > 1:
        print("\nfll_convert - converts files\n")
        print("use fll_convert.py --help to see available options \n")
        sys.exit()

    if not file:
        print ("\033[031mError: \033[039m missing name of input mesh file, option \033[031m -i \033[039m")
        sys.exit()
        
    if not solfile:
        solyes  = 'n'
        solfile = 'None'
    else:
        solyes = 'y'

    if format:
        print ("\033[031mError: \033[039m specified file format is , option\033[031m format \033[039m")
        format_grid = format
        format_sol = format
    else:

      if not format_grid:
          print ("\033[031mError: \033[039m missing grid file format, option\033[031m -fg \033[039m")
          print ("\033[031m       \033[039m available options are: \033[032m a - ASCII\033[039m")
          print ("\033[031m       \033[039m                        \033[032m b - binary format\033[039m")
          sys.exit()

      if not format_sol:
          print ("\033[031mError: \033[039m missing solution file format, option\033[031m -fs \033[039m")
          print ("\033[031m       \033[039m setting it to the same format as grid format \033[032m option -fg\033[039m")
          format_sol = format_grid


    bconly = 'n'
    if be:
        bconly='y'


    if not output:
        print ("\033[031mError: \033[039m missing output file, option \033[031m -o \033[039m")
        sys.exit()


    run(file=file, fmtg=format_grid, fmts=format_sol, ofile=output, bconly = bconly, solyes = solyes, solfile = solfile)
