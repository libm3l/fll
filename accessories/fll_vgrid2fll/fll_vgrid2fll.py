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

def run(file,fmto,ofile,mapbc,bc):
#
#  execute 
#
    print_header()
#
    path = os.path.dirname(os.path.abspath(__file__))
    cwd = os.getcwd()

    path = check_path(path=path)
    cwd = check_path(path=cwd)

    executable = path+"fll_vgrid2fll.x"

    if not os.path.isfile(file):
      print("  ")
      print("\033[031mERROR:\033[039m specified file \033[032m"+file+"\033[039m does not exist, terminating .... ") 
      sys.exit()

    print(" ")  
    print("\033[039m Specified input mesh file is:  \033[032m"+file+"\033[039m")
    print("\033[039m Specified input mapbc file is:  \033[032m"+mapbc+"\033[039m")
    print("\033[039m Specified input bc file is:  \033[032m"+bc+"\033[039m")
  
    print(" ")  
    print("\033[039m Specified output file is:  \033[032m"+ofile+"\033[039m")
    if fmto == 'b'  or fmto == 'B':
      print("\033[039m Specified output file format is: \033[032mbinary\033[039m") 
    else:
      print("\033[039m Specified output file format is: \033[032mASCII \033[039m")      

    if sys.version_info < (3,0):
      p = Popen([executable], stdin=PIPE) #NOTE: no shell=True here
      p.communicate(os.linesep.join([file, mapbc, bc, ofile, fmto]))
    else:
      p = Popen([executable], stdin=PIPE,universal_newlines=True) #NOTE: no shell=True here
      p.communicate(os.linesep.join( [file, mapbc, bc, ofile, fmto]))

def print_header():
     print("  ")
     print ("\033[031m************************************************************************************ \033[039m")
     print ("\033[031m                                                                                   \033[039m")
     print ("\033[031m               \033[039m              fll_ffa2fll   - v1.1       \033[031m                          \033[039m")
     print ("\033[031m                                                                                   \033[039m")
     print ("\033[031m             \033[039m                 conversion utility  \033[031m                    \033[039m")
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
    parser.add_argument('-i','--file',nargs=1,help='Input file')
    parser.add_argument('-o','--output_file',nargs=1,help='Output file')
    parser.add_argument('-fo','--format_output',nargs=1,help='Format of the output file - ASCII, binary')
    parser.add_argument('-m','--mapbc_file',nargs=1,help='Input file')
    parser.add_argument('-b','--bc_file',nargs=1,help='Input file')


    # Parse the command line arguments
    args = parser.parse_args()

    file = args.file[0]   if args.file else None
    format_o = args.format_output[0] if args.format_output else None
    output = args.output_file[0] if args.output_file else None
    mapbc = args.mapbc_file[0] if args.mapbc_file else None
    bc = args.bc_file[0] if args.bc_file else None


    if not len(sys.argv) > 1:
        print("\nfll_convert - converts files\n")
        print("usage: fll_convert.py [-h] [-i FILE] [-m MAPBC_FILE] [-b BC_FILE] [-o OUTPUT_FILE] [-fo FORMAT_OUTPUT]\n")
        sys.exit()

    if not file:
        print ("\033[031mError: \033[039m missing name of mesh file, option \033[031m -i \033[039m")
        sys.exit()

    if not mapbc:
        print ("\033[031mError: \033[039m missing name of mapbc file, option \033[031m -m \033[039m")
        sys.exit()

    if not bc:
        print ("\033[031mError: \033[039m missing name of bc file, option \033[031m -b \033[039m")
        sys.exit()

    if not format_o:
        print ("\033[031mError: \033[039m missing output file format, option\033[031m -f \033[039m")
        print ("\033[031m       \033[039m available options are: \033[032m a - ASCII\033[039m")
        print ("\033[031m       \033[039m                        \033[032m b - binary format\033[039m")
        sys.exit()
    
    if not output:
        print ("\033[031mError: \033[039m missing output file, option \033[031m -o \033[039m")
        sys.exit()


    run(file=file, fmto = format_o, ofile=output, mapbc=mapbc, bc=bc)
