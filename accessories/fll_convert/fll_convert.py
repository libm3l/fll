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

def run(file,fmt,efmt,ofile,ofmt):
#
#  execute 
#
    print_header()
#
    path = os.path.dirname(os.path.abspath(__file__))
    cwd = os.getcwd()

    path = check_path(path=path)
    cwd = check_path(path=cwd)

    executable = path+"fll_convert.x"

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
      
    if efmt == 'fll':
      print("\033[039m Specified input file type  is: \033[032mFLL \033[039m")  
    else:
      print("\033[039m Specified input file type is: \033[032mFFA \033[039m")  
      
    print(" ")  
    print("\033[039m Specified outpu file is:  \033[032m"+ofile+"\033[039m")
    if fmt == 'b'  or fmt == 'B':
      print("\033[039m Specified input file format is: \033[032mbinary\033[039m") 
    else:
      print("\033[039m Specified input file format is: \033[032mASCII \033[039m") 
      
    if ofmt == 'fll':
      print("\033[039m Specified output file type  is: \033[032mFLL \033[039m")  
    else:
      print("\033[039m Specified output file type is: \033[032mFFA \033[039m") 
      
    if sys.version_info < (3,0):
      p = Popen([executable], stdin=PIPE) #NOTE: no shell=True here
      p.communicate(os.linesep.join([file, fmt, efmt, ofile,ofmt]))
    else:
      p = Popen([executable], stdin=PIPE,universal_newlines=True) #NOTE: no shell=True here
      p.communicate(os.linesep.join( [file, fmt, efmt, ofile,ofmt]))

def print_header():
     print("  ")
     print ("\033[031m************************************************************************************ \033[039m")
     print ("\033[031m                                                                                   \033[039m")
     print ("\033[031m               \033[039m              fll_convert   - v1.1       \033[031m                          \033[039m")
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
    parser.add_argument('-f','--format',nargs=1,help='Format of the file - ASCII, binary')
    parser.add_argument('-o','--output_file',nargs=1,help='Output file')
    parser.add_argument('-e','--format_input',nargs=1,help='Input file format')
    parser.add_argument('-g','--format_output',nargs=1,help='Output file format')


    # Parse the command line arguments
    args = parser.parse_args()

    file = args.file[0]   if args.file else None
    format = args.format[0] if args.format else None
    eformat = args.format_input[0] if args.format_input else None
    oformat = args.format_output[0] if args.format_output else None
    output = args.output_file[0] if args.output_file else None

    if not file:
        print ("\033[031mError: \033[039m missing name of file \033[031m-i \033[039m")
        sys.exit()

    if not format:
        print ("\033[031mError: \033[039m missing file format\033[031m-c \033[032m")
        print ("\033[031m       \033[039m available options are: \033[032m a - ASCII\033[039m")
        print ("\033[031m       \033[039m                        \033[032m b - binary format\033[039m")
        sys.exit()
    
    if not output:
        print ("\033[031mError: \033[039m missing output file \033[031m-o \033[032m")
        sys.exit()

    if not eformat:
        eformat = 'fll'
    else:
        
     if not('fll') or not('ffa'):
        print ("\033[031mError: \033[039m wrong file format\033[031m-e \033[032m")
        print ("\033[031m       \033[039m available options are: \033[032m fll - fll native format\033[039m")
        print ("\033[031m       \033[039m                        \033[032m ffa - ffa format\033[039m")
        sys.exit()


    if not oformat:
        oformat = 'fll'
    else:
        
     if not('fll') or not('ffa'):
        print ("\033[031mError: \033[039m wrong file format\033[031m-e \033[032m")
        print ("\033[031m       \033[039m available options are: \033[032m fll - fll native format\033[039m")
        print ("\033[031m       \033[039m                        \033[032m ffa - ffa format\033[039m")
        sys.exit()

    run(file=file,fmt=format, efmt = eformat, ofile=output, ofmt = oformat)