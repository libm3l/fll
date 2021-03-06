#!/usr/bin/python
# 
#
#  this is a python script wj
#
import os
import sys
from subprocess import Popen, PIPE
import unicodedata
import ast

#Definitions

def run(file,fmt,efmt,scan,dir,color):
#
#  execute 
#
    print_header()
#
    path = os.path.dirname(os.path.abspath(__file__))
    cwd = os.getcwd()

    path = check_path(path=path)
    cwd = check_path(path=cwd)

    executable = path+"fll_cat.x"

    if not os.path.isfile(file):
      print("  ")
      if color == 'y':
        print("\033[031mERROR:\033[039m specified file \033[032m"+file+"\033[039m does not exist, terminating .... ") 
      else:
        print("ERROR: specified file "+file+" does not exist, terminating .... ")    
      sys.exit()

    print(" ")  
    if color == 'y':
      print("\033[039m Specified file  is:       \033[032m"+file+"\033[039m")
    else:
      print(" Specified file  is:       "+file)
    if fmt == 'b'  or fmt == 'B':
      if color == 'y':
        print("\033[039m Specified file format is: \033[032mbinary\033[039m") 
      else:
        print(" Specified file format is: binary") 
    else:
      if color == 'y':
        print("\033[039m Specified file format is: \033[032mASCII \033[039m")  
      else:
        print(" Specified file format is: ASCII ")  
    if dir == 'Y':  
        print(" ")
        print("\033[035m ... printing DIR structure only ... \033[039m")  
        if fmt == 'b'  or fmt == 'B': scan = 'Y'

    if scan == 'Y':  
        print(" ")
        if color == 'y':
          print("\033[035m ... running in scan only mode ... \033[039m")  
        else:
          print(" ... running in scan only mode ... ")  
    print(" ")  
    
    if sys.version_info < (3,0):
      p = Popen([executable], stdin=PIPE) #NOTE: no shell=True here
      p.communicate(os.linesep.join([file, fmt, efmt, scan, dir, color]))
    else:
      p = Popen([executable], stdin=PIPE,universal_newlines=True) #NOTE: no shell=True here
      p.communicate(os.linesep.join( [file, fmt, efmt, scan, dir, color]))

def print_header():
     if color == 'y':
      print("  ")
      print ("\033[031m************************************************************************************ \033[039m")
      print ("\033[031m                                                                                   \033[039m")
      print ("\033[031m               \033[039m              fll_cat   - v1.1       \033[031m                          \033[039m")
      print ("\033[031m                                                                                   \033[039m")
      print ("\033[031m             \033[039m          prints content of file on screen  \033[031m                    \033[039m")
      print ("\033[031m                                                                                  \033[039m")
      print ("\033[031m************************************************************************************ \033[039m")
     else:
      print ("************************************************************************************ ")
      print ("                                                                                   ")
      print ("                             fll_cat   - v1.1                                 ")
      print ("                                                                                   ")
      print ("                       prints content of file on screen                      ")
      print ("                                                                                  ")
      print ("************************************************************************************ ")


def check_path(path):
    if not(path.endswith("/")):
        path=path + "/"

    return path



#Script
if __name__ == "__main__":
    import argparse

    # Add command line arguments
    parser = argparse.ArgumentParser(description='FLL configure script')
    parser.add_argument('-i','--file',nargs=1,help='Files to process')
    parser.add_argument('-f','--format',nargs=1,help='Format of the file')
    parser.add_argument('-s','--scan',action='store_true',help='Scan file only',required=False)
    parser.add_argument('-D','--dir',action='store_true',help='Print DIR only',required=False)
    parser.add_argument('-e','--external_format',nargs=1,help='External format of the file')
    parser.add_argument('-c','--color',action='store_true',help='colored output',required=False)

    # Parse the command line arguments
    args = parser.parse_args()

    file = args.file[0]   if args.file else None
    format = args.format[0] if args.format else None
    eformat = args.external_format[0] if args.external_format else None
    scan = args.scan
    dir = args.dir
    color = args.color
 
    if not scan:
       scan = 'n'
    else:
       scan = 'Y'

    if not dir:
       dir = 'n'
    else:
       dir = 'Y'

    if not color:
       color = 'n'
    else:
       color = 'y'

    if not len(sys.argv) > 1:
        print("\nfll_cat - prints content of file on standard output\n")
        print("usage: fll_cat.py [-h] [-i FILE] [-f FORMAT] [-s] [-D] [-e EXTERNAL_FORMAT]\n")
        sys.exit()

    if not file:
        print ("\033[031mError: \033[039m missing name of file, option\033[031m -i \033[039m")
        sys.exit()

    if not format:
        print ("\033[031mError: \033[039m missing file format, option\033[031m -f \033[039m")
        print ("\033[031m       \033[039m available options are: \033[032m a - ASCII\033[039m")
        print ("\033[031m       \033[039m                        \033[032m b - binary format\033[039m")
        sys.exit()

    if not eformat:
        eformat = 'fll'
    else:
     if not('fll') or not('ffa'):
        print ("\033[031mError: \033[039m wrong file format, option\033[031m -e \033[039m")
        print ("\033[031m       \033[039m available options are: \033[032m fll - fll native format\033[039m")
        print ("\033[031m       \033[039m                        \033[032m ffa - ffa format\033[039m")
        sys.exit()

    run(file=file,fmt=format, efmt = eformat, scan=scan, dir=dir, color=color)
