#!/bin/python3

import os
import glob
import subprocess

DST = "src/Semantics/Builtins.hs"
DIR = "builtins/"
PRE = """
module Semantics.Builtins where

import Syntax.Raw.Abs

builtins :: [Assignment]
builtins =
"""

os.system("stack build hopfl:builtin-exe")
os.system("rm " + DST)
builtins = glob.glob(DIR + "*.ghopfl")

result = PRE

for b in builtins:
    ast = subprocess.run(
        ["stack", "exec", "builtin-exe", "--", "-i"] + [b], 
        capture_output=True
    ).stdout.decode("utf-8").replace("\\\"", "\"")[1:-2]
    
    print (ast)
    name = os.path.basename(b).split(".")[0] 
    result += "    " + ast
    if b != builtins[-1]:
        result += " ++\n    "

hs_file = open(DST, "w")
hs_file.write(result)

