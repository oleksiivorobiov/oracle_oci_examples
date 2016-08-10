#!/bin/sh
#
# $Header: ulcase.sh 06-sep-2007.13:54:17 jstenois Exp $
#
# ulcase.sh
#
# Copyright (c) 1999, 2007, Oracle. All rights reserved.  
#
#    NAME
#      ulcase.sh - run sqlldr demos
#
#    DESCRIPTION
#      Shell script to run sqlldr demos.
#      Please append to script as more ulcase* demos are added.
#
#    MODIFIED   (MM/DD/YY)
#    jstenois    09/06/07 - remove password from demo script
#    krich       01/26/04 - adding case study 11 
#    cmlim       07/20/99 - create shell script for running all sqlldr demos
#    cmlim       07/20/99 - Creation
#

echo "  "
echo "- This script runs through all of the SQL Loader demos."
echo "- It uses SQLPlus to create the necessary objects and then"
echo "- calls SQL Loader to load data.  This script uses schema"
echo "- SCOTT for all of the tests.  Whenever the script executes"
echo "- SQLPlus or SQL Loader utilities, it will prompt you for"
echo "- password of the SCOTT user.  You will need to enter this"
echo "- password every time for the script to continue.  The"
echo "- default password for user SCOTT is TIGER."
echo "  "

# CASE1
echo "  "
echo "  Starting case 1"
echo "  Calling SQL Plus to do setup for case 1"
sqlplus scott @ulcase1
echo "  Calling SQL Loader to load data for case 1"
sqlldr scott ulcase1.ctl

# CASE2
echo "  "
echo "  Starting case 2"
echo "  Calling SQL Loader to load data for case 2"
sqlldr scott ulcase2

# CASE3
echo "  "
echo "  Starting case 3"
echo "  Calling SQL Plus to do setup for case 3"
sqlplus scott @ulcase3
echo "  Calling SQL Loader to load data for case 3"
sqlldr scott ulcase3

# CASE4
echo "  "
echo "  Starting case 4"
echo "  Calling SQL Plus to do setup for case 4"
sqlplus scott @ulcase4
echo "  Calling SQL Loader to load data for case 4"
sqlldr scott ulcase4

# CASE5
echo "  "
echo "  Starting case 5"
echo "  Calling SQL Plus to do setup for case 5"
sqlplus scott @ulcase5
echo "  Calling SQL Loader to load data for case 5"
sqlldr scott ulcase5

# CASE6
echo "  "
echo "  Starting case 6"
echo "  Calling SQL Plus to do setup for case 6"
sqlplus scott @ulcase6
echo "  Calling SQL Loader to load data for case 6"
sqlldr scott ulcase6 direct=true

# CASE7
echo "  "
echo "  Starting case 7"
echo "  Calling SQL Plus to do setup for case 7"
sqlplus scott @ulcase7s
echo "  Calling SQL Loader to load data for case 7"
sqlldr scott ulcase7 
echo "  Calling SQL Plus to do cleanup for case 7"
sqlplus scott @ulcase7e

# CASE8
echo "  "
echo "  Starting case 8"
echo "  Calling SQL Plus to do setup for case 8"
sqlplus scott @ulcase8
echo "  Calling SQL Loader to load data for case 8"
sqlldr scott ulcase8 

# CASE9
echo "  "
echo "  Starting case 9"
echo "  Calling SQL Plus to do setup for case 9"
sqlplus scott @ulcase9
echo "  Calling SQL Loader to load data for case 9"
sqlldr scott ulcase9

# CASE10 
echo "  "
echo "  Starting case 10"
echo "  Calling SQL Plus to do setup for case 10"
sqlplus scott @ulcase10
echo "  Calling SQL Loader to load data for case 10"
sqlldr scott ulcase10

# CASE11
echo "  "
echo "  Starting case 11"
echo "  Calling SQL Plus to do setup for case 11"
sqlplus scott @ulcase11
echo "  Calling SQL Loader to load data for case 11"
sqlldr scott ulcase11


