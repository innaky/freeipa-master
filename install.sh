#!/bin/bash

yum -y install curl wget bzip2 git
curl -O https://beta.quicklisp.org/quicklisp.lisp
wget -c http://prdownloads.sourceforge.net/sbcl/sbcl-1.4.15-x86-64-linux-binary.tar.bz2
tar xjvf sbcl-1.4.15-x86-64-linux-binary.tar.bz2
cd sbcl-1.4.15-x86-64-linux
./install.sh
cd

sbcl --load quicklisp.lisp
(quicklisp-quickstart:install)
(ql:quickload "quicklisp-slime-helper")
(ql:add-to-init-file)
(quit)
