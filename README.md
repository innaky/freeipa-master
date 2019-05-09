# Install FreeIPA with Common Lisp
Install FreeIPA on Centos 7 with Common Lisp

# Instructions

You need a computer with access to internet and root privileges.
The exported function is `freeipa-master:main`

## Dependencies

* Installing SBCL

```bash
yum -y install git curl wget bzip2
wget -c http://prdownloads.sourceforge.net/sbcl/sbcl-1.4.15-x86-64-linux-binary.tar.bz2
tar xjvf sbcl-1.4.15-x86-64-linux-binary.tar.bz2
cd sbcl-1.4.15-x86-64-linux
./install.sh
cd ..
rm -rf sbcl-1.4.15-x86-64-linux
```

* Installing quicklisp

```bash
cd $HOME
curl -O https://beta.quicklisp.org/quicklisp.lisp
```

```bash
sbcl --load quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)' \
     --eval '(ql:quickload "quicklisp-slime-helper")' \
     --eval '(ql:add-to-init-file)' \
     --eval '(quit)'
```

## Installation freeipa-master

```bash
cd ${HOME}/quicklisp/local-projects
git clone https://github.com/innaky/freeipa-master.git
```

Execute SBCL and run

```lisp
(ql:quickload "freeipa-master")
(freeipa-master:main)
(quit)
```

## Run automatic freeipa-master installation

```bash
cd freeipa-master
./install.sh
```

# What is FreeIPA?
FreeIPA is an integrated security information management solution
combining Linux (Fedora), 389 Directory Server, MIT Kerberos, NTP,
DNS, Dogtag (Certificate System). It consists of a web interface
and command-line administration tools.

FreeIPA is an integrated Identity and Authentication solution
for Linux/UNIX networked environments. A FreeIPA server provides
centralized authentication, authorization and account information
by storing data about user, groups, hosts and other objects
necessary to manage the security aspects of a network of computers.

FreeIPA is built on top of well known Open Source components and standard
protocols with a very strong focus on ease of management and automation
of installation and configuration tasks.

Multiple FreeIPA servers can easily be configured in a FreeIPA
Domain in order to provide redundancy and scalability. The 389 Directory
Server is the main data store and provides a full multi-master LDAPv3
directory infrastructure. Single-Sign-on authentication is provided
via the MIT Kerberos KDC. Authentication capabilities are augmented by an
integrated Certificate Authority based on the Dogtag project.
Optionally Domain Names can be managed using the integrated ISC Bind server.

Security aspects related to access control, delegation of administration
tasks and other network administration tasks can be fully centralized
and managed via the Web UI or the ipa Command Line tool.
(https://www.freeipa.org/page/About).


# TODO
- [ ] Reconfiguration network files if the user need change the IPv4.
- [ ] More functional match for bad IPv4
