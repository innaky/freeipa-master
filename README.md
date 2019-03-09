# Install FreeIPA with Common Lisp
Install FreeIPA on Centos 7 with Common Lisp

# Instructions

You need a computer with access to internet install git from repository and exec the install script. Then answer the questions and wait while installing FreeIPA.

```bash
yum -y install git
git clone https://github.com/innaky/install-freeipa-common-lisp.git
cd install-freeipa-common-lisp
./install-freeipa-common-lisp/install.sh
```
# TODO
- [ ] Reconfiguration network files if the user need change the IPv4.
- [ ] More functional match for bad IPv4
