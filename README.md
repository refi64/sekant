# sekant

sekant is a simple SELinux helper tool for building policies and debugging denials.

## Installation

```bash
$ sudo dnf copr enable refi64/sekant
$ sudo dnf install sekant
# On Silverblue:
$ sudo curl -Lo /etc/yum.repos.d/_copr:copr.fedorainfracloud.org:refi64:sekant.repo \
  https://copr.fedorainfracloud.org/coprs/refi64/sekant/repo/fedora-$(lsb_release -sr)/refi64-sekant-fedora-$(lsb_release -sr).repo
$ rpm-ostree install sekant
```

## Usage

```bash
# Clear the audit log.
$ sekant audit clear
# Quick wrapper over audit2why.
$ sekant audit explain

# Build all .te files in the current directory into .pp files.
$ sekant local build
# Build just mymodule1.te and mymodule2.te.
$ sekant local build mymodule1 mymodule2
# Build all .te files in the current directory, then install them.
# (Do NOT run as sudo, it will automatically escalate privileges when needed.)
$ sekant local install
# Build and install mymodule1.te and mymodule2.te.
$ sekant local install mymodule1 mymodule2
# Same as above, but just install, don't build.
$ sekant local install --nobuild mymodule1 mymodule2
# Uninstall all modules that are build in the current directory.
$ sekant local uninstall
# Uninstall just mymodule1 and mymodule2.
$ sekant local uninstall mymodule1 mymodule2
# Clean all built files.
$ sekant local clean
```
