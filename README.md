# emacs-lisp-container

> 😶 Avoid side-effects when executing Emacs Lisp

**_emacs-lisp-container_** is a simple attempt to avoid side-effects
when executing an Emacs Lisp body.  i.e. it won’t modify the
environment.

While there are probably better alternatives for executing an Emacs
subprocess (like the
excellent [emacs-sync](https://github.com/jwiegley/emacs-async)
library), the solutions I found were not able to handle an already
existing Emacs Environment.  It simply creates a brand new emacs
process and cannot take into account the current run-time environment.

## Installation

No way to install it right now.

## Getting Started

No way to get started right now.

## License

This module and the whole directory is subject to
the [GPLv3 license](LICENSE) or late.  Emacs Packages (unless only
using an API) are subject to the GPLv3 license.

Copyright (c) 2017 Free Software Foundation

```
