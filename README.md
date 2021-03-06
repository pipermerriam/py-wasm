# py-wasm

[![Join the chat at https://gitter.im/ethereum/py-wasm](https://badges.gitter.im/ethereum/py-wasm.svg)](https://gitter.im/ethereum/py-wasm?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://circleci.com/gh/ethereum/py-wasm.svg?style=shield)](https://circleci.com/gh/ethereum/py-wasm)
[![PyPI version](https://badge.fury.io/py/py-wasm.svg)](https://badge.fury.io/py/py-wasm)
[![Python versions](https://img.shields.io/pypi/pyversions/py-wasm.svg)](https://pypi.python.org/pypi/py-wasm)
[![Docs build](https://readthedocs.org/projects/py-wasm/badge/?version=latest)](http://py-wasm.readthedocs.io/en/latest/?badge=latest)
   

A python implementation of the web assembly interpreter

Read more in the [documentation on ReadTheDocs](https://py-wasm.readthedocs.io/). [View the change log](https://py-wasm.readthedocs.io/en/latest/releases.html).

## Quickstart

```sh
pip install py-wasm
```

## Developer Setup

If you would like to hack on py-wasm, please check out the
[Ethereum Development Tactical Manual](https://github.com/pipermerriam/ethereum-dev-tactical-manual)
for information on how we do:

- Testing
- Pull Requests
- Code Style
- Documentation

### Development Environment Setup

You can set up your dev environment with:

```sh
git clone git@github.com:ethereum/py-wasm.git
cd py-wasm
virtualenv -p python3 venv
. venv/bin/activate
pip install -e .[dev]
```

### Testing Setup

During development, you might like to have tests run on every file save.

Show flake8 errors on file change:

```sh
# Test flake8
when-changed -v -s -r -1 wasm/ tests/ -c "clear; flake8 wasm tests && echo 'flake8 success' || echo 'error'"
```

Run multi-process tests in one command, but without color:

```sh
# in the project root:
pytest --numprocesses=4 --looponfail --maxfail=1
# the same thing, succinctly:
pytest -n 4 -f --maxfail=1
```

Run in one thread, with color and desktop notifications:

```sh
cd venv
ptw --onfail "notify-send -t 5000 'Test failure ⚠⚠⚠⚠⚠' 'python 3 test on py-wasm failed'" ../tests ../wasm
```

### Release setup

For Debian-like systems:
```
apt install pandoc
```

To release a new version:

```sh
make release bump=$$VERSION_PART_TO_BUMP$$
```

#### How to bumpversion

The version format for this repo is `{major}.{minor}.{patch}` for stable, and
`{major}.{minor}.{patch}-{stage}.{devnum}` for unstable (`stage` can be alpha or beta).

To issue the next version in line, specify which part to bump,
like `make release bump=minor` or `make release bump=devnum`. This is typically done from the
master branch, except when releasing a beta (in which case the beta is released from master,
and the previous stable branch is released from said branch). To include changes made with each
release, update "docs/releases.rst" with the changes, and apply commit directly to master 
before release.

If you are in a beta version, `make release bump=stage` will switch to a stable.

To issue an unstable version when the current version is stable, specify the
new version explicitly, like `make release bump="--new-version 4.0.0-alpha.1 devnum"`


# Legacy README from `pywebassembly.py`

PyWebAssembly closely follows [WebAssembly Specification, Release 1.0](https://webassembly.github.io/spec/core/_download/WebAssembly.pdf) (pdf), implementing necessary parts of chapters 2, 3, 4, 5, and 7. Each section of pywebassembly.py references its definition in the Spec document, and follows the same order as the Spec document.

Closely following the linked Spec document is useful for the following reasons.
 - The Spec document can be used as a user's manual.
 - PyWebAssembly can be audited alongside the Spec document.
 - When Wasm 1.1 is released, PyWebAssembly can be updated alongside the Spec document.
 - PyWebAssembly does not introduce invariants or design decisions that are not in the Spec document. There are many subtleties in the 150 page Spec document, and invariants may be difficult to maintain, as I have learned. So it may be naive to over-engineer something beyond the spec.
 - Implementing the Spec document has allowed me to find errors and submit fixes to the Spec document, and I have more fixes coming.

PyWebAssembly is also structured for my personal economy-of-thought as it is being developed and studied for errors.

**API**: It may be possible to limit the API to functions defined in the WebAssembly Spec section 7.1 Embedding. These functions are implemented in section "7.1" of pywebassembly.py, but please reference the spec for details. The only awkward part is that `invoke_func` requires specifying `i32.const`, `i64.const`, `f32.const`, or `f64.const` with each argument -- we are considering deviating from the spec and relaxing this requirement.

The following code "spins-up" a VM instance, instantiates a module, and invokes an exported function. See the `examples` directory for more examples.


```
# python3

import pywebassembly as wasm

file_ = open('examples/fibonacci.wasm', 'rb')
bytestar = memoryview(file_.read())			#can also use bytearray or bytes instead of memoryview
module = wasm.decode_module(bytestar)			#get module as abstract syntax
store = wasm.init_store()				#do this once for each VM instance
externvalstar = []					#imports, none for fibonacci.wasm
store,moduleinst,ret = wasm.instantiate_module(store,module,externvalstar)
externval = wasm.get_export(moduleinst, "fib")		#we want to call the function "fib"
funcaddr = externval[1]					#the address of the funcinst for "fib"
args = [["i32.const",10]]				#list of arguments, one arg in our case
store,ret = wasm.invoke_func(store,funcaddr,args)	#finally, invoke the function
print(ret)						#list [89] of return values, limitted to one value in Wasm 1.0
```




TODO:
 * Clean-up the code, make it aesthetically pleasing, more maintainable.
 * Refactor floating point values to use ctypes float32 and float64, then pass remaining floating point tests.
 * Support text format as described in chapter 6.


## examples/

Example uses of PyWebAssembly.


## tests/

Testing of PyWebAssembly.


# Notes and Conventions.

Both "PyWebAssembly" and "pywebassembly" can be used to as the name of this project.
