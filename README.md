WARNING: don't even think about asking for support :)
This is just a POC, and is not meant to be actually used in the current state.

That said, I'm more than happy to discuss how we could make it happen (in
particular, suggestions and patches welcome)

Elisp namespaces
================

This is an attempt at namespacing Elisp symbols.

Contrary to other implementations (like
https://github.com/skeeto/elisp-fakespace), we don't constrain ourselves to the
main obarray. We instead try to create the symbols at the right place directly,
which basically means in the obarray that's associated with each package.

The core of the implementation is the `in-package` macro, which rewrites its
body's symbols to point at the right places.
In particular, a symbol name of the form "<package>:<symbol>" will be resolved
as the symbol "symbol" in package "package", which is *not* backward-compatible.

Existing packages
=================

One of the challenges of introducing a proper namespacing mechanism is to
decide what to do with all the existing stuff in the global obarray.

Right now, the thing that's done is to generate an "emacs" namespace, that
contains every built-in function defined in emacs. That's certainly not
sufficient, but is enough for a small POC.

If this is deemed worth, one could plug into `after-load-functions`, and parse
`load-history` to generate packages from existing library, so that they could
at least be properly integrated.

Examples explained
==================

For clarity, we'll use the colon notation to qualify the symbols, but that's
obviously not what one would observe, as the symbol names don't contain
that qualification.

    (epack-defpackage test
      (:use emacs)
      (:export "plop"))
    
    (epack-in-package test
      (defun plop () 42))

The defun form will be transformed into:

    (emacs:defun test:plop nil 42)

That means, the `defun` symbol has been detected as exported by the "emacs"
package, while `plop` was (probably) not even existing, so that it ends up in
the "test" one.

If we didn't have the `(:use emacs)` clause in the package definition, the
expansion would have been

    (test:defun test:plop nil 42)

This allows to easily overload functions in packages (note that when proper
shadowing is in place, it will actually become useful).

Another example:

    (epack-defpackage test2)
    
    (epack-in-package test2
      (emacs:defun plop () 7))

This time, no use of emacs package, so we need to prefix the `defun` symbol to
refer to it. The defun form is transformed into:

    (emacs:defun test:plop nil 7)

Finally:

    (epack-defpackage test3
      (:use test))
    
    (epack-in-package test3
      (emacs:+ (plop) (test2:plop)))

The addition form becomes:

    (emacs:+ (test:plop) (test2:plop))

which returns 49 as expected ;)

Gotchas
=======

Implementing that kind of logic using a macro has its own cost. Namely, we work
on something that has already gone through the `read` phase, meaning that all
referenced symbols are interned in the global obarray anyway. That's unclean.

To be clear, that doesn't mean the symbols in the main obarray have the
function or value slots of their packages counterparts. It's just that
they exist, in a (mostly) harmless state.

Debugging code that's using those packages would quickly become a nightmare in
the absence of proper edebug integration. In particular, the fact that symbols
are not prefixed makes it very difficult to know what symbol we're
talking about.
