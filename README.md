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
which basically means in the obarray that's associated with each codex.

The core of the implementation is the `in-codex` macro, which rewrites its
body's symbols to point at the right places.
In particular, a symbol name of the form "<codex>:<symbol>" will be resolved
as the symbol "symbol" in codex "codex", which is *not* backward-compatible.

Why Codex ?
===========

Well, this is modeled after CL packages, but packages are something entirely
different in Emacs. I thought of "thesaurus" first, but I can't be bothered to
type this all the time. "lexicon" sounded nice, but this might be confusing
with all the lexical binding stuff that's happening right now.
"codex" is short and still refers to a collection of symbols :)

Existing codices
================

One of the challenges of introducing a proper namespacing mechanism is to
decide what to do with all the existing stuff in the global obarray.

Right now, the thing that's done is to generate an "emacs" namespace, that
contains every built-in function defined in emacs. That's certainly not
sufficient, but is enough for a small POC.

If this is deemed worth, one could plug into `after-load-functions`, and parse
`load-history` to generate codices from existing library, so that they could
at least be properly integrated.

Examples explained
==================

For clarity, we'll use the colon notation to qualify the symbols, but that's
obviously not what one would observe, as the symbol names don't contain
that qualification.

    (defcodex test
      (:use emacs)
      (:export "plop"))
    
    (in-codex test
      (defun plop () 42))

The defun form will be transformed into:

    (emacs:defun test:plop nil 42)

That means, the `defun` symbol has been detected as exported by the "emacs"
codex, while `plop` was (probably) not even existing, so that it ends up in
the "test" one.

If we didn't have the `(:use emacs)` clause in the codex definition, the
expansion would have been

    (test:defun test:plop nil 42)

This allows to easily overload functions in codex (note that when proper
shadowing is in place, it will actually become useful).

Another example:

    (defcodex test2)
    
    (in-codex test2
      (emacs:defun plop () 7))

This time, no use of emacs codex, so we need to prefix the `defun` symbol to
refer to it. The defun form is transformed into:

    (emacs:defun test:plop nil 7)

Finally:

    (defcodex test3
      (:use test))
    
    (in-codex test3
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
function or value slots of their codex counterparts. It's just that
they exist, in a (mostly) harmless state.

Debugging code that's using those codices would quickly become a nightmare in
the absence of proper edebug integration. In particular, the fact that symbols
are not prefixed makes it very difficult to know what symbol we're
talking about.
