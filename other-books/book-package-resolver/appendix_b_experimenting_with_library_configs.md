<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Appendix B: Experimenting with library configurations

Appendix A explained the machinery; this appendix is hands-on. The experiments
below let you *watch* the loader pick a library and then change its mind —
starting with ones that need no privilege at all, and ending with a whole
alternate library world built from an isolated `/etc`. They are the quickest way
to build intuition for why "which library satisfies this binary?" is a question
with so many moving answers — and, by the end, for why the resolver in this book
chooses to answer it *statically* instead.

> **Safety.** The privileged experiments (§B.4–§B.5) change how *every* program in
> a namespace or root finds its libraries. Do them in a throwaway VM or container,
> never on a machine you care about, and **never repoint the system's glibc**
> (`libc.so.6` / the loader) — see the caveat in §B.5. The unprivileged
> experiments (§B.1–§B.3) touch nothing outside a scratch directory.

The commands below were run on Ubuntu 22.04 (glibc 2.35, gcc 11.4); output is
shown as produced.

## B.1 A throwaway library to experiment with

Everything that follows uses one tiny program and a library that comes in two
versions carrying the *same soname* — the situation the whole book is about.

```c
/* greet.h */            const char *greet(void);
/* v1.c */               const char *greet(void){ return "greet v1"; }
/* v2.c */               const char *greet(void){ return "greet v2"; }
/* app.c */
#include <stdio.h>
#include "greet.h"
int main(void){ printf("app sees: %s\n", greet()); return 0; }
```

Build the app and two same-soname libraries in separate directories:

```bash
mkdir -p libv1 libv2
gcc -shared -fPIC -Wl,-soname,libgreet.so.1 -o libv1/libgreet.so.1 v1.c
gcc -shared -fPIC -Wl,-soname,libgreet.so.1 -o libv2/libgreet.so.1 v2.c
ln -s libgreet.so.1 libv1/libgreet.so        # linker name, for building app
gcc -o app app.c -Llibv1 -lgreet
```

Note the `-Wl,-soname,libgreet.so.1`: both files advertise the soname
`libgreet.so.1`, so to the loader they are interchangeable *candidates* — exactly
the ambiguity `ldconfig` and the search path resolve.

## B.2 Choosing a library with no privilege

### B.2.1 `LD_LIBRARY_PATH` — pick the directory

The binary records only the soname `libgreet.so.1`; which file satisfies it is
decided by search order, and `LD_LIBRARY_PATH` is searched before the cache:

```
$ LD_LIBRARY_PATH=$PWD/libv1 ./app
app sees: greet v1
$ LD_LIBRARY_PATH=$PWD/libv2 ./app
app sees: greet v2
```

Same binary, same soname — two different libraries, chosen entirely by the path.
This is the right tool for "run against a different build" (§A.5).

### B.2.2 `LD_PRELOAD` — interpose a single function

`LD_PRELOAD` force-loads a library *ahead* of all others, so its symbols win. It
is for **interposition** — overriding individual functions — not version
selection. A shim that intercepts `puts`:

```c
/* shim.c */
#include <stdio.h>
int puts(const char *s){
    fputs("[shim] intercepted: ", stdout);
    fputs(s, stdout); fputc('\n', stdout);
    return 0;
}
```

```
$ gcc -shared -fPIC -o libshim.so shim.c
$ ./prog
hello from the real program
$ LD_PRELOAD=$PWD/libshim.so ./prog
[shim] intercepted: hello from the real program
```

`/etc/ld.so.preload` (§B.4) is the file-based, system-wide version of this same
mechanism.

**Real-world: Termux.** The clearest production use of `LD_PRELOAD` interposition
is [Termux](https://github.com/termux/termux-exec), an *unprivileged* Linux
userland on Android that installs everything under a prefix (`$PREFIX =
/data/data/com.termux/files/usr`) with no root and no `chroot`. Its `termux-exec`
package is, in the repository's own words, "a execve() wrapper to fix problem with
shebangs," shipped as "a shared library that is meant to be preloaded with
`$LD_PRELOAD`." The problem is pure prefix-filesystem friction: a script beginning
`#!/bin/sh` or `#!/usr/bin/env python` names absolute paths that do not exist under
the prefix, so the shim interposes the `exec*` family and rewrites those
interpreter and program paths into `$PREFIX` before the call proceeds. What makes
it instructive is the *division of labour* — Android's bionic loader has no
`/etc/ld.so.cache`, so Termux finds its **libraries** via rpath into `$PREFIX/lib`
(§A.4) and uses `LD_PRELOAD` only to fix the **exec paths**. It is the prefix
pattern of §B.5, without privilege, shipped to millions of devices.

## B.3 Letting `ldconfig` manage the links — and stopping it

`ldconfig` owns the soname symlinks. You can run it on a scratch directory with
`-n` (process only that directory, don't touch the system cache) — no privilege
needed if you own the directory. Put *both* versions in one directory and point
the soname link at the older one by hand:

```
$ ls libmix/
libgreet.so.1.2.10   libgreet.so.1.2.99   libgreet.so.1 -> libgreet.so.1.2.10
$ ldconfig -n libmix
$ readlink libmix/libgreet.so.1
libgreet.so.1.2.99                 # ldconfig OVERWROTE the link to the most-recent
```

The default `ldconfig` repoints a soname link to the most-recent candidate *in
that directory* — even a valid link to an older version. Two ways to keep a
deliberate pin:

```
# -X: rebuild the cache but DON'T touch links -> your pin survives
$ ln -sf libgreet.so.1.2.10 libmix/libgreet.so.1
$ ldconfig -nX libmix
$ readlink libmix/libgreet.so.1
libgreet.so.1.2.10                 # preserved
```

Or point the link *outside* the scanned directory: with no same-soname file
present locally, `ldconfig` has nothing to upgrade to and leaves it alone — until
a same-soname file appears in the directory (e.g. a package update), at which
point the local most-recent wins and clobbers the link. (Both behaviours are
demonstrated in Appendix A's terms; the lesson is that hand-set links are a *soft*
pin, `-X` is a hard one.)

Build a private cache file without root using `-C`:

```bash
ldconfig -C "$PWD/mycache" -n libmix     # writes a cache you own
```

The catch from §A.6 still holds: the *running* loader reads its cache from the
compiled-in path (`/etc/ld.so.cache`), so a private `-C` cache only takes effect
once something makes the loader read *it* — which is what the next section
arranges.

## B.4 System-wide, file-based, in an isolated namespace

To make a change the loader actually honours without editing the real system, run
inside a **mount namespace** and bind your file over the real one. This needs
privilege but, unlike a full `chroot`, builds nothing — it just overlays one file.
The cleanest example is `/etc/ld.so.preload`, the file form of `LD_PRELOAD`:

```bash
printf '%s\n' "$PWD/libshim.so" > myreload
sudo unshare -m bash -c '
  mount --bind "'"$PWD"'/myreload" /etc/ld.so.preload
  ./prog                 # preloads libshim for EVERY program in this namespace,
'                        # with no LD_PRELOAD set — and nothing leaks to the host
```

Because the bind lives only in the namespace, the host's `/etc/ld.so.preload` is
untouched. This is also how `/etc/ld.so.preload` differs from the env var: it
applies even to programs that clear their environment, and (on a real system) even
to set-uid binaries, which ignore `LD_PRELOAD`.

## B.5 A whole alternate library world

The generalisation of §B.4 is the design worked out in Appendix A: because
`/etc/ld.so.cache` is an **absolute-path redirection table** the loader reads from
a fixed path, you can give a root (or a namespace) its *own* loader configuration
and leave everything else as the host. The loader config is exactly three files,
all under `/etc`:

- `/etc/ld.so.cache` — the soname → path table (the lever),
- `/etc/ld.so.conf` + `/etc/ld.so.conf.d/*.conf` — the extra search directories,
- `/etc/ld.so.preload` — the forced preloads.

So the recipe is: **bind the host filesystem in place for everything, supply your
own versions of just those `/etc` files, and make your custom library directory
present at the absolute path your cache names.** In a namespace that is a handful
of bind mounts:

```bash
# sketch — run in a throwaway VM, as root
sudo unshare -m bash <<'NS'
  # 1. your alternative libraries live in a directory of their own
  #    (say /opt/altlib), already populated with libgreet.so.1 -> v2
  # 2. build a cache that redirects the soname into /opt/altlib
  ldconfig -C /tmp/altcache -n /opt/altlib
  # 3. overlay only the loader config; the rest of the system is the host, in place
  mount --bind /tmp/altcache          /etc/ld.so.cache
  # (optionally also bind custom ld.so.conf / ld.so.preload here)
  # 4. anything run now resolves libgreet.so.1 via the custom cache
  ./app
NS
```

A `chroot` is the heavier form of the same idea: give the new root its own `/etc`
(holding the three files) and bind the host's `/usr`, `/bin`, `/lib`, … in place.
Either way, **the chroot's uniqueness, for library purposes, lives in `/etc`** —
that one folder holds the whole loader configuration.

### A test prefix: symlink most, override a few

A natural shape for this is a **test prefix** — a self-contained root (a directory,
or an image on a USB stick, as a Puppy Linux SFS layer would be) that **symlinks
the libraries it needs from the main system and provides only the specific ones
under test as real files** in the prefix. Rebuild the prefix's cache with
`ldconfig -r <prefix>`, and the soname of a test library resolves to the local
copy while every other soname follows its symlink back out to the host's.

Two facts from Appendix A make this hold together:

- `ldconfig` leaves an **outside-pointing** soname link alone as long as no
  same-soname file sits beside it in the prefix (Appendix A, "Case A") — so the
  "symlink to the host" entries survive a cache rebuild, while each
  locally-provided test library is the one override that wins.
- One subtlety for a true `chroot`: after `chroot`, an **absolute** symlink target
  such as `/lib/.../libfoo.so.1` resolves *inside the new root*, not on the host.
  So the host's library directories must actually be reachable under the prefix —
  bind them in place — for those symlinks to land on real files.

The **overlay** form skips the symlinks entirely, and is what Puppy Linux actually
does: a read-only base layer (the system's libraries — e.g. an SFS on the USB
image) with a writable upper layer (your libraries under test), merged by
`overlayfs`/`aufs`. The upper layer *shadows* the specific files you are testing;
everything else shows through from the base. Rebuild the cache over the merged
view and you get the same result as the symlink prefix with less bookkeeping — and
it is why a live USB system can carry a whole experimental library set as one image
without rewriting the base at all.

### A prefix is a language-agnostic virtual environment

Seen this way, a prefix is the general case of a tool many developers already know:
the **virtual environment**. A Python `venv` isolates exactly one layer — Python
packages — through `sys.path` and a wrapper, and does nothing for native libraries,
other interpreters, or command-line tools. A prefix isolates at the loader and
filesystem level, so it captures *everything* an application uses. That matters
because real applications are rarely one language: a UnifyWeaver tool like
[`plawk`](https://github.com/s243a/UnifyWeaver/tree/main/examples/plawk) — an
awk-like surface compiled through UnifyWeaver's WAM→LLVM target to a **native
binary** — is precisely the kind of artifact a prefix suits and a `venv` cannot
hold: what needs isolating is a compiled executable and its shared-library
dependencies, not a set of Python packages. (It is also, not coincidentally,
exactly the kind of binary this book's resolver reasons about — you could *confirm*
a compatibility verdict by building the prefix the verdict describes and running
the binary in it.)

The two compose rather than compete. A prefix can perfectly well *contain* a `venv`
as its Python layer — the prefix supplies the system and native libraries, a `venv`
inside it supplies the Python packages. The one wrinkle is that a `venv` is not
relocatable: its `pyvenv.cfg` records the base interpreter's absolute path and its
scripts carry absolute shebangs, so inside a `chroot` the venv must sit at the same
absolute path it was created at (with its base interpreter present), or the shebangs
need rewriting — the very `exec`-path problem `termux-exec` solves (§B.2.2).

> **The glibc exception, again.** All of this works for *ordinary* libraries. It
> does **not** let you swap glibc: `libc.so.6` and the loader (`ld-linux-*.so`) are
> the same project and must match, so redirecting `libc.so.6` to a different glibc
> breaks the loader itself. A different glibc needs a matching loader in the root —
> a real sysroot — not just a cache swap.

*(§B.1–§B.3 were run as shown on glibc 2.35; §B.4–§B.5 are the privileged
generalisation of the same, verified mechanics — run them in a throwaway
environment.)*

## B.6 Why the book reasons statically instead

Step back and count what you just manipulated to answer one question — "which
library will this program actually use?": a search-path variable, a preload
variable, two files' worth of symlinks, three `/etc` config files, a binary cache,
and a namespace or root to contain it. Every one is a place the answer can change,
and most are machine- and moment-specific.

That is the *dynamic* way to find out whether a binary runs against a given set of
libraries: construct the world, run the program, watch for `undefined symbol`. The
resolver in this book answers the same question without constructing anything — it
reasons from the symbol-and-node evidence (Chapters 3–5) to a floor, a range, and a
verdict that carries its reasons, for every release at once. The experiments here
are worth doing precisely because they show how much runtime machinery the static
answer lets you *not* touch.
