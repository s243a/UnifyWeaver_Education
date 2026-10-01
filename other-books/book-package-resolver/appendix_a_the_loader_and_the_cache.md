<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Appendix A: How the loader finds a library (and why we reason statically)

Chapter 2 said the loader matches a binary's `DT_NEEDED` sonames *by exact
string*, and left it there — because for deciding a *verdict* that is all you need.
This appendix fills in the mechanism the main line skips: how a soname string
becomes an actual file on disk, which tools build that mapping, how you override
it, and — the part that pays the book back — why, given all this machinery, we
answer compatibility questions by *reasoning* rather than by running the program
and watching what happens.

None of this is required to use the resolver. Read it if "the loader finds the
library" has always been a bit of a hand-wave.

## A.1 Three tools, three jobs

The names blur together, so pin them down first:

- **`ldd`** *reports*. Given a binary, it tells you which shared libraries would
  be loaded and from where. It creates nothing and decides nothing; it is a
  diagnostic that asks the loader what it *would* do.
- **`ldconfig`** *builds the index*. It scans the library directories, records a
  soname-to-path mapping in a cache, and creates the soname symlinks (§A.3). You
  run it after installing or removing a library.
- **`ld.so`** (the dynamic loader itself — `/lib64/ld-linux-x86-64.so.2`)
  *resolves at runtime*. When a program starts, it reads the program's
  `DT_NEEDED` list and finds a real file for each soname, consulting the cache
  `ldconfig` built.

So `ldd` is the question, `ldconfig` is the bookkeeping, and `ld.so` is the thing
that actually does the work when you run the program.

## A.2 The three names a library has

A single shared library is known by up to three names, and keeping them apart
dissolves most of the confusion:

| Name | Example | Where it lives / who makes it |
|------|---------|-------------------------------|
| **linker name** | `libselinux.so` | the `-dev` package; used at *link* time (`-lselinux`) |
| **soname** | `libselinux.so.1` | embedded in the binary's `DT_NEEDED`; the library's own `DT_SONAME` |
| **real name** | `libfoo.so.1.2.3` | the actual file on disk |

The soname is the only one the running binary records. The classic on-disk layout
is a symlink chain: the real file `libfoo.so.1.2.3`, with `libfoo.so.1` (the
soname) symlinked to it, so a program asking for `libfoo.so.1` lands on the real
file. `ldconfig` creates that soname symlink.

But the "soname is a symlink to a longer versioned file" rule is a *convention*,
not a law, and two of the libraries this book uses do not follow it. On Ubuntu
22.04 (glibc 2.35, the machine the running examples come from):

```
-rwxr-xr-x  /lib/x86_64-linux-gnu/libc.so.6          # a real 2.2 MB file, not a symlink
-rw-r--r--  /lib/x86_64-linux-gnu/libselinux.so.1    # also a real file
```

`libc.so.6` is the real shared object itself. The old `libc.so.6 -> libc-2.31.so`
symlink was the pre-**glibc 2.34** layout; the 2.34 merge (which folded
`libpthread`, `libdl`, and others into `libc`) changed it, and from then on
`libc.so.6` *is* the file. The lesson for the model: never infer a library's
version from its filename — the filename's digits are packaging, and they are
present or absent inconsistently.

## A.3 The cache `ldconfig` builds

`ldconfig` does not go by filename text. For every shared object in a scanned
directory it reads the ELF `DT_SONAME` field and groups files by that soname. When
several files declare the same soname, it picks the **highest version** — a
version-aware comparison of the suffix, so `.1.10` beats `.1.9` — points the
soname symlink at that file, and records the mapping in a binary cache,
`/etc/ld.so.cache`.

The directories it scans are the trusted defaults (`/lib`, `/usr/lib`, and the
multiarch dirs like `/lib/x86_64-linux-gnu`) plus everything named in
`/etc/ld.so.conf` and `/etc/ld.so.conf.d/*.conf`. On the example machine the cache
holds 1243 sonames, and the entries carry an ABI/arch tag so the 64- and 32-bit
builds of the same soname stay distinct:

```
libc.so.6 (libc6,x86-64, OS ABI: Linux 3.2.0) => /lib/x86_64-linux-gnu/libc.so.6
libc.so.6 (libc6, OS ABI: Linux 3.2.0)        => /lib32/libc.so.6
```

At program start the loader `mmap`s this cache and resolves each `DT_NEEDED`
soname through it, which is far cheaper than walking directories every time. The
soname symlink in the directory is really a convenience and fallback; the cache is
what the loader consults first.

## A.4 Runtime resolution order

For an ordinary (non-setuid) binary the loader looks for each soname in this order:

1. `DT_RPATH` baked into the binary (legacy, only if no `DT_RUNPATH`),
2. the **`LD_LIBRARY_PATH`** environment variable,
3. `DT_RUNPATH` baked into the binary,
4. the **cache** (`/etc/ld.so.cache`),
5. the trusted default directories.

Two safety notes: for **setuid/setgid** binaries the loader ignores
`LD_LIBRARY_PATH` and `LD_PRELOAD` entirely, and it only matches by the soname
string — a nearby version number never counts as "close enough" (the exact-match
rule of Chapter 2, now seen from the loader's side).

## A.5 Using a different — or older — library

Because the binary records only the *soname*, never a specific minor version,
"which file satisfies `libfoo.so.1`" is decided by **search order**, not by the
program. The right tools, in order of preference:

- **`LD_LIBRARY_PATH`** — put the directory holding your older `libfoo.so.1`
  first; it is searched before the cache, so your copy wins without touching
  anything system-wide.
- **rpath / runpath** (`-Wl,-rpath`) — bake a search path into the binary at link
  time.
- **`LD_PRELOAD`** is *not* the tool for this. It force-loads a specific file for
  **symbol interposition** (overriding individual functions); using it to swap a
  whole library version is fragile and easily leaves you with a mismatched mix.

And the catch that makes this book necessary: swapping in an older file only works
if that file still **exports the version nodes the binary needs**. Point a binary
that wants `fopen@GLIBC_2.34` at a library that offers only `fopen@GLIBC_2.17` and
you get

```
undefined symbol: fopen, version GLIBC_2.34
```

— the crash from Chapter 1. The soname gets you *a* file; the **version node**
decides whether that file actually satisfies you.

## A.6 A different library world entirely

The loader has the cache path (`/etc/ld.so.cache`) **compiled in**; there is no
environment variable that points it at a different cache file. So to make the
loader consult a *different* cache, you change what lives at that fixed path — and
that is exactly what **`chroot`**, a **container**, a **mount namespace**, or a
bind-mount does: it presents a whole different `/etc/ld.so.cache` and library tree
at the paths the loader already looks in. `chroot` is the classic form; containers
and user namespaces (Docker, podman, `bwrap`) are the modern, lighter-weight
versions of the same idea.

To *build* a cache for another root without entering it, `ldconfig -r <root>`
treats `<root>` as `/`, and `ldconfig -C <file>` writes to an alternate cache file
— but remember the running loader still will not *read* an arbitrary `-C` file;
that is for provisioning an image you will later `chroot` into or boot. To sidestep
the system cache for a single run, invoke an **alternate interpreter**:
`/path/to/ld-linux-x86-64.so.2 --library-path <dirs> ./prog`, or rewrite the
binary's interpreter and rpath with `patchelf`.

One library resists all of this: **glibc**. `libc.so.6` is bound tightly to the
dynamic loader itself (they are the same project), so you cannot safely downgrade
glibc per process with `LD_LIBRARY_PATH` or `LD_PRELOAD` — a loader-versus-libc
mismatch breaks badly. For glibc specifically the honest options are a container,
`patchelf --set-interpreter`, or a Nix-style wrapper.

## A.7 Why the book reasons statically

Step back and look at what every technique above has in common. To answer "will
this binary run against *that* set of libraries?" dynamically, you have to
*construct the world* — set `LD_LIBRARY_PATH`, or build a `chroot`, or spin up a
container with the release you are curious about — then run the program and watch
whether it reaches an `undefined symbol` abort. That is one experiment per release,
and for glibc you cannot even build the experiment cheaply.

The resolver in this book answers the same question without constructing any of
it. "Does release R satisfy this binary?" becomes a query over the symbol-and-node
evidence (Chapters 3–5): the floor, the compatible range, and a verdict that
carries its reasons — computed from data, for every release at once, with no cache
to swap and no container to boot. The loader's runtime machinery is what we are
*modelling*; the static resolver is what lets us reason about it ahead of time, and
— for the one library you cannot safely swap — it is the only cheap answer there is.
