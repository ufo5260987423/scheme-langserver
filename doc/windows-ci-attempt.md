# Windows CI Build Attempt Log

> Date: 2026-06-04  
> Conclusion: **Abandoned**. `compile-whole-program` output in Chez Scheme 10.x contains platform-dependent fasl objects, so the "Linux generates C → Windows links" two-stage strategy is infeasible.

---

## 1. Initial Strategy

We adopted a **two-stage build**:

1. **Linux stage**: Run `compile-chez-program` on an Ubuntu runner to produce `run.generated.c` (the tool claims this contains only platform-independent VM bytecodes).
2. **Windows stage**: On a Windows runner, use MSVC `cl.exe` to link `run.generated.c` against the `chez-exe` static library (`petite-chez.lib`) to produce `run.exe`.

Rationale for this approach:
- **Akku has no Windows port**: The package manager only runs on Linux/macOS.
- **SRFI filename colon problem**: Windows filesystems do not support `:` in filenames (e.g. `srfi/:1/lists.sls`). A large portion of `.akku/lib/` files would be impossible to extract on Windows.
- The `prepare-windows-build.sh` README claimed that `run.generated.c` "contains only Chez Scheme virtual-machine bytecodes (which are platform-independent)".

---

## 2. Issues Encountered and Fixes

### 2.1 Chez Scheme 10.4.1 Windows install layout changed

Older Chez Scheme versions (e.g. 10.0.0) placed boot files under `lib\csv10.0.0\ta6nt\`. In 10.4.1 the layout is:

```
C:\Program Files\Chez Scheme 10.4.1\
├── bin\ta6nt\          ← scheme.exe, csv1041.dll, csv1041.lib
├── boot\ta6nt\         ← scheme.h, petite.boot, scheme.boot, csv1041md.lib, csv1041mt.lib
└── examples\
```

**Fix**: After installation, copy the tree to `C:\ChezScheme` (path without spaces) and set `bootpath` to `C:\ChezScheme\boot\ta6nt`.

### 2.2 `nmake` defaults to the Unix `Makefile`

The `gwatt/chez-exe` repo contains both a Unix `Makefile` and a `Makefile.win`. `nmake` defaults to `Makefile`, which uses Unix syntax and fails with `U1034: syntax error`.

**Fix**: Explicitly pass `nmake /f Makefile.win`.

### 2.3 `gen-config.ss` unconditionally calls `(system "make")`

At the end of `gen-config.ss` it unconditionally executes `(system "make")`, which is unpredictable on a Windows runner.

**Fix**: Skip `gen-config.ss` entirely and hand-write `tools.ini`:

```ini
[NMAKE]
scheme = C:\ChezScheme\bin\ta6nt\scheme.exe
bootpath = C:\ChezScheme\boot\ta6nt
installbindir = C:\ChezScheme\bin
installlibdir = C:\ChezScheme\lib
```

### 2.4 `Makefile.win` defaults to `/MD` (DLL C runtime)

The `.c.obj` rule in `Makefile.win` hardcodes `/MD`, and it links against `csv*md.lib` (multi-threaded DLL runtime).

**Fix**: Patch the file to use `/MT` and the `*mt.lib` variants:

```powershell
(Get-Content "Makefile.win") `
  -replace '/MD ', '/MT ' `
  -replace 'csv\*md\.lib', 'csv*mt.lib' |
  Set-Content "Makefile.win"
```

### 2.5 Missing static-library dependencies (zlib / LZ4)

`csv1041mt.lib` (the Chez Scheme static kernel library) **does not embed** zlib or LZ4 code. Linking `run.exe` produced 25 unresolved external symbol errors:

```
compress2, compressBound, uncompress        (zlib)
gzdopen, gzread, gzwrite, ...               (zlib gzip)
LZ4_compress_default, LZ4_decompress_safe   (LZ4)
```

**Fix**: Install static libraries via vcpkg:

```powershell
cd C:\vcpkg
.\vcpkg install zlib:x64-windows-static
.\vcpkg install lz4:x64-windows-static
```

**Note**: The vcpkg zlib static library is named `zs.lib` (not `zlib.lib` or `libz.lib`).

Link command:

```batch
cl /nologo /MT /Fe:run.exe run.generated.c console_main.obj ^
  rpcrt4.lib ole32.lib advapi32.lib User32.lib petite-chez.lib ^
  legacy_stdio_definitions.lib C:\vcpkg\installed\x64-windows-static\lib\zs.lib ^
  C:\vcpkg\installed\x64-windows-static\lib\lz4.lib
```

### 2.6 Hidden `.version` file not uploaded by artifact action

`.version` starts with a dot and is therefore a hidden file. `actions/upload-artifact@v4` **skips hidden files by default**.

**Fix**:

```yaml
uses: actions/upload-artifact@v4
with:
  name: windows-build-files
  include-hidden-files: true   # ← essential
  path: |
    run.generated.c
    .version
```

### 2.7 `akku install` parse contamination

When building Chez Scheme from source, if the `csv10.4.1/` directory is not removed, `akku install` treats it as a project dependency and fails with:

```
Exception: Invalid directive with irritants (identifier chezscheme)
```

**Fix**: Run `rm -rf csv10.4.1` before `akku install`.

---

## 3. Why We Abandoned It

### 3.1 Local test showed `compile-whole-program` output is platform-dependent

Compiling a trivial program (only built-in imports) with `compile-whole-program` on a local machine (Chez 10.3.0) produced no machine-type markers in the `.chez` output. However, compiling `run.ss` (which imports many external libraries) produced a `run.chez` file that clearly contains `ta6le`:

```bash
$ strings run.chez | grep ta6le
ta6le

$ grep -obaP '\x74\x61\x36\x6c\x65' run.chez | wc -l
1
```

### 3.2 Recompiling from source after deleting `.so` cache still contains `ta6le`

```bash
rm -rf .akku/libobj
rm -f run.chez run.so run.wpo
scheme -q <<'SCHEME'
(compile-imported-libraries #t)
(generate-wpo-files #t)
(compile-program "run.ss")
(compile-whole-program "run.wpo" "run.chez" #t)
SCHEME

$ strings run.chez | grep -c ta6le
2
```

This demonstrates that **even when every library is recompiled from source**, `compile-whole-program` still embeds `ta6le` machine-type markers. The output is a platform-dependent fasl object file, not pure bytecodes.

### 3.3 `prepare-windows-build.sh` approach is invalid

The `prepare-windows-build.sh` README claimed the generated C file contains "only Chez Scheme virtual-machine bytecodes (which are platform-independent)". **That assumption does not hold for Chez Scheme 10.x.**

### 3.4 Cross-compilation attempt also failed

We tried Chez Scheme's cross-compilation patch `xc-ta6nt/s/xpatch`:

```bash
./configure --pb
make bootquick XM=ta6nt
cp xc-ta6nt/s/xpatch ../ta6nt-xpatch.ss
```

Loading `xpatch` failed with:

```
Exception: incompatible fasl-object machine-type pb found in ta6nt-xpatch.ss
```

`xpatch` itself is a `pb` (portable bytecode) fasl object, so it requires a `pb` build of Chez Scheme to load. A deeper attempt kept a `pb` build around to load `xpatch` and compile further, but since `compile-whole-program` output was already proven platform-dependent, this path was also blocked.

---

## 4. Script Idea: Copying Linux Akku Dependencies to Windows

Although we ultimately abandoned the approach, below is one idea for copying Linux Akku dependencies to Windows if someone tries a direct Windows build in the future.

### 4.1 Core problem

Windows does not support `:` in filenames. `.akku/lib/` contains many files like:

```
.akku/lib/srfi/:1/lists.chezscheme.sls
.akku/lib/srfi/:8/receive.chezscheme.sls
```

### 4.2 Idea: preprocess on Linux, replacing colons

```bash
#!/bin/bash
# prepare-windows-deps.sh
# Run on Linux to produce a Windows-compatible dependency package

set -euo pipefail

SRC=".akku/lib"
DST=".akku/lib-win"

rm -rf "$DST"
cp -r "$SRC" "$DST"

# Replace colons in filenames with underscores
find "$DST" -name '*:*' | while read -r f; do
    newname=$(echo "$f" | tr ':' '_')
    mv "$f" "$newname"
done

# Also need to rewrite all (library (srfi :1 ...)) declarations
# and all import clauses across every .sls file. This is tedious
# and requires a global search-and-replace.
# Suggest using sed/perl for batch processing.

# Package
tar czf akku-deps-windows.tar.gz "$DST"
```

### 4.3 Using it on Windows

```powershell
# On the Windows runner
tar xzf akku-deps-windows.tar.gz
# Then point CHEZSCHEMELIBDIRS at the preprocessed library path
$env:CHEZSCHEMELIBDIRS = "C:\project\.akku\lib-win"
# Then run compile-chez-program directly with the Windows scheme.exe
```

**Difficulty**: Every `(library (srfi :1 ...))` declaration and every `(import (srfi :1 ...))` statement must be rewritten in sync. A global search-and-replace is error-prone and easy to miss.

---

## 5. If Someone Wants to Try Again

### 5.1 Feasible directions (ordered by complexity)

| Direction | Complexity | Notes |
|-----------|------------|-------|
| **WSL2 / Docker Desktop** | Low | Run Linux inside WSL2 and build natively. GitHub Actions Windows runners do **not** support WSL2, so this only works for local development. |
| **MinGW cross-compilation** | High | Cross-compile Chez Scheme for Windows (`ta6nt`) from Linux using `mingw-w64`, then run `compile-chez-program`. The BUILDING document mentions MSYS2/MinGW support. |
| **Wine + Windows Chez Scheme** | Medium | Install Wine on a Linux runner, run the Windows `scheme.exe` and `compile-chez-program`. Wine uses a Linux filesystem underneath, which might tolerate colon filenames. |
| **Full MSYS2 build** | High | Install MSYS2 on a Windows runner, use the MinGW Chez Scheme port, and handle dependencies manually (replacing Akku). |

### 5.2 Key validation checklist

Anyone attempting this should verify locally first:

1. Does `compile-whole-program` output contain the target machine-type marker?
2. Can the target-platform Chez Scheme load that output (via `load-program`)?
3. Does `chez-exe` boot-file construction succeed correctly on the target platform?

---

## 6. Related Files and Commits

- Deleted script: `prepare-windows-build.sh`
- Modified CI: `.github/workflows/release.yaml`, `.github/workflows/manually-release.yaml`
- Related PR / commit range: `3a27e5d` ~ `0251040` (all reverted)
