#!/usr/bin/env bash
set -euo pipefail

# =============================================================================
# prepare-windows-build.sh
# Prepare a Windows-native build package for scheme-langserver on Linux.
#
# Core idea:
#   compile-chez-program generates run.generated.c, which only contains
#   Chez Scheme virtual-machine bytecodes (platform-independent). Thus we
#   can generate this C file on Linux, copy it to Windows, and link it
#   with the Windows version of chez-exe static libraries (petite-chez.lib
#   etc.) using MSVC (cl.exe) to obtain a native Windows executable run.exe.
#
# This approach completely avoids:
#   1) The Windows illegal-filename-colon issue with SRFI libraries;
#   2) The fact that AKKU cannot run natively on Windows.
# =============================================================================

DIST_DIR="dist/windows-scheme-langserver"
GENERATED_C="run.generated.c"

# ---------------------------------------------------------------------------
# 1. Verify we are in the project root
# ---------------------------------------------------------------------------
if [ ! -f "scheme-langserver.sls" ] || [ ! -f "run.ss" ] || [ ! -d ".akku" ]; then
    echo "[ERROR] Please run this script from the scheme-langserver project root."
    exit 1
fi

echo "==> Step 1/5: Ensuring AKKU dependencies are installed..."
akku install

# ---------------------------------------------------------------------------
# 2. Enter the AKKU environment and generate run.generated.c
# ---------------------------------------------------------------------------
echo "==> Step 2/5: Generating the platform-neutral C file ($GENERATED_C) on Linux..."

# Source .akku/env in a subshell so that compile-chez-program can locate all libraries
(
    export AKKU_ENV="$(pwd)"
    export R6RS_PATH="${R6RS_PATH:-}"
    export R7RS_PATH="${R7RS_PATH:-}"
    set +u
    . "$AKKU_ENV/.akku/bin/activate"

    if ! command -v compile-chez-program >/dev/null 2>&1; then
        echo "[ERROR] compile-chez-program not found. Please install chez-exe first."
        echo "        See: https://github.com/gwatt/chez-exe"
        exit 1
    fi

    # Remove old artifacts to force regeneration
    rm -f "$GENERATED_C" run.chez run.so run.wpo

    # Generate run.generated.c (and the Linux executable 'run' as a side effect)
    compile-chez-program run.ss
)

if [ ! -f "$GENERATED_C" ]; then
    echo "[ERROR] Failed to generate $GENERATED_C."
    exit 1
fi

# ---------------------------------------------------------------------------
# 3. Create the Windows build-package directory
# ---------------------------------------------------------------------------
echo "==> Step 3/5: Creating the Windows build package directory ($DIST_DIR)..."
rm -rf "$DIST_DIR"
mkdir -p "$DIST_DIR"

# ---------------------------------------------------------------------------
# 4. Copy the core file
# ---------------------------------------------------------------------------
echo "==> Step 4/5: Copying the platform-neutral C file..."
cp "$GENERATED_C" "$DIST_DIR/"

# ---------------------------------------------------------------------------
# 5. Generate Windows build scripts and instructions
# ---------------------------------------------------------------------------
echo "==> Step 5/5: Generating Windows build scripts and instructions..."

cat > "$DIST_DIR/build-windows.bat" << 'BATEOF'
@echo off
REM =============================================================================
REM Windows build script for scheme-langserver
REM =============================================================================
REM Usage:
REM   1. Install Chez Scheme on Windows (10.0.0+ x64 recommended).
REM   2. Build and install chez-exe on Windows:
REM        https://github.com/gwatt/chez-exe
REM      You MUST use "x64 Native Tools Command Prompt for VS 2022"
REM      (or the matching VS version) so that cl.exe and nmake are available.
REM      Make sure the MSVC bitness matches your Chez Scheme installation.
REM   3. Copy run.generated.c from this folder into your chez-exe build/install
REM      directory (the one containing petite-chez.lib and console_main.obj).
REM   4. Run this batch file in that directory.
REM =============================================================================

set CHEZ_EXE_DIR=%CD%
set GENERATED_C=run.generated.c
set OUT_EXE=run.exe

if not exist "%GENERATED_C%" (
    echo [ERROR] %GENERATED_C% not found. Please ensure it is in the current directory.
    pause
    exit /b 1
)

if not exist "%CHEZ_EXE_DIR%\petite-chez.lib" (
    echo [WARNING] petite-chez.lib not found in the current directory.
    echo           Please make sure this script is run from the chez-exe build/install directory.
    pause
)

echo [INFO] Compiling %OUT_EXE% ...
cl /nologo /MD /Fe:%OUT_EXE% %GENERATED_C% console_main.obj rpcrt4.lib ole32.lib advapi32.lib User32.lib petite-chez.lib

if %errorlevel% neq 0 (
    echo [ERROR] Build failed.
    pause
    exit /b 1
)

echo [SUCCESS] %OUT_EXE% generated.
pause
BATEOF

cat > "$DIST_DIR/build-windows-static.bat" << 'BATEOF'
@echo off
REM Static CRT linking version (replaces /MD with /MT)
REM All other instructions are the same as build-windows.bat.

set GENERATED_C=run.generated.c
set OUT_EXE=run.exe

if not exist "%GENERATED_C%" (
    echo [ERROR] %GENERATED_C% not found.
    pause
    exit /b 1
)

echo [INFO] Building statically-linked %OUT_EXE% ...
cl /nologo /MT /Fe:%OUT_EXE% %GENERATED_C% console_main.obj rpcrt4.lib ole32.lib advapi32.lib User32.lib petite-chez.lib

if %errorlevel% neq 0 (
    echo [ERROR] Build failed.
    pause
    exit /b 1
)

echo [SUCCESS] %OUT_EXE% generated (statically linked C runtime).
pause
BATEOF

cat > "$DIST_DIR/README-WINDOWS.txt" << 'TXTEOF'
Native Windows Build Instructions for scheme-langserver
=======================================================

1. Principle
------------
The file run.generated.c in this package was produced on Linux using
compile-chez-program. It contains only Chez Scheme virtual-machine bytecodes
(which are platform-independent), so it can be taken to Windows and linked
with the Windows build of chez-exe to produce a native run.exe.

Benefits:
  * Completely avoids the illegal-colon-in-filename problem of SRFI libraries
    on Windows.
  * Does NOT require AKKU to run natively on Windows.

2. Prerequisites
----------------
a) Chez Scheme for Windows (10.0.0+ x64 recommended)
   Download: https://github.com/cisco/ChezScheme/releases

b) chez-exe for Windows
   Source: https://github.com/gwatt/chez-exe
   Note: Building chez-exe on Windows requires the MSVC toolchain (cl.exe,
   nmake). You MUST open an "x64 Native Tools Command Prompt for VS 2022"
   (or 2019) and ensure the MSVC bitness matches your Chez Scheme install.

   Example build steps:
     scheme --script gen-config.ss --bootpath "C:\Program Files\Chez Scheme 10.0.0\lib\csv10.0.0\ta6nt"
     nmake /f Makefile.win

3. Building run.exe
-------------------
1. Copy run.generated.c from this folder into your chez-exe build directory
   (the directory that contains petite-chez.lib and console_main.obj).

2. In that directory, double-click build-windows.bat (or run it from the
   command line) to produce run.exe.

3. If you prefer a static CRT link (fewer DLL dependencies), use
   build-windows-static.bat instead.

4. Verification
---------------
After run.exe is built, open a normal Windows CMD or PowerShell (NOT WSL)
and run:
     run.exe --help
If you see the help message, the build succeeded.

5. Notes
--------
* The resulting run.exe still needs a matching MSVC runtime.
  - build-windows.bat uses /MD, so the target machine needs the
    corresponding VC++ Redistributable.
  - build-windows-static.bat uses /MT, so no extra runtime is required.

* If the linker cannot find petite-chez.lib or console_main.obj, make sure
  build-windows.bat is executed inside the chez-exe build directory.

* If you see errors like "unresolved external symbol _Sscheme_init", the
  MSVC compiler bitness does not match the Chez Scheme bitness (e.g. you
  used the x86 tools with an x64 Chez install). Switch to the correct
  Native Tools Command Prompt.
TXTEOF

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------
echo ""
echo "=========================================="
echo "Windows build package is ready: $DIST_DIR/"
echo ""
echo "Contents:"
ls -lh "$DIST_DIR/"
echo ""
echo "Next step: copy $DIST_DIR/ to Windows"
echo "           and follow README-WINDOWS.txt to finish the build."
echo "=========================================="
