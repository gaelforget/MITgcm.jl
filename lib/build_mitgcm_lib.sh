#!/bin/bash
#
# build_mitgcm_lib.sh - Build MITgcm as a shared library for Julia interop
#
# Usage: ./build_mitgcm_lib.sh MITGCM_DIR EXPERIMENT OUTPUT_DIR [WRAPPER_SRC]
#
# Arguments:
#   MITGCM_DIR   - Path to MITgcm source directory
#   EXPERIMENT   - Name of the verification experiment (e.g. global_oce_latlon)
#   OUTPUT_DIR   - Where to place the shared library and run directory
#   WRAPPER_SRC  - (optional) Path to mitgcm_wrapper.F; defaults to lib/mitgcm_wrapper.F
#
# This script:
#   1. Configures and builds MITgcm for the specified experiment
#   2. Compiles the library wrapper (mitgcm_wrapper.F)
#   3. Links everything into a shared library (libmitgcm.dylib / libmitgcm.so)
#   4. Sets up a run directory with input files
#
# The resulting library can be loaded by Julia via ccall.

set -e

# ============================================================
# Configuration
# ============================================================

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

if [ $# -lt 3 ]; then
    echo "Usage: $0 MITGCM_DIR EXPERIMENT OUTPUT_DIR [WRAPPER_SRC]"
    exit 1
fi

MITGCM_DIR="$1"
EXPERIMENT="$2"
OUTPUT_DIR="$3"
WRAPPER_SRC="${4:-$SCRIPT_DIR/mitgcm_wrapper.F}"

VERIFICATION_DIR="$MITGCM_DIR/verification/$EXPERIMENT"
BUILD_DIR="$VERIFICATION_DIR/build"
CODE_DIR="$VERIFICATION_DIR/code"
INPUT_DIR="$VERIFICATION_DIR/input"

echo "=============================================="
echo "Building MITgcm as shared library"
echo "=============================================="
echo "MITgcm dir:   $MITGCM_DIR"
echo "Experiment:   $EXPERIMENT"
echo "Build dir:    $BUILD_DIR"
echo "Wrapper src:  $WRAPPER_SRC"
echo "Output dir:   $OUTPUT_DIR"
echo ""

# ============================================================
# Check prerequisites
# ============================================================

if [ ! -d "$MITGCM_DIR" ]; then
    echo "ERROR: MITgcm directory not found: $MITGCM_DIR"
    exit 1
fi

if [ ! -f "$WRAPPER_SRC" ]; then
    echo "ERROR: Wrapper source not found: $WRAPPER_SRC"
    exit 1
fi

if ! command -v gfortran &> /dev/null; then
    echo "ERROR: gfortran not found. Please install gfortran."
    exit 1
fi

mkdir -p "$OUTPUT_DIR"

# ============================================================
# Step 1: Build MITgcm (standard build to get all object files)
# ============================================================

echo "Step 1: Building MITgcm for $EXPERIMENT..."
echo "----------------------------------------------"

mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

GENMAKE2="$MITGCM_DIR/tools/genmake2"

# Detect platform for genmake2 options
UNAME_S=$(uname -s)
UNAME_M=$(uname -m)

GENMAKE_OPTS="-mods $CODE_DIR"

if [ "$UNAME_S" = "Darwin" ] && [ "$UNAME_M" = "arm64" ]; then
    OPTFILE="$MITGCM_DIR/tools/build_options/darwin_arm64_gfortran"
    if [ -f "$OPTFILE" ]; then
        GENMAKE_OPTS="$GENMAKE_OPTS -optfile $OPTFILE"
    fi
elif [ "$UNAME_S" = "Darwin" ] && [ "$UNAME_M" = "x86_64" ]; then
    OPTFILE="$MITGCM_DIR/tools/build_options/darwin_amd64_gfortran"
    if [ -f "$OPTFILE" ]; then
        GENMAKE_OPTS="$GENMAKE_OPTS -optfile $OPTFILE"
    fi
fi

# Run genmake2 (only if Makefile doesn't exist or is outdated)
if [ ! -f "$BUILD_DIR/Makefile" ]; then
    echo "  Running genmake2..."
    $GENMAKE2 $GENMAKE_OPTS 2>&1 | tail -5
    echo "  Running make depend..."
    make depend 2>&1 | tail -3
fi

# For shared library mode, gfortran's -fconvert=big-endian does NOT work
# when loaded from a non-Fortran host (Julia/C).  Add _BYTESWAPIO so MITgcm
# does its own byte-swapping in the MDS I/O routines.
if ! grep -q '_BYTESWAPIO' "$BUILD_DIR/Makefile" 2>/dev/null; then
    echo "  Adding _BYTESWAPIO flag for shared library compatibility..."
    sed -i.bak 's/^DEFINES = /DEFINES = -D_BYTESWAPIO /' "$BUILD_DIR/Makefile"
    echo "  Cleaning and rebuilding with _BYTESWAPIO..."
    make clean 2>&1 | tail -2
    make depend 2>&1 | tail -3
fi

echo "  Compiling MITgcm..."
make -j$(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 4) 2>&1 | tail -5

echo "  MITgcm build complete."
echo ""

# ============================================================
# Step 2: Compile the wrapper
# ============================================================

echo "Step 2: Compiling library wrapper..."
echo "----------------------------------------------"

cd "$BUILD_DIR"

# Extract compiler flags from the generated Makefile
FC=$(make -p -q 2>/dev/null | grep '^FC = ' | head -1 | sed 's/^FC = //')
FFLAGS=$(make -p -q 2>/dev/null | grep '^FFLAGS = ' | head -1 | sed 's/^FFLAGS = //')
FOPTIM=$(make -p -q 2>/dev/null | grep '^FOPTIM = ' | head -1 | sed 's/^FOPTIM = //')
INCLUDES=$(make -p -q 2>/dev/null | grep '^INCLUDES = ' | head -1 | sed 's/^INCLUDES = //')
DEFINES=$(make -p -q 2>/dev/null | grep '^DEFINES = ' | head -1 | sed 's/^DEFINES = //')
ROOTDIR=$(make -p -q 2>/dev/null | grep '^ROOTDIR = ' | head -1 | sed 's/^ROOTDIR = //')
TOOLSDIR="${ROOTDIR}/tools"

# If FC is empty, default to gfortran
FC=${FC:-gfortran}

echo "  Compiler: $FC"
echo "  Flags:    $FFLAGS $FOPTIM"
echo "  Defines:  $DEFINES"

# Copy wrapper source to build directory (so #include finds headers)
cp "$WRAPPER_SRC" "$BUILD_DIR/mitgcm_wrapper.F"

# Preprocess using the same CPP pipeline as MITgcm's Makefile:
#   cat file.F | /usr/bin/cpp -traditional -P $(DEFINES) $(INCLUDES) | set64bitConst.sh
cat mitgcm_wrapper.F | /usr/bin/cpp -traditional -P $DEFINES $INCLUDES | \
    "$TOOLSDIR/set64bitConst.sh" > mitgcm_wrapper.for

# Compile the preprocessed file
$FC $FFLAGS $FOPTIM -fPIC -c mitgcm_wrapper.for -o mitgcm_wrapper.o

echo "  Wrapper compiled."
echo ""

# ============================================================
# Step 3: Link into shared library
# ============================================================

echo "Step 3: Linking shared library..."
echo "----------------------------------------------"

# Collect all object files except main.o (which has PROGRAM MAIN)
OBJ_FILES=$(ls *.o | grep -v '^main\.o$' | tr '\n' ' ')

LIBS=$(make -p -q 2>/dev/null | grep '^LIBS = ' | head -1 | sed 's/^LIBS = //')

# Extract library directories from LIBS for rpath
LIB_DIRS=$(echo "$LIBS" | tr ' ' '\n' | grep '^-L' | sed 's/^-L//')

if [ "$UNAME_S" = "Darwin" ]; then
    SHLIB_NAME="libmitgcm.dylib"
    SHLIB_FLAGS="-dynamiclib -install_name @rpath/$SHLIB_NAME"
    # Add rpath entries for all library directories
    for dir in $LIB_DIRS; do
        SHLIB_FLAGS="$SHLIB_FLAGS -Wl,-rpath,$dir"
    done
else
    SHLIB_NAME="libmitgcm.so"
    SHLIB_FLAGS="-shared"
    for dir in $LIB_DIRS; do
        SHLIB_FLAGS="$SHLIB_FLAGS -Wl,-rpath,$dir"
    done
fi

$FC $SHLIB_FLAGS -o "$OUTPUT_DIR/$SHLIB_NAME" $OBJ_FILES $LIBS

echo "  Created: $OUTPUT_DIR/$SHLIB_NAME"
echo ""

# ============================================================
# Step 4: Set up run directory
# ============================================================

echo "Step 4: Setting up run directory..."
echo "----------------------------------------------"

RUN_DIR="$OUTPUT_DIR/run"
mkdir -p "$RUN_DIR"

# Link input files
cd "$RUN_DIR"

# Link all input files
for f in "$INPUT_DIR"/*; do
    fname=$(basename "$f")
    if [ "$fname" != "prepare_run" ] && [ ! -e "$fname" ]; then
        ln -sf "$f" .
    fi
done

# Link .bin files from tutorial_global_oce_latlon (prepare_run does this)
# This section is specific to the global_oce_latlon experiment.
TUTORIAL_INPUT="$MITGCM_DIR/verification/tutorial_global_oce_latlon/input"
if [ "$EXPERIMENT" = "global_oce_latlon" ] && [ -d "$TUTORIAL_INPUT" ]; then
    echo "  Linking binary data from tutorial experiment..."
    for f in "$TUTORIAL_INPUT"/*.bin; do
        fname=$(basename "$f")
        if [ ! -e "$fname" ]; then
            ln -sf "$f" .
        fi
    done
    # prepare_run also creates lev_sst_startdec.tmp (Dec record prepended)
    SST_FILE="$TUTORIAL_INPUT/lev_sst.bin"
    if [ -f "$SST_FILE" ] && [ ! -f "lev_sst_startdec.tmp" ]; then
        dd if="$SST_FILE" bs=14400 count=1 skip=11 of=lev_sst_dec.tmp 2>/dev/null
        cat lev_sst_dec.tmp "$SST_FILE" > lev_sst_startdec.tmp
        rm -f lev_sst_dec.tmp
    fi
fi

# Link binary from build (some experiments need this)
if [ -f "$BUILD_DIR/mitgcmuv" ] && [ ! -e "mitgcmuv" ]; then
    ln -sf "$BUILD_DIR/mitgcmuv" .
fi

# Link the shared library
if [ ! -e "$SHLIB_NAME" ]; then
    ln -sf "$OUTPUT_DIR/$SHLIB_NAME" .
fi

# Override data.diagnostics to disable file output (avoids conflicts when
# the timestep is changed at runtime from Julia).
cat > data.diagnostics << 'EOF'
# Diagnostics disabled for shared-library mode (timestep may change at runtime)
 &DIAGNOSTICS_LIST
 &
 &DIAG_STATIS_PARMS
 &
EOF

# Create eedata if not present (needed for EEBOOT)
if [ ! -f "eedata" ]; then
    cat > eedata << 'EOF'
# Example "eedata" file
# Lines beginning "#" are comments
# nTx - No. threads per process in X
# nTy - No. threads per process in Y
 &EEPARMS
 nTx=1,
 nTy=1,
 &
# Note: Some systems use & as the namelist terminator (i.e., not /).
#       This is set at compile time (CPP: NML_TERMINATOR).
EOF
fi

echo "  Run directory: $RUN_DIR"
echo ""

# ============================================================
# Done
# ============================================================

echo "=============================================="
echo "Build complete!"
echo ""
echo "Shared library: $OUTPUT_DIR/$SHLIB_NAME"
echo "Run directory:  $RUN_DIR"
echo "=============================================="
