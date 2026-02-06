#!/bin/bash
#
# build_mitgcm_lib.sh - Build MITgcm as a shared library for Julia interop
#
# Usage: ./build_mitgcm_lib.sh [MITGCM_DIR]
#
# This script:
#   1. Configures and builds MITgcm for the global_oce_latlon experiment
#   2. Compiles the library wrapper (mitgcm_wrapper.F)
#   3. Links everything into a shared library (libmitgcm.dylib / libmitgcm.so)
#
# The resulting library can be loaded by Julia via ccall.

set -e

# ============================================================
# Configuration
# ============================================================

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
MITGCM_DIR="${1:-$(cd "$SCRIPT_DIR/../../../MITgcm" && pwd)}"
EXPERIMENT="global_oce_latlon"
VERIFICATION_DIR="$MITGCM_DIR/verification/$EXPERIMENT"
BUILD_DIR="$VERIFICATION_DIR/build"
CODE_DIR="$VERIFICATION_DIR/code"
INPUT_DIR="$VERIFICATION_DIR/input"
WRAPPER_SRC="$SCRIPT_DIR/mitgcm_wrapper.F"
OUTPUT_DIR="$SCRIPT_DIR"

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
    echo "Usage: $0 [path_to_MITgcm]"
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
FC=$(grep '^FC=' Makefile | head -1 | sed 's/FC=//')
FFLAGS=$(grep '^FFLAGS=' Makefile | head -1 | sed 's/FFLAGS=//')
FOPTIM=$(grep '^FOPTIM=' Makefile | head -1 | sed 's/FOPTIM=//')
INCLUDES=$(grep '^INCLUDES=' Makefile | head -1 | sed 's/INCLUDES=//')
CPP=$(grep '^CPP=' Makefile | head -1 | sed 's/CPP=//')

# If FC is empty, default to gfortran
FC=${FC:-gfortran}

echo "  Compiler: $FC"
echo "  Flags:    $FFLAGS $FOPTIM"

# Preprocess and compile the wrapper
# The CPP step expands macros (#include, #ifdef, etc.)
$CPP $INCLUDES $WRAPPER_SRC > mitgcm_wrapper_pp.f 2>/dev/null || \
    /usr/bin/cpp -traditional -P $INCLUDES $WRAPPER_SRC > mitgcm_wrapper_pp.f

$FC $FFLAGS $FOPTIM -fPIC -c mitgcm_wrapper_pp.f -o mitgcm_wrapper.o

echo "  Wrapper compiled."
echo ""

# ============================================================
# Step 3: Link into shared library
# ============================================================

echo "Step 3: Linking shared library..."
echo "----------------------------------------------"

# Collect all object files except main.o (which has PROGRAM MAIN)
OBJ_FILES=$(ls *.o | grep -v '^main\.o$' | tr '\n' ' ')

LIBS=$(grep '^LIBS=' Makefile | head -1 | sed 's/LIBS=//')

if [ "$UNAME_S" = "Darwin" ]; then
    SHLIB_NAME="libmitgcm.dylib"
    SHLIB_FLAGS="-dynamiclib -install_name @rpath/$SHLIB_NAME"
else
    SHLIB_NAME="libmitgcm.so"
    SHLIB_FLAGS="-shared"
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
if [ -f "$INPUT_DIR/prepare_run" ]; then
    # Use the experiment's prepare_run script approach: link all input files
    for f in "$INPUT_DIR"/*; do
        fname=$(basename "$f")
        if [ "$fname" != "prepare_run" ] && [ ! -e "$fname" ]; then
            ln -sf "$f" .
        fi
    done
fi

# Link binary from build (some experiments need this)
if [ -f "$BUILD_DIR/mitgcmuv" ] && [ ! -e "mitgcmuv" ]; then
    ln -sf "$BUILD_DIR/mitgcmuv" .
fi

# Link the shared library
if [ ! -e "$SHLIB_NAME" ]; then
    ln -sf "$OUTPUT_DIR/$SHLIB_NAME" .
fi

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
echo ""
echo "To run the Julia example:"
echo "  cd $RUN_DIR"
echo "  julia $SCRIPT_DIR/run_global_oce_latlon.jl"
echo "=============================================="
