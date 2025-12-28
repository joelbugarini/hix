#!/bin/bash
#
# Build RPM package for Hix
# This script builds the Hix application and creates an RPM package
#
# Usage: ./build-rpm.sh [version]
#

set -e

# Get version from hix.cabal if not provided
if [ -z "$1" ]; then
    VERSION=$(grep "^version:" ../../hix.cabal | awk '{print $2}')
else
    VERSION="$1"
fi

echo "Building Hix RPM package version $VERSION"

# Get the project root directory
PROJECT_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
PACKAGING_DIR="$(cd "$(dirname "$0")" && pwd)"
BUILD_DIR="$PACKAGING_DIR/rpmbuild"

# Clean previous builds
echo "Cleaning previous builds..."
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"/{BUILD,BUILDROOT,RPMS,SOURCES,SPECS,SRPMS}

# Update version in spec file if needed
SPEC_FILE="$PACKAGING_DIR/hix.spec"
if [ -f "$SPEC_FILE" ]; then
    sed -i "s/^Version:.*/Version: $VERSION/" "$SPEC_FILE"
    sed -i "s/^%define version.*/%define version $VERSION/" "$SPEC_FILE"
fi

# Create source tarball with correct directory structure
echo "Creating source tarball..."
cd "$PROJECT_ROOT"
TARBALL_NAME="hix-${VERSION}.tar.gz"
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

# Copy files to temp directory with versioned name
mkdir -p "$TEMP_DIR/hix-${VERSION}"
rsync -a --exclude='.git' \
      --exclude='.stack-work' \
      --exclude='dist' \
      --exclude='*.swp' \
      --exclude='*.swo' \
      --exclude='*~' \
      --exclude='utils/packaging/rpmbuild' \
      --exclude='utils/installer/output' \
      "$PROJECT_ROOT/" "$TEMP_DIR/hix-${VERSION}/"

# Create tarball from the versioned directory
cd "$TEMP_DIR"
tar -czf "$BUILD_DIR/SOURCES/$TARBALL_NAME" "hix-${VERSION}"
rm -rf "$TEMP_DIR"
trap - EXIT

# Copy spec file to SPECS directory
cp "$SPEC_FILE" "$BUILD_DIR/SPECS/"

# Build the RPM
echo "Building RPM package..."
cd "$BUILD_DIR"
rpmbuild --define "_topdir $BUILD_DIR" \
         --define "_builddir $BUILD_DIR/BUILD" \
         --define "_buildrootdir $BUILD_DIR/BUILDROOT" \
         -ba SPECS/hix.spec

# Find the built RPM
RPM_FILE=$(find RPMS -name "hix-${VERSION}-*.rpm" | head -1)

if [ -n "$RPM_FILE" ]; then
    echo ""
    echo "✅ RPM package built successfully!"
    echo "📦 Package location: $BUILD_DIR/$RPM_FILE"
    echo ""
    echo "To install the RPM package, run:"
    echo "  sudo rpm -ivh $BUILD_DIR/$RPM_FILE"
    echo ""
    echo "Or to upgrade an existing installation:"
    echo "  sudo rpm -Uvh $BUILD_DIR/$RPM_FILE"
else
    echo "❌ Error: RPM package not found after build"
    exit 1
fi

