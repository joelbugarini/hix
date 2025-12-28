# RPM Packaging for Hix

This directory contains the RPM packaging files for building Hix packages for Fedora and other RPM-based distributions.

## Files

- `hix.spec` - RPM spec file defining the package structure and build process
- `build-rpm.sh` - Build script that automates the RPM creation process
- `README.md` - This file

## Prerequisites

Before building an RPM package, ensure you have:

- **rpmbuild**: RPM build tool
  ```bash
  sudo dnf install rpm-build
  ```

- **Stack**: Haskell Tool Stack (already required for building Hix)

## Building the RPM Package

### Quick Build

Run the build script from the packaging directory:

```bash
cd utils/packaging
./build-rpm.sh
```

The script will:
1. Extract the version from `hix.cabal` (or use the version you provide)
2. Create a source tarball
3. Build the RPM package using `rpmbuild`
4. Output the built RPM file location

### Manual Build

If you prefer to build manually:

1. Create the source tarball:
   ```bash
   cd /path/to/hix
   tar -czf hix-0.3.5.4.tar.gz --exclude='.git' --exclude='.stack-work' .
   ```

2. Set up RPM build directories:
   ```bash
   mkdir -p ~/rpmbuild/{BUILD,BUILDROOT,RPMS,SOURCES,SPECS,SRPMS}
   ```

3. Copy files:
   ```bash
   cp hix-0.3.5.4.tar.gz ~/rpmbuild/SOURCES/
   cp utils/packaging/hix.spec ~/rpmbuild/SPECS/
   ```

4. Build the RPM:
   ```bash
   cd ~/rpmbuild
   rpmbuild -ba SPECS/hix.spec
   ```

5. Find the built package:
   ```bash
   find RPMS -name "hix-*.rpm"
   ```

## Installing the RPM Package

After building, install the package with:

```bash
sudo rpm -ivh utils/packaging/rpmbuild/RPMS/x86_64/hix-0.3.5.4-1.x86_64.rpm
```

Or upgrade an existing installation:

```bash
sudo rpm -Uvh utils/packaging/rpmbuild/RPMS/x86_64/hix-0.3.5.4-1.x86_64.rpm
```

## Package Output

The RPM package will be named:
- **hix-{version}-{release}.{arch}.rpm**

For example: `hix-0.3.5.4-1.x86_64.rpm`

This matches the naming convention used by the Windows installer (`hix-setup-{version}.exe`).

## Package Contents

The RPM package includes:
- `/usr/bin/hix` - The main executable
- `/usr/share/doc/hix/README.md` - Documentation
- `/usr/share/doc/hix/LICENSE` - License file
- `/usr/share/doc/hix/CHANGELOG.md` - Changelog (if present)

## Updating the Version

When updating the version, update the following files:
1. `hix.cabal` (source of truth)
2. `utils/packaging/hix.spec` - Update the `%define version` line
3. The build script will automatically extract the version from `hix.cabal`, but you can also pass it as an argument:
   ```bash
   ./build-rpm.sh 0.3.5.5
   ```

## Troubleshooting

### Build fails with "stack: command not found"

Ensure Stack is installed and in your PATH. The spec file requires Stack to build the application.

### Build fails with permission errors

The build script creates a local `rpmbuild` directory. Ensure you have write permissions in `utils/packaging/`.

### Package installs but `hix` command doesn't work

Verify the executable was installed correctly:
```bash
which hix
/usr/bin/hix --version
```

## Comparison with Windows Installer

| Feature | Windows (Inno Setup) | Fedora (RPM) |
|---------|---------------------|--------------|
| Output file | `hix-setup-0.3.5.4.exe` | `hix-0.3.5.4-1.x86_64.rpm` |
| Executable location | `C:\Program Files\Hix\hix.exe` | `/usr/bin/hix` |
| Installation | Graphical installer | `rpm -ivh` or `dnf install` |
| PATH setup | Automatic (registry) | Automatic (standard bin directory) |
| Uninstallation | Control Panel | `rpm -e hix` or `dnf remove hix` |

