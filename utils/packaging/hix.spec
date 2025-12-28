%global debug_package %{nil}

# Package metadata (update version to match hix.cabal)
%define name hix
%define version 0.3.5.4
%define release 1
%define appname Hix

Summary: Flexible code generator for template-driven code generation
Name: %{name}
Version: 0.3.5.4
Release: %{release}%{?dist}
License: GPLv2
Group: Development/Tools
Source0: %{name}-%{version}.tar.gz
URL: https://github.com/joelbugarini/hix
Packager: StackPatterns
BuildArch: x86_64
BuildRequires: stack >= 3.0
Requires: glibc

%description
Hix is a flexible, template-driven code generator that turns simple JSON models 
into full-featured source code. Designed for developers who value consistency, 
automation, and clean architecture, Hix helps you build smarter, faster, and 
with fewer bugs.

Features:
- One model → many files
- Template everything: Python, TypeScript, HTML, SQL, C#, you name it
- Drop-in CLI with editor integration
- Built with Haskell for safe, predictable rendering

%prep
%setup -q -n %{name}-%{version}

%build
# Build the project using Stack
# The source is extracted to %{_builddir}/%{name}-%{version} by %setup
cd %{_builddir}/%{name}-%{version}
stack build

# Prepare installation directory for packaging
mkdir -p %{_builddir}/install-dir/usr/bin
stack install --local-bin-path %{_builddir}/install-dir/usr/bin

%install
# Create directory structure
mkdir -p %{buildroot}%{_bindir}
mkdir -p %{buildroot}%{_docdir}/%{name}

# Install the executable
install -m 755 %{_builddir}/install-dir/usr/bin/hix %{buildroot}%{_bindir}/hix

# Copy documentation files from source
cd %{_builddir}/%{name}-%{version}

# Install documentation
install -m 644 README.md %{buildroot}%{_docdir}/%{name}/README.md
install -m 644 LICENSE %{buildroot}%{_docdir}/%{name}/LICENSE
# Install CHANGELOG if it exists (optional)
[ -f CHANGELOG.md ] && install -m 644 CHANGELOG.md %{buildroot}%{_docdir}/%{name}/CHANGELOG.md || true

%files
%defattr(-,root,root,-)
%{_bindir}/hix
%doc %{_docdir}/%{name}/README.md
%doc %{_docdir}/%{name}/LICENSE
# CHANGELOG.md is conditionally included - uncomment if you want it in the package
%doc %{_docdir}/%{name}/CHANGELOG.md

%changelog
* %(date +'%a %b %d %Y') StackPatterns <example@example.com> - %{version}-%{release}
- Initial RPM package for Hix %{version}
- Based on Windows installer configuration

