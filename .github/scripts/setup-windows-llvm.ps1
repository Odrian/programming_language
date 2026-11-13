param(
    [Parameter(Mandatory = $true)]
    [string]$InstallDir
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'
Set-StrictMode -Version Latest

function Invoke-Checked {
    param([string]$Program, [string[]]$Arguments)

    Write-Host "Running $Program $($Arguments -join ' ')"
    & $Program @Arguments
    if ($LASTEXITCODE -ne 0) {
        throw "$Program exited with code $LASTEXITCODE"
    }
}

function Get-VerifiedArchive {
    param([string]$Uri, [string]$Path, [string]$Sha256)

    Write-Host "Downloading: $Uri to $Path (up to 3 retries)"
    Invoke-WebRequest -Uri $Uri -OutFile $Path -MaximumRetryCount 3 -RetryIntervalSec 2
    Write-Host ('Downloaded {0:N1} MiB' -f ((Get-Item -LiteralPath $Path).Length / 1MB))

    $actualHash = (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash
    if ($actualHash -ne $Sha256) {
        throw "SHA-256 mismatch for $Uri`: expected $Sha256, got $actualHash"
    }
    Write-Host "SHA-256 verified: $actualHash"
}

# Set up cl.exe, link.exe and the SDK library paths for this step and later steps.
Write-Host '::group::[1/6] Configure MSVC x64 environment'
$vswhere = Join-Path ${env:ProgramFiles(x86)} 'Microsoft Visual Studio/Installer/vswhere.exe'
$vsPath = Invoke-Checked $vswhere @(
    '-latest', '-products', '*',
    '-requires', 'Microsoft.VisualStudio.Component.VC.Tools.x86.x64',
    '-property', 'installationPath'
)
if (-not $vsPath) {
    throw 'Visual Studio C++ Build Tools for x64 were not found.'
}

Write-Host "Visual Studio installation: $vsPath"
$vcvars = Join-Path $vsPath 'VC/Auxiliary/Build/vcvars64.bat'
$environment = Invoke-Checked $env:ComSpec @('/d', '/c', "call `"$vcvars`" >nul && set")
foreach ($line in $environment) {
    if ($line -match '^(PATH|INCLUDE|LIB|LIBPATH)=(.*)$') {
        Set-Item -Path "Env:$($Matches[1])" -Value $Matches[2]
    }
}
foreach ($name in @('PATH', 'INCLUDE', 'LIB', 'LIBPATH')) {
    if (-not [Environment]::GetEnvironmentVariable($name)) {
        throw "MSVC environment is missing $name"
    }
}
Get-Command cl.exe, link.exe | Format-Table Name, Source
Write-Host 'MSVC environment ready: PATH, INCLUDE, LIB, LIBPATH'
Write-Host '::endgroup::'

Write-Host '::group::[2/6] Prepare LLVM 21.1.8 development archive'
$InstallDir = [IO.Path]::GetFullPath($InstallDir)
$workDir = Join-Path ([IO.Path]::GetTempPath()) 'pr-llvm-21.1.8-setup'
New-Item -ItemType Directory -Force -Path $InstallDir, $workDir | Out-Null
$llvmConfig = Join-Path $InstallDir 'bin/llvm-config.exe'
Write-Host "SDK directory: $InstallDir"
Write-Host "Temporary build directory: $workDir"

if (-not (Test-Path -LiteralPath $llvmConfig)) {
    # 7-Zip is preinstalled on windows-2022; local runs also need it installed.
    $sevenZip = Join-Path $env:ProgramFiles '7-Zip/7z.exe'
    if (-not (Test-Path -LiteralPath $sevenZip)) {
        $sevenZip = (Get-Command 7z.exe -ErrorAction Stop).Source
    }
    Write-Host "Using 7-Zip: $sevenZip"

    Write-Host 'No existing llvm-config.exe found; downloading LLVM SDK (about 899 MiB).'
    $archive = Join-Path $workDir 'llvm.tar.xz'
    Get-VerifiedArchive `
        -Uri 'https://github.com/llvm/llvm-project/releases/download/llvmorg-21.1.8/clang%2Bllvm-21.1.8-x86_64-pc-windows-msvc.tar.xz' `
        -Path $archive `
        -Sha256 '749d22f565fcd5718dbed06512572d0e5353b502c03fe1f7f17ee8b8aca21a47'
    $tarArchive = Join-Path $workDir 'llvm.tar'
    Write-Host "Decompressing XZ into TAR: $tarArchive"
    Invoke-Checked $sevenZip @('x', $archive, "-o$workDir", '-y')
    Write-Host ('XZ decompression complete; TAR size: {0:N1} MiB' -f ((Get-Item -LiteralPath $tarArchive).Length / 1MB))
    Write-Host "Removing downloaded XZ archive: $archive"
    Remove-Item -LiteralPath $archive

    Write-Host "Extracting TAR into $InstallDir"
    Invoke-Checked $sevenZip @('x', $tarArchive, "-o$InstallDir", '-y')
    Write-Host 'TAR extraction complete'

    # 7-Zip preserves the release archive's top-level directory. Move its contents
    # within the destination volume instead of copying the SDK between C: and D:.
    $extractedRoot = Join-Path $InstallDir 'clang+llvm-21.1.8-x86_64-pc-windows-msvc'
    if (-not (Test-Path -LiteralPath (Join-Path $extractedRoot 'bin/llvm-config.exe'))) {
        throw "Unexpected LLVM archive layout: llvm-config.exe not found under $extractedRoot"
    }
    Write-Host "Moving SDK contents from $extractedRoot to $InstallDir"
    Get-ChildItem -LiteralPath $extractedRoot -Force | Move-Item -Destination $InstallDir
    Remove-Item -LiteralPath $extractedRoot

    Write-Host "SDK extraction complete; removing intermediate TAR archive: $tarArchive"
    Remove-Item -LiteralPath $tarArchive
} else {
    Write-Host "Reusing existing SDK at $InstallDir; skipping download and extraction. SDK validation follows."
}
Write-Host '::endgroup::'

Write-Host '::group::[3/6] Inspect LLVM SDK version, headers and dependencies'
$version = (Invoke-Checked $llvmConfig @('--version')).Trim()
if ($version -ne '21.1.8') {
    throw "Expected LLVM 21.1.8, got $version"
}
if (-not (Test-Path -LiteralPath (Join-Path $InstallDir 'include/llvm-c/Core.h'))) {
    throw 'The LLVM SDK is missing its development headers.'
}
Write-Host "LLVM version verified: $version; development header llvm-c/Core.h found."
$libDir = (Invoke-Checked $llvmConfig @('--libdir')).Trim()
$systemLibraries = ((Invoke-Checked $llvmConfig @('--system-libs', '--link-static')) -join ' ') -split '\s+' |
    Where-Object { $_ }
Write-Host "LLVM library directory: $libDir"
Write-Host "Required system libraries: $($systemLibraries -join ' ')"
Write-Host '::endgroup::'

Write-Host '::group::[4/6] Provide libxml2 if required'
# Official Windows archives can omit libxml2s.lib even though llvm-config requires it:
# https://github.com/llvm/llvm-project/issues/172890
$xmlLibrary = Join-Path $libDir 'libxml2s.lib'
if (($systemLibraries -contains 'libxml2s.lib') -and -not (Test-Path -LiteralPath $xmlLibrary)) {
    Write-Host 'LLVM requires libxml2s.lib, but it is missing; building libxml2 2.9.12 with LLVM release settings.'
    $archive = Join-Path $workDir 'libxml2.tar.gz'
    Get-VerifiedArchive `
        -Uri 'https://github.com/GNOME/libxml2/archive/refs/tags/v2.9.12.tar.gz' `
        -Path $archive `
        -Sha256 '8a4ddd706419c210b30b8978a51388937fd9362c34fc9a3d69e4fcc6f8055ee0'
    Write-Host "Extracting libxml2 sources into $workDir"
    Invoke-Checked tar.exe @('-xf', $archive, '-C', $workDir)

    # Match do_build_libxml in LLVM 21.1.8's llvm/utils/release/build_llvm_release.bat.
    $disabledFeatures = @(
        'C14N', 'CATALOG', 'DEBUG', 'DOCB', 'FTP', 'HTML', 'HTTP', 'ICONV', 'ICU',
        'ISO8859X', 'LEGACY', 'LZMA', 'MEM_DEBUG', 'MODULES', 'PATTERN', 'PROGRAMS',
        'PUSH', 'PYTHON', 'READER', 'REGEXPS', 'RUN_DEBUG', 'SAX1', 'SCHEMAS',
        'SCHEMATRON', 'TESTS', 'THREAD_ALLOC', 'VALID', 'WRITER', 'XINCLUDE', 'XPATH',
        'XPTR', 'ZLIB'
    )
    $buildDir = Join-Path $workDir 'libxml2-build'
    $cmakeArgs = @(
        '-S', (Join-Path $workDir 'libxml2-2.9.12'),
        '-B', $buildDir, '-G', 'Ninja',
        '-DCMAKE_C_COMPILER=cl', '-DCMAKE_BUILD_TYPE=Release',
        '-DCMAKE_MSVC_RUNTIME_LIBRARY=MultiThreaded', '-DBUILD_SHARED_LIBS=OFF',
        '-DLIBXML2_WITH_OUTPUT=ON', '-DLIBXML2_WITH_THREADS=ON', '-DLIBXML2_WITH_TREE=ON'
    )
    $cmakeArgs += $disabledFeatures | ForEach-Object { "-DLIBXML2_WITH_$_=OFF" }
    Write-Host 'Configuring libxml2: Release, static library, static MSVC CRT, Ninja generator.'
    Invoke-Checked cmake $cmakeArgs
    Write-Host 'Building libxml2 with 2 parallel jobs.'
    Invoke-Checked cmake @('--build', $buildDir, '--target', 'LibXml2', '--parallel', '2')
    Write-Host "Installing compiled library: $xmlLibrary"
    Copy-Item -LiteralPath (Join-Path $buildDir 'libxml2s.lib') -Destination $xmlLibrary
} elseif ($systemLibraries -contains 'libxml2s.lib') {
    Write-Host "libxml2s.lib already exists at $xmlLibrary; skipping build."
} else {
    Write-Host 'llvm-config does not require libxml2s.lib; skipping build.'
}
Write-Host '::endgroup::'

Write-Host '::group::[5/6] Verify LLVM and system libraries'
# Fail before Cargo if the archive is incomplete or the SDK environment is wrong.
$llvmLibraries = ((Invoke-Checked $llvmConfig @('--libnames', '--link-static')) -join ' ') -split '\s+' |
    Where-Object { $_ }
foreach ($library in $llvmLibraries) {
    if (-not (Test-Path -LiteralPath (Join-Path $libDir $library))) {
        throw "LLVM library is missing: $library"
    }
}
Write-Host "Verified $(@($llvmLibraries).Count) LLVM libraries in $libDir"
$searchPaths = @($libDir) + ($env:LIB -split ';' | Where-Object { $_ })
foreach ($library in $systemLibraries) {
    $found = $false
    foreach ($directory in $searchPaths) {
        if (Test-Path -LiteralPath (Join-Path $directory $library)) {
            Write-Host "Found system library: $(Join-Path $directory $library)"
            $found = $true
            break
        }
    }
    if (-not $found) {
        throw "LLVM system library is missing: $library"
    }
}
Write-Host "LLVM $version SDK verified at $InstallDir"
Write-Host "Verified $(@($systemLibraries).Count) system libraries."
Write-Host '::endgroup::'

Write-Host '::group::[6/6] Export build environment'
$env:LLVM_SYS_211_PREFIX = $InstallDir
$env:LLVM_CONFIG_PATH = $llvmConfig
$env:PATH = "$(Join-Path $InstallDir 'bin');$env:PATH"
$env:LIB = "$libDir;$env:LIB"
# The official LLVM build and the matching libxml2 build use the static MSVC CRT.
# llvm-sys then requests static system libraries, which rustc must locate before
# invoking link.exe. rustc needs explicit -L paths, not just link.exe's LIB.
$rustFlags = @('-C', 'target-feature=+crt-static')
foreach ($directory in ($searchPaths | Select-Object -Unique)) {
    $rustFlags += "-Lnative=$directory"
}
# Cargo's unit-separator encoding keeps SDK paths containing spaces intact.
$env:CARGO_ENCODED_RUSTFLAGS = $rustFlags -join [char]0x1f
Write-Host "LLVM_SYS_211_PREFIX=$env:LLVM_SYS_211_PREFIX"
Write-Host "LLVM_CONFIG_PATH=$env:LLVM_CONFIG_PATH"
Write-Host "Rust compiler arguments: $($rustFlags -join ' ')"
Write-Host 'Prepended LLVM bin to PATH and LLVM lib to LIB; preserved MSVC environment.'

if ($env:GITHUB_ENV) {
    Write-Host 'Exporting PATH, INCLUDE, LIB, LIBPATH, LLVM_SYS_211_PREFIX, LLVM_CONFIG_PATH and CARGO_ENCODED_RUSTFLAGS for subsequent Actions steps.'
    foreach ($name in @('PATH', 'INCLUDE', 'LIB', 'LIBPATH', 'LLVM_SYS_211_PREFIX', 'LLVM_CONFIG_PATH', 'CARGO_ENCODED_RUSTFLAGS')) {
        "$name=$([Environment]::GetEnvironmentVariable($name))" |
            Out-File -FilePath $env:GITHUB_ENV -Encoding utf8 -Append
    }
} else {
    Write-Host 'GITHUB_ENV is not set; environment changes apply only to the current PowerShell process.'
}
Write-Host '::endgroup::'
Write-Host 'Windows LLVM SDK setup completed successfully; ready to run Cargo.'
