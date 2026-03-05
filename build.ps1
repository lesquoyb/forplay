#!/usr/bin/env pwsh
# build.ps1 - Build script for ForPlay
# Usage: .\build.ps1

$ErrorActionPreference = "Stop"

$PROJECT_DIR = Split-Path -Parent $MyInvocation.MyCommand.Path
$BUILD_DIR   = Join-Path $PROJECT_DIR "build"
$SRC_DIR     = Join-Path $PROJECT_DIR "src"
$SDL2_DIR    = Join-Path $PROJECT_DIR "deps\fortran-sdl2"
$SOCKET_DIR  = Join-Path $PROJECT_DIR "deps\fortran-socket"

$FC = "gfortran"

# SDL2 flags for MSYS2/MinGW
$SDL2_CFLAGS = "-IC:\msys64\mingw64\include\SDL2"
$SDL2_LIBS   = "-LC:\msys64\mingw64\lib -lmingw32 -lSDL2main -lSDL2 -lSDL2_ttf"

# Socket lib (Windows needs ws2_32)
$SOCKET_LIBS = "-lws2_32"

# Fortran-sdl2 source files (in dependency order)
$SDL2_SOURCES = @(
    "$SDL2_DIR\src\sdl2\sdl2_c_types.F90"
    "$SDL2_DIR\src\sdl2\sdl2_stdinc.f90"
    "$SDL2_DIR\src\sdl2\sdl2_audio.f90"
    "$SDL2_DIR\src\sdl2\sdl2_blendmode.f90"
    "$SDL2_DIR\src\sdl2\sdl2_cpuinfo.f90"
    "$SDL2_DIR\src\sdl2\sdl2_gamecontroller.f90"
    "$SDL2_DIR\src\sdl2\sdl2_error.f90"
    "$SDL2_DIR\src\sdl2\sdl2_events.f90"
    "$SDL2_DIR\src\sdl2\sdl2_filesystem.f90"
    "$SDL2_DIR\src\sdl2\sdl2_hints.f90"
    "$SDL2_DIR\src\sdl2\sdl2_joystick.f90"
    "$SDL2_DIR\src\sdl2\sdl2_keyboard.f90"
    "$SDL2_DIR\src\sdl2\sdl2_log.f90"
    "$SDL2_DIR\src\sdl2\sdl2_messagebox.f90"
    "$SDL2_DIR\src\sdl2\sdl2_rect.f90"
    "$SDL2_DIR\src\sdl2\sdl2_pixels.f90"
    "$SDL2_DIR\src\sdl2\sdl2_platform.f90"
    "$SDL2_DIR\src\sdl2\sdl2_scancode.f90"
    "$SDL2_DIR\src\sdl2\sdl2_surface.f90"
    "$SDL2_DIR\src\sdl2\sdl2_render.f90"
    "$SDL2_DIR\src\sdl2\sdl2_keycode.f90"
    "$SDL2_DIR\src\sdl2\sdl2_mouse.f90"
    "$SDL2_DIR\src\sdl2\sdl2_rwops.f90"
    "$SDL2_DIR\src\sdl2\sdl2_thread.f90"
    "$SDL2_DIR\src\sdl2\sdl2_timer.f90"
    "$SDL2_DIR\src\sdl2\sdl2_version.f90"
    "$SDL2_DIR\src\sdl2\sdl2_video.f90"
    "$SDL2_DIR\src\sdl2\sdl2_opengl.f90"
    "$SDL2_DIR\src\sdl2.f90"
    "$SDL2_DIR\src\sdl2_ttf.f90"
)

# Fortran-socket source files
$SOCKET_SOURCES = @(
    "$SOCKET_DIR\src\constants.f90"
    "$SOCKET_DIR\src\socket_lib.f90"
)

# Our source files (in dependency order)
$APP_SOURCES = @(
    "$SRC_DIR\maze.f90"
    "$SRC_DIR\game.f90"
    "$SRC_DIR\network.f90"
    "$SRC_DIR\main.f90"
)

# Create build directory
if (-not (Test-Path $BUILD_DIR)) {
    New-Item -ItemType Directory -Path $BUILD_DIR | Out-Null
}

Write-Host "=== Building ForPlay ===" -ForegroundColor Cyan
Write-Host ""

# Step 1: Compile SDL2 bindings
Write-Host "[1/4] Compiling SDL2 bindings..." -ForegroundColor Yellow
$sdl2_args = @("-c", $SDL2_CFLAGS, "-J$BUILD_DIR") + $SDL2_SOURCES
Push-Location $BUILD_DIR
& $FC $sdl2_args
if ($LASTEXITCODE -ne 0) { Pop-Location; throw "Failed to compile SDL2 bindings" }
Pop-Location
Write-Host "  OK" -ForegroundColor Green

# Step 2: Compile socket library
Write-Host "[2/4] Compiling socket library..." -ForegroundColor Yellow
$socket_args = @("-c", "-cpp", "-D_WIN32", "-I$SOCKET_DIR\src", "-J$BUILD_DIR") + $SOCKET_SOURCES
Push-Location $BUILD_DIR
& $FC $socket_args
if ($LASTEXITCODE -ne 0) { Pop-Location; throw "Failed to compile socket library" }
Pop-Location
Write-Host "  OK" -ForegroundColor Green

# Step 3: Compile application
Write-Host "[3/4] Compiling application..." -ForegroundColor Yellow
$app_args = @("-c", "-cpp", "-D_WIN32", "-I$BUILD_DIR", "-J$BUILD_DIR") + $APP_SOURCES
Push-Location $BUILD_DIR
& $FC $app_args
if ($LASTEXITCODE -ne 0) { Pop-Location; throw "Failed to compile application" }
Pop-Location
Write-Host "  OK" -ForegroundColor Green

# Step 4: Link everything
Write-Host "[4/4] Linking..." -ForegroundColor Yellow
$obj_files = Get-ChildItem "$BUILD_DIR\*.o" | ForEach-Object { $_.FullName }
$link_args = @("-o", "$BUILD_DIR\forplay.exe") + $obj_files + $SDL2_LIBS.Split(" ") + $SOCKET_LIBS.Split(" ")
& $FC $link_args
if ($LASTEXITCODE -ne 0) { throw "Failed to link" }
Write-Host "  OK" -ForegroundColor Green

Write-Host ""
Write-Host "=== Build successful! ===" -ForegroundColor Green
Write-Host "Executable: $BUILD_DIR\forplay.exe"
Write-Host ""
Write-Host "Run with:  .\build\forplay.exe"
