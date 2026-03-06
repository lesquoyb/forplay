# ForPlay

Asymmetric multiplayer maze game over the network, written in Fortran with SDL2.
## The story

**THE LABYRINTH OF SHADOWS**

Somewhere beneath the crumbling ruins of an ancient fortress lies a maze no map has ever captured… a labyrinth where light dies, echoes lie, and every step could be your last.

You awaken alone in total darkness.

The air is cold. The stone walls are damp. Somewhere in the distance, something moves.

Are you **the Lost One** — a desperate soul trapped in a pitch-black maze, feeling your way along ancient walls, listening for danger, searching for the one path that leads to freedom? Your heart pounds with every sound… because you are **not alone**.

Or are you **the Master of the Maze** — slow, silent, and patient. You know every corridor, every hidden passage, every deadly trap. The labyrinth is your kingdom, and the intruder is your prey. Set snares, close passages, and hunt the wanderer who dares invade your domain.

In the darkness, only one will prevail.

**Escape… or be captured.**
**Survive… or become part of the labyrinth forever.**


## Gameplay

Two players compete in a randomly generated maze. The roles are asymmetric:

- **Hider** (host, red) — sees the full maze layout but has limited vision of items and the opponent. Goal: evade the seeker.
- **Seeker** (client, blue) — full fog of war (only sees nearby cells). Goal: find and reach the hider.

The game is **turn-based**. Each turn, the active player can:

- **Move** one cell (arrow keys)
- **Use an item** from their inventory (keys 1-6)
- **Pass** their turn (Space)

The hider goes first, then the seeker, alternating. The game ends when the seeker reaches the hider's cell.

### Items

Items are scattered on the maze floor. A player picks them up automatically by walking over them (max inventory: 6).

| Item | Letter | Effect |
|------|--------|--------|
| **Dash** | D | Dash in a straight line until hitting a wall, reveals the path traversed. Requires choosing a direction. |
| **Vision** | V | Permanently increases vision radius by +1 to +3 (random). |
| **Light** | L | Reveals the entire map for the current turn. |
| **Speed** | S | +1 action per turn (permanent). |

### Configurable parameters

The host can adjust before launching:

- Maze dimensions (width × height, odd values recommended)
- Minimum spawn distance between players
- Number of each item type
- Number of actions per turn for each player (speed)

A **tooltip** is shown when hovering over each parameter to explain its effect.

### In-game controls

| Key | Action |
|-----|--------|
| Arrows | Move / choose item direction |
| 1-6 | Select an inventory item |
| Space | Pass turn |
| Escape | Quit game (confirmation required) |

## Local build

### Prerequisites

- **gfortran** (GCC Fortran) — tested with version 14
- **SDL2** and **SDL2_ttf** — development libraries
- **fpm** (Fortran Package Manager) — version 0.10.0 recommended

### Windows (MSYS2 / MinGW64)

```bash
# Install dependencies via MSYS2
pacman -S mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_ttf

# Install fpm (if not already present)
# Download from https://github.com/fortran-lang/fpm/releases

# Clone with submodules
git clone --recursive https://github.com/<user>/forplay.git
cd forplay

# Build
fpm build --flag "-D_WIN32"

# Run
fpm run --flag "-D_WIN32"
```

The executable is located in `build/gfortran_*/app/forplay.exe`.

### Linux

```bash
# Install dependencies (Debian/Ubuntu)
sudo apt-get install gfortran libsdl2-dev libsdl2-ttf-dev fonts-dejavu-core

# Install fpm
# https://github.com/fortran-lang/fpm/releases or via setup-fpm

# Clone and build
git clone --recursive https://github.com/<user>/forplay.git
cd forplay

# Patch fpm.toml for Linux link libraries
sed -i 's/^link = .*/link = ["SDL2", "SDL2_ttf"]/' fpm.toml

fpm build --profile release
fpm run
```

### macOS

```bash
# Install dependencies
brew install gcc sdl2 sdl2_ttf

# Install fpm from source (prebuilt binary may not work)
git clone --depth 1 --branch v0.10.0 https://github.com/fortran-lang/fpm.git /tmp/fpm-src
cd /tmp/fpm-src && FC=gfortran-14 bash install.sh --prefix=/usr/local && cd -

# Clone and build
git clone --recursive https://github.com/<user>/forplay.git
cd forplay

# Patch fpm.toml for macOS link libraries
sed -i '' 's/^link = .*/link = ["SDL2", "SDL2_ttf"]/' fpm.toml

BREW_PREFIX=$(brew --prefix)
fpm build --profile release --compiler gfortran-14 \
  --flag "-D__APPLE__ -L${BREW_PREFIX}/lib -I${BREW_PREFIX}/include"
```

> **Note:** the default `fpm.toml` contains Windows link libraries (`mingw32`, `SDL2main`, `ws2_32`…). On Linux and macOS, replace the `link = …` line with `["SDL2", "SDL2_ttf"]` before building.

## Building via GitHub Actions

The repository includes an automatic release workflow (`.github/workflows/release.yml`) that builds for **Windows**, **Linux** and **macOS**.

### Using on a fork

1. **Fork** the repository on GitHub
2. GitHub Actions are enabled automatically on the fork
3. To trigger a build:
   - **Manually**: go to the *Actions* tab → *Release* → *Run workflow*
   - **Automatically**: push a version tag:
     ```bash
     git tag v1.0.0
     git push origin v1.0.0
     ```
4. The workflow produces three archives:
   - `forplay-windows-x86_64.zip` — executable + SDL2/MinGW DLLs
   - `forplay-linux-x86_64.tar.gz`
   - `forplay-macos-arm64.tar.gz`
5. When triggered by a `v*` tag, a **GitHub Release** is created automatically with the three archives available for download.

Build artifacts are also available individually in the *Actions* tab of each workflow run, even without a tag.

## How to play online

1. The **host** player launches the game and picks *Host a game*
   - The screen shows their local IP, public IP and port (12345)
   - They configure the parameters and wait for a client to connect
2. The **client** player launches the game and picks *Join a game*
   - They enter the host's IP address and port, then connect
3. The host clicks *Start game* — the maze is generated and the game begins

> **LAN:** use the local IP.
> **Internet:** use the public IP. The host must forward port 12345 (TCP) on their router.

## Project structure

```
forplay/
├── app/main.f90          # Entry point, SDL2 UI, networking
├── src/
│   ├── game.f90          # Game logic (turns, items, visibility)
│   ├── maze.f90          # Maze generation, BFS
│   └── network.f90       # Network functions (server, client, IP)
├── deps/
│   ├── fortran-sdl2/     # SDL2 bindings for Fortran
│   └── fortran-socket/   # Socket bindings for Fortran
├── fpm.toml              # fpm build configuration
└── .github/workflows/
    └── release.yml       # Cross-platform CI/CD
```

## License

MIT — see [LICENSE](LICENSE).
