# Maze Generator

<img src="icon.png" alt="Maze Generator Icon" width="128"/>

A simple maze generator built with OCaml and SDL2. This project serves as a good example of how to build a graphical application in OCaml, including handling dependencies and creating a containerized build with Docker.

## File Structure

```
.
├── bin
│   ├── benchmarks
│   │   ├── a_star.ml
│   │   ├── bfs.ml
│   │   ├── dfs.ml
│   │   └── dijkstras.ml
│   ├── dune
│   ├── generators
│   └── main.ml
├── Dockerfile
├── dune-project
├── icon.png
├── lib
│   └── dune
├── README.md
└── test
    ├── dune
    └── test_Maze.ml
```

## Getting Started

### Prerequisites

*   OCaml (version 5.1.0 or later)
*   Dune (version 3.1 or later)
*   OPAM (OCaml Package Manager)
*   SDL2 and SDL2_image libraries

On macOS, you can install the dependencies with Homebrew:
```bash
brew install sdl2 sdl2_image
```

On Debian/Ubuntu:
```bash
sudo apt-get install libsdl2-dev libsdl2-image-dev
```

### Building and Running Locally

1.  **Install OCaml dependencies:**
    ```bash
    opam install . --deps-only -y
    ```

2.  **Build and run the application:**
    ```bash
    dune exec bin/main.exe
    ```

### Building and Running with Docker

1.  **Build the Docker image:**
    ```bash
    docker build -t maze-app .
    ```

2.  **Run the container:**
    The command to run the container varies by operating system.

    **On Linux:**
    ```bash
    xhost +local:docker
    docker run --rm -e DISPLAY=$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix maze-app
    ```

    **On macOS (with XQuartz):**
    ```bash
    docker run --rm -e DISPLAY=host.docker.internal:0 maze-app
    ```
