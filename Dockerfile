FROM ocaml/opam:ocaml-5.1-debian

RUN sudo apt-get update && sudo apt-get install -y libsdl2-dev libsdl2-image-dev

RUN opam install . --deps-only -y

WORKDIR /app

COPY . .

RUN opam exec -- dune build

CMD ["_build/default/bin/main.exe"] 