FROM ocaml/opam:alpine-ocaml-4.14

RUN opam repo add rocq-released https://rocq-prover.org/opam/released --all-switches &&\
    opam install --deps-only coq-compcert --yes &&\
    opam install -b coq-compcert --yes &&\
    opam install rocq-elpi --yes
