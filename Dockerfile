FROM rust:latest

RUN apt-get update && apt-get install -y \
    llvm-19-dev \
    libpolly-19-dev \
    clang \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

ENV LLVM_SYS_191_PREFIX=/usr/lib/llvm-19

WORKDIR /workspace

CMD ["/bin/bash"]
