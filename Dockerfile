From ubuntu:trusty

COPY llvm-checking /

COPY stack-2.5.1-linux-x86_64.tar.gz  /


COPY z3-4.8.1.016872a5e0f6-x64-ubuntu-14.04.zip /


RUN apt-get update && apt-get install -y clang-3.3 llvm-3.3 llvm-3.3-dev llvm-3.3-runtime libllvm3.3 openssh-server xz-utils  unzip graphviz  git zlib1g.dev build-essential libgmp-dev libtinfo-dev


RUN cd / && unzip stack-2.5.1-linux-x86_64.tar.gz    && unzip z3-4.8.1.016872a5e0f6-x64-ubuntu-14.04.zip 


RUN ln -s  /stack-2.5.1-linux-x86_64/bin/stack /bin/stack && ln -s /z3-4.8.1.016872a5e0f6-x64-ubuntu-14.04 /bin/z3


RUN cd /llvm-checking && stack build


