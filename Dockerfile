From ubuntu:trusty

RUN mkdir /root/.stack/

COPY config.yaml   /root/.stack/

COPY sources.list /etc/apt

COPY stack-2.5.1-linux-x86_64.tar.gz  /


COPY z3-4.8.1.016872a5e0f6-x64-ubuntu-14.04.zip /


RUN apt-get update && apt-get install -y  wget python-pip clang-3.3 llvm-3.3 llvm-3.3-dev llvm-3.3-runtime libllvm3.3 openssh-server xz-utils  unzip graphviz  git zlib1g.dev build-essential libgmp-dev libtinfo-dev



RUN cd / && wget https://picture-1257147077.cos.ap-shanghai.myqcloud.com/llvm-checking.zip && tar -zxvf stack-2.5.1-linux-x86_64.tar.gz    && unzip z3-4.8.1.016872a5e0f6-x64-ubuntu-14.04.zip  && unzip llvm-checking.zip

RUN  ln -s  /stack-2.5.1-linux-x86_64/stack /bin/stack && ln -s /z3-4.8.1.016872a5e0f6-x64-ubuntu-14.04/bin/z3  /bin/z3 && pip install wllvm


RUN cd /llvm-checking && stack build


