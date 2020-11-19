# 项目简介

#### llvm-checking：基于LLVM的C/C++代码安全检查工具

llvm-checking是基于[LLVM](https://www.llvm.org/) 和我们之前项目[llvm-slicing](https://gitee.com/nuptPisa/llvm-slicing), 致力于通过LLVM中间表示语言IR智能分析和检查前端语言（如C/C++）代码规范性与安全性等。

#### 软件架构
基于单子的分布式架构


#### 安装说明
本项目核心代码的开发语言是函数式语言[Haskell](https://www.haskell.org/)，所依赖的Haskell开源库包括（可通过`cabal install`安装）：
1.  [llvm-analysis](http://hackage.haskell.org/package/llvm-analysis)

  由于该库在[Hackage](http://hackage.haskell.org/)上很久没有更新了，现如需安装，建议直接下载并依次安装本仓库[lib/llvm-analysis](./lib/llvm-analysis/)文件夹中经微调适配后的库：[llvm-base-types](./lib/llvm-analysis/llvm-base-types-0.3.zip),[llvm-data-interop](./lib/llvm-analysis/llvm-data-interop-0.3.zip),[llvm-analysis](./lib/llvm-analysis/llvm-analysis-0.3.zip)，以及[llvm-tools](./lib/llvm-analysis/llvm-tools-0.2.0.1.zip)（该库也可用于测试）。

2.  临时依赖库[foreign-inference](https://github.com/travitch/foreign-inference)（后续可能移去或替换）

   同样地，建议直接下载安装[lib/foreign-inference](.lib/foreign-inference/)文件夹中相关库：svm-simple, hbgl-experimental, archive-inspection，[foreign-inference](./lib/foreign-inference/foreign-inference-0.3.zip)，可用其中iiglue测试。

3.  Python库：[whole-program-llvm](https://github.com/travitch/whole-program-llvm)

   该库主要是将C/C++工程项目（多个文件）编译并抽取成一个整体llvm的bitcode文件，方便后续分析。
   也可以直接通过pip命令安装： `pip install wllvm`  

4.  本版本0.3的可支持llvm 3.0-3.3版本，如需更高版本的可参见0.3.5分支（可支持到llvm-3.5)。

#### 使用说明

  **仅供组内开发和调试，禁止外传或公开！**    
因本私有仓库还在倾力开发中，部分代码库还没有移植/替换过来，所以目前仅供项目组成员内部开发测试使用，禁止外传或Fork成公有仓库，切记！


#### 参与贡献

  1.  如需Fork，也请Fork成个人私有仓库；
  2.  先大家尽量push修改到develop分支，后面大家统一无冲突后再合并到master分支；
  3.  也可在其他的或个人新建的分支进行临时调试或测验使用。


#### 其他

  1.  ToDo Lists 

