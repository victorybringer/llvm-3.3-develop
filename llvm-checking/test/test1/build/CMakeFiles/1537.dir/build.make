# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.9

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /opt/cmake-3.9.1/bin/cmake

# The command to remove a file.
RM = /opt/cmake-3.9.1/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test/build

# Include any dependencies generated for this target.
include CMakeFiles/1537.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/1537.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/1537.dir/flags.make

CMakeFiles/1537.dir/1537.cpp.o: CMakeFiles/1537.dir/flags.make
CMakeFiles/1537.dir/1537.cpp.o: ../1537.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/1537.dir/1537.cpp.o"
	/usr/local/bin/wllvm++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/1537.dir/1537.cpp.o -c /zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test/1537.cpp

CMakeFiles/1537.dir/1537.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/1537.dir/1537.cpp.i"
	/usr/local/bin/wllvm++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test/1537.cpp > CMakeFiles/1537.dir/1537.cpp.i

CMakeFiles/1537.dir/1537.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/1537.dir/1537.cpp.s"
	/usr/local/bin/wllvm++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test/1537.cpp -o CMakeFiles/1537.dir/1537.cpp.s

CMakeFiles/1537.dir/1537.cpp.o.requires:

.PHONY : CMakeFiles/1537.dir/1537.cpp.o.requires

CMakeFiles/1537.dir/1537.cpp.o.provides: CMakeFiles/1537.dir/1537.cpp.o.requires
	$(MAKE) -f CMakeFiles/1537.dir/build.make CMakeFiles/1537.dir/1537.cpp.o.provides.build
.PHONY : CMakeFiles/1537.dir/1537.cpp.o.provides

CMakeFiles/1537.dir/1537.cpp.o.provides.build: CMakeFiles/1537.dir/1537.cpp.o


# Object files for target 1537
1537_OBJECTS = \
"CMakeFiles/1537.dir/1537.cpp.o"

# External object files for target 1537
1537_EXTERNAL_OBJECTS =

lib1537.a: CMakeFiles/1537.dir/1537.cpp.o
lib1537.a: CMakeFiles/1537.dir/build.make
lib1537.a: CMakeFiles/1537.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX static library lib1537.a"
	$(CMAKE_COMMAND) -P CMakeFiles/1537.dir/cmake_clean_target.cmake
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/1537.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/1537.dir/build: lib1537.a

.PHONY : CMakeFiles/1537.dir/build

CMakeFiles/1537.dir/requires: CMakeFiles/1537.dir/1537.cpp.o.requires

.PHONY : CMakeFiles/1537.dir/requires

CMakeFiles/1537.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/1537.dir/cmake_clean.cmake
.PHONY : CMakeFiles/1537.dir/clean

CMakeFiles/1537.dir/depend:
	cd /zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test /zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test /zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test/build /zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test/build /zyz/llvm-slicing/src/llvm-slicing/0.6/tests/project2/test/build/CMakeFiles/1537.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/1537.dir/depend

