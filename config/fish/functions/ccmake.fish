function ccmake
    set executable (which cmake)

    set name (uname -n)

    switch $argv
    case "b"
        $executable --build build
        return
    case 'clean'
        $executable --build build --target clean
        set argv ''
    case "d"
        $executable -S . -B build -D CMAKE_CXX_COMPILER=$HOME/.local/llvm/bin/clang++ -D CMAKE_C_COMPILER=$HOME/.local/llvm/bin/clang -G Ninja
    case '*'
    end

    switch (basename $PWD)
    case "contour"

        if test $name = "bahamankolibri.lin.tuni.fi"
           set EXTRA -DCONTOUR_QT_VERSION=5
        end

        switch $argv
        case "fast"
                $executable -S . -B build -D CMAKE_CXX_COMPILER=$CXX  \
                -D CMAKE_BUILD_TYPE=Debug \
                -G Ninja $EXTRA

        case "release"
                $executable -S . -B build -D CMAKE_CXX_COMPILER=$CXX  \
                -D CMAKE_BUILD_TYPE=Release \
                -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX \
                $EXTRA \
                -G Ninja
        case '*'
                $executable -S . -B build -D CMAKE_CXX_COMPILER=$CXX  \
                -D CMAKE_BUILD_TYPE=Debug \
                -D PEDANTIC_COMPILER=ON \
                -D ENABLE_TIDY=ON \
                -D CONTOUR_TESTING=ON \
                -D LIBTERMINAL_LOG_TRACE=ON \
                -G Ninja $EXTRA

        end

    case "boxed-cpp"
        $executable -S . -B build -D CMAKE_CXX_COMPILER=$CXX \
                    -D PEDANTIC_COMPILER=ON \
                    -D ENABLE_TIDY=ON \
                    -D BOXED_CPP_TESTS=ON \
                    -G Ninja $argv

    case "kokkos"
         if test $name = "bahamankolibri.lin.tuni.fi"
            set EXTRA -DKokkos_ARCH_TURING75=ON
         end
	 $EXTRA
         switch $argv
         case "cuda"
              $executable -S . -B build -G Ninja \
              -D CMAKE_CXX_COMPILER=$CXX \
              -D CMAKE_C_COMPILER=$CC \
              -D CMAKE_BUILD_TYPE=Debug \
              -D Kokkos_ENABLE_SERIAL=ON \
              -D Kokkos_ENABLE_OPENMP=OFF \
              -D Kokkos_ENABLE_CUDA=ON \
              -D Kokkos_ENABLE_TESTS=ON \
              $EXTRA
         case "serial"
              $executable -S . -B build -G Ninja \
              -D CMAKE_CXX_COMPILER=$CXX \
              -D CMAKE_C_COMPILER=$CC \
              -D CMAKE_BUILD_TYPE=Debug \
              -D Kokkos_ENABLE_SERIAL=ON \
              -D Kokkos_ENABLE_OPENMP=ON \
              -D Kokkos_ENABLE_CUDA=OFF \
              -D Kokkos_ENABLE_TESTS=ON \
              $EXTRA
        case '*'
              $executable -S . -B build -G Ninja \
              -D CMAKE_CXX_COMPILER=$CXX \
              -D CMAKE_C_COMPILER=$CC \
              -D CMAKE_BUILD_TYPE=Debug \
              -D Kokkos_ENABLE_SERIAL=ON \
              -D Kokkos_ENABLE_OPENMP=ON \
              -D Kokkos_ENABLE_CUDA=ON \
              -D Kokkos_ENABLE_TESTS=ON \
              $EXTRA
        end

    case "llvm-project"
         $executable -S llvm -B build \
         -G Ninja \
         -D CMAKE_BUILD_TYPE=Release \
         -D LLVM_PARALLEL_LINK_JOBS=2 \
         -D LLVM_ENABLE_PROJECTS="clang;clang-tools-extra;lld;lldb;openmp;" \
         -D LLVM_ENABLE_RUNTIMES=all \
         -D LLVM_TARGETS_TO_BUILD="X86;NVPTX" $argv

    case "cpython"
         ./configure --enable-optimizations --prefix=$HOME/.local
         make -j
         make test
         make install
         $HOME/.local/bin/python -m ensurepip --upgrade
    case '*'
        $executable -S . -B build -D CMAKE_CXX_COMPILER=$CXX -D CMAKE_C_COMPILER=$CC -D CMAKE_INSTALL_PREFIX=$LOC_HOME -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON  $argv
        return
    end
end
