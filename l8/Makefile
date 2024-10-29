CXX = g++
CXXFLAGS = -Wall -std=c++11

all:
	-$(MAKE) divide_ubsan
	-$(MAKE) divide_no_sanitize
	-$(MAKE) bounds_asan
	-$(MAKE) bounds_no_sanitize
	-$(MAKE) static_error_reject
	-$(MAKE) static_error_allow

# Part 1: Divide-by-Zero
divide_ubsan: CXXFLAGS += -fsanitize=undefined
divide_ubsan: divide_by_zero.cpp
	$(CXX) $(CXXFLAGS) -o divide_ubsan divide_by_zero.cpp

divide_no_sanitize: divide_by_zero.cpp
	$(CXX) $(CXXFLAGS) -o divide_no_sanitize divide_by_zero.cpp

# Part 2: Out-of-Bounds
bounds_asan: CXXFLAGS += -fsanitize=address
bounds_asan: out_of_bounds.cpp
	$(CXX) $(CXXFLAGS) -o bounds_asan out_of_bounds.cpp

bounds_no_sanitize: out_of_bounds.cpp
	$(CXX) $(CXXFLAGS) -o bounds_no_sanitize out_of_bounds.cpp

# Part 3: Static Error
static_error_reject: CXXFLAGS += -Werror -Wall -Wuninitialized
static_error_reject: static_error.cpp
	$(CXX) $(CXXFLAGS) -o static_error_reject static_error.cpp

static_error_allow: CXXFLAGS += -Wno-uninitialized
static_error_allow: static_error.cpp
	$(CXX) $(CXXFLAGS) -o static_error_allow static_error.cpp

# Clean up all generated binaries
clean:
	rm -f divide_ubsan divide_no_sanitize bounds_asan bounds_no_sanitize static_error_reject static_error_allow
