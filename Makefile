LISP:="sbcl"
BA:=buildapp
QUICK_LISP?=$(HOME)/quicklisp/

ifeq ($(shell [ -f $(QUICK_LISP)/setup.lisp ] && echo exists),)
$(error The QUICK_LISP environment variable must point to your quicklisp install)
endif

BUILD_APP_FLAGS=--manifest-file $(QUICK_LISP)/local-projects/system-index.txt \
	--asdf-tree $(QUICK_LISP)/dists/quicklisp/software \
	--load-system join-exe

all: join
.PHONY:  clean

join: join.lisp join-exe.lisp
	$(BA) $(BUILD_APP_FLAGS) --output $@ --entry "join-exe:main"

clean:
	rm -f join *.fasl *.lx32fsl
