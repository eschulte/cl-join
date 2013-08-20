LISP:="sbcl"
CLC:=cl-launch
QUICK_LISP?=$(HOME)/quicklisp/
CL_SETUP=cl-launch-setup.lisp
CLFLAGS=--no-include --system join-exe --lisp $(LISP) --dump '!' -f $(CL_SETUP)

all: join
.PHONY:  clean

$(CL_SETUP):
	echo "(load \"$(QUICK_LISP)/setup.lisp\")" >$@

join: join.lisp join-exe.lisp $(CL_SETUP)
	$(CLC) $(CLFLAGS) --output $@ -r join-exe:main

clean:
	rm -f join *.fasl
