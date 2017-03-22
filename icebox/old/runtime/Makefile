# param
CC = gcc
FLAGS = \
	-Wall                            \
	-Wextra                          \
	-Wno-unused-parameter            \
	-Wno-sign-compare                \
	-Wno-pointer-sign                \
	-Wno-missing-field-initializers  \
	-Wformat=2                       \
	-Wstrict-aliasing=2              \
	-Wdisabled-optimization          \
	-Wfloat-equal                    \
	-Wpointer-arith                  \
	-Wdeclaration-after-statement    \
	-Wbad-function-cast              \
	-Wcast-align                     \
	-Wredundant-decls                \
	-Winline
INCLUDES = -I .
FLAGS += -g -O0 -D DEBUG
OUT = vl.out
SRC = $(wildcard *.c)
OBJS = $(SRC:%.c=%.o)

# make
all: ${OUT}
	./${OUT}

clean:
	rm -rf ${OUT} ${OBJS}

# depend
.c.o:
	${CC} ${FLAGS} ${INCLUDES} -c $<

${OUT}: ${OBJS}
	${CC} -o $@ ${OBJS}
