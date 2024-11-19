SRC_DIR		:= srcs
OBJ_DIR		:= objs
FILES		:=\
				TypeClass.hs\
				MyPrint.hs\
				Lexer.hs\
				AST.hs\
				Parser.hs\
				PolynomialBase.hs\
				Polynomial.hs\
				Algebra.hs\
				Solver.hs\
				main.hs\

SRCS		:=	$(FILES:%.hs=$(SRC_DIR)/%.hs)
OBJS		:=	$(FILES:%.hs=$(OBJ_DIR)/%.o)
NAME		:= computor

GHC 		:= ghc
GHC_FLAGS	:= -c -i$(SRC_DIR) -package containers
LD_FLAGS	:= -package containers

.PHONY:		all
all:		$(NAME)

$(NAME):	$(OBJS)
	$(GHC) $(LD_FLAGS) -o $(NAME) $(OBJS)

$(OBJ_DIR)/%.o:	$(SRC_DIR)/%.hs
	@mkdir -p $(OBJ_DIR)
	$(GHC) $(GHC_FLAGS) $< -o $@

.PHONY:		clean
clean:
	rm -rf $(OBJ_DIR) $(SRC_DIR)/*.hi $(TARGET)

.PHONY:		fclean
fclean:		clean
	rm -f $(NAME)

.PHONY:		re
re:			fclean all
