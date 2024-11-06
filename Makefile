SRC_DIR		:= srcs
OBJ_DIR		:= objs
FILES		:=\
				Lexer.hs\
				main.hs\

SRCS		:=	$(FILES:%.hs=$(SRC_DIR)/%.hs)
OBJS		:=	$(FILES:%.hs=$(OBJ_DIR)/%.o)
NAME		:= computor

GHC 		:= ghc
GHC_FLAGS	:= -c -i$(SRC_DIR)

.PHONY:		all
all:		$(NAME)

$(NAME):	$(OBJS)
	$(GHC) -o $(NAME) $(OBJS)

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
