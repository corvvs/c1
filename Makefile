NAME		:= computor

.PHONY:		all
all:		$(NAME)

$(NAME): build
	stack install

.PHONY:		build
build:		
	stack build

.PHONY:		clean
clean:
	stack clean	

.PHONY:		fclean
fclean:		clean
	$(RM) $(NAME)

.PHONY:		re
re:			fclean all
