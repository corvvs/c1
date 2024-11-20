NAME		:= computor

.PHONY:		all
all:		$(NAME)

$(NAME): build
	stack install

.PHONY:		build
build:		
	stack build --install-ghc

.PHONY:		clean
clean:
	stack clean	

.PHONY:		fclean
fclean:		clean
	$(RM) $(NAME)

.PHONY:		re
re:			fclean all

.PHONY:	up
up:
	docker-compose up --build -d

.PHONY:	down
down:
	docker-compose down

.PHONY:	it
it:
	docker-compose exec app bash
