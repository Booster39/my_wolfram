##
## EPITECH PROJECT, 2022
## B-FUN-400-PAR-4-1-wolfram-abdoulaye.doucoure
## File description:
## Makefile
##

BINARY_PATH := $(shell stack path --local-install-root)

NAME    =   wolfram


all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean:
	stack purge

re: fclean all

.PHONY: all clean fclean re