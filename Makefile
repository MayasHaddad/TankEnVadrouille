#Votre nom sans espace (remplacer par _)
NAME = Amghar_Nassim_et_Haddad_Mayas

PUIOPATH = /net/x2/u/enseignant/fbobot/public/

ifneq "$(wildcard $(PUIOPATH)/*)" ""
OCAMLPATH = $(PUIOPATH)
else
OCAMLPATH = ""
endif

OB_OPT := -classic-display -tag pkg_graphics -tag pkg_unix \
	-tag pkg_sdl -tag pkg_sdl.sdlimage -tag pkg_sdl.sdlttf -tag pkg_sdl.sdlgfx

#Pour simplifier la semantique d'ocamlbuild...
OB_OPT := -no-links $(OB_OPT)

#À décommenter pour que les .annot soit générés. Dans emacs le
# raccourci ctrl-c ctrl-t affiche alors le type de l'expression
OB_OPT := -tag dtypes $(OB_OPT)

#Pour que les fichiers .output soit crée par ocamlyacc ocamllex
OB_OPT := -yaccflag "-v" -lexflag " -v" $(OB_OPT)

#Pour utiliser menhir à la place d'ocamlyacc
#OB_OPT := -use-menhir $(OB_OPT)

OCAMLBUILD = OCAMLPATH=$(OCAMLPATH) ocamlbuild $(OB_OPT)

.PHONY: comp test

all: comp

comp:
	@rm -f main
	$(OCAMLBUILD) main.native
	@ln -fs _build/main.native main

test:
	@rm -f test
	$(OCAMLBUILD) test.native
	@ln -fs _build/test.native test


clean:
	$(OCAMLBUILD) -clean
	rm -f $(DOC).docdir main

FILES = data/* *.ml* Makefile

export:
	rm -rf $(NAME)
	mkdir $(NAME)
	cp --parents -f $(FILES) $(NAME)
	tar cjf $(NAME).bz2 $(NAME)

#Just for me ;)
PROJECT_NAME = projet2011
PROJECT_FILES = data/* myocamlbuild.ml test.ml main.ml iO.ml iO.mli Makefile
export_sujet:
	rm -rf $(PROJECT_NAME) $(PROJECT_NAME).bz2 $(PROJECT_NAME).zip
	mkdir $(PROJECT_NAME)
	cp --parents -f $(PROJECT_FILES) $(PROJECT_NAME)
	tar cjf $(PROJECT_NAME).bz2 $(PROJECT_NAME)
	zip -r $(PROJECT_NAME).zip $(PROJECT_NAME)
