.SECONDARY: .done/haskell .done/manual
.PHONY: all clean dialects format manual

HASKELL_SOURCES := $(shell find src/ -type f -name '*.hs' -or -name '*.lhs')
MANUAL_SOURCES  := $(shell find ./doc/ -type f)
MANUAL_OUTPUTS  := $(foreach src,$(MANUAL_SOURCES),$(subst ./doc/,./_site/,$(subst .pro,.html,$(src))))

CABAL_INSTALL ?= cabal v2-install --installdir=bin --overwrite-policy=always

#
# PHONY RULES
#

all: dialects manual

clean: 
	cabal v2-clean	
	rm -rf ./bin ./_site ./_cache ./dist

dialects: bin/prosidy-manual bin/prosidy-markup

format: bin/brittany
	@bash ./scripts/format

manual: $(MANUAL_OUTPUTS)

#
# Haskell executables
#

bin/prosidy-manual bin/prosidy-markup: .done/haskell

bin/brittany:
	$(CABAL_INSTALL) brittany

.done/haskell: $(HASKELL_SOURCES)
	$(CABAL_INSTALL) exe:prosidy-manual exe:prosidy-markup
	@mkdir -p .done && touch $@

#
# Manual output
#

$(MANUAL_OUTPUTS): .done/manual

.done/manual: bin/prosidy-manual $(MANUAL_SOURCES)
	bin/prosidy-manual rebuild
	@mkdir -p .done && touch $@
