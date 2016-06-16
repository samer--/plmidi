TARGET=plmidi
CFLAGS+=-fPIC -Wall
FRAMEWORKS=-framework CoreMIDI -framework CoreServices -framework IOKit

SOBJ=$(PACKSODIR)/$(TARGET).$(SOEXT)

all:  $(SOBJ)

$(SOBJ): c/$(TARGET).o
	mkdir -p $(PACKSODIR)
	$(LD) $(FRAMEWORKS) $(LDSOFLAGS) -o $@ $(SWISOLIB) $<
	strip -x $@

check::
install::
clean:
	rm -f c/$(TARGET).o
distclean: clean
	rm -f $(SOBJ)

install-me:
	swipl -f none -g "pack_install(.,[upgrade(true)]), halt"

publish:
	swipl -f none -g "pack_property(plmidi,download(D)), pack_install(D,[upgrade(true),interactive(false)]), halt"
 
