TARGET=plmidi
CFLAGS+=-fPIC -Wall
LIBS="-ld-options,-framework CoreMIDI -framework CoreServices"

SOBJ=$(PACKSODIR)/$(TARGET).$(SOEXT)

all:  $(SOBJ)

$(SOBJ): c/$(TARGET).o
   mkdir -p $(PACKSODIR)
   $(LD) $(LDSOFLAGS) -o $@ $(SWISOLIB) $< $(LIBS)
   strip -x $@

check::
install::
clean:
	rm -f c/$(TARGET).o
distclean: clean
   rm -f $(SOBJ)

install-me:
	swipl -f none -g "pack_install(.,[upgrade(true)]), halt"
 
