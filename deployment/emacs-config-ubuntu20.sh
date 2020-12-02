#!/bin/bash

../emacs/configure CC=clang \
		   --with-libotf LIBOTF_LIBS="$(pkg-config --libs libotf)" LIBOTF_CFLAGS="$(pkg-config --cflags libotf)" \
		   --with-harfbuzz HARFBUZZ_LIBS="$(pkg-config --libs harfbuzz)" HARFBUZZ_CFLAGS="$(pkg-config --cflags harfbuzz)" \
		   --with-zlib --with-compress-install --with-gpm --with-threads \
		   --with-mailutils --with-json --with-modules --prefix=/usr/local \
		   --with-included-regex --with-xpm --with-jpeg --with-tiff --with-gif \
		   --with-png --with-rsvg --with-imagemagick --with-xft \
		   --with-dbus \
		   --with-x-toolkit=lucid \
		   --without-pop --with-mailutils \
		   --without-toolkit-scroll-bars

## Build steps
# make -j$(nproc --ignore=2)
## Package steps
# make prefix=~/emacs-install install
# tar cJvf ~/emacs-install.tar.xz -C ~/emacs-install .
## Prepare uninstall script
# echo "#!/bin/sh" > ~/emacs-remove.sh
# echo "INSTALL_PREFIX=${PREFIX:=/usr/local}" >> ~/emacs-remove.sh
# find ~/emacs-install/ -type f -printf 'rm ${PREFIX}/%P\n' >> ~/emacs-remove.sh
# find ~/emacs-install/ -mindepth 1 -type d -printf 'rmdir ${PREFIX}/%P\n' | sort -r  >> ~/emacs-remove.sh
## Install
# sudo tar -xJv --no-same-owner -f ~/emacs-install.tar.xz -C /usr/local/
## Remove steps
# sudo PREFIX=/usr/local bash ~/emacs-remove.sh

# --without-gconf --without-gsettings \
#../emacs/configure --with-modules  --without-gpm 
