#!/bin/sh

set -v
set -e
cd "/home/anton/documents/SecretCloud2"
mkdir -p /tmp/debian-package-name-0.0.0.0
cp *.lpi /tmp/debian-package-name-0.0.0.0/
cp *.lpr /tmp/debian-package-name-0.0.0.0/
cp *.pas /tmp/debian-package-name-0.0.0.0/
cp *.lfm /tmp/debian-package-name-0.0.0.0/
cp *.ico /tmp/debian-package-name-0.0.0.0/


cd /tmp/debian-package-name-0.0.0.0
rm -rf DEBUILD
rm -f DEBUILD.sh

cd ..
tar czf debian-package-name_0.0.0.0.orig.tar.gz debian-package-name-0.0.0.0
mv debian-package-name-0.0.0.0 "/home/anton/documents/SecretCloud2/DEBUILD"
mv debian-package-name_0.0.0.0.orig.tar.gz "/home/anton/documents/SecretCloud2/DEBUILD"

cd "/home/anton/documents/SecretCloud2/DEBUILD/debian-package-name-0.0.0.0"
mkdir -p debian/source
echo "1.0" > debian/source/format
echo "8" > debian/compat
mv ../control debian/
mv ../rules debian/
chmod +x debian/rules
mv ../changelog debian/
mv ../copyright debian/
debuild -us -uc -S
cd ..
xterm -e "debsign *.changes"
