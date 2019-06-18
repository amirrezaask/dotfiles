echo "Step one, Secure the keys"
sudo apt install -y libxcb1-dev libxcb-keysyms1-dev libpango1.0-dev libxcb-util0-dev libxcb-icccm4-dev libyajl-dev libstartup-notification0-dev libxcb-randr0-dev libev-dev libxcb-cursor-dev libxcb-xinerama0-dev libxcb-xkb-dev libxkbcommon-dev libxkbcommon-x11-dev autoconf libxcb-xrm0 libxcb-xrm-dev automake

cd /tmp

echo "Step two, Ascend from darkness"

git clone https://www.github.com/Airblader/i3 i3-gaps

cd i3-gaps
echo "Step three, Rain fire"
autoreconf --force --install

rm -rf build/
mkdir -p build && cd build/

echo "Step four, Unleash the horde"
../configure --prefix=/usr --sysconfdir=/etc --disable-sanitizers
echo "Step five, Skewer the winged beast"
make
echo "Freedom"
sudo make install
