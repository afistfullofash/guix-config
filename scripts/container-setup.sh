#!/bin/sh
set -eux

export DEBIAN_FRONTEND=noninteractive

print_log () {
    echo ""
    echo ""
    echo ""
    echo "$1"
    echo ""
    echo ""
    echo ""
}

print_log "Updating packages and installing guix"

apt-get update
apt-get install -y guile-3.0

apt install -y guix
apt install -y netbase gpg xz-utils ca-certificates

print_log "Clone git repos"

cd /opt
git clone https://github.com/afistfullofash/afistfullofash.git
git clone https://github.com/afistfullofash/guix-config.git
mkdir -p ~/.config/guix
cat > ~/.config/guix/channels.scm <<'EOF'
(list (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix.git")
        (branch "master")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'afistfullofash)
        (url "https://github.com/afistfullofash/afistfullofash")
        (branch "main"))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))
EOF

print_log "Create Guix Builder Users"

# Ensure guix build users exist (handy in containers)
groupadd -r guixbuild 2>/dev/null || true

mkdir -p /var/empty
chmod 0555 /var/empty

for i in $(seq -w 1 10); do
  id "guixbuilder$i" >/dev/null 2>&1 || \
    useradd -r \
      -g nogroup \
      -G guixbuild \
      -d /var/empty \
      -s /usr/sbin/nologin \
      -c "Guix build user $i" \
      "guixbuilder$i"

    usermod -a -G guixbuild "guixbuilder$i"
done

print_log "Starting Guix Deamon"

guix-daemon --build-users-group=guixbuild --disable-chroot &
# wait for the daemon socket to exist
 while [ ! -S /var/guix/daemon-socket/socket ]; do sleep 0.1; done

print_log "Updating Guix"

guix pull
hash guix

print_log "Configure user home to follow the current default user home"

cd /opt/guix-config
guix home reconfigure -L /opt/afistfullofash -L /opt/guix-config -e "(use-modules (affa-guix-config home nymph)) nymph-home-environment"
