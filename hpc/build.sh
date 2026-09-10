#!/bin/bash
# Build the precompile container and record its identity.
#
#   ./hpc/build.sh                 # build, then write hpc/image.lock
#   ./hpc/build.sh --check         # verify an existing .sif against image.lock
#
# Run from the repository root on a machine with apptainer and an entry in
# /etc/subuid. The AIMS HPC has neither a build subcommand that works
# unprivileged nor a subuid entry for the account, so the image is built here
# and copied across once. See hpc/README.md.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

DEF=hpc/bayesnec-precompile.def
SIF="${SIF:-bayesnec-precompile.sif}"
LOCK=hpc/image.lock

# The digest appears twice in the definition file -- once where apptainer reads
# it, once where the manifest records it -- because %post cannot see the
# bootstrap header. Assert they agree rather than trust that they do.
from_digest=$(grep -m1 '^From:' "$DEF" | sed 's/.*@//')
post_digest=$(grep -m1 '^ *BASE_DIGEST=' "$DEF" | cut -d= -f2-)
if [ "$from_digest" != "$post_digest" ]; then
  echo "digest mismatch in $DEF:" >&2
  echo "  From:        $from_digest" >&2
  echo "  BASE_DIGEST: $post_digest" >&2
  exit 1
fi

write_lock() {
  local sif="$1"
  apptainer exec "$sif" cat /opt/bayesnec-precompile/manifest.txt > "$LOCK"
  printf 'sif_sha256: %s\n' "$(sha256sum "$sif" | cut -d' ' -f1)" >> "$LOCK"
}

if [ "${1:-}" = "--check" ]; then
  [ -f "$SIF" ] || { echo "no $SIF here; build it first" >&2; exit 1; }
  tmp=$(mktemp); trap 'rm -f "$tmp"' EXIT
  apptainer exec "$SIF" cat /opt/bayesnec-precompile/manifest.txt > "$tmp"
  printf 'sif_sha256: %s\n' "$(sha256sum "$SIF" | cut -d' ' -f1)" >> "$tmp"
  if diff -u "$LOCK" "$tmp"; then
    echo "$SIF matches $LOCK"
  else
    echo >&2
    echo "$SIF does not match $LOCK. Rebuilding the image changes what the" >&2
    echo "vignettes are precompiled with, so update $LOCK deliberately and say" >&2
    echo "so on the pull request -- do not overwrite it as a side effect." >&2
    exit 1
  fi
  exit 0
fi

command -v apptainer > /dev/null || { echo "apptainer not on PATH" >&2; exit 1; }
grep -q "^$(id -un):" /etc/subuid || {
  echo "no /etc/subuid entry for $(id -un); --fakeroot will fail" >&2; exit 1; }

# The build unpacks a base image of tens of thousands of small files, so where
# it does that decides how long it takes. Under WSL the repository is on a 9p
# mount, where the extraction alone ran for over ten minutes without finishing;
# on the Linux filesystem it is a couple of minutes. Default to $TMPDIR, which
# is on real disk here, and refuse a tmpfs, which is 16GB on this workstation
# and too small for the intermediate root filesystem.
export APPTAINER_TMPDIR="${APPTAINER_TMPDIR:-${TMPDIR:-/tmp}/apptainer-$(id -un)}"
mkdir -p "$APPTAINER_TMPDIR"
fstype=$(stat -f -c %T "$APPTAINER_TMPDIR")
case "$fstype" in
  tmpfs|v9fs|9p|fuseblk|drvfs)
    echo "APPTAINER_TMPDIR ($APPTAINER_TMPDIR) is on $fstype; set it to a" >&2
    echo "directory on a local Linux filesystem with ~15GB free." >&2
    exit 1 ;;
esac

# Built beside the scratch directory and moved into place, for the same reason:
# apptainer writes the squashfs incrementally, and doing that over 9p is far
# slower than writing the finished file once.
staged="$APPTAINER_TMPDIR/$(basename "$SIF")"
echo "==> building $SIF from $DEF (30-60 min: cmdstan is compiled)"
echo "    working in $APPTAINER_TMPDIR"
apptainer build --fakeroot --force "$staged" "$DEF"
mv -f "$staged" "$SIF"

echo "==> recording image identity in $LOCK"
write_lock "$SIF"
cat "$LOCK"
