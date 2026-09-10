#!/bin/bash
# Precompile one or more vignettes on the AIMS HPC and bring the output back.
#
#   ./hpc/precompile-hpc.sh example7                # deploy, submit, wait, fetch
#   ./hpc/precompile-hpc.sh example2 example3       # several, in sequence
#   ./hpc/precompile-hpc.sh --no-wait example7      # submit and return the id
#   ./hpc/precompile-hpc.sh --fetch example7        # collect a finished run
#   ./hpc/precompile-hpc.sh                         # every vignette
#
# Run from the repository root. It copies the working tree, not the commit, so
# an uncommitted vignette edit is precompiled as it stands; what was sent is
# recorded in PROVENANCE alongside it.
#
# On success the vignette's .Rmd and the figures the run wrote are copied into
# vignettes/, and nothing else is. A failed job stages no output at all, so
# there is nothing to copy back -- see the exit-status note in hpc/README.md.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

HOST="${HOST:-rfisher@hpc-l001.aims.gov.au}"
DEST="${DEST:-/export/scratch/${HOST%%@*}/bayesnec-precompile}"
SIF="${SIF:-bayesnec-precompile.sif}"
POLL="${POLL:-60}"

wait_for_job=1
fetch_only=0
args=()
for a in "$@"; do
  case "$a" in
    --no-wait) wait_for_job=0 ;;
    --fetch) fetch_only=1 ;;
    -*) echo "unknown option: $a" >&2; exit 2 ;;
    *) args+=("$a") ;;
  esac
done

# Default to every vignette, which is what precompile.R does when given none.
if [ "${#args[@]}" -eq 0 ]; then
  mapfile -t args < <(cd vignettes && ls *.Rmd.orig | sed 's/\.Rmd\.orig$//')
fi

fetch() {
  local n=0
  for v in "${args[@]}"; do
    if ! ssh "$HOST" "test -d $DEST/out/$v"; then
      echo "no output staged for $v -- the job failed, or has not finished" >&2
      continue
    fi
    rsync -a "$HOST:$DEST/out/$v/vignettes/" vignettes/
    n=$((n + 1))
  done
  [ "$n" -gt 0 ] || { echo "nothing collected" >&2; exit 1; }
  echo
  echo "collected into vignettes/. What changed:"
  git status --short vignettes/
}

if [ "$fetch_only" -eq 1 ]; then fetch; exit 0; fi

[ -f "$SIF" ] || {
  echo "no $SIF here. Build it first:  ./hpc/build.sh" >&2; exit 1; }
./hpc/build.sh --check

echo "==> creating $DEST on $HOST"
ssh "$HOST" "mkdir -p $DEST/logs $DEST/out"

# Excludes rather than an explicit list of what to send, so that a directory
# added to the package later is copied rather than silently omitted. What is
# excluded is either a build artefact, a local scratch area, or the image.
echo "==> syncing the working tree"
# --delete and not --delete-excluded: excluded paths are protected on the
# receiver, which is what keeps the copied image, the previous run's staged
# output and the job-local library from being removed on every deploy.
rsync -a --delete \
  --exclude '.git' --exclude '.Rproj.user' \
  --exclude 'cache' --exclude 'check' --exclude 'doc' --exclude 'docs' \
  --exclude 'Meta' --exclude 'superceded' --exclude 'article' \
  --exclude 'ignore' --exclude 'prior_audit' --exclude 'prompts' \
  --exclude 'notes' --exclude '*.sif' --exclude '.apptainer-tmp' \
  --exclude 'lib' --exclude 'logs' --exclude 'out' --exclude 'vignettes.txt' \
  ./ "$HOST:$DEST/"

# What was sent, recorded on the machine that ran it. The image identity is in
# hpc/image.lock, which is part of the tree above and is checked again by the
# job itself before anything is fitted.
{
  echo "deployed: $(date -Is)"
  echo "by: $(id -un)@$(hostname)"
  echo "branch: $(git rev-parse --abbrev-ref HEAD)"
  echo "commit: $(git rev-parse HEAD)"
  echo "vignettes: ${args[*]}"
  echo "uncommitted at deploy:"
  git status --porcelain | sed 's/^/  /'
} > .PROVENANCE.tmp
rsync -a .PROVENANCE.tmp "$HOST:$DEST/PROVENANCE"
rm -f .PROVENANCE.tmp

printf '%s\n' "${args[@]}" > .vignettes.tmp
rsync -a .vignettes.tmp "$HOST:$DEST/vignettes.txt"
rm -f .vignettes.tmp

# Copied only when the remote copy is not already this image. The image changes
# when a dependency changes, not when the branch does, so this is rare.
remote_sha=$(ssh "$HOST" "sha256sum $DEST/$SIF 2>/dev/null | cut -d' ' -f1" || true)
if [ "$remote_sha" != "$(sha256sum "$SIF" | cut -d' ' -f1)" ]; then
  # ~20 minutes over the VPN, measured at about 600 kB/s on 2026-09-10.
  # --partial so an interrupted copy resumes rather than starting again.
  echo "==> copying the container ($(du -h "$SIF" | cut -f1); about 20 minutes)"
  rsync -a --partial --progress "$SIF" "$HOST:$DEST/"
else
  echo "==> container already present and matching"
fi

# %1 -- one task at a time. cmdstanr does not lock the compile cache: two tasks
# that need the same Stan program would write the same .stan file and run make
# on the same executable path at once. Vignettes share programs wherever a
# family and equation coincide, so this is not hypothetical. Serialising costs
# wall-clock only on the first, cold run; afterwards the tasks are reading the
# cache rather than writing it. Raise it deliberately, once the cache is warm.
n=${#args[@]}
echo "==> submitting $n task(s)"
JOB=$(ssh "$HOST" "bash -lc 'cd $DEST && chmod +x hpc/run.precompile && \
  module load slurm >/dev/null 2>&1; \
  sbatch --parsable --array=1-$n%1 hpc/run.precompile'")
echo "job $JOB: ${args[*]}"

if [ "$wait_for_job" -eq 0 ]; then
  cat <<TXT

Submitted. To follow it:

  ssh $HOST 'squeue -j $JOB'
  ssh $HOST 'tail -f $DEST/logs/precompile-${JOB}_1.log'

To collect the output when it finishes:

  ./hpc/precompile-hpc.sh --fetch ${args[*]}
TXT
  exit 0
fi

echo "==> waiting (polling every ${POLL}s; Ctrl-C is safe, the job keeps running)"
# A poll that cannot reach the cluster is treated as "still running", not as
# "finished". Otherwise a dropped VPN part-way through a run would end the wait
# and send the script on to collect output that does not exist yet.
while :; do
  if state=$(ssh -o BatchMode=yes "$HOST" \
       "bash -lc 'module load slurm >/dev/null 2>&1; squeue -h -j $JOB -o %T'"); then
    [ -n "$state" ] || break
  else
    echo "  (could not reach $HOST; retrying)" >&2
  fi
  sleep "$POLL"
done

echo "==> job $JOB finished; collecting"
fetch
