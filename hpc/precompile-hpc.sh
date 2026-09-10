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

mapfile -t available < <(cd vignettes && ls -1 *.Rmd.orig | sed 's/\.Rmd\.orig$//')
[ "${#available[@]}" -gt 0 ] || {
  echo "no vignettes/*.Rmd.orig here. Run this from the repository root." >&2
  exit 1; }

# Default to every vignette, which is what precompile.R does when given none.
if [ "${#args[@]}" -eq 0 ]; then
  args=("${available[@]}")
fi

# Names are normalised to the stem and checked here, not left to precompile.R
# inside the job. Checked there, a typo or a name from another branch is caught
# only after the tree sync, the lock check, a possible 20-minute image copy and
# R CMD INSTALL. Normalised here, because run.precompile reads vignettes.txt as
# a bare stem: `example3.Rmd` would make it copy `example3.Rmd.Rmd`, and a name
# with a directory in it would make it write a marker file into a directory
# that does not exist.
norm=()
for v in "${args[@]}"; do
  v=$(basename "$v"); v="${v%.orig}"; v="${v%.Rmd}"
  found=0
  for a in "${available[@]}"; do [ "$a" = "$v" ] && found=1 && break; done
  if [ "$found" -eq 0 ]; then
    echo "no vignettes/$v.Rmd.orig on this branch." >&2
    echo "Available: ${available[*]}" >&2
    exit 1
  fi
  norm+=("$v")
done
args=("${norm[@]}")

fetch() {
  local n=0 rc
  # Written by the deploy in this same invocation, or read back from the last
  # one when --fetch is used on its own.
  local want="${DEPLOY_ID:-}"
  [ -n "$want" ] || want=$(cat .last-deploy 2>/dev/null || true)
  for v in "${args[@]}"; do
    # `|| rc=$?` and not `; rc=$?`: a simple command that fails is not exempt
    # from errexit, so the plain form ended the script before either branch
    # below could run, and --fetch exited silently on the commonest case there
    # is -- a job that has not finished.
    rc=0; ssh -o BatchMode=yes "$HOST" "test -d $DEST/out/$v" || rc=$?
    # 255 is ssh itself failing. Reporting that as a failed job would be wrong
    # and would send someone to look at the wrong thing.
    if [ "$rc" -eq 255 ]; then
      echo "could not reach $HOST; not collecting" >&2
      exit 1
    elif [ "$rc" -ne 0 ]; then
      echo "no output staged for $v -- the job failed, or has not finished" >&2
      continue
    fi
    # The staged output survives a redeploy, because out/ is excluded from the
    # sync that would otherwise delete it. Without this check, deploying a
    # second branch and collecting before its job had started would return the
    # first branch's vignette, and say "collected".
    local stamped
    stamped=$(ssh -o BatchMode=yes "$HOST" \
      "sed -n 's/^deploy_id: //p' $DEST/out/$v/STAMP 2>/dev/null" || true)
    if [ -z "$stamped" ]; then
      echo "$v: staged output has no STAMP -- it predates this check. Rerun it." >&2
      continue
    fi
    if [ -z "$want" ]; then
      echo "$v: no record of a deployment here to compare with. Rerun it." >&2
      continue
    fi
    if [ "$stamped" != "$want" ]; then
      echo "$v: staged output is from deployment $stamped, and the last one from" >&2
      echo "  this working tree was $want. Not collecting it; rerun the vignette." >&2
      continue
    fi
    rsync -a --exclude STAMP "$HOST:$DEST/out/$v/vignettes/" vignettes/
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

# A second deployment while an array is still queued would rewrite the tree, and
# vignettes.txt with it, underneath the tasks that have not started. Task 3 would
# then precompile whatever is on line 3 of the new file, against the new source,
# and stage it under that name -- output that looks valid and is not. The
# job-local library is shared in the same way. The array is submitted at %1, so
# a full run is a long time to leave that window open.
running=$(ssh -o BatchMode=yes "$HOST" "bash -lc 'module load slurm >/dev/null 2>&1; \
  squeue -h -u \$USER -n bnec-precompile -o %A'" || true)
if [ -n "$running" ]; then
  echo "a precompile job is already queued or running on $HOST:" >&2
  echo "  $(echo "$running" | tr '\n' ' ')" >&2
  echo "Wait for it, or cancel it with scancel. Deploying now would rewrite the" >&2
  echo "tree underneath it." >&2
  exit 1
fi

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
# A per-deployment identifier, and not the commit alone. The script copies the
# working tree rather than the commit, so two deployments from the same HEAD with
# different uncommitted edits are indistinguishable by commit -- and committing
# anything at all between deploying and collecting would make a commit check
# reject good output. The job writes this into the staged output and fetch
# compares it.
DEPLOY_ID="$(date -u +%Y%m%dT%H%M%SZ)-$$-$RANDOM"
{
  echo "deploy_id: $DEPLOY_ID"
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

# Kept so that a later `--fetch` in a separate invocation knows which deployment
# it is collecting. Untracked; .gitignore covers it.
printf '%s\n' "$DEPLOY_ID" > .last-deploy

printf '%s\n' "${args[@]}" > .vignettes.tmp
rsync -a .vignettes.tmp "$HOST:$DEST/vignettes.txt"
rm -f .vignettes.tmp

# Copied under a fixed name, which is the name run.precompile looks for. $SIF
# is a local path and may be called anything -- it is often built somewhere
# other than the repository, because the repository is on a slow mount under
# WSL -- so its basename must not be what identifies it on the cluster.
REMOTE_SIF="$DEST/bayesnec-precompile.sif"
# Copied only when the remote copy is not already this image. The image changes
# when a dependency changes, not when the branch does, so this is rare.
# `|| true` is not used here: it would make an ssh failure look like "no image
# present" and start a needless 20-minute copy of 700MB. sha256sum's own failure
# on a missing file is separated from ssh's by the exit status.
if remote_sha=$(ssh -o BatchMode=yes "$HOST" \
     "sha256sum $REMOTE_SIF 2>/dev/null | cut -d' ' -f1"); then :
elif [ "$?" -eq 255 ]; then
  echo "could not reach $HOST" >&2; exit 1
else
  remote_sha=""
fi
if [ "$remote_sha" != "$(sha256sum "$SIF" | cut -d' ' -f1)" ]; then
  # About 20 minutes over the VPN, measured at roughly 900 kB/s on 2026-09-10.
  # --partial so an interrupted copy resumes rather than starting again.
  echo "==> copying the container ($(du -h "$SIF" | cut -f1); about 20 minutes)"
  rsync -a --partial --info=progress2 "$SIF" "$HOST:$REMOTE_SIF"
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
  ssh $HOST 'tail -f $DEST/logs/precompile-${JOB}_1.log'   # stdout and stderr

To collect the output when it finishes:

  ./hpc/precompile-hpc.sh --fetch ${args[*]}
TXT
  exit 0
fi

echo "==> waiting (polling every ${POLL}s; Ctrl-C is safe, the job keeps running)"
# A poll that cannot reach the cluster is treated as "still running", not as
# "finished". Otherwise a dropped VPN part-way through a run would end the wait
# and send the script on to collect output that does not exist yet.
# squeue stops knowing about a job once slurmctld has purged the record, which
# is five minutes after it ends by default, and reports that by exiting non-zero
# -- indistinguishable from an unreachable host. So a non-zero exit falls
# through to sacct, which keeps the record, and the loop ends on a terminal
# state there. Without that, a connection lost across the completion window
# becomes a permanent wait under a misleading message.
while :; do
  if state=$(ssh -o BatchMode=yes "$HOST" \
       "bash -lc 'module load slurm >/dev/null 2>&1; squeue -h -j $JOB -o %T'"); then
    [ -n "$state" ] || break
  else
    acct=$(ssh -o BatchMode=yes "$HOST" \
      "bash -lc 'module load slurm >/dev/null 2>&1; sacct -j $JOB -X -n -o State'" \
      || true)
    if printf '%s' "$acct" | grep -qE 'COMPLETED|FAILED|CANCELLED|TIMEOUT|OUT_OF_MEMORY|NODE_FAIL'; then
      break
    fi
    echo "  (could not reach $HOST, or the job record is not yet visible; retrying)" >&2
  fi
  sleep "$POLL"
done

echo "==> job $JOB finished; collecting"
fetch
