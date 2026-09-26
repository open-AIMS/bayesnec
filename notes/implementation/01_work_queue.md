# Work queue for the backlog run

Read `00_protocol.md` first. The specification for every item is
`notes/tasks/backlog-run-claude.md`, cited below by section; RF's rulings are in
`03_decisions.md`. The plan for a reader who was not in the conversation is
`notes/tasks/backlog-run-human.md`.

Rebuilt 2026-09-26. The queue that stood here was the training-course run of
2026-09-14. Every issue and pull request it listed has since closed or merged,
so it is replaced rather than amended; it is in the history of this file.

`predev` was at `21472c39` when this queue was written, with the same tree as
`dev` at `c3824643`. `DESCRIPTION` `Version` was 2.1.3.39. New `NEWS.md` entries
go under `# bayesnec 2.2.0`.

## The queue

Work down the table. The status column holds the pull request once one is open,
and `merged`, `stopped` or `skipped` after it; a stopped or skipped item has a
line in `05_run_log.md` saying why. An item whose decision is still open is
skipped when reached, and taken up once the decision is recorded in
`03_decisions.md`. §3 of the specification gives the reason for each position.

| # | issue | specification | decision | status |
|---|---|---|---|---|
| 1 | #416 | §4 item 1 | none | |
| 2 | #417 | §4 item 2 | none | |
| 3 | #415 | §4 item 3 | D20 | |
| 4 | #299 | §4 item 4 | D23 | |
| 5 | #120 | §4 item 5 | D19 | |
| 6 | #404, #403 | §4 item 6 | D21, D22 | |
| 7 | #412 | §4 item 7 | D28 | |
| 8 | #398 | §4 item 8 | none | |
| 9 | #400 | §4 item 9 | D30 | |
| 10 | #397 | §4 item 10 | none | |
| 11 | #44 | §4 item 11 | D19, D25 | |
| 12 | #410 | §4 item 12 | D29 | |
| 13 | #404 | §4 item 13 | D22 | after PR #402, or after item 12 if it has not merged |
| 14 | #413 | §4 item 14 | D24 | after PR #402, or after item 12 if it has not merged |
| 15 | #410 | §4 item 15 | D29 | after PR #402, or after item 12 if it has not merged |
| 16 | #419 | §4 item 16 | D31 | after PR #402, or after item 12 if it has not merged |
| bg | #418 | §4, the background item | D26 | notes only, straight to `predev`; start at the beginning |
| end | precompile | §4, the precompile and the store refit | D18 | test passed (job 914152); submit by 16:00 AEST 2026-09-27 |

## At the end of the run

- File the issues listed in §6 of the specification.
- Record in `05_run_log.md` the precompile job identifiers, the `predev` commit
  submitted, and every item merged after it.
- Leave every issue the run implemented open, with its comment. They close when
  `predev` reaches `dev`.
- State in `05_run_log.md` where the run finished and what, if anything, it
  stopped on.
