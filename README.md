# PACT codebase

Development:
- `nix develop` to set up a development environment, where all necessary
  packages are available
- For backend development
  * `stack build --file-watch`
  * `stack test --file-watch --ghc-options="-freverse-errors -j4 +RTS -A128M
    -n2m -RTS`
- For frontend development/design: `./scripts/devel-pact-web-server.sh`
- Set up local hoogle server: `hoogle serve --local`, then check
  `localhost:8080`
- Generate test coverage report: `stack test --enable-coverage`

# To do

- Staging environment
- Finish "coins" feature
- Private profile
  * How much sports did you do? In total, this month..
  * Create a separate page to list your friends and one for coaches
- Public profile
  * Private profile minus any editing rights
  * Add button for friendship request & show status of friendship request
- Add a page with the raking per sport
- Add "Walking", "Climbing" and "Acrogym" as sports
- "Like" button for events
  * You can see you liked something already, and cancel your like
  * You can see who liked what things, includes links to their public profile
- Coach can see who joined his events, including a link to their public profiles
- Coach confirms whether you went to the event, not you
  * Fix bug first (including bug report & regression test)
- Add "how long did you do the sport?" to each workout submission
- Show all dates as dd/mm/yyyy
- When adding a picture, ask to select a circle within the picture to use
  instead
- Put coming up coach organized workouts at the top of the newsfeed?
- Add "duration" as optional for each workout type
- Put all newsfeed events anti-chronologically



