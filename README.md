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

Simple things to start with:
- Show all dates as dd/mm/yyyy
- Coach confirms whether you went to the event, not you
  * Fix bug first (including bug report & regression test)
- Add "Walking", "Climbing", "Gym" and "Acrogym" as sports

- Staging environment
- Finish "coins" feature, and call it "experience" (ask Sander a better name)
- Private profile
  * How much sports did you do? In total, this month..
  * Create a separate page to list your friends and one for coaches
  * Current friends shouldn't be in "add friends" page. Same for coaches.
- Public profile
  * Private profile minus any editing rights
  * Add button for friendship request & show status of friendship request
- Make it possible to stop being a coach, unfriend, unfollow..
- "Like" button for events
  * You can see you liked something already, and cancel your like
  * You can see who liked what things, includes links to their public profile

- Coach can see who joined his events, including a link to their public profiles
- Add "how long did you do the sport?" to each workout submission
- When adding a picture, ask to select a circle within the picture to use
  instead
- Put coming up coach organized workouts at the top of the newsfeed?
- Add "duration" as optional for each workout type

# Later

- Put all newsfeed events anti-chronologically? (Talk to Paul first)
- Add a page with the raking per sport
- "Community" concept
  * New type of user = community leaders. People can follow communities, put
    together a news feed..

## Later

- Allow people to post text and pictures with motivational and positive things
  related to positive lifestyles: Sports pics, nice meal, friendship..



