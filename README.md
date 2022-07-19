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

Designs: https://www.figma.com/file/PwiAes2EP56eGAinXodw4H/PACT-VERSION-1?node-id=0%3A1&fbclid=IwAR3RZs0Nbk11MHc2U8LBCPS8uI2FObCMtAYKS4qkkwogWXn39tZcgtBwGPA

# To do

Design:
- Profile page: Split up in clear sections, and turn each section into a nice
  "button"
- Boxes: Make each reported workout look good, and include a free-form field
- Form submission for workouts

First goal: More interaction between the founders.
- Add a free form "description" to each workout submission
- Add pictures to workouts
- Add workout-free pictures: Food, (healthy) fun

MVP (with customers):
- Allow to cut out a circle out of each picture, to be used instead
  * Then finalize MVP design with Sander
- Implement Sander's designs together
- Private profile
  * How much sports did you do? In total, this month..
  * Create a separate page to list your friends and one for coaches
  * Current friends shouldn't be in "add friends" page. Same for coaches.
- Public profile
  * Private profile minus any editing rights
  * Add button for friendship request & show status of friendship request
- Make it possible to stop being a coach, unfriend, unfollow..
- "Like" button for events???
  * You can see you liked something already, and cancel your like
  * You can see who liked what things, includes links to their public profile
- Finish "coins" feature, and call it "experience" (ask Sander a better name)
- Coach can see who joined his events, including a link to their public profiles
- Put coming up coach organized workouts at the top of the newsfeed?
- Replace dropdowns by a separate page with icons

# Later

- Coach confirms whether you went to the event, not you
  * Fix bug first (including bug report & regression test)
- Staging environment
- Switch to Cassius to use variables (e.g. for the primary colors) and type-safe
  URLs
- Clean up CSS
- Put all newsfeed events anti-chronologically? (Talk to Paul first)
- Leaderboard + ranking per sport
- "Community" concept
  * New type of user = community leaders. People can follow communities, put
    together a news feed..
- Build an app: https://cordova.apache.org/
- sydtest-webdriver: Selenium tests



