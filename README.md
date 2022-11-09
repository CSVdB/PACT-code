# PACT codebase

Development:
- `nix develop` to set up a development environment, where all necessary
  packages are available
- For backend development
  * `stack build --file-watch`
  * `stack test --file-watch --ghc-options="-freverse-errors -j4 +RTS -A128M
    -n2m -RTS`
- For frontend development/design: `./scripts/devel-pact-web-server.sh`, then
  check `localhost:8000`
- Set up local hoogle server: `hoogle serve --local`, then check
  `localhost:8080`
- Generate test coverage report: `stack test --enable-coverage`

Before creating a PR: In the root of the repository, run `cp assets/*
pact-web-server/assets/`.

Designs: https://www.figma.com/file/PwiAes2EP56eGAinXodw4H/PACT-VERSION-1?node-id=0%3A1&fbclid=IwAR3RZs0Nbk11MHc2U8LBCPS8uI2FObCMtAYKS4qkkwogWXn39tZcgtBwGPA

# To do

## November

Paul:
- List all atoms & molecules from the website
- Set up storeboard: Pencil and paper, can be very ugly but should have all the
  web pages and how they connect
- Implement the atoms & the first molecule in HTML & CSS

Sander:
- Design all atoms & molecules from the website

## December

Nick:
- Ask Kai: Could you help us implement the website for a couple grand?
- Integrate atoms & first molecule into website
- Set up Selenium tests
  * Include clicking the "JoinWorkout" button
- Make sure CoachWorkout confirmations (i.e. "did you actually go") show up, and
  show up for the coach instead of the user
  * They seem to show up too late. Might be a timezone issue?

Paul:
- Implement the other molecules in HTML & CSS

## January

Nick:
- Integrate the remaining molecules in the website

## Later

- "Like" button for UserWorkouts
  * You can see you liked something already, and cancel your like
  * You can see how many people liked your post
  * The #likes is displayed in ranges: 0 likes, 1-10 etc. Based on which range
    you're in, some icon on the workout changes.
  * You can max give out X likes per day. X increases with your PACT experience.
  * Likes are called "giving imPACT"
  * When you receive a like, you get PACT coins
- Add comments on other people's posts
- Ask people after they did a CoachWorkout, what they think thought of the
  workout
- Speed up the tests
- Allow to cut out a circle out of each picture, to be used instead
  * Then finalize MVP design with Sander
- Private profile
  * How much sports did you do? In total, this month..
- Public profile
  * Private profile minus any editing rights
  * Add button for friendship request & show status of friendship request
  * ListParticipants page links to everyone's public profile, including the
    coach
- Make it possible to stop being a coach, unfriend, unfollow..
- Put coming up coach organized workouts at the top of the newsfeed?
- Replace dropdowns by a separate page with icons
- Replace "amount" by "duration" in CoachWorkout, remove past CoachWorkouts, and
  title with difficulty level (beginner-intermediary-expert-`all_levels`)

Notes:
- Differences with Strava:
  * Focus on habit formation: Add goals and streaks
  * Set up challenges, e.g. jog 2x/w for 4 weeks in a row. We can define one
    challenge each month to be central, app-wide.

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



