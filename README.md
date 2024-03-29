# PACT codebase

Run `pre-commit run -a` to run the pre-commit hooks.

Development:
- `nix develop` to set up a development environment, where all necessary
  packages are available
- For development/design: feedback web-server
- Set up local hoogle server: `hoogle serve --local`, then check
  `localhost:8080`
- Generate test coverage report: `stack test --enable-coverage`

Designs: https://www.figma.com/file/PwiAes2EP56eGAinXodw4H/PACT-VERSION-1?node-id=0%3A1&fbclid=IwAR3RZs0Nbk11MHc2U8LBCPS8uI2FObCMtAYKS4qkkwogWXn39tZcgtBwGPA

# Adding an Oura token

Navigate to your database and run
`sqlite3 pact.sqlite3`

Within the interactive SQLite, run
```sql
UPDATE user
SET oura_token = "<ouraToken>"
WHERE name = "<userName>";
```

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
- Replace "amount" by "duration" in CoachWorkout, remove past CoachWorkouts, and
  title with difficulty level (beginner-intermediary-expert-`all_levels`)

## Later

- Private profile
  * How much sports did you do? In total, this month..
- Public profile
  * Private profile minus any editing rights
  * Add button for friendship request & show status of friendship request
  * ListParticipants page links to everyone's public profile, including the
    coach
- "Like" button for UserWorkouts
  * You can see you liked something already, and cancel your like
  * You can see how many people liked your post
  * The #likes is displayed in ranges: 0 likes, 1-10 etc. Based on which range
    you're in, some icon on the workout changes.
  * You can max give out X likes per day. X increases with your PACT experience.
  * Likes are called "giving imPACT"
  * When you receive a like, you get PACT coins
- Add comments on other people's posts
- Speed up the tests
- Allow to cut out a circle out of each picture, to be used instead
  * Then finalize MVP design with Sander
- Make it possible to stop being a coach, unfriend, unfollow..
- Replace dropdowns by a separate page with icons

Maybe:
- Ask people after they did a CoachWorkout, what they think thought of the
  workout
- Put coming up coach organized workouts at the top of the newsfeed?

Notes:
- Differences with Strava:
  * Focus on habit formation: Add goals and streaks
  * Set up challenges, e.g. jog 2x/w for 4 weeks in a row. We can define one
    challenge each month to be central, app-wide.



