# PACT frontend

How to run this in development:
- Go to the root of this repository and run `nix-shell`
- In the nix shell, navigate to `frontend-server`
- Run `npm install` to install the necessary javascript dependencies
- Set up the compiler (`spago`) with a file watcher by running `npm run watch`
- Open a new terminal, go into a nix-shell and, within `frontend-server`, run
  `npm run serve-dev`
- Open your browser and go to the URL `localhost:1234`
- Your browser will automatically update every time something changes in the
  frontend codebase

How to run this in deployment:
- Within a nix shell, from `frontend-server`, run `npm run s`
- The frontend server is set up on `localhost:8080`



