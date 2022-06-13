# plex-client

## Execute  

* Run `stack exec -- plex-client` to see "We're inside the application!"
* With `stack exec -- plex-client --verbose` you will see the same message, with more logging.

## Install

Run `stack install` followed by `plex-client --help` in order to see which command line options are
available.

## Run tests

`stack test`

## Development

You can run a local hoogle instance by issuing the following commands to make
the hoogle script executable and starting the hoogle server:

```bash
$ chmod +x dev-scripts/run-hoogle.sh
$ ./dev-scripts/run-hoogle.sh
```

The repository comes with a `.vscode/settings.json` file that sets the hoogle
vscode extension to point to the local hoogle instance, which means you can get
instant results right in your editor by bringing up the command palette and
executing "Hoogle Search" (or by pressing Shift+Alt+H by default).
