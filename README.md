inotify
=======

Supervisted Erlang binding for inotify

## Description

The library interfaces with inotify using and Erlang NIF. The NIF spawns off a
thread which then polls the inotify file descriptor for any changes to a
watched descriptor.

## Installation

Add the library to your rebar config

```erlang
{deps, [
   {notify, {git, "https://github.com/erlsci/inotify.git", {tag, "v0.5.0,"}}},
   ....
]}.
```

Then just use `rebar3` to download, compile (including the NIF), and use in
your code:

```bash
$ rebar3 compile
```

## Usage

### API

