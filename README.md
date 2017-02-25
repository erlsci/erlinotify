inotify
=======

Notification of inotify events

## Description

The library interfaces with inotify using erlang NIF. The NIF spawns
off a thread which then polls the inotify file descriptor for any
changes to a watched descriptor.

## Installation

Add the library to your rebar config

```
{
  deps,
  [
   {notify, {git, "https://github.com/erlsci/inotify.git", {tag, "v0.5.0,"}}},
   ....
  ]
}.

```

Then just use rebar to get the deps and use in your code.

## Usage

### API

