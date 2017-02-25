#ifndef __inotify_NIF_QUEUE_H
#define __inotify_NIF_QUEUE_H

#include "erl_nif.h"

typedef struct
{
  ErlNifTid qthread;
  int fd;
  ErlNifPid pid;
} state_t;

static ErlNifResourceType* inotify_nif_RESOURCE = NULL;

static ERL_NIF_TERM
inotify_nif_start(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
inotify_nif_stop(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM
inotify_nif_add_watch(ErlNifEnv* env, int argc,
                          const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
inotify_nif_remove_watch(ErlNifEnv* env, int argc,
                          const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
atom_event(ErlNifEnv* env, ulong mask);

static ERL_NIF_TERM
atom_file_type(ErlNifEnv* env, ulong mask);

int
read_events(ErlNifEnv* env, void* obj);

static void*
thr_main(void* obj);


#endif
