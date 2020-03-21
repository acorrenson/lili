open Term

type type_env

val type_check : term -> type_env -> ptype option
