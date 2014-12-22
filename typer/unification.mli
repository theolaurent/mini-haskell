exception Failure of string list

val unify : Schema.prefix -> Ty.ty -> Ty.ty -> Schema.prefix
val polyunify : Schema.prefix -> Schema.schema -> Schema.schema -> Schema.prefix * Schema.schema
