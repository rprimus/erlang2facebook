-define(API_EC_SUCCESS, 0);

  %%
  %% GENERAL ERRORS
  %%
-define(API_EC_UNKNOWN, 1);
-define(API_EC_SERVICE, 2);
-define(API_EC_METHOD, 3);
-define(API_EC_TOO_MANY_CALLS, 4);
-define(API_EC_BAD_IP, 5);

  %%
  %% PARAMETER ERRORS
  %%
-define(API_EC_PARAM, 100);
-define(API_EC_PARAM_API_KEY, 101);
-define(API_EC_PARAM_SESSION_KEY, 102);
-define(API_EC_PARAM_CALL_ID, 103);
-define(API_EC_PARAM_SIGNATURE, 104);
-define(API_EC_PARAM_USER_ID, 110);
-define(API_EC_PARAM_USER_FIELD, 111);
-define(API_EC_PARAM_SOCIAL_FIELD, 112);
-define(API_EC_PARAM_ALBUM_ID, 120);

  %%
  %% USER PERMISSIONS ERRORS
  %%
-define(API_EC_PERMISSION, 200);
-define(API_EC_PERMISSION_USER, 210);
-define(API_EC_PERMISSION_ALBUM, 220);
-define(API_EC_PERMISSION_PHOTO, 221);

-define(FQL_EC_PARSER, 601);
-define(FQL_EC_UNKNOWN_FIELD, 602);
-define(FQL_EC_UNKNOWN_TABLE, 603);
-define(FQL_EC_NOT_INDEXABLE, 604);
 
get_error_description(?API_EC_SUCCESS) -> "Success";
get_error_description(?API_EC_UNKNOWN) -> "An unknown error occurred";
get_error_description(?API_EC_SERVICE) -> "Service temporarily unavailable";
get_error_description(?API_EC_METHOD) -> "Unknown method";
get_error_description(?API_EC_TOO_MANY_CALLS) -> "Application request limit reached";
get_error_description(?API_EC_BAD_IP) -> "Unauthorized source IP address";
get_error_description(?API_EC_PARAM) -> "Invalid parameter";
get_error_description(?API_EC_PARAM_API_KEY) -> "Invalid API key";
get_error_description(?API_EC_PARAM_SESSION_KEY) -> "Session key invalid or no longer valid";
get_error_description(?API_EC_PARAM_CALL_ID) -> "Call_id must be greater than previous";
get_error_description(?API_EC_PARAM_SIGNATURE) -> "Incorrect signature";
get_error_description(?API_EC_PARAM_USER_ID) -> "Invalid user id";
get_error_description(?API_EC_PARAM_USER_FIELD) -> "Invalid user info field";
get_error_description(?API_EC_PARAM_SOCIAL_FIELD) -> "Invalid user field";
get_error_description(?API_EC_PARAM_ALBUM_ID) -> "Invalid album id";
get_error_description(?API_EC_PERMISSION) -> "Permissions error";
get_error_description(?API_EC_PERMISSION_USER) -> "User not visible";
get_error_description(?API_EC_PERMISSION_ALBUM) -> "Album not visible";
get_error_description(?API_EC_PERMISSION_PHOTO) -> "Photo not visible";
get_error_description(?FQL_EC_PARSER) -> "FQL: Parser Error";
get_error_description(?FQL_EC_UNKNOWN_FIELD) -> "FQL: Unknown Field";
get_error_description(?FQL_EC_UNKNOWN_TABLE) -> "FQL: Unknown Table";
get_error_description(?FQL_EC_NOT_INDEXABLE) -> "FQL: Statement not indexable";
get_error_description(?FQL_EC_UNKNOWN_FUNCTION) -> "FQL: Attempted to call unknown function";
get_error_description(?FQL_EC_INVALID_PARAM) -> "FQL: Invalid parameter passed in";
get_error_description(Error) ->
    ["Unknown error ", integer_to_list(Error)].

