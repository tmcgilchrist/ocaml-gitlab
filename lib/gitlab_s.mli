module type Gitlab = sig
  type rate = Core

  (** Functions corresponding to direct API requests return
      {!Response.t} values inside of {!Monad.t} values so that more
      information about the request can be made
      available. {!Monad.(>>~)} is a convenience operator that lets
      you bind directly to the carried value. *)
  module Response : sig
    (** [redirect] indicates whether the originally requested
        endpoint should continue to be used in the future. *)
    type redirect =
      | Temporary of Uri.t  (** The redirection is temporary. *)
      | Permanent of Uri.t  (** The redirection is permanent. *)

    type 'a t = private < value : 'a ; redirects : redirect list ; .. >
    (** ['a t] is an API response containing a payload of type
        ['a]. {b Do not} refer to this type explicitly as its identity and
        representation are subject to change (e.g. a family of object
        types may replace it before 3.0). *)

    val value : < value : 'a ; .. > -> 'a
    (** [value r] is the payload in response [r]. *)

    val redirects : < redirects : redirect list ; .. > -> redirect list
    (** [redirects r] is the sequence of redirects prior to response [r]. *)

    val final_resource : redirect list -> redirect option
    (** [final_resource rs] is the single redirect, if any redirects
        occurred, that describes the overall redirect chain [rs]. If
        any redirect [rs] is temporary, [final_resource rs] will be a
        temporary redirect to the final URI. If all redirects [rs] are
        permanent, [final_resource rs] will be a permanent redirect to
        the final URI. *)
  end

  (** All API requests are bound through this monad which encapsulates
      an Lwt cooperative thread and includes some state which may be
      set via {!API} functions. *)
  module Monad : sig
    type 'a t
    (** ['a t] is an Lwt thread sensitive to GitLab API state. *)

    val return : 'a -> 'a t
    (** [return x] is the value [x] wrapped in a state-sensitive Lwt thread. *)

    val bind : ('a -> 'b t) -> 'a t -> 'b t
    (** [bind m f] is the eventual value of [f] applied to the
        contents of [m]. Its argument order is designed for currying. *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] is {!bind} [m (fun x -> return (f x))]. Its argument
        order is designed for currying. *)

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    (** [m >>= f] is [{!bind} f m]. *)

    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    (** [m >|= f] is [{!map} f m]. *)

    val ( >>~ ) : 'a Response.t t -> ('a -> 'b t) -> 'b t
    (** [m >>~ f] is [m >|= {!Response.value} >>= f]. *)

    val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
    (** [catch try with] is the result of trying [try]. If [try]
        succeeds, its result is returned. If [try] raises an
        exception, [with] is applied to the exception and the result
        of [with] is returned. *)

    val fail : exn -> 'a t
    (** [fail exn] raises exception [exn] inside of the monad. *)

    val run : 'a t -> 'a Lwt.t
    (** [run m] is the Lwt thread corresponding to the sequence of API
        actions represented by [m]. Once a {!t} has been [run], any
        GitLab API state such as associated default security tokens or
        declared user agent string will not be available in
        subsequently bound functions. *)

    val embed : 'a Lwt.t -> 'a t
    (** [embed lwt] is an Lwt thread lifted into the GitLab API
        monad. Its monadic state will be inherited from any monadic
        values bound before it. *)
  end

  module Token : sig
    type t
    (** [t] is the abstract type of a token. *)

    val of_string : string -> t
    (** [of_string token_string] is the abstract token value
        corresponding to the string [token_string]. *)

    val to_string : t -> string
    (** [to_string token] is the string serialization of [token]. *)
  end

  type +'a parse = string -> 'a Lwt.t
  (** ['a parse] is the type of functions which extract meaningful
      values from GitLab responses. *)

  type 'a handler = (Cohttp.Response.t * string -> bool) * 'a
  (** ['a handler] is the type of response handlers which consist of
      an activation predicate (fst) and a parse function (snd). *)

  (** Each request to GitLan is made to a specific [Endpoint] in
      GitLab's REST-like API. *)
  module Endpoint : sig
    (** Some endpoints expose resources which change over time and
        responses from those endpoints may contain version metadata
        which can be used to make low-cost conditional requests
        (e.g. cache validation). *)
    module Version : sig
      (** [t] is a version of a resource representation. *)
      type t =
        | Etag of string  (** An entity tag identifier *)
        | Last_modified of string
            (** A timestamp conforming to the HTTP-date production *)
    end
  end

  (** The [Stream] module provides an abstraction to GitLab's paginated
      endpoints. Stream creation does not initiate any network
      activity. When requests are made, results are buffered
      internally. Streams are not mutable. *)
  module Stream : sig
    type 'a t
    (** ['a t] is a stream consisting roughly of a buffer and a means
        to refill it. *)

    type 'a parse = string -> 'a list Lwt.t
    (** ['a parse] is the type of functions which extract elements
        from a paginated response. *)

    val next : 'a t -> ('a * 'a t) option Monad.t
    (** [next s] is the next element of the stream and a stream
        continuation if one exists. The input stream is not
        modified. This function offers an efficient, lazy, and uniform
        means to iterate over ordered API results which may be too
        numerous to fit into a single request/response pair. *)

    val map : ('a -> 'b list Monad.t) -> 'a t -> 'b t
    (** [map f s] is the lazy stream of [f] applied to elements of [s]
        as they are demanded. *)

    val fold : ('a -> 'b -> 'a Monad.t) -> 'a -> 'b t -> 'a Monad.t
    (** [fold f a s] is the left fold of [f] over the elements of [s]
        with a base value of [a]. {b Warning:} this function may
        result in {i many} successive API transactions. *)

    val find : ('a -> bool) -> 'a t -> ('a * 'a t) option Monad.t
    (** [find p s] is the first value in [s] satisfying [p] if one
        exists and a stream continuation for further ingestion. *)

    val iter : ('a -> unit Monad.t) -> 'a t -> unit Monad.t
    (** [iter f s] is activated after the application of [f] to each
        element of [s]. *)

    val to_list : 'a t -> 'a list Monad.t
    (** [to_list s] is a list with each element of [s]. {b Warning:}
        this function may result in {i many} successive API transactions. *)

    val of_list : 'a list -> 'a t
    (** [of_list l] is a stream with each element of [l].
        Occasionally, it is useful to write interfaces which operate
        generically on streams. [of_list] allows you to use list
        values with those interfaces. *)

    val poll : 'a t -> 'a t option Monad.t
    (** [poll stream] is a stream with items newer than [stream]'s
        items and will not resolve until any timeouts indicated by
        GitLab have elapsed. By default, GitLab throttles polling
        requests to once per minute per URL endpoint. *)

    val since : 'a t -> Endpoint.Version.t -> 'a t
    (** [since stream version] is [stream] with [version] but without
        any other change, i.e. the stream is not reset to its
        beginning. Used in conjunction with [poll], [since] enables
        low-cost conditional re-synchronization of local state with
        GitLab state. *)

    val version : 'a t -> Endpoint.Version.t option
    (** [version stream] is the version of [stream] if one is
        known. After any stream element is forced, the stream version
        will be available unless GitLab violates its API specification. *)
  end

  module API : sig
    val code_handler : expected_code:Cohttp.Code.status_code -> 'a -> 'a handler
    (** [code_handler ~expected_code parse] is a response handler that
        fires for responses with status [expected_code] and applies
        [parse]. *)

    val get :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a parse ->
      'a Response.t Monad.t
    (** [get ?rate ?fail_handlers ?expected_code ?headers ?token
        ?params uri p] is the [p]-parsed response to a GitLab API HTTP
        GET request to [uri] with extra query parameters [params] and
        extra headers [headers]. If [token] is supplied, it will be
        used instead of any token bound into the monad. [p] will only
        fire if the response status is [expected_code] which defaults
        to [200 OK]. If the response status is not [expected_code],
        [fail_handlers], if any, will be checked in the order
        supplied. The [rate] parameter determines which rate limit
        accounting regime will be used for caching rate limit values
        in response headers. *)

    val get_stream :
      ?rate:rate ->
      ?fail_handlers:'a Stream.parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a Stream.parse ->
      'a Stream.t
    (** [get_stream uri stream_p] is the {!Stream.t} encapsulating
        lazy [stream_p]-parsed responses to GitLab API HTTP GET
        requests to [uri] and
        {{:https://docs.gitlab.com/ee/api/index.html#pagination}its
        successors}. For an explanation of the other
        parameters, see {!get}. *)

    val post :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?body:string ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a parse ->
      'a Response.t Monad.t
    (** [post uri p] is the [p]-parsed response to a GitLab API HTTP
        POST request to [uri]. For an explanation of the other
        parameters, see {!get}. *)

    val delete :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a parse ->
      'a Response.t Monad.t
    (** [delete uri p] is the [p]-parsed response to a GitLab API HTTP
        DELETE request to [uri]. For an explanation of the other
        parameters, see {!get}. *)

    val patch :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?body:string ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a parse ->
      'a Response.t Monad.t
    (** [patch uri p] is the [p]-parsed response to a GitLab API HTTP
        PATCH request to [uri]. For an explanation of the other
        parameters, see {!get}. *)

    val put :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?body:string ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a parse ->
      'a Response.t Monad.t
    (** [put uri p] is the [p]-parsed response to a GitLab API HTTP
        PUT request to [uri]. For an explanation of the other
        parameters, see {!get}. *)

    val set_user_agent : string -> unit Monad.t
    (** [set_user_agent ua] contains monadic state that will cause
        bound requests to use the [User-Agent] header value of [ua]. *)

    val set_token : Token.t -> unit Monad.t
    (** [set_token token] contains monadic state that will cause bound
        requests to use [token] for authentication by default. This
        function enables the creation of large, generic monadic
        compositions that do not have to be parameterized by
        authentication token. *)

    val string_of_message : Gitlab_t.message -> string
    (** [string_of_message message] is the English language error
        message that GitLab generated in [message]. *)
  end

  module Event : sig
    val all : token:Token.t -> unit -> Gitlab_t.events Response.t Monad.t
    (** [all ~token] get a list of events for the authenticated user.

        See {{:https://docs.gitlab.com/ee/api/events.html#list-currently-authenticated-users-events}List currently authenticated user’s events}
    *)
  end

  (** The [User] module provides access to User {{:https://docs.gitlab.com/14.0/ee/api/users.html}API}.
   *)
  module User : sig
    val by_id : id:string -> unit -> Gitlab_t.user Response.t Monad.t
    (** [by_id ~id ()] is the user information for user [id].

        See {{:https://docs.gitlab.com/14.0/ee/api/users.html#for-user}Single User}.
     *)

    val by_name : name:string -> unit -> Gitlab_t.users Response.t Monad.t
    (** [by_name ~name ()] search for user by [name].

        See {{:https://docs.gitlab.com/14.0/ee/api/users.html#for-user}List Users}.
     *)

    val projects : id:string -> unit -> Gitlab_t.projects Response.t Monad.t
    (** [projects ~id ()] list user projects for user [id].

        See {{:https://docs.gitlab.com/14.0/ee/api/projects.html#list-user-projects}List User Projects}.
     *)

    val merge_requests :
      token:Token.t ->
      ?state:Gitlab_t.state ->
      ?milestone:string ->
      ?labels:string list ->
      ?author:string ->
      ?author_username:string ->
      ?my_reaction:string ->
      ?scope:Gitlab_t.scope ->
      unit ->
      Gitlab_t.merge_request Stream.t
    (** [merge_requests ()] list all merge requests the authenticated user has access to.

        See {{:https://docs.gitlab.com/14.0/ee/api/merge_requests.html#list-merge-requests}List merge requests}.
    *)

    val events :
      token:Token.t ->
      id:string ->
      ?action:string ->
      ?target_type:string ->
      unit ->
      Gitlab_t.events Response.t Monad.t
    (** [events ~token ~id] get the contribution events for the specified user.

        See {{:https://docs.gitlab.com/ee/api/events.html#get-user-contribution-events}Get user contribution events}.
    *)
  end

  (** The [Project] module provides access to {{:https://docs.gitlab.com/ee/api/projects.html}Project API}. *)
  module Project : sig
    val merge_requests :
      ?token:Token.t ->
      ?state:Gitlab_t.state ->
      ?milestone:string ->
      ?labels:string list ->
      ?author:string ->
      ?author_username:string ->
      ?my_reaction:string ->
      ?scope:Gitlab_t.scope ->
      id:string ->
      unit ->
      Gitlab_t.merge_request Stream.t
    (** [merge_requests ?token ~id ()] list all merge requests for project [id].

        See {{:https://docs.gitlab.com/14.0/ee/api/merge_requests.html#list-project-merge-requests}List project merge requests}.
     *)

    val merge_request :
      ?token:Token.t ->
      project_id:string ->
      merge_request_iid:string ->
      unit ->
      Gitlab_j.merge_request Response.t Monad.t
    (** [merge_request ?token ~project_id ~merge_request_iid ()] shows information about a single merge request.

        See {{:https://docs.gitlab.com/14.0/ee/api/merge_requests.html#get-single-mr}Get single merge request}.
     *)

    val merge_request_participants :
      ?token:Token.t ->
      project_id:string ->
      merge_request_iid:string ->
      unit ->
      Gitlab_j.users Response.t Monad.t
    (** [merge_request_participants ?token ~project_id ~merge_request_iid ()] gets a list of merge request participants.

        See {{:https://docs.gitlab.com/14.0/ee/api/merge_requests.html#get-single-mr-participants}Get a list of merge request participants}.
     *)

    val merge_request_commits :
      ?token:Token.t ->
      project_id:string ->
      merge_request_iid:string ->
      unit ->
      Gitlab_j.commits Response.t Monad.t
    (** [merge_request_commits ?token ~project_id ~merge_request_iid ()] gets a list of merge request commits.

       See {{:https://docs.gitlab.com/14.0/ee/api/merge_requests.html#get-single-mr-commits}Get single merge request commits}.
     *)

    val merge_request_changes :
      ?token:Token.t ->
      project_id:string ->
      merge_request_iid:string ->
      unit ->
      Gitlab_j.changes Response.t Monad.t
    (** [merge_request_changes ?token ~project_id ~merge_request_iid ()] shows information about the merge request including its files and changes.

       See {{:https://docs.gitlab.com/ee/api/merge_requests.html#get-single-mr-changes}Get single MR changes}.
    *)

    val events :
      token:Token.t ->
      project_id:string ->
      ?action:string ->
      ?target_type:string ->
      unit ->
      Gitlab_t.events Response.t Monad.t
    (** [events ~project_id] get visible events for a project.

        See {{:https://docs.gitlab.com/ee/api/events.html#list-a-projects-visible-events}:ist a projects visible events}.
    *)
  end

  (** The [Group] module provies access to {{:https://docs.gitlab.com/ee/api/groups.html}Group API}. *)
  module Group : sig
    val merge_requests :
      ?token:Token.t ->
      ?state:Gitlab_t.state ->
      ?milestone:string ->
      ?labels:string list ->
      ?author:string ->
      ?author_username:string ->
      ?my_reaction:string ->
      ?scope:Gitlab_t.scope ->
      id:string ->
      unit ->
      Gitlab_t.merge_request Stream.t
    (** [merge_requests ?token ~id ()] list all merge requests for group [id].

        See {{:https://docs.gitlab.com/14.0/ee/api/merge_requests.html#list-group-merge-requests}List group merge requests}.
     *)
  end
end

(** A module of this type is required in order to construct a
    {!Gitlab} module using {!Gitlab_core.Make}. *)
module type Time = sig
  val now : unit -> float
  (** [now ()] is the current UNIX epoch time in seconds. *)

  val sleep : float -> unit Lwt.t
  (** [sleep sec] activates after [sec] seconds have elapsed. *)
end
