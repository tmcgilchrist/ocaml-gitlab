module type Gitlab = sig
  type rate_limit = { limit : int; remaining : int; reset : float }
  type rates = { core : rate_limit option }

  exception Message of Cohttp.Code.status_code * Gitlab_t.message

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

    val ( *> ) : 'a t -> 'b t -> 'b t
    (** [m *> n] is [{m >>= fun _ -> n}]. *)

    val ( >>~ ) : 'a Response.t t -> ('a -> 'b t) -> 'b t
    (** [m >>~ f] is [m >|= {!Response.value} >>= f]. *)

    val ( >|~ ) : 'a Response.t t -> ('a -> 'b) -> 'b t
    (** [m >|~ f] is [m >|= {!Response.value} >|= f]. *)

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

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  end

  (** The [Scope] module abstracts GitLab's
      {{:https://docs.gitlab.com/ee/integration/oauth_provider.html#authorized-applications}authorization
      scopes}. *)
  module Scope : sig
    val to_string : Gitlab_t.scope -> string
    (** [to_string scope] is the string GitLab uses to indicate
        the scope constructor [scope]. *)

    val of_string : string -> Gitlab_t.scope option
    (** [scope_of_string scope] is the constructor corresponding to
        the GitLab scope constructor [scope] if one exists. *)

    val list_to_string : Gitlab_t.scope list -> string
    (** [string_of_scopes scopes] is the serialization for a list of
        scopes [scopes] which GitLab accepts as a set of scopes in its
        API. *)

    val list_of_string : string -> Gitlab_t.scope list option
    (** [scopes_of_string scopes] are the scope constructors
        corresponding to the serialized list of constructors
        [scopes]. *)

    val all : Gitlab_t.scope list
    (** [all] is a list containing every scope constructor known. *)
  end

  (** Authentication Token for accessing GitLab APIs.

      Of the several ways provided in GitLab, we currently support:
      {ul {- Personal Access Tokens}
          {- Project Access Tokens}
          {- OAuth Authorization Codes}
      }
   *)
  module Token : sig
    type oauth = { access_token : string; refresh_token : string }

    type t =
      | AccessToken of string (* Either a Personal or Project Access Token. *)
      | OAuthToken of oauth  (** [t] is the abstract type of a token. *)

    type grant_type = AuthorizationCode | RefreshToken

    val create_url :
      client_id:string ->
      redirect_uri:Uri.t ->
      state:string ->
      scopes:Gitlab_t.scope list ->
      unit ->
      Uri.t
    (** Create URL for Authorisation code flow. {{:https://docs.gitlab.com/ee/api/oauth2.html#authorization-code-flow}}
      *)

    val of_code :
      client_id:string ->
      client_secret:string ->
      ?grant_type:grant_type ->
      redirect_uri:string ->
      code:string ->
      unit ->
      t option Lwt.t
    (** Retrieve an OAuth Token using the default grant_type [AuthorizationCode].
        Use the [refresh_token] along with a grant_type of [RefreshToken] to request a
        refreshed OAuth Token.
      *)

    val of_string : string -> t
    (** [of_string] create an [AccessToken]. *)

    val to_string : t -> string
    (** [to_string token] is the string serialization of [token]. *)
  end

  type +'a parse = string -> 'a Lwt.t
  (** ['a parse] is the type of functions which extract meaningful
      values from GitLab responses. *)

  type 'a handler = (Cohttp.Response.t * string -> bool) * 'a
  (** ['a handler] is the type of response handlers which consist of
      an activation predicate (fst) and a parse function (snd). *)

  (** Each request to GitLab is made to a specific [Endpoint] in
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

    val take : int -> 'a t -> 'a t
    (** [take n s] is the lazy stream of the first [n] elements of [s]
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

  (** Perform GitLab API requests. *)
  module API : sig
    val code_handler : expected_code:Cohttp.Code.status_code -> 'a -> 'a handler
    (** [code_handler ~expected_code parse] is a response handler that
        fires for responses with status [expected_code] and applies
        [parse]. *)

    val get :
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

    val get_rate : ?token:Token.t -> unit -> rates Monad.t
    (** [get_rate ()] is the cached rate limit information. *)

    val get_rate_limit : ?token:Token.t -> unit -> int option Monad.t
    (** [get_rate_limit ()] is the cached total request quota for the current token. *)

    val get_rate_remaining : ?token:Token.t -> unit -> int option Monad.t
    (** [get_rate_remaining ()] is the  cached remaining request quota for the current token. *)

    val get_rate_reset : ?token:Token.t -> unit -> float option Monad.t
    (** [get_rate_reset ()] is the cached, UNIX
        epoch expiry time (s) when the remaining request quota will be
        reset to the total request quota for the current token. *)

    val string_of_message : Gitlab_t.message -> string
    (** [string_of_message message] is the English language error
        message that GitLab generated in [message]. *)
  end

  (** User contribution {{:https://docs.gitlab.com/ee/api/events.html}events}.
  *)
  module Event : sig
    val all :
      token:Token.t ->
      ?before:string ->
      ?after:string ->
      ?scope:string ->
      ?sort:Gitlab_t.sort ->
      ?target_type:Gitlab_t.event_target_type ->
      ?action:Gitlab_t.event_action_type ->
      unit ->
      Gitlab_t.events Response.t Monad.t
    (** [all ~token] get a list of events for the authenticated user.

        See {{:https://docs.gitlab.com/ee/api/events.html#list-currently-authenticated-users-events}List currently authenticated user’s events}
    *)
  end

  (** The [User] module provides access to {{:https://docs.gitlab.com/14.0/ee/api/users.html}User API}.
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

    val current_user :
      token:Token.t -> unit -> Gitlab_t.current_user Response.t Monad.t
    (** [current_user ~token ()] is the current user for [token].
        See {{:https://docs.gitlab.com/ee/api/users.html#list-current-user-for-normal-users}Current Authenticated User.}
    *)

    val projects :
      id:string -> unit -> Gitlab_t.projects_short Response.t Monad.t
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
      ?scope:Gitlab_t.merge_request_scope ->
      unit ->
      Gitlab_t.merge_request Stream.t
    (** [merge_requests ()] list all merge requests the authenticated user has access to.

        See {{:https://docs.gitlab.com/14.0/ee/api/merge_requests.html#list-merge-requests}List merge requests}.
    *)

    val issues :
      token:Token.t ->
      ?state:Gitlab_t.state ->
      ?assignee:string ->
      ?assignee_username:string ->
      ?milestone:string ->
      ?labels:string list ->
      ?author:string ->
      ?author_username:string ->
      ?scope:Gitlab_t.merge_request_scope ->
      unit ->
      Gitlab_t.issue Stream.t
    (** [issues ~token ()] returns a list of project issues.

         See {{:https://docs.gitlab.com/ee/api/issues.html#list-issues}List issues}.
       *)

    val events :
      token:Token.t ->
      id:string ->
      ?action:Gitlab_t.event_action_type ->
      ?target_type:Gitlab_t.event_target_type ->
      ?per_page:int ->
      ?after:string ->
      ?sort:Gitlab_t.sort ->
      unit ->
      Gitlab_t.event Stream.t
    (** [events ~token ~id] get the contribution events for the specified user.

        See {{:https://docs.gitlab.com/ee/api/events.html#get-user-contribution-events}Get user contribution events}.
     *)

    (** Personal access tokens for {! User} authentication.

        They may be used to:

        {ul {- Authenticate with the GitLab API.}
            {- Authenticate with Git using HTTP Basic Authentication.}}
     *)
    module PersonalAccessToken : sig
      val tokens :
        token:Token.t ->
        ?user_id:int ->
        unit ->
        Gitlab_t.personal_access_tokens Response.t Monad.t
      (** [person_access_tokens ~token ?user_id ()] get the Personal Access Tokens for the current user. Administrators can use the [~user_id] parameter to filter by user.
          See {{:https://docs.gitlab.com/ee/api/personal_access_tokens.html#list-personal-access-tokens}List personal access tokens}.
       *)

      val revoke : token:Token.t -> id:int -> unit -> unit Response.t Monad.t
      (** [revoke_person_access_tokens ~token ~id] Revoke a personal access token.
          See {{:https://docs.gitlab.com/ee/api/personal_access_tokens.html#revoke-a-personal-access-token}Revoke a personal access token}.
       *)

      val create :
        token:Token.t ->
        user_id:int ->
        Gitlab_t.new_token ->
        unit ->
        Gitlab_t.personal_access_token Response.t Monad.t
      (** Create a personal access token for [~user_id]. See {{:https://docs.gitlab.com/ee/api/users.html#create-a-personal-access-token}Create a personal access token}.
       *)
    end
  end

  (** The [Project] module provides access to {{:https://docs.gitlab.com/ee/api/projects.html}Project API}. *)
  module Project : sig
    val create :
      token:Token.t ->
      name:string ->
      ?description:string ->
      unit ->
      Gitlab_t.project_short Response.t Monad.t
    (** [create ~token ~name ?description ()] Creates a new project owned by the authenticated user.

        See {{:https://docs.gitlab.com/ee/api/projects.html#create-project}Create project}.
    *)

    val by_name :
      ?token:Token.t ->
      owner:string ->
      name:string ->
      unit ->
      Gitlab_t.projects_short Response.t Monad.t
    (** [by_name ~owner ~name ()] retrieves projects owned by [owner] with a name like [name].
        Depending on the [name] used this will return 1 or more matches. Supply a [token] to access private projects.
    *)

    val by_short_ref :
      ?token:Token.t ->
      short_ref:string ->
      unit ->
      Gitlab_t.project_short option Response.t Monad.t
    (** [by_short_ref ~short_ref ()] retrieves the project with [short_ref] (a short ref is a string on the
        form [<namespace>/<project name>]), returns [None] if the project doesn't
        exist.

        Supply a [token] to access private projects.

        See {{:https://docs.gitlab.com/ee/api/projects.html#get-single-project}Get single project}.
    *)

    val by_id :
      ?token:Token.t ->
      project_id:int ->
      unit ->
      Gitlab_t.project_short option Response.t Monad.t
    (** [by_id ~project_id] retrieve the project for [project_id], returns [None] if the project doesn't
        exist. Supply a [token] to access private projects.

        See {{:https://docs.gitlab.com/ee/api/projects.html#get-single-project}Get a single project}.
    *)

    module Branch : sig
      val branches :
        ?token:Token.t ->
        project_id:int ->
        ?search:string ->
        unit ->
        Gitlab_t.branch_full Stream.t
      (** [branches ?token ~project_id] lists repository branches from a project, sorted by name alphabetically.
        Supply a [token] to access private projects.

        See {{:https://docs.gitlab.com/ee/api/branches.html#list-repository-branches}List repository branches}.
       *)

      val branch :
        ?token:Token.t ->
        project_id:int ->
        branch:string ->
        unit ->
        Gitlab_t.branch_full Response.t Monad.t
      (** [branch ?token ~name] gets a single project repository branch.
        Supply a [token] to access private projects.

        See {{:https://docs.gitlab.com/ee/api/branches.html#get-single-repository-branch}Get a single repository branch}.
       *)

      val create :
        token:Token.t ->
        project_id:int ->
        branch:string ->
        ref:string ->
        unit ->
        Gitlab_t.branch_full Response.t Monad.t
      (** [create ~token ~branch ~ref] Create a new branch in the repository.

        See {{:https://docs.gitlab.com/ee/api/branches.html#create-repository-branch}Create repository branch}.
       *)

      val delete :
        token:Token.t ->
        project_id:int ->
        branch:string ->
        unit ->
        unit Response.t Monad.t
      (** [delete ~token ~branch] Delete a branch in the repository.

        See {{:https://docs.gitlab.com/ee/api/branches.html#delete-repository-branch}Delete repository branch}.
       *)

      val delete_merged :
        token:Token.t -> project_id:int -> unit -> unit Response.t Monad.t
      (** [delete ~token ~branch] Delete a branch in the repository.

        See {{:https://docs.gitlab.com/ee/api/branches.html#delete-merged-branches}Delete merged repository branches}.
       *)
    end

    val pipelines :
      token:Token.t ->
      project_id:int ->
      ?per_page:int ->
      ?status:Gitlab_t.pipeline_status ->
      ?source:Gitlab_t.pipeline_source ->
      ?sha:string ->
      ?ref_:string ->
      ?username:string ->
      ?updated_after:string ->
      ?updated_before:string ->
      ?sort:Gitlab_t.sort ->
      ?order_by:[ `Id | `Ref | `Status | `Updated_at | `User_id ] ->
      ?scope:Gitlab_t.pipeline_scope ->
      unit ->
      Gitlab_t.pipeline Stream.t
    (** [pipelines ~token ~project_id ()] list all pipelines the authenticated user has access to in [project_id].

        See {{:https://docs.gitlab.com/ee/api/pipelines.html#list-project-pipelines}List project pipelines}.
     *)

    val pipeline :
      token:Token.t ->
      project_id:int ->
      pipeline_id:int ->
      unit ->
      Gitlab_t.single_pipeline Response.t Monad.t
    (** [pipeline ~token ~project_id ~pipeline_id ()] get one single pipeline [pipeline_id] in [project_id].

        See {{:https://docs.gitlab.com/ee/api/pipelines.html#get-a-single-pipeline} Get a single pipeline}.
     *)

    val pipeline_jobs :
      token:Token.t ->
      project_id:int ->
      ?per_page:int ->
      ?scope:Gitlab_t.pipeline_job_scope ->
      ?include_retried:bool ->
      pipeline_id:int ->
      unit ->
      Gitlab_t.pipeline_job Stream.t
    (** [pipeline_jobs ~token ~project_id pipeline_id ()] get a list of jobs for
        a pipeline [pipeline_id] of a project [project_id].

        See {{:https://docs.gitlab.com/ee/api/jobs.html#list-pipeline-jobs}List pipeline jobs}.
     *)

    val job_trace :
      token:Token.t ->
      project_id:int ->
      job_id:int ->
      unit ->
      string option Response.t Monad.t
    (** [job_trace ~token ~project_id job_id ()] get the log (trace) of a
        specific job [job_id] of a project [project-id].

        See {{:https://docs.gitlab.com/ee/api/jobs.html#get-a-log-file}Get a log file}.
     *)

    val merge_requests :
      ?token:Token.t ->
      ?state:Gitlab_t.state ->
      ?milestone:string ->
      ?labels:string list ->
      ?author:string ->
      ?author_username:string ->
      ?my_reaction:string ->
      ?scope:Gitlab_t.merge_request_scope ->
      ?created_after:string ->
      ?created_before:string ->
      ?updated_after:string ->
      ?updated_before:string ->
      ?sort:Gitlab_t.sort ->
      ?order_by:[ `Created_at | `Title | `Updated_at ] ->
      ?target_branch:string ->
      ?wip:bool ->
      ?per_page:int ->
      id:int ->
      unit ->
      Gitlab_t.merge_request Stream.t
    (** [merge_requests ?token ~id ()] list all merge requests for project [id].

        See {{:https://docs.gitlab.com/14.0/ee/api/merge_requests.html#list-project-merge-requests}List project merge requests}.
     *)

    val merge_request :
      ?token:Token.t ->
      project_id:int ->
      merge_request_iid:string ->
      unit ->
      Gitlab_t.merge_request Response.t Monad.t
    (** [merge_request ?token ~project_id ~merge_request_iid ()] shows information about a single merge request.

        See {{:https://docs.gitlab.com/14.0/ee/api/merge_requests.html#get-single-mr}Get single merge request}.
     *)

    val merge_request_participants :
      ?token:Token.t ->
      project_id:int ->
      merge_request_iid:string ->
      unit ->
      Gitlab_t.users Response.t Monad.t
    (** [merge_request_participants ?token ~project_id ~merge_request_iid ()] gets a list of merge request participants.

        See {{:https://docs.gitlab.com/14.0/ee/api/merge_requests.html#get-single-mr-participants}Get a list of merge request participants}.
     *)

    val merge_request_commits :
      ?token:Token.t ->
      project_id:int ->
      merge_request_iid:string ->
      unit ->
      Gitlab_t.commits Response.t Monad.t
    (** [merge_request_commits ?token ~project_id ~merge_request_iid ()] gets a list of merge request commits.

       See {{:https://docs.gitlab.com/14.0/ee/api/merge_requests.html#get-single-mr-commits}Get single merge request commits}.
     *)

    val merge_request_changes :
      ?token:Token.t ->
      project_id:int ->
      merge_request_iid:string ->
      unit ->
      Gitlab_t.changes Response.t Monad.t
    (** [merge_request_changes ?token ~project_id ~merge_request_iid ()] shows information about the merge request including its files and changes.

       See {{:https://docs.gitlab.com/ee/api/merge_requests.html#get-single-mr-changes}Get single MR changes}.
    *)

    val merge_request_pipelines :
      ?token:Token.t ->
      project_id:int ->
      merge_request_iid:int ->
      unit ->
      Gitlab_t.pipeline Stream.t
    (** [merge_request_pipelines ?token ~project_id ~merge_request_iid ()] gets a list of merge request pipelines.

       See {{:https://docs.gitlab.com/ee/api/merge_requests.html#list-mr-pipelines}List MR pipelines }.
    *)

    val events :
      token:Token.t ->
      project_id:int ->
      ?action:Gitlab_t.event_action_type ->
      ?target_type:Gitlab_t.event_target_type ->
      unit ->
      Gitlab_t.events Response.t Monad.t
    (** [events ~project_id] get visible events for a project.

        See {{:https://docs.gitlab.com/ee/api/events.html#list-a-projects-visible-events}List a projects visible events}.
    *)

    val all_projects :
      token:Token.t ->
      ?owned:bool ->
      ?search:string ->
      ?with_programming_language:string ->
      unit ->
      Gitlab_t.project_short Stream.t
    (** [all_projects ~token ()] Get a list of all visible projects across GitLab for the authenticated user.

        See {{:https://docs.gitlab.com/ee/api/projects.html#list-all-projects}List all projects}.
    *)

    (** External Status Checks API. {{:https://docs.gitlab.com/ee/api/status_checks.html}} *)
    module ExternalStatusCheck : sig
      val list_for_merge_request :
        token:Token.t ->
        project_id:int ->
        merge_request_iid:string ->
        unit ->
        Gitlab_t.status_checks Response.t Monad.t
      (** [list_for_merge_request ~project_id ~merge_request_iid] For a single merge request, list the external status checks that apply to it and their status.

          See {{:https://docs.gitlab.com/ee/api/status_checks.html#list-status-checks-for-a-merge-request}List status checks for a merge request}.
      *)

      val set_status :
        token:Token.t ->
        project_id:int ->
        merge_request_iid:string ->
        sha:string ->
        external_status_check_id:string ->
        unit ->
        Gitlab_t.external_status_check Response.t Monad.t
      (** [set_status ~project_id ~merge_request_iid ~sha ~external_status_check_id] For a single merge request, use the API to inform GitLab that a merge request has passed a check by an external service.

          See {{:https://docs.gitlab.com/ee/api/status_checks.html#set-status-of-an-external-status-check}Set status of an external status check}.
      *)

      val checks :
        token:Token.t ->
        project_id:int ->
        unit ->
        Gitlab_t.external_status_checks Response.t Monad.t
      (** [checks ~project_id ] request project's external status checks.

          See {{:https://docs.gitlab.com/ee/api/status_checks.html#get-project-external-status-checks}Get project external status checks}.
      *)

      val create :
        token:Token.t ->
        project_id:int ->
        name:string ->
        external_url:string ->
        ?protected_branch_ids:int list ->
        unit ->
        Gitlab_t.external_status_check Response.t Monad.t
      (** [create ] create a new external status check for a project.

            See {{:https://docs.gitlab.com/ee/api/status_checks.html#create-external-status-check}Create external status check}.
       *)

      val delete :
        token:Token.t ->
        project_id:int ->
        rule_id:int ->
        unit ->
        unit Response.t Monad.t
      (** [delete ~project_id ~rule_id] an external status check for a project.

          See {{:https://docs.gitlab.com/ee/api/status_checks.html#delete-external-status-check}Delete external status check}.
      *)

      val update :
        token:Token.t ->
        project_id:int ->
        rule_id:int ->
        ?name:string ->
        ?external_url:string ->
        ?protected_branch_ids:int list ->
        unit ->
        Gitlab_t.external_status_check Response.t Monad.t
      (** [update ~project_id ~rule_id] an external status check for a project.


            See {{:https://docs.gitlab.com/ee/api/status_checks.html#update-external-status-check}Update external status check}.
        *)
    end

    (** [Commit] operates on a repository's {{:https://docs.gitlab.com/ee/api/commits.html}commits}. *)
    module Commit : sig
      val commits :
        ?token:Token.t ->
        project_id:int ->
        ?ref_name:string ->
        ?since:string ->
        ?until:string ->
        ?path:string ->
        ?all:bool ->
        unit ->
        Gitlab_t.commit Stream.t

      (** [commits ?token ~project_id ()] list all commits for a project.

          See {{:https://docs.gitlab.com/ee/api/commits.html#list-repository-commits}List repository commits}.
      *)

      val commit :
        ?token:Token.t ->
        project_id:int ->
        sha:string ->
        ?stats:bool ->
        unit ->
        Gitlab_t.commit Response.t Monad.t
      (** [commit ?token ~project_id ~sha] get a specified commit by the commit hash or name of a branch or tag.

          See {{:https://docs.gitlab.com/ee/api/commits.html#get-a-single-commit}Get a single commit}.
      *)

      val comments :
        ?token:Token.t ->
        project_id:int ->
        sha:string ->
        unit ->
        Gitlab_t.commit_comment Stream.t
      (** [comments ?token ~project_id ~sha] get comments of a commit.

          See {{:https://docs.gitlab.com/ee/api/commits.html#get-the-comments-of-a-commit}Get the comments of a commit}.
      *)

      val comment :
        ?token:Token.t ->
        project_id:int ->
        sha:string ->
        Gitlab_t.new_comment ->
        unit ->
        Gitlab_t.commit_commented Response.t Monad.t
      (** [comment ~token ~project_id ~sha ~new_comment] adds a comment to a commit.

          See {{:https://docs.gitlab.com/ee/api/commits.html#post-comment-to-commit}Post comment to a commit}.
      *)

      val statuses :
        ?token:Token.t ->
        project_id:int ->
        sha:string ->
        ?ref_name:string ->
        ?stage:string ->
        ?name:string ->
        ?all:bool ->
        unit ->
        Gitlab_t.commit_status Stream.t
      (** [statuses ~token ~project_id ~sha] lists the statuses of a commit.

          See {{:https://docs.gitlab.com/ee/api/commits.html#list-the-statuses-of-a-commit}List the statuses of a commit in a project}.
      *)

      val status :
        token:Token.t ->
        project_id:int ->
        sha:string ->
        Gitlab_t.new_status ->
        unit ->
        Gitlab_t.commit_status Response.t Monad.t
      (** [status ~token ~project_id ~sha ~state] adds or updates the build status of a commit.

          See {{:https://docs.gitlab.com/ee/api/commits.html#post-the-build-status-to-a-commit}Post the build status to a commit}.
       *)
    end

    (** [Milestone] operates on a [Project]'s milestones.
        There is a separate Group [Milestone] module.
     *)
    module Milestone : sig
      val milestones :
        ?token:Token.t ->
        project_id:int ->
        ?state:Gitlab_t.milestone_state ->
        ?title:string ->
        ?search:string ->
        unit ->
        Gitlab_t.milestones Response.t Monad.t
      (** [milestones ~project_id] returns a list of project milestones.

         See {{:https://docs.gitlab.com/ee/api/milestones.html#list-project-milestones}List project milestones}.
       *)

      val milestone :
        ?token:Token.t ->
        project_id:int ->
        milestone_id:int ->
        unit ->
        Gitlab_t.milestone Response.t Monad.t
      (** [milestone ~project_id ~milestone_id] get a single project milestone.

         See {{:https://docs.gitlab.com/ee/api/milestones.html#get-single-milestone}Get a single milestone}.
       *)

      val create :
        token:Token.t ->
        project_id:int ->
        Gitlab_t.new_milestone ->
        unit ->
        Gitlab_t.milestone Response.t Monad.t
      (** [create ~project_id ~title] create a project milestone.

          See {{:https://docs.gitlab.com/ee/api/milestones.html#create-new-milestone}Create a new milestone}.
       *)

      val update :
        token:Token.t ->
        project_id:int ->
        milestone_id:int ->
        ?title:string ->
        ?description:string ->
        ?due_date:string ->
        ?start_date:string ->
        ?state_event:Gitlab_t.milestone_state ->
        unit ->
        Gitlab_t.milestone Response.t Monad.t
      (** [update ~project_id ~milestone_id] update an existing milestone.

          See {{:https://docs.gitlab.com/ee/api/milestones.html#edit-milestone}Edit a milestone}.
       *)

      val delete :
        token:Token.t ->
        project_id:int ->
        milestone_id:int ->
        unit ->
        unit Response.t Monad.t
      (** [delete ~project_id ~milestone_id] delete a project milestone.

          See {{:https://docs.gitlab.com/ee/api/milestones.html#delete-project-milestone}Delete a milestone}.
        *)
    end

    (** Project access tokens for {! Project} authentication.

        They may be used to:

        {ul {- Authenticate with the GitLab API.}
            {- Authenticate with Git using HTTP Basic Authentication.}}
         *)
    module ProjectAccessToken : sig
      val tokens :
        token:Token.t ->
        project_id:int ->
        unit ->
        Gitlab_t.project_access_tokens Response.t Monad.t
      (** [tokens ~token ?project_id ()] get the Project Access Tokens for [~project_id].
          See {{:https://docs.gitlab.com/ee/api/resource_access_tokens.html#list-project-access-tokens}List project access tokens}.
       *)

      val revoke :
        token:Token.t ->
        project_id:int ->
        id:int ->
        unit ->
        unit Response.t Monad.t
      (** [revoke ~token ~id] Revoke a project access token.
          See {{:https://docs.gitlab.com/ee/api/resource_access_tokens.html#revoke-a-project-access-token}Revoke a project access token}.
       *)

      val create :
        token:Token.t ->
        project_id:int ->
        Gitlab_t.new_token ->
        unit ->
        Gitlab_t.project_access_token Response.t Monad.t
      (** Create a project access token for [~project_id]. See {{:https://docs.gitlab.com/ee/api/resource_access_tokens.html#create-a-project-access-token}Create a project access token}.
       *)
    end

    module Issue : sig
      val list :
        ?token:Token.t -> project_id:int -> unit -> Gitlab_t.issue Stream.t
      (** [list ?token ~project_id] Request a list of a project’s issues.
         See {{:https://docs.gitlab.com/ee/api/issues.html#list-project-issues}List project issues}.
        *)

      val by_id :
        ?token:Token.t ->
        project_id:int ->
        issue_id:int ->
        unit ->
        Gitlab_t.issue Response.t Monad.t
      (** [by_id ?token ~project_id ~issue_id] Get a single project issue.
        See {{:https://docs.gitlab.com/ee/api/issues.html#single-project-issue}Single project issue}.
        *)

      val create :
        token:Token.t ->
        project_id:int ->
        Gitlab_t.create_issue ->
        unit ->
        Gitlab_t.issue Response.t Monad.t
      (** Creates a new issue.
          See {{:https://docs.gitlab.com/ee/api/issues.html#new-issue}New issue}.
        *)
    end

    module Hook : sig
      val list :
        ?token:Token.t ->
        project_id:int ->
        unit ->
        Gitlab_t.project_hooks Response.t Monad.t
      (** [list ?token ~project_id] Request a list of a project’s hooks.
        See {{:https://docs.gitlab.com/ee/api/projects.html#list-project-hooks}List project hooks}.
        *)

      val by_id :
        ?token:Token.t ->
        project_id:int ->
        hook_id:int ->
        unit ->
        Gitlab_t.project_hook Response.t Monad.t
      (** [by_id ?token ~project_id ~hook_id] Get a single project hook.
        See {{:https://docs.gitlab.com/ee/api/projects.html#get-project-hook}Get project hook}.
        *)

      val create :
        token:Token.t ->
        project_id:int ->
        Gitlab_t.create_project_hook ->
        unit ->
        Gitlab_t.project_hook Response.t Monad.t
      (** Creates a new webhook.
        See {{:https://docs.gitlab.com/ee/api/projects.html#add-project-hook}Add project hook}.
        *)
    end

    (** The [Notes] module provides access to
        {{:https://docs.gitlab.com/ee/api/notes.html#merge-requests}Notes
        API}.
     *)
    module Notes : sig
      (** The [Merge_request] module provides access to
          {{:https://docs.gitlab.com/ee/api/notes.html#merge-requests}Merge
          requests notes API}.
       *)
      module Merge_request : sig
        val list :
          ?token:Token.t ->
          project_id:int ->
          merge_request_iid:string ->
          ?sort:Gitlab_t.sort ->
          unit ->
          Gitlab_t.note Stream.t
        (** [list ?token ~project_id ~merge_request_iid]
            Request a list of a merge request notes. See
            {{:https://docs.gitlab.com/ee/api/notes.html#list-all-merge-request-notes}List
            all merge request notes}.
         *)

        val by_id :
          ?token:Token.t ->
          project_id:int ->
          merge_request_iid:string ->
          note_id:int ->
          unit ->
          Gitlab_t.note Response.t Monad.t
        (** [by_id ?token ~project_id ~merge_request_iid ~note_id]
            Get a single note for a given merge request. See
            {{:https://docs.gitlab.com/ee/api/notes.html#get-single-merge-request-note}Get
            single merge request note}.
         *)

        val create :
          token:Token.t ->
          project_id:int ->
          merge_request_iid:string ->
          create_note:Gitlab_t.create_note ->
          unit ->
          Gitlab_t.note Response.t Monad.t
        (** [create ?token ~project_id ~merge_request_iid ~create_note]
            Creates a new note. See
            {{:https://docs.gitlab.com/ee/api/notes.html#create-new-merge-request-note}Create
            new merge request note}.
         *)

        val update :
          token:Token.t ->
          project_id:int ->
          merge_request_iid:string ->
          note_id:int ->
          body:string ->
          unit ->
          Gitlab_t.note Response.t Monad.t
        (** [update ?token ~project_id ~merge_request_iid ~note_id ~body]
          Updates a note. See
          {{:https://docs.gitlab.com/ee/api/notes.html#modify-existing-merge-request-note}Modify
          existing note of a merge request}.
       *)

        val delete :
          token:Token.t ->
          project_id:int ->
          merge_request_iid:string ->
          note_id:int ->
          unit ->
          unit Response.t Monad.t
        (** [delete ?token ~project_id ~merge_request_iid ~note_id]
          Deletes a note. See
          {{:https://docs.gitlab.com/ee/api/notes.html#delete-a-merge-request-note}Delete
          a merge request note}.
       *)
      end
    end
  end

  (** The [Group] module provides access to {{:https://docs.gitlab.com/ee/api/groups.html}Group API}. *)
  module Group : sig
    module Project : sig
      val by_name :
        ?token:Token.t ->
        owner:string ->
        name:string ->
        unit ->
        Gitlab_t.projects_short Response.t Monad.t
      (** [by_name ~group ~name ()] retrieves projects owned by [group] with a name like [name].
        Depending on the [name] used this will return 1 or more matches. Supply a [token] to access private projects.

        There is no direct fetch by name API in GitLab.
     *)
    end

    val merge_requests :
      ?token:Token.t ->
      ?state:Gitlab_t.state ->
      ?milestone:string ->
      ?labels:string list ->
      ?author:string ->
      ?author_username:string ->
      ?my_reaction:string ->
      ?scope:Gitlab_t.merge_request_scope ->
      id:string ->
      unit ->
      Gitlab_t.merge_request Stream.t
    (** [merge_requests ?token ~id ()] list all merge requests for group [id].

        See {{:https://docs.gitlab.com/14.0/ee/api/merge_requests.html#list-group-merge-requests}List group merge requests}.
     *)

    (** [Milestone] operates on a [Group]'s milestones. *)
    module Milestone : sig
      val milestones :
        ?token:Token.t ->
        group_id:int ->
        ?state:Gitlab_t.milestone_state ->
        ?title:string ->
        ?search:string ->
        unit ->
        Gitlab_t.milestones Response.t Monad.t
      (** [milestones ~group_id] returns a list of group milestones.

         See {{:https://docs.gitlab.com/ee/api/milestones.html#list-group-milestones}List group milestones}.
       *)

      val milestone :
        ?token:Token.t ->
        group_id:int ->
        milestone_id:int ->
        unit ->
        Gitlab_t.milestone Response.t Monad.t
      (** [milestone ~group_id ~milestone_id] get a single group milestone.

         See {{:https://docs.gitlab.com/ee/api/milestones.html#get-single-milestone}Get a single milestone}.
       *)

      val create :
        token:Token.t ->
        group_id:int ->
        Gitlab_t.new_milestone ->
        unit ->
        Gitlab_t.milestone Response.t Monad.t
      (** [create ~group_id ~title] create a group milestone.

          See {{:https://docs.gitlab.com/ee/api/milestones.html#create-new-milestone}Create a new milestone}.
       *)

      val update :
        token:Token.t ->
        group_id:int ->
        milestone_id:int ->
        ?title:string ->
        ?description:string ->
        ?due_date:string ->
        ?start_date:string ->
        ?state_event:Gitlab_t.milestone_state ->
        unit ->
        Gitlab_t.milestone Response.t Monad.t
      (** [update ~group_id ~milestone_id] update an existing milestone.

          See {{:https://docs.gitlab.com/ee/api/milestones.html#edit-milestone}Edit a milestone}.
       *)

      val delete :
        token:Token.t ->
        group_id:int ->
        milestone_id:int ->
        unit ->
        unit Response.t Monad.t
      (** [delete ~group_id ~milestone_id] delete a group milestone.

          See {{:https://docs.gitlab.com/ee/api/milestones.html#delete-group-milestone}Delete a milestone}.
        *)
    end

    (** [Issue] operates on a [Group]'s issues.
       There is a separate [User] and [Project] issue module.
    *)
    module Issue : sig
      val issues :
        token:Token.t ->
        group_id:int ->
        ?state:Gitlab_t.state ->
        ?assignee:string ->
        ?assignee_username:string ->
        ?milestone:string ->
        ?labels:string list ->
        ?author:string ->
        ?author_username:string ->
        ?scope:Gitlab_t.merge_request_scope ->
        unit ->
        Gitlab_t.issue Stream.t
      (** [issues ~token ~group_id ()] returns a list of group issues.

        See {{:https://docs.gitlab.com/ee/api/issues.html#list-group-issues}List group issues}.
      *)
    end
  end

  (** The [Runners] module provides access to {{:https://docs.gitlab.com/ee/api/runners.html}Runners API}.
  *)
  module Runners : sig
    val list : token:Token.t -> unit -> Gitlab_t.runners Response.t Monad.t
    (** [list ~token] Get a list of specific runners available to the user.*)
  end
end

(** A module of this type is required in order to construct a
    {!Gitlab} module using {!Gitlab_core.Make}. *)
module type Env = sig
  val debug : bool
  (** [debug] is the initial debugging flag value. *)

  val gitlab_uri : string
  (** [gitlab_uri] is the gitlab instance to connect to. *)
end

(** A module of this type is required in order to construct a
    {!Gitlab} module using {!Gitlab_core.Make}. *)
module type Time = sig
  val now : unit -> float
  (** [now ()] is the current UNIX epoch time in seconds. *)

  val sleep : float -> unit Lwt.t
  (** [sleep sec] activates after [sec] seconds have elapsed. *)
end
