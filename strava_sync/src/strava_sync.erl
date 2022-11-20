-module(strava_sync).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    application:ensure_all_started(hackney),
    RefreshTokenUrl = refresh_token_url(),
    #{<<"access_token">> := AccessToken} = make_request(post, RefreshTokenUrl, [], <<>>, []),
    AfterUnix = integer_to_list(one_month_ago()),
    ActivitiesUrl = activities_url(AfterUnix),
    HeaderHead = <<"Bearer ">>,
    Headers = [{<<"Authorization">>, <<HeaderHead/binary, AccessToken/binary>>}],
    Activities = make_request(get, ActivitiesUrl, Headers, <<>>, []),
    % {ok, Activities} = file:read_file("activities.json"),
    RunsSummary = runs_summary(Activities, empty_summary()),
    Template = bbmustache:parse_file(<<"../README.mustache">>),
    file:write_file("../README.md", bbmustache:compile(Template, RunsSummary)),
    io:format("Sumary: ~p~n One month ago: ~p~n", [RunsSummary, one_month_ago()]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
refresh_token_url() ->
    StravaClientId = os:getenv("STRAVA_CLIENT_ID"),
    StravaRefreshToken = os:getenv("STRAVA_REFRESH_TOKEN"),
    StravaClientSecret = os:getenv("STRAVA_CLIENT_SECRET"),

    "https://www.strava.com/oauth/token?" ++
        "grant_type=refresh_token&refresh_token=" ++ StravaRefreshToken ++ "&" ++
        "client_id=" ++ StravaClientId ++ "&client_secret=" ++ StravaClientSecret.

activities_url(AfterUnix) ->
    "https://www.strava.com/api/v3/athlete/activities?after=" ++ AfterUnix ++ "&per_page=200".

make_request(Method, Url, Headers, Payload, Options) ->
    {ok, _S, _H, Ref} = hackney:request(Method, Url, Headers, Payload, Options),
    {ok, Body} = hackney:body(Ref),
    % file:write_file("activities.json", Body),
    jsx:decode(Body, [{return_maps, true}]).

one_month_ago() ->
    OneMonthAgo = edate:shift(edate:today(), -1, month),
    EpochSeconds = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    calendar:datetime_to_gregorian_seconds({OneMonthAgo, {0, 0, 0}}) - EpochSeconds.

runs_summary(
    [],
    Acc = #{
        "total_time" := TotalTime,
        "total_distance" := TotalDistance
    }
) ->
    Acc#{
        "total_time" => convert_seconds:convert(TotalTime),
        "total_distance" => float_to_binary(TotalDistance / 1000, [{decimals, 2}])
    };
runs_summary([#{<<"type">> := <<"Run">>} = H | T], Acc) ->
    NewSummary = add_run(Acc, H),
    runs_summary(T, NewSummary);
runs_summary([_H | T], Acc) ->
    runs_summary(T, Acc).

% todo from and to day in last list item signature
empty_summary() ->
    #{
        "total_distance" => 0,
        "total_time" => 0,
        "avg_pace" => 0,
        "total_elevation_gain" => 0,
        "number_of_runs" => 0,
        "from_date" => edate:date_to_string(edate:shift(edate:today(), -1, month)),
        "to_date" => edate:date_to_string(edate:today())
    }.

add_run(
    #{
        "total_distance" := TotalDistance,
        "total_time" := TotalTime,
        "avg_pace" := AvgPace,
        "total_elevation_gain" := TotalElevationGain,
        "number_of_runs" := NumberOfRuns
    } = Summary,
    #{
        <<"distance">> := RunDistance,
        <<"elapsed_time">> := RunTime,
        <<"average_speed">> := RunPace,
        <<"total_elevation_gain">> := RunAscent
    }
) ->
    Summary#{
        "total_distance" => TotalDistance + RunDistance,
        "total_time" => TotalTime + RunTime,
        "avg_pace" => AvgPace + RunPace,
        "total_elevation_gain" => TotalElevationGain + RunAscent,
        "number_of_runs" => NumberOfRuns + 1
    }.
