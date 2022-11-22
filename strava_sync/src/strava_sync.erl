-module(strava_sync).

-export([main/1]).

-type args() :: list().
-type epoch() :: calendar:time().
-type method() :: hackney:method().
-type url() :: hackney:url().
-type headers() :: hackney:headers().
-type body() :: hackney:body().
-type options() :: hackney:options().
-type summary() :: #{list() => number() | list()}.

%% escript Entry point
-spec main(args()) -> no_return().
main(_Args) ->
    application:ensure_all_started(hackney),
    RefreshTokenUrl = refresh_token_url(),
    #{<<"access_token">> := AccessToken} = make_request(post, RefreshTokenUrl, [], <<>>, []),
    AfterUnix = integer_to_list(one_month_ago()),
    ActivitiesUrl = activities_url(AfterUnix),
    HeaderHead = <<"Bearer ">>,
    Headers = [{<<"Authorization">>, <<HeaderHead/binary, AccessToken/binary>>}],
    Activities = make_request(get, ActivitiesUrl, Headers, <<>>, []),
    RunsSummary = runs_summary(Activities, empty_summary()),
    Template = bbmustache:parse_file(<<"../README.mustache">>),
    file:write_file("../README.md", bbmustache:compile(Template, RunsSummary)),
    io:format("Sumary: ~p~n One month ago: ~p~n", [RunsSummary, one_month_ago()]),
    erlang:halt(0).

-spec refresh_token_url() -> list().
refresh_token_url() ->
    StravaClientId = os:getenv("STRAVA_CLIENT_ID"),
    StravaRefreshToken = os:getenv("STRAVA_REFRESH_TOKEN"),
    StravaClientSecret = os:getenv("STRAVA_CLIENT_SECRET"),

    "https://www.strava.com/oauth/token?" ++
        "grant_type=refresh_token&refresh_token=" ++ StravaRefreshToken ++ "&" ++
        "client_id=" ++ StravaClientId ++ "&client_secret=" ++ StravaClientSecret.

-spec activities_url(epoch()) -> list().
activities_url(AfterUnix) ->
    "https://www.strava.com/api/v3/athlete/activities?after=" ++ AfterUnix ++ "&per_page=200".

-spec make_request(method(), url(), headers(), body(), options()) -> map().
make_request(Method, Url, Headers, Payload, Options) ->
    {ok, _S, _H, Ref} = hackney:request(Method, Url, Headers, Payload, Options),
    {ok, Body} = hackney:body(Ref),
    % file:write_file("activities.json", Body),
    jsx:decode(Body, [{return_maps, true}]).

-spec one_month_ago() -> epoch().
one_month_ago() ->
    OneMonthAgo = edate:shift(edate:today(), -1, month),
    EpochSeconds = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    calendar:datetime_to_gregorian_seconds({OneMonthAgo, {0, 0, 0}}) - EpochSeconds.

-spec runs_summary([map()], summary()) -> summary().
runs_summary(
    [],
    Acc = #{
        "total_time" := TotalTime,
        "total_distance" := TotalDistance,
        "total_elevation_gain" := TotalElevationGain
    }
) ->
    Acc#{
        "total_time" => convert_seconds:convert(TotalTime),
        "total_distance" => float_to_binary(TotalDistance / 1000, [{decimals, 2}]),
        "total_elevation_gain" => float_to_binary(TotalElevationGain, [{decimals, 2}])
    };
runs_summary([#{<<"type">> := <<"Run">>} = H | T], Acc) ->
    NewSummary = add_run(Acc, H),
    runs_summary(T, NewSummary);
runs_summary([_H | T], Acc) ->
    runs_summary(T, Acc).

-spec empty_summary() -> summary().
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

-spec add_run(summary(), map()) -> summary().
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
summary_test_() ->
    {ok, Activities} = file:read_file("activities.json"),
    ActivitiesDecoded = jsx:decode(Activities, [{return_maps, true}]),
    RunsSummary = runs_summary(ActivitiesDecoded, empty_summary()),

    [
        ?_assertEqual(
            #{
                "avg_pace" => 32.654,
                "from_date" => edate:date_to_string(edate:shift(edate:today(), -1, month)),
                "number_of_runs" => 13,
                "to_date" => edate:date_to_string(edate:today()),
                "total_distance" => <<"84.29">>,
                "total_elevation_gain" => <<"3328.50">>
            },
            maps:remove("total_time", RunsSummary)
        )
    ].
% #TODO total_time
-endif.
