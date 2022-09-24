Mix.install([
  {:req, "~> 0.2"},
  {:timex, "~> 3.7.9"}
])

strava_client_id = System.get_env("STRAVA_CLIENT_ID")
strava_refresh_token = System.get_env("STRAVA_REFRESH_TOKEN")
strava_client_secret = System.get_env("STRAVA_CLIENT_SECRET")

refresh_token_url =
  "https://www.strava.com/oauth/token?" <>
    "grant_type=refresh_token&refresh_token=#{strava_refresh_token}&" <>
    "client_id=#{strava_client_id}&client_secret=#{strava_client_secret}"

%{
  body: %{
    "access_token" => access_token,
    "refresh_token" => refresh_token
  }
} = Req.post!(refresh_token_url)

# TODO push refresh token to github secret might not be needed...

if System.get_env("DBEUG") == "1" do
  IO.puts(
    "\nAccess token: #{access_token}\nRefresh token: #{refresh_token}\n\n" <>
      "refresh token url: #{refresh_token_url}"
  )
end

after_unix = Date.utc_today() |> Timex.shift(months: -1) |> Timex.to_unix()

{:ok, %{body: activities}} =
  Req.get("https://www.strava.com/api/v3/athlete/activities?after=#{after_unix}&per_page=200",
    headers: [{"Authorization", "Bearer #{access_token}"}]
  )

summary_acc = %{
  total_distance: 0,
  total_time: 0,
  avg_pace: 0,
  total_elevation_gain: 0
}

{summary, number_of_runs} =
  activities
  |> Enum.filter(&(&1["type"] == "Run"))
  |> Enum.reduce({summary_acc, 0}, fn %{
                                        "distance" => run_distance,
                                        "elapsed_time" => run_time,
                                        "average_speed" => run_pace,
                                        "total_elevation_gain" => run_ascent
                                      },
                                      {%{
                                         total_distance: total_distance,
                                         total_time: total_time,
                                         avg_pace: avg_pace,
                                         total_elevation_gain: total_elevation_gain
                                       }, index} ->
    {
      %{
        total_distance: total_distance + run_distance,
        total_time: total_time + run_time,
        avg_pace: avg_pace + run_pace,
        total_elevation_gain: total_elevation_gain + run_ascent
      },
      index + 1
    }
  end)

# TODO generate stats (Runs, Elevation Gain, Avg/pace, Distance, From date)

defmodule Format do
  @one_minute 60
  @one_hour 3600
  def to_hh_mm_ss(seconds) when seconds >= @one_hour do
    h = div(seconds, @one_hour)

    m =
      seconds
      |> rem(@one_hour)
      |> div(@one_minute)
      |> pad_int()

    s =
      seconds
      |> rem(@one_hour)
      |> rem(@one_minute)
      |> pad_int()

    "#{h}:#{m}:#{s}"
  end

  def to_hh_mm_ss(seconds) do
    m = div(seconds, @one_minute)

    s =
      seconds
      |> rem(@one_minute)
      |> pad_int()

    "00:#{m}:#{s}"
  end

  defp pad_int(int, padding \\ 2) do
    int
    |> Integer.to_string()
    |> String.pad_leading(padding, "0")
  end
end

# Avg/Pace: #{(summary.avg_pace / number_of_runs) |> :erlang.float_to_binary(decimals: 2)}

new_readme =
  "README.md.eex"
  |> File.read!()
  |> EEx.eval_string(
    number_of_runs: number_of_runs,
    total_elevation_gain: :erlang.float_to_binary(summary.total_elevation_gain, decimals: 2),
    total_time: Format.to_hh_mm_ss(summary.total_time),
    total_distance:
      summary.total_distance |> Kernel./(1000) |> :erlang.float_to_binary(decimals: 2)
  )

File.write!("README.md", new_readme)
