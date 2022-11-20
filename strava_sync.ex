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

defmodule Convert do
  @minute 60
  @hour @minute * 60
  @day @hour * 24
  @week @day * 7
  @divisor [@week, @day, @hour, @minute, 1]

  def sec_to_str(sec) do
    {_, [s, m, h, d, w]} =
      Enum.reduce(@divisor, {sec, []}, fn divisor, {n, acc} ->
        {rem(n, divisor), [div(n, divisor) | acc]}
      end)

    ["#{w} wk", "#{d} d", "#{h} hr", "#{m} min", "#{s} sec"]
    |> Enum.reject(fn str -> String.starts_with?(str, "0") end)
    |> Enum.join(", ")
  end
end

new_readme =
  "README.md.eex"
  |> File.read!()
  |> EEx.eval_string(
    number_of_runs: number_of_runs,
    total_elevation_gain: :erlang.float_to_binary(summary.total_elevation_gain, decimals: 2),
    total_time: Convert.sec_to_str(summary.total_time),
    total_distance:
      summary.total_distance |> Kernel./(1000) |> :erlang.float_to_binary(decimals: 2),
    from_date: after_unix |> DateTime.from_unix!() |> DateTime.to_date(),
    to_date: DateTime.utc_now() |> DateTime.to_date()
  )

File.write!("README.md", new_readme)
