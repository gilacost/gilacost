Mix.install([
  {:req, "~> 0.2"},
  {:timex, "~> 3.7.9"}
])

strava_client_id = System.get_env("STRAVA_CLIENT_ID")
strava_refresh_token = System.get_env("STRAVA_REFRESH_TOKEN")
strava_client_secret = System.get_env("STRAVA_CLIENT_SECRET")

# E.g ["2022-01-01", "g12205803", "0"]
[from, gear_id, chunk_index] = System.argv()
{chunk_index_int, ""} = Integer.parse(chunk_index)

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

after_unix = DateTime.from_iso8601("#{from}T00:00:00Z") |> elem(1) |> DateTime.to_unix()

{:ok, %{body: activities}} =
  Req.get("https://www.strava.com/api/v3/athlete/activities?after=#{after_unix}&per_page=200",
    headers: [{"Authorization", "Bearer #{access_token}"}]
  )

# Strava api rate limit is 100 requests every 15 minutes let's chunk the activities into 50

activities
|> Enum.filter(&(&1["type"] == "Run"))
|> Enum.reject(fn %{"gear_id" => gear_id} ->
  gear_id != nil
end)
|> Enum.chunk_every(50)
|> Enum.at(chunk_index_int)
|> Enum.each(fn %{"id" => activity_id} ->
  IO.puts("Updating activity with id #{activity_id} to use gear #{gear_id}")

  {:ok, %{body: %{"gear_id" => ^gear_id}}} =
    Req.put("https://www.strava.com/api/v3/activities/#{activity_id}",
      headers: [
        {"Authorization", "Bearer #{access_token}"},
        {"Content-Type", "application/json"}
      ],
      body: Jason.encode!(%{"gear_id" => gear_id})
    )

  Process.sleep(1000)
end)
