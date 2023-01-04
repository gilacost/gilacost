Mix.install([
  {:csv, "~> 2.0.0"}
])

concepts_to_reject = [
  "TRANSFERENCIA SEPA",
  "COMPENSACIÓN DE REDONDEOS",
  "SUSCRIPCIÓN FONDO",
  "REEMBOLSO FONDO"
]

acc_init = %{
  "COMISIÓN CUSTODIA INVERSIS" => 0,
  "COMISIÓN DE GESTIÓN INDEXA" => 0,
  "TRANSFERENCIA SEPA (RETIRADA)" => 0
}

"movimientos_2023-01-04.csv"
|> File.stream!()
|> CSV.decode!(headers: true, validate_row_length: true)
|> Enum.into([])
|> Enum.reject(fn %{"Movimiento" => concept} -> concept in concepts_to_reject end)
|> Enum.reduce(acc_init, fn %{"Movimiento" => concept, "Importe" => value}, acc ->
  {_current_value, new_acc} =
    Map.get_and_update(acc, concept, fn current_value ->
      {current_value, current_value + (value |> Float.parse() |> elem(0))}
    end)

  new_acc
end)
|> IO.inspect()
