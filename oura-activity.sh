curl --request GET 'https://api.ouraring.com/v2/usercollection/daily_activity?start_date=2023-11-01&end_date=2023-11-07' --header 'Authorization: Bearer 2G32LBAM3Q2ZN6TRERAK4MTWMON6AOGQ' | python -m json.tool > activity.json
curl --request GET 'https://api.ouraring.com/v2/usercollection/daily_readiness?start_date=2023-11-01&end_date=2023-11-07' --header 'Authorization: Bearer 2G32LBAM3Q2ZN6TRERAK4MTWMON6AOGQ' | python -m json.tool > readiness.json
curl --request GET 'https://api.ouraring.com/v2/usercollection/daily_sleep?start_date=2023-11-01&end_date=2023-11-07' --header 'Authorization: Bearer 2G32LBAM3Q2ZN6TRERAK4MTWMON6AOGQ' | python -m json.tool > sleep.json
# Once you have these JSON blob, you read out `data`. This provides a list of
# elements. In each element, you read out `day` and `score`, and put that into
# the PACT dB.
