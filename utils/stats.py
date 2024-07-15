# statistics functions

def percentile(d, p):
  'value to percentile'
  return round((len(d[d < p]) / len(d)) * 100)

def get_value(d, p):
  'percentile to value'
  return d[round((p / 100) * len(d))]

def truncate(x: float, places: int) -> float:
  s = str(x).split(".")
  integer_part = s[0]
  fractional_part = s[1][:places]
  return float(integer_part + "." + fractional_part)