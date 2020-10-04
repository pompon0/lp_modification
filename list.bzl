def list_existing():
  existing = native.existing_rules()
  for x in existing:
    print(x,existing[x])
