# example
# in WORKSPACE:
#
# load("//:help.bzl", "list_existing")
# list_existing()

def list_existing():
  existing = native.existing_rules()
  for x in existing:
    print(x,existing[x])

# example:
# output_groups(
#   name = "dump",
#   target = ":some_library",
# )
def _output_groups(ctx):
  target = ctx.attr.input
  print(input.output_groups)

output_groups = rule(
  implementation = _output_groups,
  attrs = {
    "target": attr.label(),
  },
)
