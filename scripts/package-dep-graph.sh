#!/bin/bash -eu

################################################################################
# Check deps
################################################################################

if ! command -v dot >/dev/null 2>&1; then
    echo "\`dot\` not found. It is usually in the \`graphviz\` package."
    exit 1
fi

if ! command -v tred >/dev/null 2>&1; then
    echo "\`tred\` not found. It is usually in the \`graphviz\` package."
    exit 1
fi

################################################################################
# Argument processing
################################################################################

script=$(basename "$0")
stack_dot_flags=""
include_test_bench="0"
use_tred="1"
format="png"

while getopts "hetfsj" opt; do
  case $opt in
    h)
      echo "usage: ./${script} OPTS" >&2
      echo "OPTS:" >&2
      echo "    -h : show help" >&2
      echo "    -e : include external dependencies in graph" >&2
      echo "    -t : include test+bench packages in graph" >&2
      echo "    -f : don't use \`tred\` - render full deg graph" >&2
      echo "    -s : render as SVG" >&2
      echo "    -j : render as JPG" >&2
      exit 0
      ;;
    e)
      stack_dot_flags="${stack_dot_flags} --external"
      ;;
    t)
      include_test_bench="1"
      ;;
    f)
      use_tred="0"
      ;;
    s)
      format="svg"
      ;;
    j)
      format="jpg"
      ;;
    ?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done


################################################################################
# Generate graph
################################################################################

tmpdir=$(mktemp -d "${TMPDIR:-/tmp/}$(basename "$0").XXXXXXXXXXXX")

# 'tred' and 'dot' are in the 'graphviz' of most Linux distributions.

if [ "${include_test_bench}" = "0" ]; then
    prunefiles=$(find . -name \*.cabal -exec basename {} \; \
        | grep -v stack-work | grep "test.cabal\\|bench.cabal" \
        | sed 's/\.cabal//' | tr '\n' ',')
    if [ -n "${prunefiles}" ]; then
        stack_dot_flags="${stack_dot_flags} --prune ${prunefiles}"
    fi
fi

# This is a weird hack to satisfy shellcheck. While strange, it works, and is
# technically safer.
output="yes"
stack dot ${output:+${stack_dot_flags}} > "${tmpdir}/full-dependencies.dot"

final_dotfile=""
if [ "${use_tred}" = "1" ]; then
    final_dotfile="${tmpdir}/direct-dependencies.dot"
    tred "${tmpdir}/full-dependencies.dot" > "${final_dotfile}"
else
    final_dotfile="${tmpdir}/full-dependencies.dot"
fi

gsed -i '/cardano-sl-infra/s/solid/solid,style=filled,fillcolor=SeaGreen/' ${final_dotfile}
gsed -i '/cardano-sl-auxx/s/solid/solid,style=filled,fillcolor=SandyBrown/' ${final_dotfile}
gsed -i '/cardano-sl-generator/s/solid/solid,style=filled,fillcolor=Salmon/' ${final_dotfile}
gsed -i '/cardano-sl-binary/s/solid/solid,style=filled,fillcolor=RoyalBlue/' ${final_dotfile}

# blockchain packages
gsed -i '/cardano-sl-chain/s/solid/solid,style=filled,fillcolor=Plum/' ${final_dotfile}
gsed -i '/cardano-sl-delegation/s/solid/solid,style=filled,fillcolor=Plum/' ${final_dotfile}
gsed -i '/cardano-sl-txp/s/solid/solid,style=filled,fillcolor=Plum/' ${final_dotfile}
gsed -i '/cardano-sl-ssc/s/solid/solid,style=filled,fillcolor=Plum/' ${final_dotfile}
gsed -i '/cardano-sl-lrc/s/solid/solid,style=filled,fillcolor=Plum/' ${final_dotfile}
gsed -i '/cardano-sl-update/s/solid/solid,style=filled,fillcolor=Plum/' ${final_dotfile}
gsed -i '/cardano-sl-block/s/solid/solid,style=filled,fillcolor=Plum/' ${final_dotfile}


gsed -i '/"cardano-sl"/s/solid/solid,style=filled,fillcolor=PaleGreen/' ${final_dotfile}
gsed -i '/cardano-sl-networking/s/solid/solid,style=filled,fillcolor=Red/' ${final_dotfile}
gsed -i '/cardano-sl-core/s/solid/solid,style=filled,fillcolor=Tomato/' ${final_dotfile}
gsed -i '/cardano-sl-crypto/s/solid/solid,style=filled,fillcolor=Yellow/' ${final_dotfile}
gsed -i '/cardano-sl-db/s/solid/solid,style=filled,fillcolor=YellowGreen/' ${final_dotfile}
gsed -i '/cardano-sl-node-ipc/s/solid/solid,style=filled,fillcolor=Sienna/' ${final_dotfile}
gsed -i '/"cardano-sl-node"/s/solid/solid,style=filled,fillcolor=SkyBlue/' ${final_dotfile}
gsed -i '/cardano-sl-explorer/s/solid/solid,style=filled,fillcolor=PaleVioletRed/' ${final_dotfile}
gsed -i '/cardano-sl-client/s/solid/solid,style=filled,fillcolor=LightCoral/' ${final_dotfile}
gsed -i '/"cardano-sl-wallet"/s/solid/solid,style=filled,fillcolor=Purple/' ${final_dotfile}
gsed -i '/cardano-sl-tools/s/solid/solid,style=filled,fillcolor=VioletRed/' ${final_dotfile}
gsed -i '/cardano-sl-wallet-new/s/solid/solid,style=filled,fillcolor=Cornsilk/' ${final_dotfile}

if [ "${format}" = "svg" ]; then
    outfile="cardano-sl-pkg-deps.svg"
    dot -Tsvg "${final_dotfile}" -o ${outfile}
elif [ "${format}" = "png" ]; then
    outfile="cardano-sl-pkg-deps.png"
    dot -Tpng "${final_dotfile}" -o ${outfile}
elif [ "${format}" = "jpg" ]; then
    outfile="cardano-sl-pkg-deps.jpg"
    dot -Tjpg "${final_dotfile}" -o ${outfile}
fi

rm -rf "${tmpdir:?}/"

echo "Generated ${outfile}"
