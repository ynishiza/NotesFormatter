#!/usr/bin/env bash
set -eu -o pipefail

# shellcheck disable=SC2034
__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC2034
__file="${__dir}/$(basename "${BASH_SOURCE[0]}")"

[[ $# != 1 ]] && echo "Provide project path" && exit 1

PROJECT_PATH="$1"
PROJECT_NAME=$(basename "$PROJECT_PATH")

[[ -d "$PROJECT_PATH" ]] && echo "$PROJECT_PATH already exists" && exit 1

echo "Creating project $PROJECT_NAME at $PROJECT_PATH"

# Step: create project dir and copy files
mkdir -p "$PROJECT_PATH"
cp -r ./*  "$PROJECT_PATH"
cp .gitignore .hlint.yaml  "$PROJECT_PATH"
cd "$PROJECT_PATH"

setupFile() {
	local filename="$1"
	echo "Setting up $filename"
	sed -i ".bak" -e "s/MYPROJECT/$PROJECT_NAME/g" "$filename"
}

# Step: setup project
git init
setupFile "$PROJECT_PATH/Makefile"
setupFile "$PROJECT_PATH/hie.yaml"
setupFile "$PROJECT_PATH/package.yaml"

# Cleanup
rm -f ./*.cabal* "setup.sh" ./*.bak*

echo "Done setup"


# Step: test
echo "make compile"
make compile

echo "make ide"
make ide

echo "make lint"
make lint

echo "make document"
make document

echo "make test"
make test
