mkdir doc
cd doc

git clone -b gh-pages https://github.com/r-raymond/purple-muon.git

cd purple-muon

git config --global push.default simple
git config user.name "Travis CI"
git config user.email "travis@travis-ci.org"

rm -rf *

DOC_DIR=$(stack path | grep local-doc-root | awk '{print $2}')

mv ${DOC_DIR}/* .

git add --all
git commit -m "Deploy haddock documentaion for commit ${TRAVIS_COMMIT}"
git push --force "https://${PURPLE_MUON_GITHUB}@github.com/r-raymond/purple-muon.git" > /dev/null 2>&1
