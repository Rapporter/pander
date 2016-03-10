#!/bin/bash
GH_REPO="@github.com/Rapporter/pander.git"
FULL_REPO="https://$GH_TOKEN$GH_REPO"

mkdir out
cd out
git init
git remote add origin $FULL_REPO
git fetch
git config user.name "rapporter-travis"
git config user.email "travis"

# handle master
git checkout master
CHANGED_FILES=`git show --stat $TRAVIS_COMMIT`
echo -e "\nChanged files:\n\n$CHANGED_FILES\n\n"
# check if vignettes were updated
# if [[ $CHANGED_FILES =~ .*vignettes.*\.Rmd.* ]]
# then
  echo "Updating vignettes..."
  R -e 'devtools::install_github("rstudio/rmarkdown"); devtools::build_vignettes()'
  git add inst/doc
# fi
# check if readme was update
# if [[ $CHANGED_FILES =~ .*README\.brew.* ]]
# then
  echo "Updating README..."
  R -f ../.brewer.R
  git add README.md
# fi
git commit -m "Update by travis after $TRAVIS_COMMIT"
git push origin master
cd ../
git pull origin master
rm -rf out

mkdir out
cd out
git init
git remote add origin $FULL_REPO
git fetch
git config user.name "rapporter-travis"
git config user.email "travis"

# gh-pages handling
git checkout gh-pages
for files in '../inst/doc/*.html'; do
        cp $files .
done

# if [[ $CHANGED_FILES =~ .*\.brew.* ]]
# then
  R -f ../.brewer.R
# fi

git add .
git reset README.md
git commit -m "GH-Pages update by travis after $TRAVIS_COMMIT"
git push origin gh-pages
