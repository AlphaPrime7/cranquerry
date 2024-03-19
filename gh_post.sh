curl --user "alphaprime7:gh_token"\
  -X POST \
  -H "Content-Type: application/json" \
  -H "Accept: application/vnd.github+json" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  https://api.github.com/user/repos \
  -d '{"name":"Hello-World Again","description":"This is my first POST API!","homepage":"https://github.com","private":true,"is_template":true}'
