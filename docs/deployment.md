# Deployment guidelines

...

tbd.: parts of the state that need to be kept in mind during database destruction migration:

- static/avatars
- aula-events.json
- ...  anything else we missed?


## Enforcing file size limits

The deployment is responsible for enforcing this constraint. Since it is recommended to keep a
reverse proxy such as nginx in front of the haskell server (warp).

With nginx the configuration option is:

* `client_max_body_size 10m;`
