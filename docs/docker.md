# Share image between hosts:

``` sh
docker save alpine:latest | gzip | DOCKER_HOST=ssh://user@remotehost docker load
```

NOTE: the user of the ssh connection has to have permissions for the docker socket (`/var/run/docker.sock`)
