# Share image between hosts:

``` sh
docker save alpine:latest | gzip | DOCKER_HOST=ssh://user@remotehost docker load
```

NOTE: the user of the ssh connection has to have permissions for the docker socket (`/var/run/docker.sock`)

# Configure dns timeout
It is not possible to set dns timeout inside a Dockerfile. It must be passed on container or daemon runtime, e.g. `--dns-opt=timeout=3`.

