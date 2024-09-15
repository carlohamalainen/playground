# Minimalist Elasticsearch-Kibana-Filebeat Ansible deployment

Battled Claude Opus 3.5, ChatGPT o1-preview, and Gemini (free). Claude won although it needed help; the initial prompt wasn't enough for a one-shot answer.

> I'm deploying Elasticsearch 8.11.3 in a legacy on-prem environment. I will use Ansible.
>
> Write an Ansible inventory.yml for a single host "homelab" with user "carlo".
>
> The remote user "carlo" does not have sudo privileges.
>
> The playbook must:
>
> 1. Download and unpack the Elasticsearch 8.11.3 Linux amd64 binary in /home/carlo/elk
> 2. Download and unpack the Kibana 7.17.9 linux amd64 binary in /home/carlo/elk
> 3. Wipe the keystore (in case we are installing over a previous deployment).
> 4. Set the bootstrap password using "elasticsearch-keystore" for user "elastic" to the environment variable ELASTIC_PASSWORD.
> 5. Using the keystore, create a new user with the value of the environment variable CARLO_ELASTIC_USER and password CARLO_ELASTIC_PASSWORD. The user must have roles "superuser" and "kibana_system"
> 6. Set systemd "linger" for user "carlo"
> 7. Create systemd user jobs for carlo that start elasticsearch and kibana automatically; they should restart after 10 seconds if killed unexpectedly; "carlo" does not have sudo so everything must be in user space.
> 8. Also create an uninstall.yml playbook that stops elasticsearch and kibana, removes the systemd jobs, and wipes /home/carlo/elk

I forgot to ask for Filebeat as well.

Key features of the resulting playbook:

* Installs binary tarballs - no containers or k8s.
* Configures Elasticsearch with a custom user that has privileges to create Kibana index patterns ``.kibana``.
* Configures certificates for "transport": uses ``elasticsearch-certutil`` for CA, http, transport certificates.
* Works for a single node deployment; multiple nodes should be fine too.
* Configures Kibana to use the custom Elastic user over TLS.
* Configures Filebeat similarly; also adds a 14 day index lifecycle policy.
* Configures the custom user using ``elasticsearch-users``; does not require Elasticsearch to be up to use the API endpoint.
* Configures systemd user services for all three components - imagine an on-prem deployment without sudo privileges.

Elasticsearch isn't happy with the default value of ``vm.max_map_count`` so you need to change this and reboot:

```shell
$ cat /etc/sysctl.conf
vm.max_map_count=262144
```

Tested on Debian Bookworm 🙂🐧

Run the playbook:

```bash
ansible-playbook -i inventory.yml install_elk.yml
```

Wipe everything:

```bash
ansible-playbook -i inventory.yml uninstall_elk.yml
```

Check if Elasticsearch is up:

```shell
$ curl -k -Li -u ${CARLO_ELASTIC_USER}:${CARLO_ELASTIC_PASSWORD} https://homelab:9200
HTTP/1.1 200 OK
X-elastic-product: Elasticsearch
content-type: application/json
content-length: 533

{
  "name" : "homelab",
  "cluster_name" : "homelab-cluster",
  "cluster_uuid" : "axoMWAZqQPWHKX9E_FYELg",
  "version" : {
    "number" : "8.11.3",
    "build_flavor" : "default",
    "build_type" : "tar",
    "build_hash" : "64cf052f3b56b1fd4449f5454cb88aca7e739d9a",
    "build_date" : "2023-12-08T11:33:53.634979452Z",
    "build_snapshot" : false,
    "lucene_version" : "9.8.0",
    "minimum_wire_compatibility_version" : "7.17.0",
    "minimum_index_compatibility_version" : "7.0.0"
  },
  "tagline" : "You Know, for Search"
}
```

Check if Kibana is up:

```shell
$ curl -I -u ${CARLO_ELASTIC_USER}:${CARLO_ELASTIC_PASSWORD} http://homelab:5601/api/status
HTTP/1.1 200 OK
x-content-type-options: nosniff
referrer-policy: no-referrer-when-downgrade
content-security-policy: script-src 'unsafe-eval' 'self'; worker-src blob: 'self'; style-src 'unsafe-inline' 'self'
kbn-name: homelab
kbn-license-sig: 44f9a693287a7d91d7ee68aa30cf1d8d552255e8e0eee8833a1b0150405fd658
content-type: application/json; charset=utf-8
cache-control: private, no-cache, no-store, must-revalidate
content-length: 25709
vary: accept-encoding
Date: Sun, 15 Sep 2024 06:15:48 GMT
Connection: keep-alive
Keep-Alive: timeout=120
```