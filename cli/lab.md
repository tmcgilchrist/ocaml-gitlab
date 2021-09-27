Lab CLI
==========

lab - makes git easier with GitLab

lab api - Low-level GitLab API request interface.
```sh
$ ./lab.exe api "https://gitlab.com/api/v4/users?username=tmcgilchrist"
[
  {
    "id": 490393,
    "name": "Tim McGilchrist",
    "username": "tmcgilchrist",
    "state": "active",
    "avatar_url":
      "https://secure.gravatar.com/avatar/67afd2b4c98c9befd18c19f0ee9d94dc?s=80&d=identicon",
    "web_url": "https://gitlab.com/tmcgilchrist"
  }
]
```

```sh
$ GITLAB_DEBUG=true ./lab.exe user-name --owner-name tmcgilchrist
>>> GitLab: Requesting https://gitlab.com/api/v4/users?username=tmcgilchrist
>>> GitLab: Response code 200 OK

tmcgilchrist:490393
```

List user projects

```sh
$ GITLAB_DEBUG=true ./lab.exe user-projects  --owner 490393
>>> GitLab: Requesting https://gitlab.com/api/v4/users/490393/projects
>>> GitLab: Response code 200 OK

ocaml-gitlab
freer
```
