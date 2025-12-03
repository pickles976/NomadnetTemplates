# README

### TODO
- [x] get a basic page with comments working
- [ ] recreate it in chicken scheme
    - [ ] simple DSL
        - [x] links
        - [x] fields
        - [x] submit buttons
        - [x] improved text styling (check deepseek's suggestions)
        - [x] style should not take any arguments
        - [x] consolidate my DSL implementation with the one Deepseek provided
    - [x] user interaction
        - [x] load comments from file
        - [x] write comments to file
            - [x] test in-situ
        - [x] hooray! feature-parity achieved!
- [x] fix bug with lxmf entry

- [ ] switch from file-based comments, to sqlite-based comment system
    - [ ] create a simple sqlite database https://wiki.call-cc.org/eggref/5/sql-de-lite#preparing-a-sql-statement
    - [ ] implement comments

Design app layout:

```
pages
|____index.mu
|____app
    |_____models.scm // SQLite functions go here
    |
    |_____actions
    |     |________handle_comments.mu // Each file does CRUD + other stuff
    |
    |_____templates
          |________comments.mu // template functions
```

- [ ] compile the dsl
- [ ] update my personal node with this

- [ ] integrate recipe search
    - [ ] run hari.recipes locally
    - [ ] query the API and html
    - [ ] parse and list
    - [ ] click on list entry and load HTML
    - [ ] convert HTML to micron
- [ ] create epub search

https://wiki.call-cc.org/chicken-for-python-programmers

`sudo apt-get install chicken-bin`

```
csi -s ./framework/pages/index.mu 
```

`sudo chicken-install srfi-1`

# Compile the module
`csc -s -J micron-dsl.scm`

# Install it
chicken-install micron-dsl