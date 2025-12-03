# README

```
flowchart LR
    subgraph User's Browser
        A[Views blog.mu page]
    end

    subgraph Nomad Network Server
        B[/page/blog.mu/] --> C{Calls app via<br>`` `= ` tag};
        C --> D[/apps/show_comments.py/];
        D --> E[Reads/Writes<br>comments.txt];
        E -- Returns Micron --> D;
        D -- Rendered Output --> B;
    end

    A -- Requests Page --> B;
    B -- Final Rendered Page --> A;
```