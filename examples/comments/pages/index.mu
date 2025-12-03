#!/usr/bin/env python3
import os
from datetime import datetime

header = """
-∿
<

`c`!Hello!`! This is output from `*micron`*
Micron generates formatted text for your terminal
`a


-∿
<

>>Comments
"""

print(header)

# It's better to get the post_id dynamically, perhaps from a URL parameter
# For now, we'll keep it static as an example
post_id = "index"
comments_file = f"/home/sebas/.nomadnetwork/storage/pages/comments/{post_id}.txt"

if os.path.exists(comments_file):
    with open(comments_file, "r") as f:
        comments = f.readlines()

    for comment in reversed(comments[-10:]):  # Show last 10 comments
        try:
            name, lxmf, timestamp, text = comment.strip().split("|", 3)
            # Print formatted output in Micron
            print(f"`Feee{name}`f ({timestamp}):")
            if lxmf != "":
                print(f"`F0FD`[lxmf@{lxmf}`lxmf@{lxmf}`]`f")
            print(f"{text}")
            print("-")
        except ValueError:
            # Skip malformed lines
            pass
else:
    print("No comments yet. Be the first!")

footer = """

>>Add Your Comment

`Faaa

`B444`<16|user_name`>`b Your Name  

`B444`<64|user_lxmf`>`b LXMF Address (optional)  

`B444`<64|comment_text`>`b Comment 

`F00f`_`[Submit`:/app/handle_comment.mu`user_name|user_lxmf|comment_text|post_id=index]`_`f 
`a`b
"""

print(footer)