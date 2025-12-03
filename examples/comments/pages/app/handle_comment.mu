#!/usr/bin/env python3
import os
import time
from datetime import datetime

# Get form data from environment variables
user_name = os.environ.get('field_user_name', 'Anonymous')
user_lxmf = os.environ.get('field_user_lxmf', '')
comment_text = os.environ.get('field_comment_text', '')
post_id = os.environ.get('var_post_id', 'default')

# Sanitize input (basic example)
user_name = user_name.replace('`', "'")
comment_text = comment_text.replace('`', "'")

# Generate timestamp
timestamp = datetime.now().strftime("%Y-%m-%d %H:%M")

# Append to comments file
with open(f"/home/sebas/.nomadnetwork/storage/pages/comments/{post_id}.txt", "a") as f:
    f.write(f"{user_name}|{user_lxmf}|{timestamp}|{comment_text}\n")

# Redirect back to blog page with success message
print("`c`!Thank you for your comment!`!`a")
print("-")
print("Your comment has been submitted successfully.")
print("`F00a`_`[Return to blog`:/page/index.mu]`_`f")