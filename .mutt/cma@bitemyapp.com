set imap_user = "cma@bitemyapp.com"
set smtp_url = "smtp://cma@bitemyapp.com.smtp.gmail.com:587/"
source "gpg -d ~/.mutt/passwords/cma@bitemyapp.com.gpg |"
set from = "cma@bitemyapp.com"
set realname = "Chris Allen"

set editor = "emacs"

# Basic config, you can leave this as is
set folder = "imaps://imap.gmail.com:993"
set spoolfile = "+INBOX"
set imap_check_subscribed
set hostname = gmail.com
set mail_check = 120
set timeout = 300
set imap_keepalive = 300
set postponed = "+[GMail]/Drafts"
set record = "+[GMail]/Sent Mail"
set header_cache=~/.mutt/cache/headers
set message_cachedir=~/.mutt/cache/bodies
set certificate_file=~/.mutt/certificates
set move = no
set include
set sort = 'threads'
set sort_aux = 'reverse-last-date-received'
set auto_tag = yes
ignore "Authentication-Results:"
ignore "DomainKey-Signature:"
ignore "DKIM-Signature:"
hdr_order Date From To Cc
alternative_order text/plain text/html *
auto_view text/html
bind editor <Tab> complete-query
bind editor ^T complete
bind editor <space> noop

macro index,pager y "<enter-command>unset trash\n <delete-message>" "Gmail archive message"
macro index,pager d "<enter-command>set trash=\"imaps://imap.googlemail.com/[GMail]/Bin\"\n <delete-message>" "Gmail delete message"
macro index,pager gi "<change-folder>=INBOX<enter>" "Go to inbox"
macro index,pager ga "<change-folder>=[Gmail]/All Mail<enter>" "Go to all mail"
macro index,pager gs "<change-folder>=[Gmail]/Starred<enter>" "Go to starred messages"
macro index,pager gd "<change-folder>=[Gmail]/Drafts<enter>" "Go to drafts"
