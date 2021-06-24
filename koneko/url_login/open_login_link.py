"""Functions to open pixiv login link.
This is the only module in the url_login package that should be imported; do not import from main.py or script.py
This module is also functionally independent from the other two files. It is only in the url_login package to group code that is adapted from: https://gist.github.com/ZipFile/c9ebedb224406f4f11845ab700124362
"""

from base64 import urlsafe_b64encode
from urllib.parse import urlencode
from hashlib import sha256
from secrets import token_urlsafe
from webbrowser import open as open_url


# Latest app version can be found using GET /v1/application-info/android
LOGIN_URL = "https://app-api.pixiv.net/web/v1/login"


def s256(data):
    """S256 transformation method."""
    return urlsafe_b64encode(sha256(data).digest()).rstrip(b"=").decode("ascii")

def oauth_pkce(transform):
    """Proof Key for Code Exchange by OAuth Public Clients (RFC7636)."""
    code_verifier = token_urlsafe(32)
    code_challenge = transform(code_verifier.encode("ascii"))
    return code_verifier, code_challenge

def open_pixiv_login():
    code_verifier, code_challenge = oauth_pkce(s256)
    login_params = {
        "code_challenge": code_challenge,
        "code_challenge_method": "S256",
        "client": "pixiv-android",
    }

    url = f"{LOGIN_URL}?{urlencode(login_params)}"
    print(url)
    open_url(url)
    return code_verifier
