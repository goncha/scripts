import base64, os.path, re, socket


def debug(fmt, *argv):
    if True:
        print fmt % argv


class Server(object):
    """Simple HTTP Server"""

    _STATUS = {
        200: 'OK',
        401: 'Unauthenticated',
        404: 'Not Found',
        500: 'Internal Server Error',
        501: 'Not Implemented'
    }

    _MIME = {
        '.html': 'text/html',
        '.txt': 'text/plain',
        '.gif': 'image/gif',
        '.css': 'text/css',
        '.png': 'image/png',
        '.jpg': 'image/jpeg',
    }

    def __init__(self, wwwroot, host='localhost', port=3000, secpaths=None, passwd=None):
        self._sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self._addr = (host, port)
        self._wwwroot = os.path.abspath(wwwroot)
        self._secpaths = secpaths and [re.compile(x) for x in secpaths] or []
        self._passwd = passwd and passwd or []
        self._continue = True

    def run(self, backlog=5):
        if self._sock:
            self._sock.bind(self._addr)
            self._sock.listen(backlog)
            print 'Simple HTTP Server\'s listening on %s:%d' % self._addr
            while self._continue:
                self._handle(*self._sock.accept())

    def _handle(self, sock, addr):
        debug('Request from %s:%d', *addr)
        fd = sock.makefile('r')

        method, path, headers = self._parse_request(fd)
        fnname = method.lower()

        debug('%s - %s', method, path)

        if hasattr(self, fnname):
            fn = getattr(self, fnname)
            if callable(fn):
                status, headers, message = fn.__call__(path, headers)
            else:
                status, headers, message = self.fallback_method()
        else:
            status, headers, message = self.fallback_method()

        self._write_response(sock, status, headers, message)

        sock.close()

    def _status(self, code):
        """Return (code, message) pair."""
        if Server._STATUS.has_key(code):
            return code, Server._STATUS[code]
        else:
            return 200, Server._STATUS[200]


    def _write_response(self, sock, status, headers, message):
        """Use socket (not file) object to write binary data."""
        hdrs = {
            "Server": "WTF Server :)",
            "Pragma": "no-cache",
            "Cache-Control": "no-cache",
            "Connection": "close"
        }

        if message:
            hdrs["Content-Length"] = str(len(message))
        if headers:
            hdrs.update(headers)

        sock.sendall("HTTP/1.x %d %s\r\n" % self._status(status))
        sock.sendall("\r\n".join(["%s: %s" % (k, v) for k, v in hdrs.items()]))
        sock.sendall("\r\n\r\n")

        if message:
            sock.sendall(message)


    def _parse_request(self, fd):
        line = fd.readline().rstrip()
        method, path, _ = line.split()
        headers = {}
        while line:
            line = fd.readline().rstrip()
            if len(line) > 0:
                idx = line.index(': ')
                if idx > 0:
                    headers[line[0:idx]] = line[idx+2:]
            else:
                break
        return method, path, headers

    def _need_auth(self, path):
        for x in self._secpaths:
            if x.match(path):
                return True
        return False

    def _match_auth(self, headers):
        if headers.has_key("Authorization"):
            pair = headers["Authorization"].split()[1]
            pair = base64.b64decode(pair)
            if pair in self._passwd:
                return True
        return False

    def _mime(self, path):
        for ext, type in Server._MIME.items():
            if path.endswith(ext):
                return type
        return 'text/plain'

    def fallback_method(self):
        return 501, None, None

    def get(self, path, headers):
        param_sep = path.find("?")
        if param_sep > -1:
            path = path[0:param_sep]

        if self._need_auth(path) and (not self._match_auth(headers)):
            return 401, {'WWW-Authenticate': 'Basic realm="WTF"'}, None
        else:
            full_path = os.path.abspath(os.path.join(self._wwwroot, path[1:]))
            if full_path.startswith(self._wwwroot) and os.path.exists(full_path):
                # Read all files in binary mode
                msg = open(full_path, 'rb').read()
                return 200, {'Content-Type': self._mime(full_path)}, msg
            else:
                return 404, None, None


_PASSWD = [ "guest:guest" ]
_SECPATHS = [ "/java/sql/.*$" ]

def main(argv=[]):
    Server('D:\\share\\doc\\python\\', secpaths=_SECPATHS, passwd=_PASSWD).run()


if __name__ == '__main__':
    import sys
    main(sys.argv)
