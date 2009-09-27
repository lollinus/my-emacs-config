;;; pf-mode.el --- bare-bones pf (OpenBSD "Packet Filter") mode

;; Keywords: langauges

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Created: Jamie Zawinski <jwz@jwz.org>, 13-Oct-2004


(defvar pf-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\""    table)
    (modify-syntax-entry ?#  "<"     table)
    (modify-syntax-entry ?\n ">"     table)
    (modify-syntax-entry ?$  "/"     table)
    (modify-syntax-entry ?\\ "."     table)
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?-  "_"     table)
    (modify-syntax-entry ?.  "_"     table)
    table))

(defvar pf-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'pf-mode-map)
    (define-key map "\t" 'tab-to-tab-stop)
    map))

(defvar pf-font-lock-keywords
  (list
   ;; tables
   '("<[a-zA-Z][a-zA-Z_0-9]*>" 0 font-lock-string-face t)

   ;; macros
   '("\\b\\([a-zA-Z][a-zA-Z_0-9]*\\)\\s *=" 1 font-lock-function-name-face t)

   ;; reserved operator words
   (list
    (concat "\\(^\\|\\S_\\b\\)\\("
     (mapconcat 'identity
      '("all" "allow-opts" "altq" "anchor" "antispoof" "bandwidth" "binat"
        "binat-anchor" "bitmask" "block" "block-policy" "borrow" "cbq"
        "code" "const" "debug" "default" "drop" "dup-to" "ecn" "fastroute"
        "fingerprints" "flags" "for" "from" "group" "hfsc" "icmp-type"
        "icmp6-type" "in" "keep" "label" "limit" "linkshare" "load" "log"
        "log-all" "loginterface" "max" "max-mss" "max-src-nodes"
        "max-src-states" "min-ttl" "modulate" "nat" "nat-anchor"
        "no-df" "no-sync" "on" "optimization" "os" "out" "pass" "port"
        "priority" "priq" "proto" "qlimit" "queue" "quick" "random"
        "rdr" "rdr-anchor" "realtime" "red" "reply-to" "require-order"
        "return" "return-icmp" "return-icmp6" "return-rst" "rio"
        "round-robin" "route-to" "scrub" "set" "source-hash" "state"
        "state-policy" "static-port" "table" "tag" "tagged" "tbrsize"
        "timeout" "to" "ttl" "upperlimit" "user")
      "\\|")
     "\\)\\b\\S_")
    2 'font-lock-keyword-face t)

   ;; reserved argument words
   (list
    (concat "\\(^\\|\\S_\\b\\)\\("
     (mapconcat 'identity
      '("aggressive" "any" "conservative" "crop"
        "drop-ovl" "floating" "fragment" "group-bound" "high-latency"
        "icmp" "icmp6" "if-bound" "inet" "inet6" "loud" "misc"
        "no-route" "none" "normal" "random-id" "reassemble"
        "satellite" "tcp" "udp" "urgent"
        ":0" ":broadcast" ":network" ":peer"
        "[FSRPAUEW]+\\(/[FSRPAUEW]+\\)?")
      "\\|")
     "\\)\\b\\S_")
    2 'font-lock-string-face t)

   ;; the contents of /etc/services on OpenBSD 3.5
   (list
    (concat "\\(^\\|\\S_\\b\\)\\("
     (mapconcat 'identity
      '("afpovertcp" "afs3-bos" "afs3-callback" "afs3-errors"
        "afs3-fileserver" "afs3-kaserver" "afs3-prserver" "afs3-rmtsys"
        "afs3-update" "afs3-vlserver" "afs3-volser" "at-echo" "at-nbp"
        "at-rtmp" "at-zis" "auth" "bftp" "bgp" "biff" "bootpc" "bootps"
        "canna" "cddb" "chargen" "cmip-agent" "cmip-man" "conference"
        "courier" "csnet-ns" "cvspserver" "daap" "datametrics" "daytime"
        "discard" "domain" "echo" "efs" "eklogin" "ekshell" "ekshell2" "epmap"
        "exec" "finger" "ftp" "ftp-data" "gopher" "hostnames" "https" "hunt"
        "icb" "imap" "imap3" "imaps" "ingreslock" "ipp" "iprop" "ipx" "irc"
        "isakmp" "iso-tsap" "kauth" "kerberos" "kerberos-adm" "kerberos-iv"
        "kerberos_master" "kf" "kip" "klogin" "kpasswd" "kpop" "krb524"
        "krb_prop" "krbupdate" "kshell" "kx" "ldap" "ldaps" "link" "login"
        "microsoft-ds" "msp" "mtp" "mysql" "nameserver" "nbp" "netbios-dgm"
        "netbios-ns" "netbios-ssn" "netnews" "netstat" "netwall" "nextstep"
        "nfsd" "nnsp" "nntp" "ntalk" "ntp" "photuris" "pop2" "pop3" "pop3s"
        "postgresql" "pptp" "printer" "prospero" "prospero-np" "pwdgen" "qotd"
        "radacct" "radius" "remotefs" "rfe" "rje" "rkinit" "rlp" "route"
        "rsync" "rtelnet" "rtmp" "rtsp" "sa-msg-port" "sftp" "shell" "silc"
        "sip" "smtp" "smux" "snmp" "snmp-trap" "socks" "spamd" "spamd-cfg"
        "ssdp" "ssh" "submission" "sunrpc" "supdup" "supfiledbg" "supfilesrv"
        "svrloc" "syslog" "systat" "talk" "tcpmux" "telnet" "tempo" "tftp"
        "time" "timed" "ulistserv" "uucp" "uucp-path" "webster" "who" "whois"
        "www" "x400" "x400-snd" "xdmcp" "z3950" "zip")
      "\\|")
     "\\)\\b\\S_")
    2 'font-lock-string-face t)

   ;; Now highlight comments again, sigh...
   '("#.*$" 0 font-lock-comment-face t)

   ))

(defun pf-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map pf-mode-map)
  (setq mode-name "PF")
  (setq major-mode 'pf-mode)
  (set-syntax-table pf-mode-syntax-table)
  (run-hooks 'pf-mode-hook))

(provide 'pf-mode)
