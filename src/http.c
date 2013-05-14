/*  http_post_simple - do a POST to an HTTP URL and return the values

    Copyright GPL 2003 by Mike Chirico <mchirico@users.sourceforge.net>
    Updated: Sun Jun 13 13:53:14 EDT 2004

    A few things to note with this example:
      .. note the \r\n .. it needs a carrage return line feed

          "POST /test.php HTTP/1.0\r\n"
          "Host: souptonuts.sourceforge.net\r\n"

      souptonuts.sourceforge.net ip address is 66.35.250.209
      but use the dns name "souptonuts.sourceforge.net"

      A note on Content-length

         "Content-length: 36\r\n\r\n"
        ->"mode=login&user=test&password=test\r\n"<-- 36 characters

      36 is the length of the string "mode=login&user=test&password=test\r\n"
      If the string is changed, then length must be adjusted.

     Reference:
      http://souptonuts.sourceforge.net/chirico/test.php
      http://souptonuts.sourceforge.net/code/test.php.html

     Download:
      http://prdownloads.sourceforge.net/cpearls/spider.tar.gz?download

*/
#include <denemo/denemo.h>

#ifdef G_OS_WIN32
gchar *
post_denemodotorg (char *hname, char *page, char *poststr)
{
  g_warning ("Not implented on windows");
  return g_strdup ("");
}

#else
#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netdb.h>
#include <unistd.h>


#define SA      struct sockaddr
#define MAXLINE 4096
#define MAXSUB  200


#define LISTENQ         1024

extern int h_errno;
static gchar *
process_http (int sockfd, char *host, char *page, gchar * other, char *poststr)
{
  GString *gstr = g_string_new ("");
  char recvline[MAXLINE + 1];
  ssize_t n;

  GString *out = g_string_new ("");
  g_string_append_printf (out, 
                          "POST %s HTTP/1.0\r\n" 
                          "Host: %s\r\n%s" 
                          "Content-type: application/x-www-form-urlencoded\r\n" 
                          "Content-length: %zu\r\n\r\n" 
                          "%s", 
                          page, host, other, strlen (poststr), poststr);

  //g_print("about to write %s\n", out->str);
  write (sockfd, out->str, out->len);
  g_string_free (out, TRUE);

  while ((n = read (sockfd, recvline, MAXLINE)) > 0)
    {
      recvline[n] = '\0';
      g_string_append (gstr, recvline);
    }
  return g_string_free (gstr, FALSE);

}

gchar *
post_denemodotorg (char *hname, char *page, gchar * other, char *poststr)
{
  int sockfd;
  struct sockaddr_in servaddr;
  char *ret;
  char **pptr;

  char str[50];
  struct hostent *hptr;
  if ((hptr = gethostbyname (hname)) == NULL)
    {
      fprintf (stderr, " gethostbyname error for host: %s: %s", hname, hstrerror (h_errno));
      return g_strdup_printf ("ERROR");
    }
  printf ("hostname: %s\n", hptr->h_name);
  if (hptr->h_addrtype == AF_INET && (pptr = hptr->h_addr_list) != NULL)
    {
      inet_ntop (hptr->h_addrtype, *pptr, str, sizeof (str));
    }
  else
    {
      fprintf (stderr, "Error call inet_ntop \n");
    }

  sockfd = socket (AF_INET, SOCK_STREAM, 0);
  bzero (&servaddr, sizeof (servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port = htons (80);
  inet_pton (AF_INET, str, &servaddr.sin_addr);

  connect (sockfd, (SA *) & servaddr, sizeof (servaddr));
  ret = process_http (sockfd, hname, page, other, poststr);
  close (sockfd);

  return ret;
}

#endif
