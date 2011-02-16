//#include <stdio.h>
//#include <string.h>
//#include <sys/types.h>
//#include <fcntl.h>
#include <stdlib.h>
#include <curl/curl.h>

// g++ -lcurl main.c -o out

static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream)
{
  int written = fwrite(ptr, size, nmemb, (FILE *)stream);
  return written;
}

int main(int varnum, char ** varargs) {
  CURL *curl_handle;
  CURLcode curl_res;
  CURLINFO info;
  long http_code;
  double c_length;  

  static const char *headerfilename = "head_out";
  FILE *headerfile;
  static const char *bodyfilename = "body_out";
  FILE *bodyfile;

  if (varnum == 1) {
    printf("usage: out <URL>\n");
    exit(2);

  }

  printf("Init curl session\n");
  curl_global_init(CURL_GLOBAL_ALL);
  curl_handle = curl_easy_init();

  printf("Set url to download\n");
  // curl_easy_setopt(curl, CURLOPT_URL, "http://localhost");
  curl_easy_setopt(curl_handle, CURLOPT_URL, varargs[1]);

  /* send all data to this function  */ 
  printf("Set function handler to write\n");
  curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_data);

  /* open the files */ 
  headerfile = fopen(headerfilename,"w");
  if (headerfile == NULL) {
    printf("ERROR to open file \"head_out\"\n");
    curl_easy_cleanup(curl_handle);
    exit(2);

  }
  bodyfile = fopen(bodyfilename,"w");
  if (bodyfile == NULL) {
    printf("ERROR to open file \"body_out.txt\"\n");
    curl_easy_cleanup(curl_handle);
    exit(2);
  }


  /* we want the headers to this file handle */ 
  curl_easy_setopt(curl_handle, CURLOPT_WRITEHEADER, headerfile);

  /* we want the data to this file handle */ 
  curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, bodyfile);


  printf("Download the file\n");
  curl_res = curl_easy_perform(curl_handle);

  if( curl_res == 0) {
    printf("file downloaded\n");
  }
  else {
    printf("ERROR in dowloading file\n");
    fclose(bodyfile);
    fclose(headerfile);
    curl_easy_cleanup(curl_handle);
  }
  

  printf("Get http return code\n");
  curl_easy_getinfo(curl_handle, CURLINFO_RESPONSE_CODE, &http_code);
  printf("http code: %lu\n", http_code);

  printf("Get size of download page\n");
  curl_easy_getinfo(curl_handle, CURLINFO_SIZE_DOWNLOAD, &c_length);
  printf("length: %g\n", c_length);

  printf("END: close all files and sessions\n");
  fclose(bodyfile);
  fclose(headerfile);
  curl_easy_cleanup(curl_handle);

  return 0;
}
