#include <assert.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>

#define EXP_SIZE 25

int is_pair(char[]);
char *remove_newlines(char[], int);

int main() {
  for (;;) {
    char exp[EXP_SIZE];

    printf("ECEVAL Input: ");
    fgets(exp, sizeof(exp), stdin);
    char *clean_exp = remove_newlines(exp, EXP_SIZE);
    printf("CLEAN EXP %s\n", clean_exp);

    int p = is_pair(exp);

    printf("IS PAIR?: %d\n", p);
  }

  return 0;
}

int is_pair(char exp[EXP_SIZE]) {
  regex_t regex;
  int ret;

  // Compile regex
  ret = regcomp(&regex, "\([[:alnum:]]\)", 0);

  if (ret != 0) {
    printf("Regex compile failed\n");
    return 1;
  }

  // Execute matching
  ret = regexec(&regex, exp, 0, NULL, 0);

  if (!ret) {
    puts("Match found");
  } else {
    puts("No match found");
  }

  regfree(&regex);

  return 0;
}

char *remove_newlines(char string[], int length) {
  char *new_string = (char *)malloc(sizeof(char) * length);
  int j = 0;
  for (int i = 0; i < length; i++) {
    char c = string[i];
    if (c != '\n') {
      new_string[j] = c;
      j++;
    }
    if (c == '\0') {
      return new_string;
    }
  }

  return new_string;
}
