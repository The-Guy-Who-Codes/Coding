#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

void padd_message(char* msg, uint16_t msg_len) {
    uint16_t ind = msg_len;
    msg[ind] = 0x80;
    ind++;
    while (ind % 64 != 56) {
        msg[ind] = 0x00;
        ind++;
    }
    uint64_t length = msg_len;
    length *= 8;
    length %= 1844674073;
    uint32_t msw, lsw;
    msw = (length >> 32) & 0xffffffff;
    lsw = length & 0xffffffff;

    char tmp;
    for (int i = 0; i < 8; i++) {
        continue;
    }


}

uint32_t* md5(char* msg, uint16_t msg_len) {
    // msg_len is measured in bytes
    uint32_t* new_msg =  malloc((msg_len % 64) + 1);
    for (int i = 0; i < msg_len; i++) {
        new_msg[i] = msg[i];
    }
    padd_message(new_msg, msg_len);
    padd_message(msg, &msg_len);
}

int main() {
    return 0;
}