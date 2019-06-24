#include "big_integer.h"

#include <cstring>
#include <stdexcept>
#include <cstddef>
#include <iostream>

const uint32_t BIT = 32;
const uint64_t BASE = (1ULL << 32);

big_integer::big_integer() : sign(1) {
    bits.push_back(0);
}

big_integer::big_integer(big_integer const& other) : sign(other.sign), bits(other.bits) {}

big_integer::big_integer(int a)
{
    a >= 0 ? sign = 1 : sign = -1;
    bits.push_back(std::abs(a));
}

big_integer::big_integer(std::string const& str) : sign(1)
{
    size_t first_char = 0;
    if(str[0] == '-') {
        first_char++;
    }
    bits.push_back(0);
    for(size_t i = first_char; i < str.size(); ++i) {
        this->mul_short(10);
        *this += big_integer( (int)(str[i] - '0') );
    }
    if(str[0] == '-') {
        sign = -1;
    }
}
big_integer::~big_integer()
{
    sign = 0;
    bits.clear();
}

big_integer& big_integer::operator=(big_integer const& other)
{
    if(this != &other) {
        bits.clear();
        sign = other.sign;
        bits = other.bits;
    }
    return *this;
}

big_integer& big_integer::operator+=(big_integer const& rhs)
{
    uint64_t carry = 0;
    if(sign == rhs.sign) {
        for (size_t i = bits.size(); i < rhs.bits.size(); ++i) {
            bits.push_back(0);
        }
        for (size_t i = 0; i < bits.size(); ++i) {
            uint64_t sum = 0LL + bits[i] + ((i < rhs.bits.size()) ? rhs.bits[i] : 0) + carry;
            bits[i] = (size_t) (sum);
            carry = size_t (sum >> BIT);
        }
        if(carry != 0) {
            bits.push_back(carry);
        }
    } else if(sign == 1) {
        int tmp_sign = (this->bigger_or_equal(rhs)) ? 1 : -1;
        big_integer tmp = rhs;
        tmp.sign = 1;
        *this -= tmp;
        sign = tmp_sign;
    } else {
        int tmp_sign = rhs.bigger_or_equal(*this) ? 1 : -1;
        sign = 1;
        big_integer tmp = *this;
        *this = rhs;
        *this -= tmp;
        sign = tmp_sign;
    }
    delete_zeroes();
    if (is_zero()) {
        sign = 1;
    }
    return *this;
}

void big_integer::delete_zeroes() {
    while (bits.size() > 1 && bits.back() == 0) {
        bits.pop_back();
    }
}

bool big_integer::is_zero() const {
    if (bits.size() == 1 && bits.back() == 0) {
        return true;
    }
    return false;
}

big_integer& big_integer::operator-=(big_integer const& rhs)
{
    int64_t carry = 0;
    if(sign != rhs.sign) {
        sign = rhs.sign;
        *this += rhs;
        sign = sign == 1 ? -1 : 1;
    } else {
        if(this->bigger_or_equal(rhs)) {
            for(size_t i = 0; i < std::min(bits.size(), rhs.bits.size()); ++i) {
                int64_t dif = 1LL * bits[i] - rhs.bits[i] - carry;
                if(dif >= 0) {
                    bits[i] = dif;
                    carry = 0;
                } else {
                    bits[i] = ((1LL << BIT) + dif);
                    carry = 1;
                }
            }
            for(size_t i = rhs.bits.size(); i < bits.size(); ++i) {
                int64_t dif = 1LL * bits[i] - carry;
                if(dif >= 0) {
                    bits[i] = dif;
                    carry = 0;
                } else {
                    bits[i] = ((1LL << BIT) + dif);
                    carry = 1;
                }
            }
        } else {
            big_integer tmp = *this;
            *this = rhs;
            *this -= tmp;
            sign = (sign == 1) ? -1 : 1;
        }
    }

    delete_zeroes();
    //std::cout << "RES : " << bits.size() << " " << bits[0] << "\n";
    if (is_zero()) {
        sign = 1;
    }
    return *this;
}

void big_integer::mul_short(uint32_t rhs) {
    if( (rhs > 0 && sign == 1) || (rhs == 0) || (rhs < 0 && sign == -1)) {
        sign = 1;
    } else if((rhs > 0 && sign == -1) || (rhs < 0 && sign == 1)) {
        sign = -1;
    }
    rhs = std::abs(rhs);

    //log(*this, "{mul_short}");
    uint64_t carry = 0;
    for (size_t i = 0; i < bits.size(); ++i) {
        uint64_t mul = 1ULL * bits[i] * rhs + carry;
        bits[i] = mul % (1ULL << 32);
        //printf("{mul_short}%08x\n", rhs);
        carry = mul / (1ULL << 32);
    }
    if(carry) {
        bits.push_back(carry);
    }
}

big_integer& big_integer::operator*=(big_integer const& rhs)
{
    big_integer res;
    res.bits.resize(bits.size() + rhs.bits.size());
    for (size_t i = 0; i < bits.size(); ++i) {
        uint64_t carry = 0;
        for(size_t j = 0; j < rhs.bits.size() || carry; ++j) {
            uint64_t mul = 0;
            if (j < rhs.bits.size()) {
                mul = res.bits[i + j] + 1ULL * bits[i] * rhs.bits[j] + carry;
            } else {
                mul = res.bits[i + j] + carry;
            }
            res.bits[i+j] = uint(mul % BASE);
            carry = uint(mul / BASE);
        }
    }
    res.sign = (sign == rhs.sign || (rhs.bits.size() == 1 && rhs.bits[0] == 0) || (bits.size() == 1 && bits[0] == 0)) ? 1 : -1;

    *this = res;
    delete_zeroes();
    if (is_zero()) {
        sign = 1;
    }

    return *this;
}

size_t big_integer::mod_short(size_t rhs)
{
    uint64_t carry = 0,
             ans = 0;
    for (int pos = bits.size() - 1; pos >= 0; --pos) {
        ans = bits[pos] + (carry << BIT);
        bits[pos] = ans / rhs;
        carry = ans % rhs;
    }

    delete_zeroes();

    return int32_t(carry);
}

void big_integer::log(big_integer const& a, char const* prev) const{
    printf("%s", prev);
    printf("%c", a.sign == 1 ? '+' : '-');
     for (int i = a.bits.size() - 1; i >= 0; --i) {
         printf("%08X ", a.bits[i]);
     }
     printf("\n");
}

big_integer& big_integer::operator/=(big_integer const& rhs)
{/*
    log(*this, "A_base =  ");
    log(rhs, "B_base =  ");
    size_t ind=bits.size();
    printf("\n");*/

    if (rhs.bits.size() == 1) {
        mod_short(rhs.bits[0]);
        sign = (sign == rhs.sign) ? 1 : -1;
        return *this;
    }

    uint64_t half_base = (1ULL << (BIT - 1)),
             first = rhs.bits.back(),
             shift = 0;
    //std::cout << "FIRST : " << first << "\n";
    while (first < half_base) {
      //  std::cout << "FIRST : " << first << "\n";

        first <<= 1;
        ++shift;
    }
    //std::cout << "FIRST : " << first << "\n";
    big_integer rhs_n = (rhs << shift);
    *this <<= shift;
    rhs_n.sign = 1;
    size_t n = rhs_n.bits.size();
    size_t m = this->bits.size() - n;
    if (this->bits.size() < n) {
        *this = big_integer(0);
        return *this;
    }

//    log(*this, "A_shift = ");
//    log(rhs_n, "B_shift = ");

    big_integer q;
    q.bits.resize(m + 1);
    int t_sign = this->sign;
    this->sign = 1;
    //rhs_n <<= m * 32;
    //cout << rhs_n.sign << " " << rhs_n.bits.size() << " " << rhs_n.bits[0] << "\n";
    if (*this >= (rhs_n << m * BIT)) {
        q.bits[m] = 1;
        *this -= (rhs_n << m * BIT);
    } else {
        if (m == 0) {
            *this = big_integer(0);
            return *this;
        }
        q.bits.pop_back();
    }

    uint64_t qn;
    uint32_t bn = rhs_n.bits.back();

//    log(*this, "A_0 =     ");
//    log(rhs_n, "B_0 =     ");
//    printf("bn  =      %08X\n", bn);


//    log(rhs_n, "\n[begin]   ");
//    log(*this, "\nA_0 =     ");

    for (int pos = m; pos > 0; --pos) {

        qn = (bits[n + pos - 1] * BASE + bits[n + pos - 2]) / bn;

        //printf("QN %08X\n", qn);
        q.bits[pos - 1] = (size_t)qn;

        big_integer tmp = (rhs_n << (pos - 1) * BIT);
//        log(tmp, "{tmp}");
//        log(rhs_n, "{tmp}");
//        printf("{tmp}%08x\n", (pos - 1) * 32);
        tmp.mul_short(q.bits[pos - 1]);

//        printf("[%08X]", q.bits[pos - 1]);
//        for (int i = tmp.bits.size(); i < hui; i++)
//            printf("         ");
//        log(tmp, "");

        *this -= tmp;
//        printf("A_1=      ");
//        for (int i = bits.size(); i < hui; i++)
//            printf("         ");
//        log(*this, "");

        //printf("-----> %08X\n", q.bits[pos - 1]);
        //std::cout << this->sign << " " << *this << "\n";
        while (this->sign == -1 && !is_zero()) {

            --q.bits[pos - 1];

            *this += (rhs_n << (pos - 1) * BIT);
        }
    }


    *this = q;
    sign = (t_sign == rhs.sign) ? 1 : -1;

//    printf("\n");
//    //log(*this);
//    //log(rhs);
//    printf("\n");

    return *this;
}

big_integer& big_integer::operator%=(big_integer const& rhs)
{
    big_integer tmp = *this;
    tmp /= rhs;
    tmp *= rhs;
    *this -= tmp;
    return *this;
}

void big_integer::to_2_addition() {
    if (sign == -1) {
        bits.push_back(0);
        for (size_t i = 0; i < bits.size(); ++i) {
            bits[i] = ~bits[i];
        }
        --*this;
    }
}

void big_integer::from_2_addition() {
    if (sign == -1) {
        ++*this;
        for (size_t i = 0; i < bits.size(); ++i) {
            bits[i] = ~bits[i];
        }
        if (bits.size() > 1 && bits.back() == 0) {
            bits.pop_back();
        }
    }
}

big_integer& big_integer::operator&=(big_integer const& rhs)
{
    big_integer tmp(rhs);
    this->to_2_addition();
    tmp.to_2_addition();

    if (bits.size() < tmp.bits.size()) {
        for (size_t i = bits.size(); i < tmp.bits.size(); ++i) {
            bits.push_back(sign == 1 ? 0 : (size_t)-1);
        }
    } else if (tmp.bits.size() < bits.size()) {
        for (size_t i = tmp.bits.size(); i < bits.size(); ++i) {
            tmp.bits.push_back(tmp.sign == 1 ? 0 : (size_t)-1);
        }
    }

    for(size_t i = 0; i < bits.size(); ++i) {
        bits[i] &= tmp.bits[i];
    }

    //cout << "--> " << bits[0] << "\n";
    sign = (sign == -1 && tmp.sign == -1 ) ? -1 : 1;
    //cout << bits.size() << "\n";
    this->from_2_addition();
    //cout << bits.size() << "\n";

    delete_zeroes();
    return *this;
}

big_integer& big_integer::operator|=(big_integer const& rhs)
{
    big_integer tmp(rhs);
    this->to_2_addition();
    tmp.to_2_addition();

    if (bits.size() > tmp.bits.size()) {
        for (size_t i = bits.size(); i < tmp.bits.size(); ++i) {
            bits.push_back(sign == 1 ? 0 : -1);
        }
    } else if (tmp.bits.size() > bits.size()) {
        for (size_t i = tmp.bits.size(); i < bits.size(); ++i) {
            tmp.bits.push_back(tmp.sign == 1 ? 0 : -1);
        }
    }

    for(size_t i = 0; i < bits.size(); ++i) {
        bits[i] |= tmp.bits[i];
    }

    if (sign == -1 || tmp.sign == -1) {
        sign = -1;
    }
    this->from_2_addition();

    delete_zeroes();

    return *this;
}

big_integer& big_integer::operator^=(big_integer const& rhs)
{
    big_integer tmp(rhs);
    this->to_2_addition();
    tmp.to_2_addition();

    if (bits.size() < tmp.bits.size()) {
        for (size_t i = bits.size(); i < tmp.bits.size(); ++i) {
            bits.push_back(sign == 1 ? 0 : -1);
        }
    } else if (tmp.bits.size() < bits.size()) {
        for (size_t i = tmp.bits.size(); i < bits.size(); ++i) {
            tmp.bits.push_back(tmp.sign == 1 ? 0 : -1);
        }
    }

    for(size_t i = 0; i < bits.size(); ++i) {
        bits[i] ^= tmp.bits[i];
    }

    sign = (sign == rhs.sign) ? 1 : -1;
    this->from_2_addition();

    delete_zeroes();

    return  *this;
}

big_integer& big_integer::operator<<=(int rhs)
{
    if(is_zero() || !rhs) {
        return *this;
    }
    if(rhs < 0) {
        *this >>= (-rhs);
        return *this;
    }

    size_t blocks = rhs / BIT,
           mod    = rhs % BIT;

    if (!mod) {
        for (size_t i = 0; i < blocks; ++i) {
            bits.push_back(0);
        }

        for (int i = bits.size() - blocks - 1; i >= 0; --i) {
            bits[i + blocks] = bits[i];
            bits[i] = 0;
        }
    } else {
        std::vector<uint> tmp;
        for (size_t i = 0; i <= blocks; ++i) {
            tmp.push_back(0);
        }
        for(size_t i = blocks; i < bits.size() + blocks; ++i) {
            tmp[i] |= (bits[i - blocks] << mod);
            tmp.push_back( (bits[i - blocks] >> (BIT - mod)) );
        }
        bits = tmp;
        delete_zeroes();
    }
    return *this;
}

big_integer& big_integer::operator>>=(int rhs)
{
    if(rhs == 0 || is_zero()) {
        return *this;
    }
    if(rhs < 0) {
        *this <<= (-rhs);
    }

    size_t blocks = rhs / BIT,
           mod    = rhs % BIT;
    uint32_t with_rem = 0;

    if (blocks >= bits.size()) {
        if (sign == 1 || (bits.size() == 1 && bits[0] == 0)) {
            *this = big_integer(0);
        } else {
            *this = big_integer(-1);
        }
        return *this;
    }

    if (!mod) {
        for (size_t i = 0; i < blocks; ++i) {
            with_rem |= bits[i];
        }
        for (size_t i = bits.size() - 1; i >=  blocks; --i) {
            bits[i - blocks] = bits[i];
            bits.pop_back();
        }
        if (sign == -1 && with_rem) {
            --*this;
        }
    } else {
        uint32_t move_L_bits = 0,
                 move_R_bits = 0;
        for (size_t i = 0; i < blocks; ++i) {
            with_rem |= bits[i];
        }

        uint32_t del_bits = (bits[blocks] << (BIT - mod)) >> (BIT - mod);
        with_rem |= del_bits;

        move_R_bits = (bits[blocks] >> mod);
        for (size_t i = blocks + 1; i < bits.size(); ++i) {
            move_L_bits = (bits[i] << (BIT - mod));
            bits[i - blocks - 1] = (move_L_bits | move_R_bits);
            move_R_bits = (bits[i] >> mod);
        }
        bits[bits.size() - blocks - 1] = move_R_bits;

        for (size_t i = bits.size() - 1; i >= bits.size() - blocks; --i) {
            bits.pop_back();
        }

        if (sign == -1 && with_rem) {
            --*this;
        }
    }

    while (bits.size() > 1 && bits.back() == 0) {
        bits.pop_back();
    }
    if (bits.size() == 1 && bits[0] == 0) {
        sign = 1;
    }
    return *this;
}

big_integer big_integer::operator+() const
{
    return *this;
}

big_integer big_integer::operator-() const
{
    big_integer r = *this;
    r.sign = r.sign == 1 ? -1 : 1;
    if (r.bits.size() == 1 && r.bits.back() == 0) {
        r.sign = 1;
    }
    return r;
}

big_integer big_integer::operator~() const
{
    big_integer r = *this;
    r += 1;
    r.sign = r.sign == 1 ? -1 : 1;
    return r;
}

big_integer& big_integer::operator++()
{
    *this += 1;
    return *this;
}

big_integer big_integer::operator++(int)
{
    big_integer r = *this;
    ++*this;
    return r;
}

big_integer& big_integer::operator--()
{
    *this -= 1;
    return *this;
}

big_integer big_integer::operator--(int)
{
    big_integer r = *this;
    --*this;
    return r;
}

big_integer operator+(big_integer a, big_integer const& b)
{
    return a += b;
}

big_integer operator-(big_integer a, big_integer const& b)
{
    return a -= b;
}

big_integer operator*(big_integer a, big_integer const& b)
{
    return a *= b;
}

big_integer operator/(big_integer a, big_integer const& b)
{
    return a /= b;
}

big_integer operator%(big_integer a, big_integer const& b)
{
    return a %= b;
}

big_integer operator&(big_integer a, big_integer const& b)
{
    return a &= b;
}

big_integer operator|(big_integer a, big_integer const& b)
{
    return a |= b;
}

big_integer operator^(big_integer a, big_integer const& b)
{
    return a ^= b;
}

big_integer operator<<(big_integer a, int b)
{
    return a <<= b;
}

big_integer operator>>(big_integer a, int b)
{
    return a >>= b;
}

bool big_integer::bigger_or_equal(big_integer const& rhs) const {
    if(bits.size() > rhs.bits.size()) {
        return true;
    }
    if(bits.size() < rhs.bits.size()) {
        return false;
    }
    for(int i = bits.size() - 1; i >= 0; --i) {
        if(bits[i] > rhs.bits[i]) {
            return true;
        }
        if(bits[i] < rhs.bits[i]) {
            return false;
        }
    }
    return true;
}

bool operator==(big_integer const& a, big_integer const& b)
{
    return a.bigger_or_equal(b) && b.bigger_or_equal(a) && a.sign == b.sign;
}

bool operator!=(big_integer const& a, big_integer const& b)
{
    return !(a == b);
}

bool operator<(big_integer const& a, big_integer const& b)
{
    return (a.sign == -1 && b.sign == 1) || (a.sign == b.sign && !a.bigger_or_equal(b));
}

bool operator>(big_integer const& a, big_integer const& b)
{
    return (a.sign == 1 && b.sign == -1) || (a.sign == b.sign && !b.bigger_or_equal(a));
}

bool operator<=(big_integer const& a, big_integer const& b)
{
    return (a.sign == -1 && b.sign == 1) || (a.sign == b.sign && b.bigger_or_equal(a));
}

bool operator>=(big_integer const& a, big_integer const& b)
{
    return (a.sign == 1 && b.sign == -1) || (a.sign == b.sign && a.bigger_or_equal(b));
}

std::string to_string(big_integer const& a)
{
    if (a.bits.size() == 1 && a.bits.back() == 0) {
        return "0";
    }
    big_integer tmp = a;
    int res_sign = tmp.sign;
    tmp.sign = 1;
    std::string res = "";
    big_integer BInull(0);
    while (tmp > BInull) {
        size_t mod = tmp.mod_short(10);
        res += (mod + '0');
    }
    if(res_sign == -1) {
        res += '-';
    }
    std::reverse(res.begin(), res.end());
    return res;
}

std::ostream& operator<<(std::ostream& s, big_integer const& a)
{
    return s << to_string(a);
}
