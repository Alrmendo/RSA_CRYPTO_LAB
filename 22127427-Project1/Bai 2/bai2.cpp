#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <random>
#include <sstream>
#include <iomanip>
#include <cmath>

using namespace std;

const int BASE = 1000000000; // 10^9
const int BASE_digits = 9;

struct BigInt
{
    vector<int> digits;
    int num_sign;

    BigInt() : num_sign(1) {}

    BigInt(long long value)
    {
        *this = value;
    }

    BigInt(const string &str)
    {
        read(str);
    }

    void trim()
    {
        while (!digits.empty() && digits.back() == 0)
        {
            digits.pop_back();
        }

        if (digits.empty())
        {
            num_sign = 1;
        }
    }

    void read(const string &input)
    {
        num_sign = 1;
        digits.clear();

        int current_position = 0;

        while (current_position < (int)input.size() && (input[current_position] == '-' || input[current_position] == '+'))
        {
            if (input[current_position] == '-')
            {
                num_sign = -num_sign;
            }
            ++current_position;
        }

        for (int i = input.size() - 1; i >= current_position; i -= BASE_digits)
        {
            int block_value = 0;
            for (int j = max(current_position, i - BASE_digits + 1); j <= i; ++j)
            {
                block_value = block_value * 10 + (input[j] - '0');
            }
            digits.push_back(block_value);
        }

        trim();
    }

    void operator+=(const BigInt &value)
    {
        *this = *this + value;
    }

    void operator-=(const BigInt &value)
    {
        *this = *this - value;
    }

    void operator*=(const BigInt &value)
    {
        *this = *this * value;
    }

    void operator/=(const BigInt &value)
    {
        *this = *this / value;
    }

    void operator=(const BigInt &value)
    {
        num_sign = value.num_sign;
        digits = value.digits;
    }

    void operator=(long long value)
    {
        num_sign = 1;
        if (value < 0)
        {
            num_sign = -1;
            value = -value;
        }
        for (; value > 0; value = value / BASE)
        {
            digits.push_back(value % BASE);
        }
    }

    bool operator<(const BigInt &value) const
    {
        if (num_sign != value.num_sign)
            return num_sign < value.num_sign;

        if (digits.size() != value.digits.size())
            return digits.size() * num_sign < value.digits.size() * value.num_sign;

        for (int i = digits.size() - 1; i >= 0; --i)
        {
            if (digits[i] != value.digits[i])
                return digits[i] * num_sign < value.digits[i] * num_sign;
        }

        return false;
    }

    bool operator>(const BigInt &value) const
    {
        return value < *this;
    }

    bool operator<=(const BigInt &value) const
    {
        return !(value < *this);
    }

    bool operator>=(const BigInt &value) const
    {
        return !(*this < value);
    }

    bool operator==(const BigInt &value) const
    {
        return !(*this < value) && !(value < *this);
    }

    bool operator!=(const BigInt &value) const
    {
        return *this < value || value < *this;
    }

    BigInt operator+(const BigInt &value) const
    {
        if (num_sign == value.num_sign)
        {
            BigInt result = value;

            int carry = 0;
            int max_size = max(digits.size(), value.digits.size());

            for (int i = 0; i < max_size || carry; ++i)
            {
                if (i == (int)result.digits.size())
                    result.digits.push_back(0);

                result.digits[i] += carry + (i < (int)digits.size() ? digits[i] : 0);

                carry = result.digits[i] >= BASE;
                if (carry)
                    result.digits[i] -= BASE;
            }

            return result;
        }

        return *this - (-value);
    }

    BigInt operator-(const BigInt &value) const
    {
        if (num_sign == value.num_sign)
        {
            if (abs() >= value.abs())
            {
                BigInt result = *this;

                int carry = 0;
                for (int i = 0; i < (int)value.digits.size() || carry; ++i)
                {
                    result.digits[i] -= carry + (i < (int)value.digits.size() ? value.digits[i] : 0);
                    carry = result.digits[i] < 0;
                    if (carry)
                        result.digits[i] += BASE;
                }

                result.trim();
                return result;
            }

            return -(value - *this);
        }

        return *this + (-value);
    }

    BigInt operator*(int value) const
    {
        BigInt result = *this;
        result *= value;
        return result;
    }

    BigInt operator-() const
    {
        BigInt result = *this;
        result.num_sign = -num_sign;
        return result;
    }

    BigInt abs() const
    {
        BigInt result = *this;
        result.num_sign = 1;
        return result;
    }

    BigInt operator/(int value) const
    {
        BigInt result = *this;
        result /= value;
        return result;
    }

    BigInt operator/(const BigInt &divisor) const
    {
        return divmod(*this, divisor).first;
    }

    BigInt operator%(const BigInt &divisor) const
    {
        return divmod(*this, divisor).second;
    }

    void operator*=(int value)
    {
        if (value < 0)
        {
            num_sign = -num_sign;
            value = -value;
        }

        int carry = 0;
        for (int i = 0; i < (int)digits.size() || carry; ++i)
        {
            if (i == (int)digits.size())
            {
                digits.push_back(0);
            }

            long long current_value = (long long)digits[i] * value + carry;

            carry = (int)(current_value / BASE);
            digits[i] = (int)(current_value % BASE);
        }

        trim();
    }

    void operator/=(int value)
    {
        if (value < 0)
        {
            num_sign = -num_sign;
            value = -value;
        }

        long long remainder = 0;
        for (int i = digits.size() - 1; i >= 0; --i)
        {
            long long current_value = digits[i] + remainder * (long long)BASE;
            digits[i] = (int)(current_value / value);
            remainder = (int)(current_value % value);
        }

        trim();
    }

    int operator%(int value) const
    {
        if (value < 0)
            value = -value;

        int remainder = 0;
        for (int i = digits.size() - 1; i >= 0; --i)
        {
            remainder = (digits[i] + remainder * (long long)BASE) % value;
        }

        return remainder * num_sign;
    }

    bool isZero() const
    {
        return digits.empty() || (digits.size() == 1 && digits[0] == 0);
    }

    friend pair<BigInt, BigInt> divmod(const BigInt &dividend, const BigInt &divisor)
    {
        int normalization_factor = BASE / (divisor.digits.back() + 1);
        BigInt normalized_dividend = dividend.abs() * normalization_factor;
        BigInt normalized_divisor = divisor.abs() * normalization_factor;
        BigInt quotient, remainder;
        quotient.digits.resize(normalized_dividend.digits.size());

        for (int i = normalized_dividend.digits.size() - 1; i >= 0; --i)
        {
            remainder *= BASE;
            remainder += normalized_dividend.digits[i];

            int high_digit = remainder.digits.size() <= normalized_divisor.digits.size() ? 0 : remainder.digits[normalized_divisor.digits.size()];
            int next_high_digit = remainder.digits.size() <= normalized_divisor.digits.size() - 1 ? 0 : remainder.digits[normalized_divisor.digits.size() - 1];
            int estimated_quotient = ((long long)BASE * high_digit + next_high_digit) / normalized_divisor.digits.back();

            remainder -= normalized_divisor * estimated_quotient;
            while (remainder < 0)
            {
                remainder += normalized_divisor;
                --estimated_quotient;
            }

            quotient.digits[i] = estimated_quotient;
        }

        quotient.num_sign = dividend.num_sign * divisor.num_sign;
        remainder.num_sign = dividend.num_sign;
        quotient.trim();
        remainder.trim();

        remainder /= normalization_factor;

        return make_pair(quotient, remainder);
    }

    long long toLongLong() const
    {
        long long result = 0;
        for (int i = digits.size() - 1; i >= 0; --i)
        {
            result = result * BASE + digits[i];
        }
        return result * num_sign;
    }

    friend BigInt gcd(const BigInt &x, const BigInt &y)
    {
        return y.isZero() ? x : gcd(y, x % y);
    }

    friend BigInt lcm(const BigInt &x, const BigInt &y)
    {
        return x / gcd(x, y) * y;
    }

    friend istream &operator>>(istream &stream, BigInt &value)
    {
        string input;
        stream >> input;
        value.read(input);
        return stream;
    }

    friend ostream &operator<<(ostream &stream, const BigInt &value)
    {
        if (value.num_sign == -1)
        {
            stream << '-';
        }

        if (value.digits.empty())
        {
            stream << 0;
        }
        else
        {
            stream << value.digits.back();
        }

        for (int i = (int)value.digits.size() - 2; i >= 0; --i)
        {
            stream << setw(BASE_digits) << setfill('0') << value.digits[i];
        }

        return stream;
    }

    // Hàm nhân hai vector số lớn bằng thuật toán Karatsuba
    static vector<long long> karatsubaMultiply(const vector<long long> &a, const vector<long long> &b)
    {
        int n = a.size();
        vector<long long> result(n + n, 0);

        if (n <= 32)
        {
            for (int i = 0; i < n; ++i)
            {
                for (int j = 0; j < n; ++j)
                {
                    result[i + j] += a[i] * b[j];
                }
            }
            return result;
        }

        int half = n / 2;
        vector<long long> a1(a.begin(), a.begin() + half);
        vector<long long> a2(a.begin() + half, a.end());
        vector<long long> b1(b.begin(), b.begin() + half);
        vector<long long> b2(b.begin() + half, b.end());

        vector<long long> a1b1 = karatsubaMultiply(a1, b1);
        vector<long long> a2b2 = karatsubaMultiply(a2, b2);

        for (int i = 0; i < half; ++i)
        {
            a2[i] += a1[i];
            b2[i] += b1[i];
        }
        vector<long long> r = karatsubaMultiply(a2, b2);

        for (int i = 0; i < (int)a1b1.size(); ++i)
        {
            r[i] -= a1b1[i];
        }
        for (int i = 0; i < (int)a2b2.size(); ++i)
        {
            r[i] -= a2b2[i];
        }

        // Kết hợp kết quả
        for (int i = 0; i < (int)r.size(); ++i)
        {
            result[i + half] += r[i];
        }
        for (int i = 0; i < (int)a1b1.size(); ++i)
        {
            result[i] += a1b1[i];
        }
        for (int i = 0; i < (int)a2b2.size(); ++i)
        {
            result[i + n] += a2b2[i];
        }

        return result;
    }

    static vector<int> convert_base(const vector<int> &input, int old_digits, int new_digits)
    {
        vector<long long> powers_of_ten(max(old_digits, new_digits) + 1);
        for (int i = 1; i <= powers_of_ten.size(); ++i)
        {
            powers_of_ten[i] = powers_of_ten[i - 1] * 10;
        }

        vector<int> result;
        long long current_value = 0;
        int current_digits = 0;

        for (int i = 0; i < (int)input.size(); ++i)
        {
            current_value += input[i] * powers_of_ten[current_digits];
            current_digits += old_digits;

            while (current_digits >= new_digits)
            {
                result.push_back((int)(current_value % powers_of_ten[new_digits]));
                current_value /= powers_of_ten[new_digits];
                current_digits -= new_digits;
            }
        }

        if (current_value > 0)
        {
            result.push_back((int)current_value);
        }

        while (!result.empty() && result.back() == 0)
        {
            result.pop_back();
        }

        return result;
    }

    BigInt operator*(const BigInt &other) const
    {
        vector<int> a6 = convert_base(this->digits, BASE_digits, 6);
        vector<int> b6 = convert_base(other.digits, BASE_digits, 6);
        vector<long long> a(a6.begin(), a6.end());
        vector<long long> b(b6.begin(), b6.end());

        while (a.size() < b.size())
        {
            a.push_back(0);
        }
        while (b.size() < a.size())
        {
            b.push_back(0);
        }
        while (a.size() & (a.size() - 1))
        {
            a.push_back(0);
            b.push_back(0);
        }

        vector<long long> c = karatsubaMultiply(a, b);

        BigInt result;
        result.num_sign = this->num_sign * other.num_sign;
        long long carry = 0;

        for (int i = 0; i < (int)c.size(); ++i)
        {
            long long current = c[i] + carry;
            result.digits.push_back((int)(current % 1000000));
            carry = current / 1000000;
        }

        result.digits = convert_base(result.digits, 6, BASE_digits);
        result.trim();
        return result;
    }
};

struct GCDResult {
    BigInt gcd;
    BigInt x;
    BigInt y;
};

// Hàm tính gcd mở rộng
GCDResult gcd_extended(const BigInt &a, const BigInt &b) {
    if (b.isZero()) {
        GCDResult result = {a, BigInt(1), BigInt(0)};
        return result;
    }

    GCDResult nextResult = gcd_extended(b, a % b);
    GCDResult result;
    result.gcd = nextResult.gcd;
    result.x = nextResult.y;
    result.y = nextResult.x - (a / b) * nextResult.y;

    return result;
}

// Hàm tính nghịch đảo modular
BigInt mod_inverse(const BigInt &e, const BigInt &phi) {
    GCDResult result = gcd_extended(e, phi);

    if (result.gcd != BigInt(1)) {
        throw invalid_argument("Modular inverse does not exist");
    }

    return (result.x % phi + phi) % phi; // Đảm bảo kết quả luôn dương
}

// Hàm tìm giá trị nhỏ hơn giữa hai BigInt
BigInt min(const BigInt &a, const BigInt &b) {
    return (a < b) ? a : b;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        cerr << "Usage: " << argv[0] << " <input_file> <output_file>" << endl;
        return 1;
    }

    string inputFile = argv[1];
    string outputFile = argv[2];

    // Đọc dữ liệu từ file input
    ifstream fin(inputFile);
    if (!fin.is_open()) {
        cerr << "Error: Unable to open input file." << endl;
        return 1;
    }

    string pStr, qStr, eStr;
    getline(fin, pStr);
    getline(fin, qStr);
    getline(fin, eStr);
    fin.close();

    // Chuyển chuỗi từ file input thành BigInt
    BigInt p(pStr), q(qStr), e(eStr);

    // Kiểm tra điều kiện e < min(p, q)
    BigInt one = 1;
    BigInt pMinus1 = p - one;
    BigInt qMinus1 = q - one;

    if (e >= min(p, q)) {
        ofstream fout(outputFile);
        fout << "-1" << endl;
        fout.close();
        return 1;
    }

    // Tính phi(n) = (p - 1)(q - 1)
    BigInt phi = pMinus1 * qMinus1;

    // Tính khóa bí mật d
    try {
        BigInt d = mod_inverse(e, phi); // Tính nghịch đảo modular của e theo phi(n)

        // Ghi kết quả vào file output
        ofstream fout(outputFile);
        if (!fout.is_open()) {
            cerr << "Error: Unable to open output file." << endl;
            return 1;
        }

        fout << d << endl;
        fout.close();
    } catch (const invalid_argument &ex) {
        cerr << "Error: " << ex.what() << endl;

        ofstream fout(outputFile);
        fout << "-1" << endl;
        fout.close();
        return 1;
    }

    return 0;
}