#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <tuple>
#include <iomanip>
#include <stdexcept>

using namespace std;

const uint32_t BASE = 1000000000;

struct BigInt
{
    bool sign;
    vector<uint32_t> digit;
};

struct GCDResult
{
    BigInt gcd; // Giá trị GCD
    BigInt x;   // Hệ số x
    BigInt y;   // Hệ số y
};

uint32_t convertHex(char hex)
{
    if (isdigit(hex))
    {
        return hex - '0';
    }

    if (isxdigit(hex))
    {
        return toupper(hex) - 'A' + 10;
    }
    throw invalid_argument("Invalid hexadecimal character");
}

bool isZero(const BigInt &num)
{
    return num.digit.empty() || (num.digit.size() == 1 && num.digit[0] == 0);
}

bool isOne(const BigInt &num)
{
    return num.digit.size() == 1 && num.digit[0] == 1 && num.sign == true;
}

BigInt shiftLeft(const BigInt &num, size_t positions)
{
    if (num.digit.empty() || (num.digit.size() == 1 && num.digit[0] == 0))
    {
        return num;
    }

    BigInt result = num;
    result.digit.insert(result.digit.begin(), positions, 0);
    return result;
}

void addDigit(BigInt &number, uint32_t digit)
{
    long long int carry = digit;
    for (size_t i = 0; i < number.digit.size(); ++i)
    {
        long long int sum = static_cast<long long int>(number.digit[i]) + carry;
        number.digit[i] = sum % BASE;
        carry = sum / BASE;
        if (carry == 0)
            return;
    }
    if (carry > 0)
    {
        number.digit.push_back(static_cast<uint32_t>(carry));
    }
}

void multiply16(BigInt &number)
{
    long long int carry = 0;
    for (size_t i = 0; i < number.digit.size(); ++i)
    {
        long long int result = static_cast<long long int>(number.digit[i]) * 16 + carry;
        number.digit[i] = result % BASE;
        carry = result / BASE;
    }
    if (carry > 0)
    {
        number.digit.push_back(static_cast<uint32_t>(carry));
    }
}

BigInt hexToInt(const string &hex)
{
    BigInt result;
    result.sign = true;
    result.digit.push_back(0);

    for (size_t i = 0; i < hex.size(); ++i)
    {
        multiply16(result);
        addDigit(result, convertHex(hex[i]));
    }
    return result;
}

int compare(const BigInt &a, const BigInt &b)
{
    if (a.sign != b.sign)
    {
        if (a.sign)
        {
            return 1;
        }
        else
        {
            return -1;
        }
    }

    if (a.digit.size() != b.digit.size())
    {
        if (a.digit.size() > b.digit.size())
        {
            return 1;
        }
        else
        {
            return -1;
        }
    }

    for (size_t i = a.digit.size(); i-- > 0;)
    {
        if (a.digit[i] != b.digit[i])
        {
            if (a.digit[i] > b.digit[i])
            {
                return 1;
            }
            else
            {
                return -1;
            }
        }
    }

    return 0;
}

BigInt min(const BigInt &a, const BigInt &b)
{
    int comparisonResult = compare(a, b);

    if (comparisonResult < 0)
    {
        return a;
    }

    return b;
}

// khai báo trước
BigInt add(const BigInt &a, const BigInt &b);

BigInt subtract(const BigInt &a, const BigInt &b)
{
    BigInt result;

    // Trường hợp cả hai số đều âm
    if (!a.sign && !b.sign)
    {
        // (-a) - (-b) tương đương với b - a
        return subtract(b, a);
    }

    // Trường hợp dấu khác nhau: a - (-b) tương đương a + b
    if (a.sign != b.sign)
    {
        BigInt b_positive = b;
        b_positive.sign = a.sign;
        return add(a, b_positive);
    }

    // Nếu cả hai số có cùng dấu, so sánh giá trị tuyệt đối
    if (compare(a, b) < 0)
    {
        result = subtract(b, a);
        result.sign = !a.sign;
        return result;
    }

    result = a;
    int64_t borrow = 0;

    for (size_t i = 0; i < b.digit.size() || borrow > 0; ++i)
    {
        int64_t sub = (i < b.digit.size() ? b.digit[i] : 0) + borrow;
        if (result.digit[i] < sub)
        {
            result.digit[i] += BASE - sub;
            borrow = 1;
        }
        else
        {
            result.digit[i] -= sub;
            borrow = 0;
        }
    }

    while (result.digit.size() > 1 && result.digit.back() == 0)
    {
        result.digit.pop_back();
    }

    return result;
}

BigInt add(const BigInt &a, const BigInt &b)
{
    BigInt result;

    // Trường hợp cả hai số cùng dấu
    if (a.sign == b.sign)
    {
        result.sign = a.sign;
        result.digit.clear();

        long long int carry = 0;
        size_t maxSize = max(a.digit.size(), b.digit.size());

        for (size_t i = 0; i < maxSize || carry > 0; ++i)
        {
            long long int sum = carry;
            if (i < a.digit.size())
            {
                sum += a.digit[i];
            }
            if (i < b.digit.size())
            {
                sum += b.digit[i];
            }

            result.digit.push_back(sum % BASE);
            carry = sum / BASE;
        }

        return result;
    }
    else
    {
        if (!a.sign)
        {
            // a âm, b dương => b - |a|
            BigInt absA = a;
            absA.sign = true;
            return subtract(b, absA);
        }
        else
        {
            // a dương, b âm => a - |b|
            BigInt absB = b;
            absB.sign = true;
            return subtract(a, absB);
        }
    }
}

BigInt karatsubaMultiply(const BigInt &a, const BigInt &b)
{
    // Nếu một trong hai số là 0, trả về 0
    if (a.digit.empty() || b.digit.empty() || (a.digit.size() == 1 && a.digit[0] == 0) || (b.digit.size() == 1 && b.digit[0] == 0))
    {
        BigInt zero;
        zero.sign = true;
        zero.digit.push_back(0);
        return zero;
    }

    // Trường hợp cơ bản: nếu số chữ số nhỏ, dùng nhân trực tiếp
    if (a.digit.size() <= 64 || b.digit.size() <= 64)
    {
        BigInt result;
        result.sign = (a.sign == b.sign);
        result.digit.resize(a.digit.size() + b.digit.size(), 0);

        for (size_t i = 0; i < a.digit.size(); ++i)
        {
            long long int carry = 0;
            for (size_t j = 0; j < b.digit.size() || carry > 0; ++j)
            {
                long long int product = result.digit[i + j] + carry;
                if (j < b.digit.size())
                {
                    product += static_cast<long long int>(a.digit[i]) * b.digit[j];
                }
                result.digit[i + j] = product % BASE;
                carry = product / BASE;
            }
        }

        while (result.digit.size() > 1 && result.digit.back() == 0)
        {
            result.digit.pop_back();
        }
        return result;
    }

    // Chia số thành hai nửa
    size_t half = std::max(a.digit.size(), b.digit.size()) / 2;

    BigInt lowA, highA;
    lowA.sign = true;
    highA.sign = true;
    for (size_t i = 0; i < half && i < a.digit.size(); ++i)
    {
        lowA.digit.push_back(a.digit[i]);
    }
    for (size_t i = half; i < a.digit.size(); ++i)
    {
        highA.digit.push_back(a.digit[i]);
    }

    BigInt lowB, highB;
    lowB.sign = true;
    highB.sign = true;
    for (size_t i = 0; i < half && i < b.digit.size(); ++i)
    {
        lowB.digit.push_back(b.digit[i]);
    }
    for (size_t i = half; i < b.digit.size(); ++i)
    {
        highB.digit.push_back(b.digit[i]);
    }

    // Tính tích bằng đệ quy
    BigInt z0 = karatsubaMultiply(lowA, lowB);
    BigInt z1 = karatsubaMultiply(add(lowA, highA), add(lowB, highB));
    BigInt z2 = karatsubaMultiply(highA, highB);

    // Kết hợp các thành phần
    BigInt shiftedZ2 = shiftLeft(z2, 2 * half);
    BigInt shiftedMiddle = shiftLeft(subtract(z1, add(z2, z0)), half);
    BigInt result = add(add(shiftedZ2, shiftedMiddle), z0);

    // Gán dấu cho kết quả
    result.sign = (a.sign == b.sign);

    // Loại bỏ các chữ số 0 không cần thiết
    while (result.digit.size() > 1 && result.digit.back() == 0)
    {
        result.digit.pop_back();
    }

    return result;
}

pair<BigInt, BigInt> divide(const BigInt &dividend, const BigInt &divisor)
{
    if (divisor.digit.empty() || (divisor.digit.size() == 1 && divisor.digit[0] == 0))
    {
        throw invalid_argument("Division by zero");
    }

    // Nếu dividend nhỏ hơn divisor, trả về thương = 0, dư = dividend
    if (compare(dividend, divisor) < 0)
    {
        BigInt quotient;
        quotient.sign = true;
        quotient.digit.push_back(0);

        BigInt remainder = dividend;

        return make_pair(quotient, remainder);
    }

    BigInt quotient;
    quotient.sign = (dividend.sign == divisor.sign);
    quotient.digit.resize(dividend.digit.size(), 0);

    BigInt remainder;
    remainder.sign = dividend.sign;
    remainder.digit = dividend.digit;

    size_t shift = dividend.digit.size() - divisor.digit.size();
    BigInt shiftedDivisor = divisor;

    for (size_t i = 0; i < shift; ++i)
    {
        shiftedDivisor.digit.insert(shiftedDivisor.digit.begin(), 0);
    }

    for (size_t i = shift + 1; i-- > 0;)
    {
        uint32_t low = 0;
        uint32_t high = BASE - 1;
        uint32_t q = 0;

        // Tìm q lớn nhất sao cho shiftedDivisor * q <= remainder
        while (low <= high)
        {
            uint32_t mid = low + (high - low) / 2;

            BigInt midBigInt;
            midBigInt.sign = true;
            midBigInt.digit.push_back(mid);

            BigInt product = karatsubaMultiply(shiftedDivisor, midBigInt);
            if (compare(product, remainder) <= 0)
            {
                q = mid;
                low = mid + 1;
            }
            else
            {
                high = mid - 1;
            }
        }

        quotient.digit[i] = q;

        BigInt qBigInt;
        qBigInt.sign = true;
        qBigInt.digit.push_back(q);

        BigInt product = karatsubaMultiply(shiftedDivisor, qBigInt);

        remainder = subtract(remainder, product);

        if (i > 0)
        {
            shiftedDivisor.digit.erase(shiftedDivisor.digit.begin());
        }
    }

    while (quotient.digit.size() > 1 && quotient.digit.back() == 0)
    {
        quotient.digit.pop_back();
    }

    while (remainder.digit.size() > 1 && remainder.digit.back() == 0)
    {
        remainder.digit.pop_back();
    }

    return make_pair(quotient, remainder);
}

BigInt mod(const BigInt &a, const BigInt &b)
{
    if (b.digit.empty() || (b.digit.size() == 1 && b.digit[0] == 0))
    {
        throw invalid_argument("Modulo by zero");
    }

    BigInt remainder = divide(a, b).second;

    if (!remainder.sign)
    {
        remainder = add(remainder, b);
    }

    return remainder;
}

BigInt powerMod(const BigInt &base, const BigInt &exponent, const BigInt &modulus)
{
    if (modulus.digit.empty() ||
        (modulus.digit.size() == 1 && modulus.digit[0] == 0))
    {
        throw invalid_argument("Modulo by zero");
    }

    // Bước 1: Khởi tạo giá trị ban đầu của result là 1
    BigInt result;
    result.sign = true;
    result.digit.push_back(1);

    // Bước 2: Tính base mod modulus để đảm bảo base nằm trong khoảng [0, modulus)
    BigInt baseMod = mod(base, modulus);

    // Bước 3: Sao chép exponent để có thể thay đổi trong quá trình tính toán
    BigInt curr = exponent;

    // Bước 4: Vòng lặp xử lý từng bước
    while (!curr.digit.empty() &&
           !(curr.digit.size() == 1 && curr.digit[0] == 0))
    {

        // Kiểm tra nếu curr là số lẻ
        if (curr.digit[0] % 2 == 1)
        {
            // result = (result * baseMod) % modulus
            BigInt temp = karatsubaMultiply(result, baseMod);
            result = mod(temp, modulus);
        }

        // Giảm curr: curr = curr / 2
        BigInt two;
        two.sign = true;        // Số dương
        two.digit.push_back(2); // Chữ số là 2

        curr = divide(curr, two).first;

        // Tăng giá trị baseMod: baseMod = (baseMod * baseMod) % modulus
        BigInt tempBase = karatsubaMultiply(baseMod, baseMod);
        baseMod = mod(tempBase, modulus);
    }

    return result;
}

int main(int argc, char *argv[])
{
    // Kiểm tra số lượng tham số đầu vào
    if (argc != 3)
    {
        cerr << "Usage: " << argv[0] << " <input_file> <output_file>" << endl;
        return 1;
    }

    // Lấy tên tệp từ tham số đầu vào
    string inputFile = argv[1];
    string outputFile = argv[2];

    try
    {
        // Khởi tạo các biến cần thiết
        int x, y;
        BigInt N, e;
        vector<BigInt> messages, text;

        ifstream fin(inputFile);
        if (!fin.is_open())
        {
            throw runtime_error("Error: Cannot open input file '" + inputFile + "'.");
        }

        string line;

        getline(fin, line);
        stringstream ss(line);
        ss >> x >> y;

        if (x < 2 || x > 5)
        {
            throw runtime_error("Error: x must be in the range [2, 5].");
        }

        if (y < 10 || y > 20)
        {
            throw runtime_error("Error: y must be in the range [10, 20].");
        }

        // Đọc khóa N và e
        getline(fin, line);
        size_t split = line.find(' ');
        if (split == string::npos)
        {
            throw runtime_error("Error: Invalid input format. Expected N and e separated by a space.");
        }
        N = hexToInt(line.substr(0, split));
        e = hexToInt(line.substr(split + 1));

        for (int i = 0; i < x; ++i)
        {
            getline(fin, line);
            messages.push_back(hexToInt(line));
        }

        for (int j = 0; j < y; ++j)
        {
            getline(fin, line);
            text.push_back(hexToInt(line));
        }

        fin.close();

        // Bước 2: Xử lý message
        vector<int> results;

        for (size_t i = 0; i < messages.size(); ++i)
        {
            const BigInt &message = messages[i];
            BigInt encryptedValue = powerMod(message, e, N);
            int foundIndex = -1;

            for (size_t j = 0; j < text.size(); ++j)
            {
                if (compare(encryptedValue, text[j]) == 0)
                {
                    foundIndex = j;
                    break;
                }
            }

            results.push_back(foundIndex);
        }

        ofstream fout(outputFile);
        if (!fout.is_open())
        {
            throw runtime_error("Error: Cannot open output file '" + outputFile + "'.");
        }

        for (size_t i = 0; i < results.size(); ++i)
        {
            if (i > 0)
                fout << " ";
            fout << results[i];
        }
        fout << endl;

        fout.close();
    }
    catch (const exception &ex)
    {
        cerr << ex.what() << endl;
        return 1;
    }

    return 0;
}
