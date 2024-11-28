#include <iostream>
#include <cmath>
#include <cstdint>
#include <vector>
#include <bitset>
#include <string>
#include <fstream>
#include <ctime>

using namespace std;

// Đọc tệp hex và trả về vector chứa các ký tự hex
vector<char> readHexFile(const string &fileName)
{
    ifstream fin(fileName);
    vector<char> hex_number;
    char hex_char;

    if (!fin)
    {
        cerr << "Error: Unable to open file " << fileName << endl;
        return hex_number;
    }

    while (fin >> hex_char)
    {
        hex_number.push_back(hex_char);
    }

    fin.close();
    return hex_number;
}

void writeToFile(const string &fileName, int result)
{
    ofstream fout(fileName);
    if (fout.is_open())
    {
        fout << result << endl;
        fout.close();
    }
    else
    {
        cerr << "Error: Unable to open output file: " << fileName << endl;
    }
}

vector<char> hexToBinary(vector<char> &hex_number)
{
    vector<char> binary;
    binary.reserve(hex_number.size() * 4); // n * 4 ky tu nhi phan

    for (int i = 0; i < hex_number.size(); ++i)
    {
        // isxdigit dung cho he 16
        char hex = hex_number[i];
        if (isxdigit(hex))
        {
            int hexBin;
            if (isdigit(hex))
            {
                hexBin = hex - '0'; // hex '3' - '0' = 51 - 48 = 3
            }
            else
            {
                hexBin = tolower(hex) - 'a' + 10; // chuyen sang 0 - 5 truoc (a[97] - 'a' = 0,...) sau do +10 => 10 -> 15
            }

            bitset<4> bits(hexBin); // chuyen gtri hexBin sang nhi phan (4 bits)
            for (int j = 3; j >= 0; --j)
            {
                if (bits[j] == 1)
                {
                    binary.push_back('1');
                }
                else
                {
                    binary.push_back('0');
                }
            }
        }
    }
    return binary;
}

bool checkOdd(vector<char> &binary)
{
    if (binary.back() == '0')
    {
        return false; // chan
    }
    return true; // le
}

// n - 1 = (2^r) * m
void findKandM(vector<char> binary, vector<char> &kNum, vector<char> &mNum, int &r)
{
    binary.pop_back();
    kNum.push_back('0');
    for (int i = binary.size() - 1; i >= 0; i--)
    {
        if (binary[i] == '0')
        {
            kNum.push_back('0');
            binary.pop_back();
        }
        if (binary[i] == '1')
        {
            kNum.insert(kNum.begin(), '1');
            i = -1;
        }
    }
    r = kNum.size() + 1;
    mNum = binary;
}

vector<char> addBinary(vector<char> bin1, vector<char> bin2)
{
    vector<char> add_result;
    int carry = 0; // bien nho khi ket qua > 1

    int length1 = bin1.size();
    int length2 = bin2.size();

    // them so 0 dang truoc sao cho 2 bin cung do dai
    if (length1 < length2)
    {
        for (int i = 0; i < length2 - length1; ++i)
        {
            bin1.insert(bin1.begin(), '0');
        }
    }
    else if (length1 > length2)
    {
        for (int i = 0; i < length1 - length2; i++)
        {
            bin2.insert(bin2.begin(), '0');
        }

    }

    // cong tung bit tru trai sang phai
    for (int i = bin1.size() - 1; i >= 0; i--)
    {
        char temp = bin1[i];
        if (bin1[i] == '0' && bin2[i] == '0')
        {
            if (carry == 1)
            {
                add_result.insert(add_result.begin(), '1');
                carry = 0;
            }
            else
            {
                add_result.insert(add_result.begin(), '0');
            }
        }
        else if (bin1[i] == '1' && bin2[i] == '0')
        {
            if (carry == 1)
            {
                add_result.insert(add_result.begin(), '0');
            }
            else
            {
                add_result.insert(add_result.begin(), '1');
            }
        }
        else if (bin1[i] == '0' && bin2[i] == '1')
        {
            if (carry == 1)
            {
                add_result.insert(add_result.begin(), '0');
            }
            else
            {
                add_result.insert(add_result.begin(), '1');
            }
        }
        else if (bin1[i] == '1' && bin2[i] == '1')
        {
            if (carry == 1)
            {
                add_result.insert(add_result.begin(), '1');
            }
            else
            {
                add_result.insert(add_result.begin(), '0');
                carry = 1;
            }
        }
    }
    if (carry == 1)
    {
        add_result.insert(add_result.begin(), '1');
        carry = 0;
    }

    return add_result;
}

// bu 2
vector<char> NOT_binary(vector<char> bin)
{
    vector<char> one;
    vector<char> res;
    one.push_back('1');
    for (int i = 0; i < bin.size(); i++)
    {
        bin[i] == '0' ? bin[i] = '1' : bin[i] = '0';
    }
    res = addBinary(bin, one);
    return res;
}

vector<char> subtractBinary(vector<char> bin1, vector<char> bin2)
{
    vector<char> result;

    int length1 = bin1.size();
    int length2 = bin2.size();

    // them so 0 dang truoc sao cho 2 bin cung do dai
    if (length1 < length2)
    {
        bin1.insert(bin1.begin(), length2 - length1, '0');
    }
    else if (length1 > length2)
    {
        bin2.insert(bin2.begin(), length1 - length2, '0');
    }

    vector<char> bin_NOT;
    bin_NOT = NOT_binary(bin2);

    result = addBinary(bin1, bin_NOT);

    result.erase(result.begin());

    return result;
}

void shiftLeft(vector<char> &A, vector<char> &Q)
{
    // Luu bit dau tien cua Q de chuyen vao cuoi cua A
    char first_Q = Q[0];

    // Dịch trái từng phần tử trong A
    for (int i = 0; i < A.size() - 1; ++i)
    {
        A[i] = A[i + 1];
    }
    A[A.size() - 1] = first_Q; // Them bit dau cua Q vao cuoi A

    // Dich trai tung phan tu trong Q, them '0' vao cuo
    for (int i = 0; i < Q.size() - 1; ++i)
    {
        Q[i] = Q[i + 1];
    }
    Q[Q.size() - 1] = '0'; // bit cuoi cua Q la '0'
}

void shiftRight(vector<char> &A, vector<char> &Q)
{
    // Luu bit cuoi cung cua A de chuyen vao dau Q
    char last_A = A[A.size() - 1];

    // Dich phai tung phan tu cua A, them '0' o dau
    for (int i = A.size() - 1; i > 0; --i)
    {
        A[i] = A[i - 1];
    }
    A[0] = '0';

    // Dich phai tung phan tu trong Q, them bit cuoi cua A vao dau
    for (int i = Q.size() - 1; i > 0; --i)
    {
        Q[i] = Q[i - 1];
    }
    Q[0] = last_A; // Bit dau cua Q la bit cuoi cua A
}

vector<char> createBinaryWithZeros(int size)
{
    vector<char> result;
    for (int i = 0; i < size; i++)
    {
        result.push_back('0');
    }
    return result;
}

vector<char> multiplyBinary(vector<char> bin1, vector<char> bin2)
{
    vector<char> result;
    int length1 = bin1.size();
    int length2 = bin2.size();

    // binary = 1, = 0
    if (length1 == 1 && bin1.back() == '1')
    {
        return bin2;
    }
    if (length2 == 1 && bin2.back() == '1')
    {
        return bin1;
    }
    if ((length1 == 1 && bin1.back() == '0') || (length2 == 1 && bin2.back() == '0'))
    {
        result.push_back('0');
        return result;
    }

    // them so 0 dang truoc sao cho 2 bin cung do dai
    for (int i = 0; i < length1; i++)
    {
        bin2.insert(bin2.begin(), '0');
    }
    for (int i = 0; i < length2; i++)
    {
        bin1.insert(bin1.begin(), '0');
    }

    vector<char> A;
    A = createBinaryWithZeros(bin1.size()); // Tao mot thanh ghi A voi toan gia tri 0

    vector<char> Q1;
    Q1 = bin1; // Thanh ghi Q chua bin1
    Q1.push_back('0');      // Them mot bit phu vao cuoi Q

    vector<char> M;
    M = bin2; // M chua bin2

    int iterationCount = bin1.size(); // So lan lap = do dai nhi phan

    // Thua toan Booth
    while (iterationCount > 0)
    {
        // Kiem tra 2 bit cuoi cua Q
        if (Q1[Q1.size() - 2] == '1' && Q1[Q1.size() - 1] == '0')
        {
            A = subtractBinary(A, M); // 2 bit cuoi 1 va 0: A = A - M
        }
        else if (Q1[Q1.size() - 2] == '0' && Q1[Q1.size() - 1] == '1')
        {
            A = addBinary(A, M); // 2 bit cuoi la 0 va 1: A = A + M
        }
        shiftRight(A, Q1);
        --iterationCount;
    }
    for (int i = 0; i < Q1.size(); i++)
    {
        if (Q1[i] == '0')
        {
            Q1.erase(Q1.begin());
        }
        if (Q1[i] == '1')
        {
            i = Q1.size();
        }
    }
    Q1.pop_back();
    return Q1;
}

void divideBinary(vector<char> &bin1, vector<char> &bin2, vector<char> &quotient, vector<char> &remainder)
{
    int length1 = bin1.size();
    int length2 = bin2.size();
    int maxLength;

    // Tinh do dai can chinh
    if (length1 > length2)
    {
        if (length1 <= 64)
            maxLength = 64;
        if (length1 > 64 && length1 <= 128)
            maxLength = 128;
        if (length1 > 128 && length1 <= 256)
            maxLength = 256;
        if (length1 > 256 && length1 <= 512)
            maxLength = 512;
    }
    if (length2 > length1)
    {
        if (length2 <= 64)
            maxLength = 64;
        if (length2 > 64 && length2 <= 128)
            maxLength = 128;
        if (length2 > 128 && length2 <= 256)
            maxLength = 256;
        if (length2 > 256 && length2 <= 512)
            maxLength = 512;
    }

    for (int i = 0; i < maxLength - length2; i++)
    {
        bin2.insert(bin2.begin(), '0');
    }
    for (int i = 0; i < maxLength - length1; i++)
    {
        bin1.insert(bin1.begin(), '0');
    }

    vector<char> A;
    A = createBinaryWithZeros(bin1.size());               // thương
    vector<char> Q;
    Q = bin1;                               // số bị chia
    vector<char> M;
    M = bin2;                               // số chia

    int iterations = bin1.size();

    // Xu ly dau: neu Q hoặc M la so am, chuyen sang bu 2
    if (Q[0] == '1')
        Q = NOT_binary(Q);
    if (M[0] == '1')
        M = NOT_binary(M);

    // Bat dau chia
    while (iterations > 0)
    {
        shiftLeft(A, Q);
        A = subtractBinary(A, M); // A = A - M

        if (A[0] == '1')
        { // Nếu A âm
            Q[Q.size() - 1] = '0';
            A = addBinary(A, M); // A = A + M
        }
        else
        {
            Q[Q.size() - 1] = '1';
        }
        --iterations;
    }

    if (bin1[0] ^ bin2[0])
    {
        Q = NOT_binary(Q);
        A = NOT_binary(A);
    }

    A[0] = '0'; // ket qua con lại la duong

    // Gan ket qua
    quotient = Q;
    remainder = A;
}

vector<char> powerBinary(vector<char> &base, vector<char> &exponent)
{
    vector<char> result;
    result.push_back('1');
    vector<char> tempBase;
    tempBase = base;

    for (int i = exponent.size() - 2; i >= 0; --i)
    {
        base = multiplyBinary(base, base);

        if (exponent[i] == '1')
        {
            result = multiplyBinary(result, base);
        }
    }

    if (exponent.back() == '1')
    {
        result = multiplyBinary(result, tempBase);
    }

    return result;
}

// Kiem tra so nhi phan co bang 1 hay khong
bool isBinaryOne(const vector<char> &binary)
{
    if (binary.back() != '1')
        return false;

    for (int i = 0; i < binary.size() - 1; ++i)
    {
        if (binary[i] == '1')
            return false;
    }

    return true;
}

vector<char> intToBinary(int num)
{
    vector<char> binary;
    // Duyet qua tung bit cua so nguyen tu bit cao nhat den bit thap nhat
    for (int i = 3; i >= 0; --i)
    {
        char bit = (num & (1 << i)) ? '1' : '0'; // Su dung phep AND de xac dinh gia tri bit
        binary.push_back(bit);
    }

    return binary;
}

// Tao so ngau nhien (dang so nhi phan 4 bit)
vector<char> generateRandomBinary()
{
    vector<char> result;
    int randomNumbers[1] = {2}; // Tap hop so co san

    srand(time(NULL));
    int randomIndex = 0;

    result = intToBinary(randomNumbers[randomIndex]);
    return result;
}

bool millerRabin(vector<char> binary)
{
    int r;

    // Kiểm tra nếu số không lẻ, trả về false
    if (!checkOdd(binary))
        return false;

    // Trường hợp đặc biệt: số có 2 chữ số nhị phân
    if (binary.size() == 2)
        return true;

    // Biến lưu trữ kết quả phân rã
    vector<char> kNum;
    vector<char> mNum;

    // Phân tích số nhị phân thành r và m (n - 1 = 2^r * m)
    findKandM(binary, kNum, mNum, r);

    // Tạo số ngẫu nhiên a
    vector<char> a;
    a = generateRandomBinary();

    // Khởi tạo chỉ số và giá trị y
    vector<char> index;
    vector<char> y;
    vector<char> yPowIndex;
    index.push_back('1');

    // Tính y = a^m mod n
    y = powerBinary(a, mNum);

    for (int i = 0; i < r; ++i)
    {
        vector<char> divY;
        vector<char> modY;

        // Tính y^(2^index) mod n
        yPowIndex = powerBinary(y, index);
        divideBinary(yPowIndex, binary, divY, modY);

        // Nếu modY = 1, số này có thể là số nguyên tố
        if (isBinaryOne(modY))
        {
            return true;
        }

        // Tăng chỉ số (nhân đôi index)
        index.push_back('0');
    }

    // Nếu không tìm thấy, trả về false
    return false;
}

bool isPrime(vector<char> &binary)
{
    if (!checkOdd(binary))
    {
        return false;
    }

    if (!millerRabin(binary))
    {
        return false;
    }

    return true;
}

int main(int argc, char **argv)
{
    if (argc < 3)
    {
        cerr << "Usage: " << argv[0] << " <input_file> <output_file>" << endl;
        return 1;
    }

    const string fileIn = argv[1];
    const string fileOut = argv[2];

    vector<char> hexNum = readHexFile(fileIn);
    if (hexNum.empty())
    {
        cerr << "Error: Input file is empty or unreadable.";
        return 1;
    }

    vector<char> binary = hexToBinary(hexNum);

    if (isPrime(binary))
    {
        writeToFile(fileOut, 1);
    }
    else
    {
        writeToFile(fileOut, 0);
    }

    return 0;
}