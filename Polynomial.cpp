#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <cmath>
#include <unordered_map>

template<typename T>
void Normalize(std::vector<T> &coef) {
    for (auto it = coef.rbegin(); it != coef.rend(); ++it) {
        if (*it == T(0)) {
            coef.pop_back();
        } else {
            break;
        }
    }
}


template<typename T>
class Polynomial {
    std::vector<T> data;

public:
    template<typename Iterator>
    Polynomial(Iterator first, Iterator last) {
        data = std::vector<T>(first, last);
        Normalize(data);
    }

    Polynomial(const std::vector<T>& coef) {
        data = coef;
        Normalize(data);
    }

    Polynomial(const T& coef = T(0)) {
        data.push_back(coef);
        Normalize(data);
    }

    int Degree() const {
        if (data.empty()) {
            return -1;
        } else {
            return static_cast<int>(data.size()) - 1;
        }
    }

    friend bool operator== (const Polynomial& p1, const Polynomial& p2) {
        if (p1.data.size() != p2.data.size()) {
            return false;
        }
        for (size_t i = 0; i != p1.data.size(); ++i) {
            if (p1.data[i] != p2.data[i]) {
                return false;
            }
        }
        return true;
    }

    friend bool operator!= (const Polynomial& p1, const Polynomial& p2) {
        return !(p1 == p2);
    }

    bool operator ==(const T &coef) const {
        return *this == Polynomial<T>(coef);
    }

    bool operator !=(const T &coef) const {
        return *this != Polynomial<T>(coef);
    }

    friend Polynomial operator+ (const Polynomial& p1, const Polynomial& p2) {
        auto P = p1;
        P.data.resize(std::max(p2.data.size(), P.data.size()), T(0));
        for (size_t i = 0; i != std::min(P.data.size(), p2.data.size()); ++i) {
            P.data[i] += p2.data[i];
        }
        Normalize(P.data);
        return P;
    }

    friend Polynomial operator+ (const T& coef, const Polynomial& p1) {
        auto P = Polynomial<T>(coef) + p1;
        return P;
    }

    friend Polynomial operator+ (const Polynomial& p1, const T& coef) {
        auto P = Polynomial<T>(coef) + p1;
        return P;
    }

    friend Polynomial operator- (const Polynomial& p1, const Polynomial& p2) {
        auto P = p1;
        P.data.resize(std::max(p2.data.size(), P.data.size()), T(0));
        for (size_t i = 0; i != std::min(P.data.size(), p2.data.size()); ++i) {
            P.data[i] -= p2.data[i];
        }
        Normalize(P.data);
        return P;
    }

    friend Polynomial operator- (const T& coef, const Polynomial& p1) {
        auto P = Polynomial<T>(coef) - p1;
        return P;
    }

    friend Polynomial operator- (const Polynomial& p1, const T& coef) {
        auto P = p1 - Polynomial<T>(coef);
        return P;
    }

    Polynomial& operator-= (const Polynomial& other) {
        *this = *this - other;
        return *this;
    }

    Polynomial& operator+= (const Polynomial& other) {
        *this = *this + other;
        return *this;
    }

    Polynomial& operator-= (const T& other) {
        *this = *this - Polynomial<T>(other);
        return *this;
    }

    Polynomial& operator+= (const T& other) {
        *this = *this + Polynomial<T>(other);
        return *this;
    }

    friend Polynomial operator* (const Polynomial& p1, const Polynomial& p2) {
        std::vector<T> multiplicated(p1.data.size() + p2.data.size(), T(0));
        for (size_t i = 0; i != p1.data.size(); ++i) {
            for (size_t j = 0; j != p2.data.size(); ++j) {
                multiplicated[i + j] += p1.data[i] * p2.data[j];
            }
        }
        Normalize(multiplicated);
        auto P = Polynomial(multiplicated);
        return P;
    }

    friend Polynomial operator* (const T& p1, const Polynomial& p2) {
        auto P = Polynomial(p1);
        return P * p2;
    }

    friend Polynomial operator* (const Polynomial& p1, const T& p2) {
        auto P = Polynomial(p2);
        return P * p1;
    }

    Polynomial& operator*= (const Polynomial& other) {
        *this = *this * other;
        return *this;
    }

    Polynomial& operator*= (const T& other) {
        *this = *this * Polynomial<T>(other);
        return *this;
    }

    T operator[] (const size_t i) const {
        if (i >= data.size()) {
            T ans(0);
            return ans;
        } else {
            return data[i];
        }
    }

    auto begin() const {
        return data.begin();
    }

    auto end() const {
        return data.end();
    }

    T operator () (const T &x) const {
        auto y = T(0);
        for (int i = static_cast<int>(data.size()) - 1; i >= 0; --i) {
            y += data[i];
            auto t = i - 1;
            if (t >= 0) {
                y *= x;
            }
        }
        return y;
    }

    friend std::ostream& operator<< (std::ostream& out, const Polynomial<T> &P) {
        bool first = true;
        if (P.data.empty()) {
            out << 0;
        } else {
            for (int degree = static_cast<int>(P.data.size()) - 1; degree >= 0; --degree) {
                T a_degree = P[degree];
                if (a_degree != T(0)) {
                    if (a_degree > T(0) && !first) {
                        out << '+';
                    }
                    if (degree == 0) {
                        out << a_degree;
                    } else if (a_degree == T(-1)) {
                        out << "-x";
                    } else if (a_degree == T(1)) {
                        out << "x";
                    } else {
                        out << a_degree << "*x";
                    }
                    if (degree > 1) {
                        out << "^" << degree;
                    }
                    first = false;
                }
            }
        }
        return out;
    }

    friend Polynomial operator& (const Polynomial<T> &P, const Polynomial<T> &Q) {
        Polynomial<T> ans(T(0));
        for (auto degree = P.data.rbegin(); degree != P.data.rend(); ++degree) {
            ans *= Q;
            ans += (*degree);
        }
        return ans;
    }

    friend Polynomial operator/ (const Polynomial<T> &P, const Polynomial<T> &Q) {
        Polynomial<T> ans(T(0));
        Polynomial<T> P_nonconst = P;
        T coef = Q.data.back();
        size_t Q_degree = Q.data.size();
        while (P_nonconst.data.size() >= Q_degree) {
            T a_degree = P_nonconst.data.back() / coef;
            size_t degree = P_nonconst.data.size() - Q_degree;
            std::vector<T> query(degree + 1);
            query.back() = a_degree;
            Polynomial<T> division_res(query);
            P_nonconst -= Q * division_res;
            ans += division_res;
        }
        return ans;
    }

    friend Polynomial operator% (const Polynomial<T>& P, const Polynomial<T> &Q) {
        Polynomial<T> ans = P;
        Polynomial<T> div_res = P / Q;
        ans -= Q * div_res;
        return ans;
    }

    friend Polynomial operator, (const Polynomial<T> &P, const Polynomial<T> &Q) {
        Polynomial<T> P_nonconst = P;
        Polynomial<T> Q_nonconst = Q;
        while (Q_nonconst.data.size() != 0) {
            P_nonconst = P_nonconst % Q_nonconst;
            auto temp = P_nonconst;
            P_nonconst = Q_nonconst;
            Q_nonconst = temp;
        }
        if (P_nonconst.data.size() != 0) {
            Polynomial<T> query(P_nonconst.data.back());
            P_nonconst = P_nonconst / query;
        }
        return P_nonconst;
    }
};
