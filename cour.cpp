#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>
#include <limits>
#include <queue>

// Splits a CSV line into fields, handling quotes and escaped quotes
static std::vector<std::string> splitCsvLine(const std::string& line) {
    std::vector<std::string> result;
    std::string field;
    bool inQuotes = false;
    for (size_t i = 0; i < line.size(); ++i) {
        char c = line[i];
        if (c == '"') {
            if (inQuotes && i + 1 < line.size() && line[i + 1] == '"') {
                field += '"'; ++i;
            } else {
                inQuotes = !inQuotes;
            }
        } else if (c == ',' && !inQuotes) {
            result.push_back(field);
            field.clear();
        } else {
            field += c;
        }
    }
    result.push_back(field);
    return result;
}

// Load bank-correspondent relationships
std::map<std::string, std::vector<std::string>> loadBanks(const std::string& filename) {
    std::ifstream fin(filename);
    if (!fin) throw std::runtime_error("Cannot open file: " + filename);
    std::string line;
    std::getline(fin, line); // skip header

    std::map<std::string, std::vector<std::string>> banks;
    while (std::getline(fin, line)) {
        if (line.empty()) continue;
        auto fields = splitCsvLine(line);
        if (fields.size() != 3) continue;
        const auto& bank = fields[0];
        const auto& list = fields[2];
        std::vector<std::string> corres;
        std::istringstream ss(list);
        std::string item;
        while (std::getline(ss, item, ';')) {
            if (!item.empty()) corres.push_back(item);
        }
        banks[bank] = std::move(corres);
    }
    return banks;
}

// Commission rule and table
struct CommissionRule { double threshold, fixedFee, percent; };
using Rules = std::vector<CommissionRule>;
class CommissionTable {
public:
    void load(const std::string& filename) {
        std::ifstream fin(filename);
        if (!fin) throw std::runtime_error("Cannot open file: " + filename);
        std::string line;
        std::getline(fin, line); // skip header
        while (std::getline(fin, line)) {
            if (line.empty()) continue;
            auto f = splitCsvLine(line);
            CommissionRule r{std::stod(f[2]), std::stod(f[3]), std::stod(f[4])};
            if (f[1] == "Input") inRules_[f[0]].push_back(r);
            else outRules_[f[0]].push_back(r);
        }
        auto sortFn = [](Rules& v) {
            std::sort(v.begin(), v.end(), [](auto& a, auto& b) { return a.threshold < b.threshold; });
        };
        for (auto& kv : inRules_)  sortFn(kv.second);
        for (auto& kv : outRules_) sortFn(kv.second);
    }
    double calc(const Rules& v, double amt) const {
        CommissionRule sel = v.front();
        for (const auto& r : v) {
            if (amt <= r.threshold) { sel = r; break; }
            sel = r;
        }
        return sel.fixedFee + sel.percent * amt;
    }
    double inFee(const std::string& b, double amt) const { return calc(inRules_.at(b), amt); }
    double outFee(const std::string& b, double amt) const { return calc(outRules_.at(b), amt); }
private:
    std::unordered_map<std::string, Rules> inRules_, outRules_;
};

struct Edge { int to; double weight; };

// Build graph: edge exists if A and B share a common correspondent
std::vector<std::vector<Edge>> buildGraph(
    const std::map<std::string, std::vector<std::string>>& corr,
    const CommissionTable& ct,
    const std::vector<std::string>& banks,
    double amount) {
    int n = banks.size();
    std::unordered_map<std::string, int> idx;
    for (int i = 0; i < n; ++i) idx[banks[i]] = i;
    std::vector<std::unordered_set<std::string>> sets(n);
    for (int i = 0; i < n; ++i) {
        for (const auto& c : corr.at(banks[i])) sets[i].insert(c);
    }
    std::vector<std::vector<Edge>> graph(n);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (i == j) continue;
            bool common = false;
            for (const auto& c : sets[i]) {
                if (sets[j].count(c)) { common = true; break; }
            }
            if (!common) continue;
            double w = ct.outFee(banks[i], amount) + ct.inFee(banks[j], amount);
            graph[i].push_back({j, w});
        }
    }
    return graph;
}

// Dijkstra with hop limit (maxEdges)
std::pair<double, std::vector<int>> dijkstraLimited(
    const std::vector<std::vector<Edge>>& g,
    int src, int dst, int maxEdges) {
    int n = g.size();
    const double INF = std::numeric_limits<double>::infinity();
    std::vector<std::vector<double>> dist(n, std::vector<double>(maxEdges+1, INF));
    std::vector<std::vector<std::pair<int,int>>> prev(n, std::vector<std::pair<int,int>>(maxEdges+1, {-1,-1}));
    using State = std::tuple<double,int,int>;
    std::priority_queue<State, std::vector<State>, std::greater<>> pq;
    dist[src][0] = 0;
    pq.emplace(0, src, 0);
    while (!pq.empty()) {
        auto [d,u,h] = pq.top(); pq.pop();
        if (d > dist[u][h]) continue;
        if (u == dst) break;
        if (h == maxEdges) continue;
        for (const auto& e : g[u]) {
            int v = e.to, nh = h + 1;
            double nd = d + e.weight;
            if (nd < dist[v][nh]) {
                dist[v][nh] = nd;
                prev[v][nh] = {u, h};
                pq.emplace(nd, v, nh);
            }
        }
    }
    double best = INF;
    int bestH = -1;
    for (int h = 1; h <= maxEdges; ++h) {
        if (dist[dst][h] < best) { best = dist[dst][h]; bestH = h; }
    }
    std::vector<int> path;
    if (bestH >= 0) {
        for (int u = dst, h = bestH; u != -1; ) {
            path.push_back(u);
            auto pr = prev[u][h]; u = pr.first; h = pr.second;
        }
        std::reverse(path.begin(), path.end());
    }
    return {best, path};
}

// Wrapper to find route for given amount
std::pair<double, std::vector<int>> findRoute(
    double amount, int s, int t,
    const std::map<std::string, std::vector<std::string>>& corr,
    const CommissionTable& ct,
    const std::vector<std::string>& banks,
    int maxEdges) {
    auto graph = buildGraph(corr, ct, banks, amount);
    return dijkstraLimited(graph, s, t, maxEdges);
}

int main() {
    try {
        auto corr = loadBanks("Banks1.csv");
        CommissionTable ct; ct.load("Commissions.csv");
        std::vector<std::string> banks;
        for (const auto& kv : corr) banks.push_back(kv.first);
        std::unordered_map<std::string,int> idx;
        for (int i = 0; i < (int)banks.size(); ++i) idx[banks[i]] = i;

        std::cout << "Available banks:\n";
        for (const auto& b : banks) std::cout << "  " << b << "\n";
        std::cout << "Enter source bank: ";
        std::string src, dst;
        std::getline(std::cin, src);
        std::cout << "Enter destination bank: ";
        std::getline(std::cin, dst);
        std::cout << "Enter transfer amount: ";
        double X; std::cin >> X;

        if (!idx.count(src) || !idx.count(dst)) {
            std::cerr << "Invalid bank name.\n";
            return 1;
        }
        int s = idx[src], t = idx[dst];
        const int MAX_EDGES = 2; // <= 1 intermediary

        double bestCost = std::numeric_limits<double>::infinity();
        std::vector<int> bestPath;
        int bestK = 1;
        double bestPart = X;

        for (int k = 1; k <= 5; ++k) {
            double part = X / k;
            auto [costPerPart, path] = findRoute(part, s, t, corr, ct, banks, MAX_EDGES);
            if (costPerPart == std::numeric_limits<double>::infinity()) continue;
            double totalCost = costPerPart * k;
            if (totalCost < bestCost) {
                bestCost = totalCost;
                bestPath = path;
                bestK = k;
                bestPart = part;
            }
        }

        if (bestCost == std::numeric_limits<double>::infinity()) {
            std::cout << "No route within 1 intermediary for any split up to 5." << std::endl;
        } else {
            std::cout << "Optimal split: " << bestK << " parts of " << bestPart << " RUB each" << std::endl;
            std::cout << "Total commission: " << bestCost << " RUB" << std::endl;
            std::cout << "Path: ";
            for (int i = 0; i < (int)bestPath.size(); ++i) {
                std::cout << banks[bestPath[i]];
                if (i + 1 < (int)bestPath.size()) std::cout << " -> ";
            }
            std::cout << std::endl;
        }
    } catch (const std::exception& ex) {
        std::cerr << "Error: " << ex.what() << std::endl;
        return 1;
    }
    return 0;
}
