#include <iostream>
#include "Utils.hpp"


std::string util::String::get_data() {
    return data;
}

void util::String::print() {
    std::cout << data << std::endl;
}

std::vector<util::String> util::String::splitAt(std::string delimiter) {
    std::vector<util::String> parts;
    size_t last = 0;
    size_t next = 0;
    // find all occurences of delimiter in data and append the parts
    // inbetween to vector parts
    while ((next = data.find(delimiter, last)) != std::string::npos) {
        parts.push_back(util::String(data.substr(last, next-last)));
        last = next + 1;
    }
    // don't forget the last one
    parts.push_back(util::String(data.substr(last)));
    return parts;
}
