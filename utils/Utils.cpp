#include <iostream>
#include <algorithm>
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

std::vector<int> util::parse_ints(std::vector<util::String> strints) {
    // convert a vector of Strings to a vector of Ints
    std::vector<int> ints;
    for (util::String strint : strints) {
        if (strint.get_data().empty())
            continue;
        ints.push_back(std::stoi(strint.get_data()));
    }
    return ints;
}

int util::String::asint() {
    return std::stoi(data);
}

bool util::String::equals(std::string other) {
    return data == other;
}


std::vector<int> util::diff(std::vector<int> in) {
    std::vector<int> diff(in.size()-1);
    for (int i=0; i<diff.size(); i++) {
        diff[i] = in[i+1] - in[i];
    }
    return diff;
}

unsigned long long int util::n_tuples_or_triples(int n) {
    // number of ways to combine two or three subsequent elements
    // in a n long sequence, eg (..)...(..)(...)
    if (n <= 1)
        return 1;
    if (n == 2)
        return 2;
    if (n == 3)
        return 4;
    return util::n_tuples_or_triples(n-1)
         + util::n_tuples_or_triples(n-2)
         + util::n_tuples_or_triples(n-3);
}


/* -------- BOOTLOADER ------ */

Command str_to_command(std::string input) {
    std::vector<util::String> command = util::String(input).splitAt(" ");
    Instruction instruction;
    if      (command[0].equals("nop")) instruction = Instruction::NOP;
    else if (command[0].equals("acc")) instruction = Instruction::ACC;
    else if (command[0].equals("jmp")) instruction = Instruction::JMP;
    else instruction = Instruction::NOP;
    int value = command[1].asint();
    return Command {instruction, value};
}

BootLoader::BootLoader(std::vector<Command> c) {
    code = c;
    reset();
}

void BootLoader::reset() {
    pos = 0;
    acc = 0;
    history.clear();
}

void BootLoader::set_code(std::vector<Command> c) {
    code = c;
}

int BootLoader::get_result() {
    return acc;
}

BootStatus BootLoader::run() {
    while (pos < code.size()) {
        // if we were already at pos, we are in an infinite loop and need to stop
        if (std::find(history.begin(), history.end(), pos) != history.end())
            return BootStatus::INFLOOP;

        history.push_back(pos);
        switch (code[pos].instruction) {
            case Instruction::NOP:
                if (log)
                    std::cout << "line " << pos << ": nop " << code[pos].value << ", acc = " << acc << std::endl;
                pos++;
                break;
            case Instruction::ACC:
                if (log)
                    std::cout << "line " << pos << ": acc " << code[pos].value << ", acc = " << acc << std::endl;
                acc += code[pos].value;
                pos++;
                break;
            case Instruction::JMP:
                if (log)
                    std::cout << "line " << pos << ": jmp " << code[pos].value << ", acc = " << acc << std::endl;
                pos += code[pos].value;
                break;
        }
    }
    return BootStatus::DONE;
}

void BootLoader::set_command(int pos, Command c) {
    code[pos] = c;
}

void BootLoader::set_log(bool l) {
    log = l;
}
