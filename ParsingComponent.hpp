#ifndef PARSINGCOMPONENT
    #define PARSINGCOMPONENT
    #include <iostream>
    #include <string>
    #include <fstream>
    #include <sstream>
    #include "Circuit.hpp"
    #include "Factory.hpp"

class ParsingComponent {
    public:
    virtual ~ParsingComponent() {}
    int readfile (const std::string &filename);
    int elemExtractChipset(Circuit &Circuit);
    int elemExtractLinks(Circuit &Circuit);
    std::string trim(const std::string &str) {
    size_t first = str.find_first_not_of(" \t");
    if (first == std::string::npos) return "";
    size_t last = str.find_last_not_of(" \t");
    return str.substr(first, (last - first + 1));
}


    std::stringstream ChipsetSection;
    std::stringstream LinksSection;
};
#endif