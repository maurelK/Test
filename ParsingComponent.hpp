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
 


    std::stringstream ChipsetSection;
    std::stringstream LinksSection;
};
#endif