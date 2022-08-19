#include <Rcpp.h>
#include <unordered_map>
#define ChrVec CharacterVector
using namespace Rcpp;

// [[Rcpp::export]]
ChrVec rcpp_mapvalues(ChrVec input, ChrVec from, ChrVec to) {
  // Initialize unordered mapping class
  std::unordered_map<std::string, std::string> mapping;
  
  // Populate the mapping between 'from' and 'to' vectors
  // Length check of from and to in R wrapper function
  // Converts 'from' and 'to' values to std::string for map insertion
  for (int i = 0; i < from.length(); ++i) {
    mapping[as<std::string>(from[i])] = as<std::string>(to[i]);
  }
  
  // Create a clone of input vector input
  ChrVec output = clone(input);
  
  for (int i = 0; i < output.length(); ++i) {
    // Iterate through each element in vector output
    // If element is found in mapping keys, replace element
    // Value not found gives mapping.end()
    // it = std::pair (key, value), retrieve value w/ -> second
    auto it = mapping.find(as<std::string>(output[i]));
    if (it != mapping.end()) {
      output[i] = it->second;
    }
  }
  return output;
}
