#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include <yacas/yacas.h>

#include <sstream>

namespace {
    static std::stringstream _side_effects;
    static CYacas* _yacas = nullptr;

    static
    void yacas_initialize(std::string alternative_path, bool ryacas_init = true)
    {
        _yacas = new CYacas(_side_effects);        
        
        Rcpp::Environment base_env = Rcpp::Environment::base_env();
        Rcpp::Function system_file = base_env["system.file"];
        
        std::string scripts_path = Rcpp::as<std::string>(system_file(Rcpp::Named("package", "Ryacas"), "yacas"));
        
        // During development, the path can be overwritten
        if (!alternative_path.empty()) {
          scripts_path = alternative_path;
          Rcpp::Rcout << " - Searching for yacas at \"" << scripts_path << "\"" << std::endl;
        }

        if (!scripts_path.empty()) {
            if (scripts_path.back() != '/') {
                scripts_path.push_back('/');
            }
            _yacas->Evaluate(std::string("DefaultDirectory(\"") +  scripts_path + "\");");
        }

        if (!_yacas->IsError()) {
            _yacas->Evaluate("Load(\"yacasinit.ys\");");
        }

        if (!_yacas->IsError()) {
          _yacas->Evaluate("PrettyPrinter'Set();");
        }

        if (!_yacas->IsError() && ryacas_init) {
          _yacas->Evaluate("Load(\"../yacas-custom/ryacasinit.ys\");");
        }

        if (_yacas->IsError()) {
            const std::string msg = "Failed to initialize yacas: " + _yacas->Error();
            
            _yacas = nullptr;
            Rcpp::stop(msg);
        }        
    }
}

// [[Rcpp::export]]
void yacas_init_force_path(std::string path, bool ryacas_init = true)
{
  Rcpp::Rcout << "Trying to initialise internal yacas (with" << (ryacas_init ? "" : "out") << " Ryacas init): " << std::endl;
  yacas_initialize(path, ryacas_init);
  Rcpp::Rcout << "Done." << std::endl;
}

// [[Rcpp::export]]
void yacas_init_force(bool ryacas_init = true)
{
  yacas_init_force_path(std::string(), ryacas_init);
}

// Evaluate yacas expression
// 
// This is a low-level function for evaluating yacas expression represented as
// string.
// 
// @param expr Yacas expression
// @return Result of evaluating \code{expr} by yacas and
// side-effects of the evaluation
// 
// @examples
// yac_core("D(x)Sin(x^2)")
// 
// @concept yac_communication
// @keywords internal
// 
// [[Rcpp::export]]
std::vector<std::string> yac_core(std::string expr)
{
  if (!_yacas) {
    yacas_initialize(std::string(), true);
  }
  
  _side_effects.clear();
  _side_effects.str("");
  
  _yacas->Evaluate(expr);
  
  if (_yacas->IsError()) {
    std::string err = _yacas->Error();
    std::string msg = "Yacas returned this error: " + err;
    
    Rcpp::stop(msg);
  }
  
  const std::vector<std::string> results = {
    _side_effects.str(),
    _yacas->Result()
  };
  
  return results;
}


/*
#include "yacas/include/yacas/string_utils.h"

// [[Rcpp::export]]
void trims(std::string s) {
  // sapply(c("s1", " s2", "s3 "), trims)
  // For testing src/yacas/include/yacas/string_utils.h 
  Rcpp::Rcout << "ltrim     = '" << ltrim(s) << "'" << std::endl;
  Rcpp::Rcout << "ltrim_old = '" << ltrim_old(s) << "'" << std::endl;
  Rcpp::Rcout << "rtrim     = '" << rtrim(s) << "'" << std::endl;
  Rcpp::Rcout << "rtrim_old = '" << rtrim_old(s) << "'" << std::endl;
}
*/

