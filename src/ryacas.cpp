#include <Rcpp.h>

#include <yacas/yacas.h>

#include <sstream>

namespace {
    static std::stringstream _side_effects;
    static CYacas* _yacas = nullptr;

    static
    void yacas_initialize()
    {
        _yacas = new CYacas(_side_effects);        
        
        /* This works for installed packages, but not during development with
         *   devtools::load_all()
         */
        //Rcpp::Environment base_env = Rcpp::Environment::base_env();
        //Rcpp::Function system_file = base_env["system.file"];
        /* hence this instead where a global variable called
         *   ryacas_devel_use_devtools
         * must be existing
         */
        Rcpp::Environment env = Rcpp::Environment::base_env();
        Rcpp::Function system_file = env["system.file"];
        
        try {
          Rcpp::Environment global_env = Rcpp::Environment::global_env();
          
          if (global_env.get("ryacas_devel_use_devtools") != R_NilValue) {
            env = Rcpp::Environment::namespace_env("devtools");
            system_file = env["shim_system.file"];
          }
        } catch (...) {  }
        /* <--> */
        
        std::string scripts_path = Rcpp::as<std::string>(system_file(Rcpp::Named("package", "Ryacas"), "yacas"));

        if (!scripts_path.empty()) {
            if (scripts_path.back() != '/')
                scripts_path.push_back('/');
            _yacas->Evaluate(std::string("DefaultDirectory(\"") +  scripts_path + "\");");
        }

        if (!_yacas->IsError())
            _yacas->Evaluate("Load(\"yacasinit.ys\");");

        if (!_yacas->IsError())
            _yacas->Evaluate("PrettyPrinter'Set(\"OMForm\");");

        if (_yacas->IsError()) {
            const std::string msg = "Failed to initialize yacas: " + _yacas->Error() + 
              "\n" +
              "If you are using devtools::load_all(), be sure that " + 
              "a variable called \"ryacas_devel_use_devtools\" exists.";
            
            _yacas = nullptr;
            Rcpp::stop(msg);
        }
    }
}

// [[Rcpp::export(name = ".yacas_init_force")]]
void yacas_init_force()
{
  yacas_initialize();
}

// [[Rcpp::export]]
std::vector<std::string> yacas_evaluate(std::string expr)
{
    if (!_yacas)
        yacas_initialize();

    _side_effects.clear();
    _side_effects.str("");
    
    _yacas->Evaluate(expr);

    const std::vector<std::string> results = {
        _side_effects.str(),
        _yacas->Result()
    };
    
    return results;
}
