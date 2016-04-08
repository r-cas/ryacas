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

        Rcpp::Environment base_env = Rcpp::Environment::base_env();
        Rcpp::Function system_file = base_env["system.file"];
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
            const std::string msg = "Failed to initialize yacas: " + _yacas->Error();
            _yacas = nullptr;
            Rcpp::stop(msg);
        }
    }
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
