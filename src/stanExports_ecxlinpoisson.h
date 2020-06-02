// Generated by rstantools.  Do not edit by hand.

/*
    Package licensed under the "MIT License (MIT)"
    
    Permission is hereby granted, free of charge, to any person obtaining a copy of
    bayesnec and associated documentation files (the "Software"), to deal in
    the Software without restriction, including without limitation the rights to
    use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
    the Software, and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions:
    
    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
    FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
    COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
    IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
    CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.19.1
#include <stan/model/model_header.hpp>
namespace model_ecxlinpoisson_namespace {
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
static int current_statement_begin__;
stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_ecxlinpoisson");
    reader.add_event(45, 43, "end", "model_ecxlinpoisson");
    return reader;
}
#include <stan_meta_header.hpp>
class model_ecxlinpoisson : public prob_grad {
private:
        int N;
        std::vector<int> Y;
        int K_top;
        matrix_d X_top;
        int K_beta;
        matrix_d X_beta;
        vector_d C_1;
        int prior_only;
public:
    model_ecxlinpoisson(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_ecxlinpoisson(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, random_seed__, pstream__);
    }
    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning
        current_statement_begin__ = -1;
        static const char* function__ = "model_ecxlinpoisson_namespace::model_ecxlinpoisson";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 5;
            context__.validate_dims("data initialization", "N", "int", context__.to_vec());
            N = int(0);
            vals_i__ = context__.vals_i("N");
            pos__ = 0;
            N = vals_i__[pos__++];
            check_greater_or_equal(function__, "N", N, 1);
            current_statement_begin__ = 6;
            validate_non_negative_index("Y", "N", N);
            context__.validate_dims("data initialization", "Y", "int", context__.to_vec(N));
            Y = std::vector<int>(N, int(0));
            vals_i__ = context__.vals_i("Y");
            pos__ = 0;
            size_t Y_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < Y_k_0_max__; ++k_0__) {
                Y[k_0__] = vals_i__[pos__++];
            }
            current_statement_begin__ = 7;
            context__.validate_dims("data initialization", "K_top", "int", context__.to_vec());
            K_top = int(0);
            vals_i__ = context__.vals_i("K_top");
            pos__ = 0;
            K_top = vals_i__[pos__++];
            check_greater_or_equal(function__, "K_top", K_top, 1);
            current_statement_begin__ = 8;
            validate_non_negative_index("X_top", "N", N);
            validate_non_negative_index("X_top", "K_top", K_top);
            context__.validate_dims("data initialization", "X_top", "matrix_d", context__.to_vec(N,K_top));
            X_top = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(N, K_top);
            vals_r__ = context__.vals_r("X_top");
            pos__ = 0;
            size_t X_top_j_2_max__ = K_top;
            size_t X_top_j_1_max__ = N;
            for (size_t j_2__ = 0; j_2__ < X_top_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < X_top_j_1_max__; ++j_1__) {
                    X_top(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 9;
            context__.validate_dims("data initialization", "K_beta", "int", context__.to_vec());
            K_beta = int(0);
            vals_i__ = context__.vals_i("K_beta");
            pos__ = 0;
            K_beta = vals_i__[pos__++];
            check_greater_or_equal(function__, "K_beta", K_beta, 1);
            current_statement_begin__ = 10;
            validate_non_negative_index("X_beta", "N", N);
            validate_non_negative_index("X_beta", "K_beta", K_beta);
            context__.validate_dims("data initialization", "X_beta", "matrix_d", context__.to_vec(N,K_beta));
            X_beta = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(N, K_beta);
            vals_r__ = context__.vals_r("X_beta");
            pos__ = 0;
            size_t X_beta_j_2_max__ = K_beta;
            size_t X_beta_j_1_max__ = N;
            for (size_t j_2__ = 0; j_2__ < X_beta_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < X_beta_j_1_max__; ++j_1__) {
                    X_beta(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 12;
            validate_non_negative_index("C_1", "N", N);
            context__.validate_dims("data initialization", "C_1", "vector_d", context__.to_vec(N));
            C_1 = Eigen::Matrix<double, Eigen::Dynamic, 1>(N);
            vals_r__ = context__.vals_r("C_1");
            pos__ = 0;
            size_t C_1_j_1_max__ = N;
            for (size_t j_1__ = 0; j_1__ < C_1_j_1_max__; ++j_1__) {
                C_1(j_1__) = vals_r__[pos__++];
            }
            current_statement_begin__ = 13;
            context__.validate_dims("data initialization", "prior_only", "int", context__.to_vec());
            prior_only = int(0);
            vals_i__ = context__.vals_i("prior_only");
            pos__ = 0;
            prior_only = vals_i__[pos__++];
            // initialize transformed data variables
            // execute transformed data statements
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 18;
            validate_non_negative_index("b_top", "K_top", K_top);
            num_params_r__ += K_top;
            current_statement_begin__ = 19;
            validate_non_negative_index("b_beta", "K_beta", K_beta);
            num_params_r__ += K_beta;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_ecxlinpoisson() { }
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;
        current_statement_begin__ = 18;
        if (!(context__.contains_r("b_top")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable b_top missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("b_top");
        pos__ = 0U;
        validate_non_negative_index("b_top", "K_top", K_top);
        context__.validate_dims("parameter initialization", "b_top", "vector_d", context__.to_vec(K_top));
        Eigen::Matrix<double, Eigen::Dynamic, 1> b_top(K_top);
        size_t b_top_j_1_max__ = K_top;
        for (size_t j_1__ = 0; j_1__ < b_top_j_1_max__; ++j_1__) {
            b_top(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(b_top);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable b_top: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 19;
        if (!(context__.contains_r("b_beta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable b_beta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("b_beta");
        pos__ = 0U;
        validate_non_negative_index("b_beta", "K_beta", K_beta);
        context__.validate_dims("parameter initialization", "b_beta", "vector_d", context__.to_vec(K_beta));
        Eigen::Matrix<double, Eigen::Dynamic, 1> b_beta(K_beta);
        size_t b_beta_j_1_max__ = K_beta;
        for (size_t j_1__ = 0; j_1__ < b_beta_j_1_max__; ++j_1__) {
            b_beta(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(b_beta);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable b_beta: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }
    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {
        typedef T__ local_scalar_t__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning
        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
            // model parameters
            current_statement_begin__ = 18;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> b_top;
            (void) b_top;  // dummy to suppress unused var warning
            if (jacobian__)
                b_top = in__.vector_constrain(K_top, lp__);
            else
                b_top = in__.vector_constrain(K_top);
            current_statement_begin__ = 19;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> b_beta;
            (void) b_beta;  // dummy to suppress unused var warning
            if (jacobian__)
                b_beta = in__.vector_constrain(K_beta, lp__);
            else
                b_beta = in__.vector_constrain(K_beta);
            // model body
            {
            current_statement_begin__ = 25;
            validate_non_negative_index("nlp_top", "N", N);
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> nlp_top(N);
            stan::math::initialize(nlp_top, DUMMY_VAR__);
            stan::math::fill(nlp_top, DUMMY_VAR__);
            stan::math::assign(nlp_top,multiply(X_top, b_top));
            current_statement_begin__ = 27;
            validate_non_negative_index("nlp_beta", "N", N);
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> nlp_beta(N);
            stan::math::initialize(nlp_beta, DUMMY_VAR__);
            stan::math::fill(nlp_beta, DUMMY_VAR__);
            stan::math::assign(nlp_beta,multiply(X_beta, b_beta));
            current_statement_begin__ = 29;
            validate_non_negative_index("mu", "N", N);
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> mu(N);
            stan::math::initialize(mu, DUMMY_VAR__);
            stan::math::fill(mu, DUMMY_VAR__);
            current_statement_begin__ = 30;
            for (int n = 1; n <= N; ++n) {
                current_statement_begin__ = 32;
                stan::model::assign(mu, 
                            stan::model::cons_list(stan::model::index_uni(n), stan::model::nil_index_list()), 
                            (get_base1(nlp_top, n, "nlp_top", 1) - (get_base1(nlp_beta, n, "nlp_beta", 1) * get_base1(C_1, n, "C_1", 1))), 
                            "assigning variable mu");
            }
            current_statement_begin__ = 35;
            lp_accum__.add(normal_log(b_top, 2, 100));
            current_statement_begin__ = 36;
            lp_accum__.add(gamma_log(b_beta, 0.0001, 0.0001));
            current_statement_begin__ = 38;
            if (as_bool(logical_negation(prior_only))) {
                current_statement_begin__ = 39;
                lp_accum__.add(poisson_log_log(Y, mu));
            }
            }
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
        lp_accum__.add(lp__);
        return lp_accum__.sum();
    } // log_prob()
    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }
    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("b_top");
        names__.push_back("b_beta");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back(K_top);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(K_beta);
        dimss__.push_back(dims__);
    }
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;
        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_ecxlinpoisson_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        Eigen::Matrix<double, Eigen::Dynamic, 1> b_top = in__.vector_constrain(K_top);
        size_t b_top_j_1_max__ = K_top;
        for (size_t j_1__ = 0; j_1__ < b_top_j_1_max__; ++j_1__) {
            vars__.push_back(b_top(j_1__));
        }
        Eigen::Matrix<double, Eigen::Dynamic, 1> b_beta = in__.vector_constrain(K_beta);
        size_t b_beta_j_1_max__ = K_beta;
        for (size_t j_1__ = 0; j_1__ < b_beta_j_1_max__; ++j_1__) {
            vars__.push_back(b_beta(j_1__));
        }
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            if (!include_gqs__ && !include_tparams__) return;
            if (!include_gqs__) return;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }
    static std::string model_name() {
        return "model_ecxlinpoisson";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t b_top_j_1_max__ = K_top;
        for (size_t j_1__ = 0; j_1__ < b_top_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "b_top" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t b_beta_j_1_max__ = K_beta;
        for (size_t j_1__ = 0; j_1__ < b_beta_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "b_beta" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t b_top_j_1_max__ = K_top;
        for (size_t j_1__ = 0; j_1__ < b_top_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "b_top" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        size_t b_beta_j_1_max__ = K_beta;
        for (size_t j_1__ = 0; j_1__ < b_beta_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "b_beta" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
    }
}; // model
}  // namespace
typedef model_ecxlinpoisson_namespace::model_ecxlinpoisson stan_model;
#endif
