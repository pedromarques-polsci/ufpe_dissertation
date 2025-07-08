* ssc install xtabond2

capture cd "C:\Users\Pedro\Documents\ws_dissertation"
use final_data\gmm_data.dta

egen countrynum = group(iso3c)

xtset countrynum year, yearly

* Without instruments
xtabond2 log_cg_pcp_sexp log_real_cmd_exports_pcp l1.log_cg_pcp_sexp maj kof_trade_df ///
	dp_ratio_old v2pariglef_ord def_prop_gdp, ///
	iv(log_cg_pcp_sexp log_real_cmd_exports_pcp l1.log_cg_pcp_sexp maj kof_trade_df ///
	dp_ratio_old v2pariglef_ord def_prop_gdp) ///
	robust twostep

* With instruments
xtabond2 log_cg_pcp_sexp log_real_cmd_exports_pcp l1.log_cg_pcp_sexp /// 
maj kof_trade_df dp_ratio_old v2pariglef_ord def_prop_gdp, ///
	gmm(log_real_cmd_exports_pcp def_prop_gdp, laglimits(3 5) collapse) ///
	iv(l1.log_cg_pcp_sexp maj kof_trade_df dp_ratio_old v2pariglef_ord) ///
	robust twostep
	
* INSTRUCTIONS
* collapse = No instrument for every time period (Small Sample)
* laglimits = Restricted lags (Small Sample)
* gmm = difference and lagged instruments are created
* iv = exogenous variables
* nolevel = difference GMM

* Leftover
ivregress gmm log_cg_pcp_sexp maj kof_trade_df ///
dp_ratio_old v2pariglef_ord def_prop_gdp i.countrynum ///
(log_real_cmd_exports_pcp = L3.log_real_cmd_exports_pcp)

xtabond2 log_cg_pcp_sexp log_real_cmd_exports_pcp l1.log_cg_pcp_sexp maj kof_trade_df ///
	dp_ratio_old v2pariglef_ord def_prop_gdp, ///
	gmm(log_real_cmd_exports_pcp,  laglimits(3 5)  collapse) ///
	iv(def_prop_gdp maj kof_trade_df dp_ratio_old v2pariglef_ord) ///
	robust twostep nolevel

xtabond2 log_cg_pcp_sexp log_real_cmd_exports_pcp l1.log_cg_pcp_sexp maj kof_trade_df ///
	dp_ratio_old v2pariglef_ord def_prop_gdp, ///
	gmm(log_real_cmd_exports_pcp l1.log_cg_pcp_sexp maj kof_trade_df ///
	def_prop_gdp  v2pariglef_ord, laglimits(3 5) collapse) ///
	iv(l1.log_cg_pcp_sexp) ///
	robust twostep
