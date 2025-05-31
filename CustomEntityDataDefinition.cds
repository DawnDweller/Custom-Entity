@ObjectModel.query.implementedBy:'ABAP:ZFI007_CL_ROOT_MUS_SAT'
@EndUserText.label: 'FI008 - Mus_sat_mutabakat Data Def'
@Search.searchable: true
@Metadata.allowExtensions: true

define root custom entity ZFI007_R_MUS_SAT_MUTABAKAT
{
      @Search.defaultSearchElement: true
  key customer             : kunnr;
  key supplier             : lifnr;
      customer_name        : name1_gp;
      supplier_name        : name1_gp;
      @Semantics.amount.currencyCode: 'Currency'
      debit_amount         : abap.curr(13,2);
      @Semantics.amount.currencyCode: 'Currency'
      credit_amount        : abap.curr(13,2);
      currency             : waers;
      tax_number1          : stcd1;
      tax_number2          : stcd2;
      telephone_number1    : telf1;
      fax_number           : telfx;
      distribution_channel : vtweg;
      material             : matnr;
      report_type          : zfi007_e_report_type;
      boolean_m            : abap_boolean;
      boolean_s            : abap_boolean;
      company_Code         : bukrs;
      posting_Date         : budat;
      ledger               : fins_ledger;
       eposta        : zfi006_e_email;
}
