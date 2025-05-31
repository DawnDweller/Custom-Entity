CLASS zfi007_cl_root_mus_sat DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.


  PROTECTED   SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zfi007_cl_root_mus_sat IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

    "-------------------

    DATA lt_output TYPE STANDARD TABLE OF zfi007_r_mus_sat_mutabakat.
    DATA  lt_output_group TYPE TABLE OF zfi007_r_mus_sat_mutabakat.

    DATA: lr_customer             TYPE RANGE OF zfi007_r_mus_sat_mutabakat-customer,
          lr_supplier             TYPE RANGE OF zfi007_r_mus_sat_mutabakat-supplier,
          lr_customer_name        TYPE RANGE OF zfi007_r_mus_sat_mutabakat-customer_name,
          lr_supplier_name        TYPE RANGE OF zfi007_r_mus_sat_mutabakat-supplier_name,
          lr_debit_amount         TYPE RANGE OF zfi007_r_mus_sat_mutabakat-debit_amount,
          lr_credit_amount        TYPE RANGE OF zfi007_r_mus_sat_mutabakat-credit_amount,
          lr_currency             TYPE RANGE OF zfi007_r_mus_sat_mutabakat-currency,
          lr_tax_number1          TYPE RANGE OF zfi007_r_mus_sat_mutabakat-tax_number1,
          lr_tax_number2          TYPE RANGE OF zfi007_r_mus_sat_mutabakat-tax_number2,
          lr_telephone_number1    TYPE RANGE OF zfi007_r_mus_sat_mutabakat-telephone_number1,
          lr_fax_number           TYPE RANGE OF zfi007_r_mus_sat_mutabakat-fax_number,
          lr_distribution_channel TYPE RANGE OF zfi007_r_mus_sat_mutabakat-distribution_channel,
          lr_material             TYPE RANGE OF zfi007_r_mus_sat_mutabakat-material,
          lr_report_type          TYPE RANGE OF zfi007_e_report_type,
          lr_company_code         TYPE RANGE OF zfi007_r_mus_sat_mutabakat-company_Code,
          lr_posting_date         TYPE RANGE OF zfi007_r_mus_sat_mutabakat-posting_date,
          lr_ledger               TYPE RANGE OF zfi007_r_mus_sat_mutabakat-ledger.


    DATA(lo_paging) = io_request->get_paging( ).
    DATA(lv_top) = lo_paging->get_page_size( ).
    DATA(lv_skip) = lo_paging->get_offset( ).
    DATA(lv_where_clause) =  io_request->get_filter( )->get_as_sql_string( ).
    DATA(lt_sort) =  io_request->get_sort_elements( ).

    TRY.
        DATA(lt_filter) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range.
        "handle exception
    ENDTRY.

    IF line_exists( lt_filter[ name = 'CUSTOMER' ] ).
      lr_customer = CORRESPONDING #( lt_filter[ name = 'CUSTOMER' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'SUPPLIER' ] ).
      lr_supplier = CORRESPONDING #( lt_filter[ name = 'SUPPLIER' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'CUSTOMER_NAME' ] ).
      lr_customer_name = CORRESPONDING #( lt_filter[ name = 'CUSTOMER_NAME' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'SUPPLIER_NAME' ] ).
      lr_supplier_name = CORRESPONDING #( lt_filter[ name = 'SUPPLIER_NAME' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'DEBIT_AMOUNT' ] ).
      lr_debit_amount = CORRESPONDING #( lt_filter[ name = 'DEBIT_AMOUNT' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'CREDIT_AMOUNT' ] ).
      lr_credit_amount = CORRESPONDING #( lt_filter[ name = 'CREDIT_AMOUNT' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'CURRENCY' ] ).
      lr_currency = CORRESPONDING #( lt_filter[ name = 'CURRENCY' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'TAX_NUMBER1' ] ).
      lr_tax_number1 = CORRESPONDING #( lt_filter[ name = 'TAX_NUMBER1' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'TAX_NUMBER2' ] ).
      lr_tax_number2 = CORRESPONDING #( lt_filter[ name = 'TAX_NUMBER2' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'TELEPHONE_NUMBER1' ] ).
      lr_telephone_number1 = CORRESPONDING #( lt_filter[ name = 'TELEPHONE_NUMBER1' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'FAX_NUMBER' ] ).
      lr_fax_number = CORRESPONDING #( lt_filter[ name = 'FAX_NUMBER' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'DISTRIBUTION_CHANNEL' ] ).
      lr_distribution_channel = CORRESPONDING #( lt_filter[ name = 'DISTRIBUTION_CHANNEL' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'MATERIAL' ] ).
      lr_material = CORRESPONDING #( lt_filter[ name = 'MATERIAL' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'REPORT_TYPE' ] ).
      lr_report_type = CORRESPONDING #( lt_filter[ name = 'REPORT_TYPE' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'COMPANY_CODE' ] ).
      lr_company_code = CORRESPONDING #( lt_filter[ name = 'COMPANY_CODE' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'POSTING_DATE' ] ).
      lr_posting_date = CORRESPONDING #( lt_filter[ name = 'POSTING_DATE' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'LEDGER' ] ).
      lr_ledger = CORRESPONDING #( lt_filter[ name = 'LEDGER' ]-range ).
    ENDIF.

    IF lr_report_type[ 1 ]-low = 'M'.


      SELECT COUNT(*)
          FROM i_journalentryitem
         INNER JOIN i_customer ON i_journalentryitem~customer = i_customer~customer
         LEFT JOIN I_BusPartEmailAddressTP_3 ON I_journalentryitem~Customer = I_BusPartEmailAddressTP_3~BusinessPartner
         WHERE i_journalentryitem~customer IN @lr_customer
           AND i_customer~CustomerName IN @lr_customer_name
           AND i_journalentryitem~AmountInTransactionCurrency IN @lr_debit_amount
           AND i_journalentryitem~TransactionCurrency IN @lr_currency
           AND i_customer~TaxNumber1 IN @lr_tax_number1
           AND i_customer~TaxNumber2 IN @lr_tax_number1
           AND i_customer~TelephoneNumber1 IN @lr_telephone_number1
           AND i_customer~FaxNumber IN @lr_fax_number
           AND i_journalentryitem~DistributionChannel IN @lr_distribution_channel
           AND i_journalentryitem~CompanyCode IN @lr_company_code
           AND i_journalentryitem~PostingDate IN @lr_posting_date
           AND i_journalentryitem~Ledger IN @lr_ledger
           AND i_journalentryitem~ClearingJournalEntry = ''

           INTO @DATA(lv_count).


      SELECT i_journalentryitem~customer AS customer,
             i_journalentryitem~companyCode AS company_Code,
             i_customer~CustomerName AS customer_name,
             i_journalentryitem~postingDate AS posting_Date,
             i_journalentryitem~AmountInTransactionCurrency AS debit_amount,
             i_journalentryitem~AmountInTransactionCurrency AS credit_amount,
             i_journalentryitem~TransactionCurrency AS currency,
             i_customer~TaxNumber1 AS Tax_Number1,
             i_customer~TaxNumber2 AS Tax_Number2,
             i_customer~TelephoneNumber1 AS telephone_number1,
             i_customer~FaxNumber AS Fax_Number,
             i_journalentryitem~DistributionChannel AS distribution_channel
      FROM i_journalentryitem
      INNER JOIN i_customer ON i_journalentryitem~customer = i_customer~customer
      LEFT JOIN I_BusPartEmailAddressTP_3 ON I_journalentryitem~Customer = I_BusPartEmailAddressTP_3~BusinessPartner
      WHERE i_journalentryitem~customer IN @lr_customer
        AND i_customer~CustomerName IN @lr_customer_name
        AND i_journalentryitem~AmountInTransactionCurrency IN @lr_debit_amount
        AND i_journalentryitem~TransactionCurrency IN @lr_currency
        AND i_customer~TaxNumber1 IN @lr_tax_number1
        AND i_customer~TaxNumber2 IN @lr_tax_number2
        AND i_customer~TelephoneNumber1 IN @lr_telephone_number1
        AND i_customer~FaxNumber IN @lr_fax_number
        AND i_journalentryitem~DistributionChannel IN @lr_distribution_channel
        AND i_journalentryitem~CompanyCode IN @lr_company_code
        AND i_journalentryitem~PostingDate IN @lr_posting_date
        AND i_journalentryitem~Ledger IN @lr_ledger
        AND i_journalentryitem~ClearingJournalEntry = ''
      ORDER BY i_journalentryitem~customer
      INTO CORRESPONDING FIELDS OF TABLE @lt_output.
*    UP TO @lv_top ROWS
*    OFFSET @lv_skip.


      LOOP AT lt_output INTO DATA(ls_output)
        GROUP BY ( customer = ls_output-customer )
        REFERENCE INTO DATA(lr_output_group).

        APPEND INITIAL LINE TO lt_output_group ASSIGNING FIELD-SYMBOL(<fs_output_group>).
        <fs_output_group>-customer = lr_output_group->customer.


        LOOP AT GROUP lr_output_group INTO DATA(ls_output_group_s).
          <fs_output_group>-boolean_m = abap_false.
          <fs_output_group>-boolean_s = abap_true. "Hide Supplier fields
          <fs_output_group>-customer_name = ls_output_group_s-customer_name.
          <fs_output_group>-currency = ls_output_group_s-currency.
          <fs_output_group>-tax_number1 = ls_output_group_s-tax_number1.
          <fs_output_group>-tax_number2 = ls_output_group_s-tax_number2.
          <fs_output_group>-telephone_number1 = ls_output_group_s-telephone_number1.
          <fs_output_group>-fax_number = ls_output_group_s-fax_number.
          <fs_output_group>-distribution_channel = ls_output_group_s-distribution_channel.
          <fs_output_group>-company_Code = ls_output_group_s-company_Code.
          <fs_output_group>-posting_Date = ls_output_group_s-posting_Date.
          IF ls_output_group_s-debit_amount > 0.
            <fs_output_group>-debit_amount += ls_output_group_s-debit_amount.
          ELSEIF ls_output_group_s-credit_amount <= 0.
            <fs_output_group>-credit_amount += ls_output_group_s-credit_amount.
          ENDIF.
        ENDLOOP.
        DATA(lv_total) = <fs_output_group>-debit_amount + <fs_output_group>-credit_amount.
        IF lv_total > 0.
          <fs_output_group>-debit_amount = lv_total.
          <fs_output_group>-credit_amount = 0.
        ELSEIF lv_total < 0.
          <fs_output_group>-credit_amount = lv_total.
          <fs_output_group>-debit_amount = 0.
        ELSE.
          <fs_output_group>-credit_amount = 0.
          <fs_output_group>-debit_amount = 0.
        ENDIF.
      ENDLOOP.
      lt_output = lt_output_group.


    ELSEIF lr_report_type[ 1 ]-low = 'S'.

      SELECT COUNT(*)
           FROM i_journalentryitem
          INNER JOIN i_supplier ON i_journalentryitem~supplier = i_supplier~supplier
          LEFT JOIN I_BusPartEmailAddressTP_3 ON I_journalentryitem~Supplier = I_BusPartEmailAddressTP_3~BusinessPartner
          WHERE  i_journalentryitem~supplier IN @lr_supplier
            AND i_supplier~SupplierName IN @lr_supplier_name
            AND i_journalentryitem~AmountInTransactionCurrency IN @lr_debit_amount
            AND i_journalentryitem~TransactionCurrency IN @lr_currency
            AND i_supplier~TaxNumber1 IN @lr_tax_number1
            AND i_supplier~TaxNumber2 IN @lr_tax_number1
            AND i_supplier~PhoneNumber1 IN @lr_telephone_number1
            AND i_supplier~FaxNumber IN @lr_fax_number
            AND i_journalentryitem~Product IN @lr_material
            AND i_journalentryitem~CompanyCode IN @lr_company_code
            AND i_journalentryitem~PostingDate IN @lr_posting_date
            AND i_journalentryitem~Ledger IN @lr_ledger
            AND i_journalentryitem~ClearingJournalEntry = ''
            INTO @lv_count.


      SELECT
           i_journalentryitem~supplier,
           i_journalentryitem~companyCode AS company_Code,
           i_journalentryitem~postingDate AS posting_Date,
           i_supplier~supplierName AS supplier_name,
             i_journalentryitem~AmountInTransactionCurrency AS debit_amount,
             i_journalentryitem~AmountInTransactionCurrency AS credit_amount,
             i_journalentryitem~TransactionCurrency AS currency,
             i_supplier~TaxNumber1 AS Tax_Number1,
             i_supplier~TaxNumber2 AS Tax_Number2,
             i_supplier~PhoneNumber1 AS telephone_number1,
             i_supplier~FaxNumber AS Fax_Number,
           i_journalentryitem~Product AS material
      FROM i_journalentryitem
      INNER JOIN i_supplier ON i_journalentryitem~supplier = i_supplier~supplier
      LEFT JOIN I_BusPartEmailAddressTP_3 ON I_journalentryitem~Supplier = I_BusPartEmailAddressTP_3~BusinessPartner
      WHERE i_journalentryitem~supplier IN @lr_supplier
        AND i_supplier~SupplierName IN @lr_supplier_name
        AND i_journalentryitem~AmountInTransactionCurrency IN @lr_debit_amount
        AND i_journalentryitem~TransactionCurrency IN @lr_currency
        AND i_supplier~TaxNumber1 IN @lr_tax_number1
        AND i_supplier~TaxNumber2 IN @lr_tax_number2
        AND i_supplier~PhoneNumber1 IN @lr_telephone_number1
        AND i_supplier~FaxNumber IN @lr_fax_number
        AND i_journalentryitem~Product IN @lr_material
        AND i_journalentryitem~CompanyCode IN @lr_company_code
        AND i_journalentryitem~PostingDate IN @lr_posting_date
        AND i_journalentryitem~Ledger IN @lr_ledger
        AND i_journalentryitem~ClearingJournalEntry = ''
      ORDER BY i_journalentryitem~supplier
      INTO CORRESPONDING FIELDS OF TABLE @lt_output.
*    UP TO @lv_top ROWS
*    OFFSET @lv_skip.


      LOOP AT lt_output INTO DATA(ls_output2)
        GROUP BY ( supplier = ls_output2-supplier )
        REFERENCE INTO DATA(lr_output_group2).

        APPEND INITIAL LINE TO lt_output_group ASSIGNING FIELD-SYMBOL(<fs_output_group2>).
        <fs_output_group2>-supplier = lr_output_group2->supplier.


        LOOP AT GROUP lr_output_group2 INTO DATA(ls_output_group_s2).
          <fs_output_group2>-boolean_s = abap_false.
          <fs_output_group2>-boolean_m = abap_true. "Hide Customer fields.
          <fs_output_group2>-supplier_name = ls_output_group_s2-supplier_name.
          <fs_output_group2>-currency = ls_output_group_s2-currency.
          <fs_output_group2>-tax_number1 = ls_output_group_s2-tax_number1.
          <fs_output_group2>-tax_number2 = ls_output_group_s2-tax_number2.
          <fs_output_group2>-telephone_number1 = ls_output_group_s2-telephone_number1.
          <fs_output_group2>-fax_number = ls_output_group_s2-fax_number.
          <fs_output_group2>-material = ls_output_group_s2-material.
          <fs_output_group2>-company_Code = ls_output_group_s2-company_Code.
          <fs_output_group2>-posting_Date = ls_output_group_s2-posting_Date.
          IF ls_output_group_s2-debit_amount > 0.
            <fs_output_group2>-debit_amount += ls_output_group_s2-debit_amount.
          ELSEIF ls_output_group_s2-credit_amount <= 0.
            <fs_output_group2>-credit_amount += ls_output_group_s2-credit_amount.
          ENDIF.
        ENDLOOP.
        lv_total = <fs_output_group2>-debit_amount + <fs_output_group2>-credit_amount.
        IF lv_total > 0.
          <fs_output_group2>-debit_amount = lv_total.
          <fs_output_group2>-credit_amount = 0.
        ELSEIF lv_total < 0.
          <fs_output_group2>-credit_amount = lv_total.
          <fs_output_group2>-debit_amount = 0.
        ELSE.
          <fs_output_group2>-credit_amount = 0.
          <fs_output_group2>-debit_amount = 0.
        ENDIF.
      ENDLOOP.

      lt_output = lt_output_group.

    ENDIF.



*    IF sy-subrc IS INITIAL. "Removed this line because if no values returned, I still have to display it. lv_count should be printed.

      IF io_request->is_total_numb_of_rec_requested(  ).
        io_response->set_total_number_of_records( lv_count ).
      ENDIF.

      io_response->set_data( lt_output ).

*    ENDIF.


  ENDMETHOD.

ENDCLASS.
