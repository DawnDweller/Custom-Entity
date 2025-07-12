CLASS zfi007_cl_root_mus_sat DEFINITION
  PUBLIC
  INHERITING FROM cx_rap_query_provider
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
    DATA lt_output_branch TYPE STANDARD TABLE OF zfi007_r_mus_sat_mutabakat.
    DATA lt_output_m TYPE STANDARD TABLE OF zfi007_r_mus_sat_mutabakat.
    DATA lt_output_s TYPE STANDARD TABLE OF zfi007_r_mus_sat_mutabakat.
    DATA lt_output_group TYPE TABLE OF zfi007_r_mus_sat_mutabakat.
    DATA lt_output_group_m TYPE TABLE OF zfi007_r_mus_sat_mutabakat.
    DATA lt_output_group_s TYPE TABLE OF zfi007_r_mus_sat_mutabakat.
    DATA lt_temp TYPE TABLE OF zfi007_r_mus_sat_mutabakat.
    DATA lv_total TYPE zfi007_r_mus_sat_mutabakat-credit_amount.
    DATA lv_total_m TYPE zfi007_r_mus_sat_mutabakat-credit_amount.
    DATA lv_total_s TYPE zfi007_r_mus_sat_mutabakat-credit_amount.

*    TYPES: BEGIN OF ty_output,
*             customer              TYPE zfi007_r_mus_sat_mutabakat-customer,
*             supplier              TYPE zfi007_r_mus_sat_mutabakat-supplier,
*             customer_name         TYPE zfi007_r_mus_sat_mutabakat-customer_name,
*             supplier_name         TYPE zfi007_r_mus_sat_mutabakat-supplier_name,
*             debit_amount          TYPE zfi007_r_mus_sat_mutabakat-debit_amount,
*             credit_amount         TYPE zfi007_r_mus_sat_mutabakat-credit_amount,
*             currency              TYPE zfi007_r_mus_sat_mutabakat-currency,
*             tax_number1           TYPE zfi007_r_mus_sat_mutabakat-tax_number1,
*             tax_number2           TYPE zfi007_r_mus_sat_mutabakat-tax_number2,
*             telephone_number1     TYPE zfi007_r_mus_sat_mutabakat-telephone_number1,
*             fax_number            TYPE zfi007_r_mus_sat_mutabakat-fax_number,
*             distribution_channel  TYPE zfi007_r_mus_sat_mutabakat-distribution_channel,
*             material              TYPE zfi007_r_mus_sat_mutabakat-material,
*             report_type           TYPE zfi007_e_report_type,
*             company_Code          TYPE zfi007_r_mus_sat_mutabakat-company_Code,
*             posting_Date          TYPE c length 23, "Posting Date
*             ledger                TYPE zfi007_r_mus_sat_mutabakat-ledger,
*             branch_account        TYPE zfi007_r_mus_sat_mutabakat-branch_account,
*             boolean_s            TYPE abap_bool, "Supplier
*             boolean_m            TYPE abap_bool, "Customer
*           END OF ty_output.

*    DATA lt_output TYPE STANDARD TABLE OF ty_output.
*    DATA  lt_output_group TYPE TABLE OF ty_output.

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
          lr_company_code         TYPE RANGE OF zfi007_r_mus_sat_mutabakat-company_code,
          lr_posting_date         TYPE RANGE OF zfi007_r_mus_sat_mutabakat-posting_date,
          lr_ledger               TYPE RANGE OF zfi007_r_mus_sat_mutabakat-ledger,
*          lr_customerHeadOffice   TYPE RANGE OF zfi007_r_mus_sat_mutabakat-customerHeadOffice,
*          lr_supplierHeadOffice   TYPE RANGE OF zfi007_r_mus_sat_mutabakat-supplierHeadOffice,
          lr_branchaccount        TYPE RANGE OF zfi007_r_mus_sat_mutabakat-branchaccount,
          lr_organizationbpname2  TYPE RANGE OF zfi007_r_mus_sat_mutabakat-organizationbpname2.


    DATA(lo_paging) = io_request->get_paging( ).
    DATA(lv_top) = lo_paging->get_page_size( ).
    IF lv_top < 0.
      lv_top = 1.
    ENDIF.
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

*    IF line_exists( lt_filter[ name = 'CUSTOMERHEADOFFICE' ] ).
*      lr_customerHeadOffice = CORRESPONDING #( lt_filter[ name = 'CUSTOMERHEADOFFICE' ]-range ).
*    ENDIF.
*
*     IF line_exists( lt_filter[ name = 'SUPPLIERHEADOFFICE' ] ).
*      lr_supplierHeadOffice = CORRESPONDING #( lt_filter[ name = 'SUPPLIERHEADOFFICE' ]-range ).
*    ENDIF.

    IF line_exists( lt_filter[ name = 'BRANCHACCOUNT' ] ).
      lr_branchaccount = CORRESPONDING #( lt_filter[ name = 'BRANCHACCOUNT' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'ORGANIZATIONBPNAME2' ] ).
      lr_organizationbpname2 = CORRESPONDING #( lt_filter[ name = 'ORGANIZATIONBPNAME2' ]-range ).
    ENDIF.

    IF lr_posting_date IS NOT INITIAL.
      DATA(posting_date_first) = lr_posting_date[ 1 ]-low.
      DATA(posting_date_last) = lr_posting_date[ 1 ]-high.
      DATA(final_posting_date) = |{ posting_date_first+6(2) }.{ posting_date_first+4(2) }.{ posting_date_first+0(4) } - { posting_date_last+6(2) }.{ posting_date_last+4(2) }.{ posting_date_last+0(4) }|.
    ENDIF.

    IF lr_report_type[ 1 ]-low IS INITIAL.
      lr_report_type[ 1 ]-low = 'M'. "Default value for report type
    ENDIF.


    CASE lines( lr_report_type ).
      WHEN 1.
        IF lr_report_type[ 1 ]-low = 'M'.


          SELECT i_journalentryitem~customer AS customer,
                 i_journalentryitem~companycode AS company_code,
                 i_customer~customername AS customer_name,
                 i_journalentryitem~amountincompanycodecurrency AS debit_amount,
                 i_journalentryitem~accountingdocument AS accountingdocument,""
                 i_journalentryitem~amountincompanycodecurrency AS credit_amount,
                 i_journalentryitem~CompanyCodeCurrency AS currency,
                 i_customer~taxnumber1 AS tax_number1,
                 i_customer~taxnumber2 AS tax_number2,
                 i_customer~telephonenumber1 AS telephone_number1,
                 i_customer~faxnumber AS fax_number,
                 i_journalentryitem~distributionchannel AS distribution_channel,
*                 i_CustomerCompany~CustomerHeadOffice AS customerHeadOffice,
                 i_operationalacctgdocitem~branchaccount AS branchaccount,
                 i_customer~organizationbpname2 AS organizationbpname2,
                  i_journalentryitem~LedgerGLLineItem AS LedgerGLLineItem,
                 CASE WHEN i_operationalacctgdocitem~branchaccount IS NOT INITIAL THEN bp3~EmailAddress ELSE i_buspartemailaddresstp_3~EmailAddress END AS eposta
          FROM i_journalentryitem
          INNER JOIN i_customer ON i_journalentryitem~customer = i_customer~customer
         LEFT JOIN i_operationalacctgdocitem WITH PRIVILEGED ACCESS ON i_journalentryitem~customer = i_operationalacctgdocitem~customer
          AND i_journalentryitem~companycode = i_operationalacctgdocitem~companycode
          AND i_journalentryitem~accountingdocument = i_operationalacctgdocitem~accountingdocument
          AND i_journalentryitem~fiscalyear = i_operationalacctgdocitem~fiscalyear
*          INNER JOIN i_CustomerCompany ON i_journalentryitem~Customer = i_CustomerCompany~Customer
          LEFT JOIN i_buspartemailaddresstp_3 ON i_journalentryitem~customer = i_buspartemailaddresstp_3~businesspartner
          LEFT JOIN i_buspartemailaddresstp_3 AS bp3 ON i_operationalacctgdocitem~branchaccount = bp3~businesspartner
          WHERE i_journalentryitem~customer IN @lr_customer
            AND i_customer~customername IN @lr_customer_name
            AND i_journalentryitem~amountincompanycodecurrency IN @lr_debit_amount
            AND i_journalentryitem~CompanyCodeCurrency IN @lr_currency
            AND i_customer~taxnumber1 IN @lr_tax_number1
            AND i_customer~taxnumber2 IN @lr_tax_number2
            AND i_customer~telephonenumber1 IN @lr_telephone_number1
            AND i_customer~faxnumber IN @lr_fax_number
            AND i_journalentryitem~distributionchannel IN @lr_distribution_channel
            AND i_journalentryitem~companycode IN @lr_company_code
            AND i_journalentryitem~postingdate IN @lr_posting_date
            AND i_journalentryitem~ledger IN @lr_ledger
            AND i_journalentryitem~clearingjournalentry = ''
            AND i_journalentryitem~specialglcode <> 'W'
            AND i_journalentryitem~financialaccounttype = 'D'
            AND i_customer~organizationbpname2 IN @lr_organizationbpname2
*            AND i_operationalacctgdocitem~branchaccount IN @lr_branchaccount "Do not activate this
            AND i_journalentryitem~isreversed = @abap_false
          ORDER BY i_journalentryitem~customer, i_journalentryitem~accountingdocument, i_operationalacctgdocitem~branchaccount, i_journalentryitem~LedgerGLLineItem
          INTO CORRESPONDING FIELDS OF TABLE @lt_output.

          DELETE ADJACENT DUPLICATES FROM lt_output COMPARING customer accountingdocument branchaccount LedgerGLLineItem.

*SELECT i_buspartemailaddresstp_3~EmailAddress AS eposta,
*       where
*
**LOOP AT lt_output reference int
*LOOP AT lt_output reference into data(lr_output_mail) where branchaccount is not initial.
*    lr_output_mail->
*ENDLOOP.


          LOOP AT lt_output INTO DATA(ls_output)
            GROUP BY ( customer = ls_output-customer branchaccount = ls_output-branchaccount )
            REFERENCE INTO DATA(lr_output_group).


*            READ table lt_output reference into data(lr_output) with key branchAccount = ''.

            APPEND INITIAL LINE TO lt_output_group ASSIGNING FIELD-SYMBOL(<fs_output_group>).
            <fs_output_group>-customer = lr_output_group->customer.
            <fs_output_group>-branchaccount = lr_output_group->branchaccount.

            LOOP AT GROUP lr_output_group INTO DATA(ls_output_group_s).
              <fs_output_group>-boolean_m            = abap_false.
              <fs_output_group>-boolean_s            = abap_true. "Hide Supplier fields
              <fs_output_group>-customer_name        = ls_output_group_s-customer_name.
              <fs_output_group>-currency             = ls_output_group_s-currency.
              <fs_output_group>-tax_number1          = ls_output_group_s-tax_number1.
              <fs_output_group>-tax_number2          = ls_output_group_s-tax_number2.
              <fs_output_group>-telephone_number1    = ls_output_group_s-telephone_number1.
              <fs_output_group>-fax_number           = ls_output_group_s-fax_number.
              <fs_output_group>-distribution_channel = ls_output_group_s-distribution_channel.
              <fs_output_group>-company_code         = ls_output_group_s-company_code.
              <fs_output_group>-organizationbpname2  = ls_output_group_s-organizationbpname2.
              <fs_output_group>-LedgerGLLineItem     = ls_output_group_s-LedgerGLLineItem.
              <fs_output_group>-eposta               = ls_output_group_s-eposta.
*          <fs_output_group>-posting_Date         = ls_output_group_s-posting_Date.
*          <fs_output_group>-posting_Date2         = |{ lr_posting_date[ 1 ]-low }. - { lr_posting_date[ 1 ]-high }|.
              IF lr_posting_date IS INITIAL.
                <fs_output_group>-posting_date2         = ''.
              ELSE.
                <fs_output_group>-posting_date2         = final_posting_date.
              ENDIF.
              IF ls_output_group_s-debit_amount      > 0.
                <fs_output_group>-debit_amount       += ls_output_group_s-debit_amount.
              ELSEIF ls_output_group_s-credit_amount <= 0.
                <fs_output_group>-credit_amount      += ls_output_group_s-credit_amount.
              ENDIF.
            ENDLOOP.
            lv_total = <fs_output_group>-debit_amount + <fs_output_group>-credit_amount.
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

          "LAST SELECT COUNT - Customer
          SELECT
                       *
                  FROM @lt_output AS lt_output
                  ORDER BY lt_output~customer
                  INTO CORRESPONDING FIELDS OF TABLE @lt_temp
                UP TO @lv_top ROWS
                OFFSET @lv_skip.

          SELECT COUNT( * )
           FROM @lt_output AS lt_output
                         INTO @DATA(lv_cnt).



        ELSEIF lr_report_type[ 1 ]-low = 'S'.

          SELECT
               i_journalentryitem~supplier,
               i_journalentryitem~companycode AS company_code,
*           i_journalentryitem~postingDate AS posting_Date,
               i_supplier~suppliername AS supplier_name,
                 i_journalentryitem~amountincompanycodecurrency AS debit_amount,
                 i_journalentryitem~accountingdocument AS accountingdocument,""
                 i_journalentryitem~amountincompanycodecurrency AS credit_amount,
                 i_journalentryitem~CompanyCodeCurrency AS currency,
                 i_supplier~taxnumber1 AS tax_number1,
                 i_supplier~taxnumber2 AS tax_number2,
                 i_supplier~phonenumber1 AS telephone_number1,
                 i_supplier~faxnumber AS fax_number,
               i_journalentryitem~product AS material,
*               i_SupplierCompany~SupplierHeadOffice AS SupplierHeadOffice,
               i_operationalacctgdocitem~branchaccount AS branchaccount,
                i_journalentryitem~LedgerGLLineItem AS LedgerGLLineItem,
            CASE WHEN i_operationalacctgdocitem~branchaccount IS NOT INITIAL THEN bp3~EmailAddress ELSE i_buspartemailaddresstp_3~EmailAddress END AS eposta
          FROM i_journalentryitem
          INNER JOIN i_supplier ON i_journalentryitem~supplier = i_supplier~supplier
          LEFT JOIN i_operationalacctgdocitem WITH PRIVILEGED ACCESS ON i_journalentryitem~supplier = i_operationalacctgdocitem~supplier
          AND i_journalentryitem~companycode = i_operationalacctgdocitem~companycode
          AND i_journalentryitem~accountingdocument = i_operationalacctgdocitem~accountingdocument
          AND i_journalentryitem~fiscalyear = i_operationalacctgdocitem~fiscalyear
*          INNER JOIN i_SupplierCompany ON i_journalentryitem~Supplier = i_SupplierCompany~Supplier
        LEFT JOIN i_buspartemailaddresstp_3 ON i_journalentryitem~supplier = i_buspartemailaddresstp_3~businesspartner
        LEFT JOIN i_buspartemailaddresstp_3 AS bp3 ON i_operationalacctgdocitem~branchaccount = bp3~businesspartner
          WHERE i_journalentryitem~supplier IN @lr_supplier
            AND i_supplier~suppliername IN @lr_supplier_name
            AND i_journalentryitem~amountincompanycodecurrency IN @lr_debit_amount
            AND i_journalentryitem~CompanyCodeCurrency IN @lr_currency
            AND i_supplier~taxnumber1 IN @lr_tax_number1
            AND i_supplier~taxnumber2 IN @lr_tax_number2
            AND i_supplier~phonenumber1 IN @lr_telephone_number1
            AND i_supplier~faxnumber IN @lr_fax_number
            AND i_journalentryitem~product IN @lr_material
            AND i_journalentryitem~companycode IN @lr_company_code
            AND i_journalentryitem~postingdate IN @lr_posting_date
            AND i_journalentryitem~ledger IN @lr_ledger
            AND i_journalentryitem~clearingjournalentry IS INITIAL
            AND i_journalentryitem~specialglcode <> 'W'    "revised
            AND i_journalentryitem~financialaccounttype = 'K'    "revised
*        AND i_operationalacctgdocitem~branchaccount IN @lr_branchaccount "Do not activate this
            AND i_supplier~organizationbpname2 IN @lr_organizationbpname2
            AND i_journalentryitem~isreversed IS INITIAL
          ORDER BY i_journalentryitem~supplier, i_journalentryitem~accountingdocument, i_operationalacctgdocitem~branchaccount, i_journalentryitem~LedgerGLLineItem
          INTO CORRESPONDING FIELDS OF TABLE @lt_output.

          DELETE ADJACENT DUPLICATES FROM lt_output COMPARING supplier accountingdocument branchAccount LedgerGLLineItem.

          LOOP AT lt_output INTO DATA(ls_output2)
            GROUP BY ( supplier = ls_output2-supplier branchaccount = ls_output2-branchaccount )
            REFERENCE INTO DATA(lr_output_group2).

            APPEND INITIAL LINE TO lt_output_group ASSIGNING FIELD-SYMBOL(<fs_output_group2>).
            <fs_output_group2>-supplier = lr_output_group2->supplier.
            <fs_output_group2>-branchaccount = lr_output_group2->branchaccount.

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
              <fs_output_group2>-company_code = ls_output_group_s2-company_code.
              <fs_output_group2>-organizationbpname2 = ls_output_group_s2-organizationbpname2.
              <fs_output_group2>-LedgerGLLineItem = ls_output_group_s2-LedgerGLLineItem.
              <fs_output_group2>-eposta = ls_output_group_s2-eposta.
              IF lr_posting_date IS INITIAL.
                <fs_output_group2>-posting_date2         = ''.
              ELSE.
                <fs_output_group2>-posting_date2         = final_posting_date.
              ENDIF.
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

          "LAST SELECT COUNT - Supplier
          SELECT
                         *
                    FROM @lt_output AS lt_output
                    ORDER BY lt_output~supplier
                    INTO CORRESPONDING FIELDS OF TABLE @lt_temp
                  UP TO @lv_top ROWS
                  OFFSET @lv_skip.

          SELECT COUNT( * )
           FROM @lt_output AS lt_output
                         INTO @lv_cnt.


        ENDIF.
      WHEN OTHERS. "Both M and S

        "If both M and S are selected, then I have to display both customers and suppliers.
        "I will use the same logic as above, but I will not hide any fields.




        SELECT i_journalentryitem~customer AS customer,
               i_journalentryitem~companycode AS company_code,
               i_customer~customername AS customer_name,
               i_journalentryitem~amountincompanycodecurrency AS debit_amount,
               i_journalentryitem~accountingdocument AS accountingdocument,""
               i_journalentryitem~amountincompanycodecurrency AS credit_amount,
               i_journalentryitem~CompanyCodeCurrency AS currency,
               i_customer~taxnumber1 AS tax_number1,
               i_customer~taxnumber2 AS tax_number2,
               i_customer~telephonenumber1 AS telephone_number1,
               i_customer~faxnumber AS fax_number,
               i_journalentryitem~distributionchannel AS distribution_channel,
*                 i_CustomerCompany~CustomerHeadOffice AS customerHeadOffice,
               i_operationalacctgdocitem~branchaccount AS branchaccount,
               i_customer~organizationbpname2 AS organizationbpname2,
                i_journalentryitem~LedgerGLLineItem AS LedgerGLLineItem,
             CASE WHEN i_operationalacctgdocitem~branchaccount IS NOT INITIAL THEN bp3~EmailAddress ELSE i_buspartemailaddresstp_3~EmailAddress END AS eposta
        FROM i_journalentryitem
        INNER JOIN i_customer ON i_journalentryitem~customer = i_customer~customer
       LEFT JOIN i_operationalacctgdocitem WITH PRIVILEGED ACCESS ON i_journalentryitem~customer = i_operationalacctgdocitem~customer
        AND i_journalentryitem~companycode = i_operationalacctgdocitem~companycode
        AND i_journalentryitem~accountingdocument = i_operationalacctgdocitem~accountingdocument
        AND i_journalentryitem~fiscalyear = i_operationalacctgdocitem~fiscalyear
*          INNER JOIN i_CustomerCompany ON i_journalentryitem~Customer = i_CustomerCompany~Customer
        LEFT JOIN i_buspartemailaddresstp_3 ON i_journalentryitem~customer = i_buspartemailaddresstp_3~businesspartner
        LEFT JOIN i_buspartemailaddresstp_3 AS bp3 ON i_operationalacctgdocitem~branchaccount = bp3~businesspartner
        WHERE i_journalentryitem~customer IN @lr_customer
          AND i_customer~customername IN @lr_customer_name
          AND i_journalentryitem~amountincompanycodecurrency IN @lr_debit_amount
          AND i_journalentryitem~CompanyCodeCurrency IN @lr_currency
          AND i_customer~taxnumber1 IN @lr_tax_number1
          AND i_customer~taxnumber2 IN @lr_tax_number2
          AND i_customer~telephonenumber1 IN @lr_telephone_number1
          AND i_customer~faxnumber IN @lr_fax_number
          AND i_journalentryitem~distributionchannel IN @lr_distribution_channel
          AND i_journalentryitem~companycode IN @lr_company_code
          AND i_journalentryitem~postingdate IN @lr_posting_date
          AND i_journalentryitem~ledger IN @lr_ledger
          AND i_journalentryitem~clearingjournalentry = ''
          AND i_journalentryitem~specialglcode <> 'W' "revised
          AND i_journalentryitem~financialaccounttype = 'D'    "revised
*            AND I_CustomerCompany~CustomerHeadOffice IN @lr_customerHeadOffice
          AND i_customer~organizationbpname2 IN @lr_organizationbpname2
*            AND i_operationalacctgdocitem~branchaccount IN @lr_branchaccount "Do not activate this
          AND i_journalentryitem~isreversed = @abap_false
        ORDER BY i_journalentryitem~customer, i_journalentryitem~accountingdocument, i_operationalacctgdocitem~branchaccount, i_journalentryitem~LedgerGLLineItem
        INTO CORRESPONDING FIELDS OF TABLE @lt_output_m.

        DELETE ADJACENT DUPLICATES FROM lt_output_m COMPARING customer accountingdocument branchaccount LedgerGLLineItem.

        LOOP AT lt_output_m INTO DATA(ls_output_m)
          GROUP BY ( customer = ls_output_m-customer branchaccount = ls_output_m-branchaccount )
          REFERENCE INTO DATA(lr_output_group_m).

          APPEND INITIAL LINE TO lt_output_group_m ASSIGNING FIELD-SYMBOL(<fs_output_group_m>).
          <fs_output_group_m>-customer = lr_output_group_m->customer.
          <fs_output_group_m>-branchaccount = lr_output_group_m->branchaccount.

          LOOP AT GROUP lr_output_group_m INTO DATA(ls_output_group_s_m).
*          <fs_output_group_M>-boolean_m            = abap_false.
*          <fs_output_group_M>-boolean_s            = abap_true. "Hide Supplier fields
            <fs_output_group_m>-customer_name               = ls_output_group_s_m-customer_name.
            <fs_output_group_m>-currency                    = ls_output_group_s_m-currency.
            <fs_output_group_m>-tax_number1                 = ls_output_group_s_m-tax_number1.
            <fs_output_group_m>-tax_number2                 = ls_output_group_s_m-tax_number2.
            <fs_output_group_m>-telephone_number1           = ls_output_group_s_m-telephone_number1.
            <fs_output_group_m>-fax_number                  = ls_output_group_s_m-fax_number.
            <fs_output_group_m>-distribution_channel        = ls_output_group_s_m-distribution_channel.
            <fs_output_group_m>-company_code                = ls_output_group_s_m-company_code.
            <fs_output_group_m>-organizationbpname2         = ls_output_group_s_m-organizationbpname2.
            <fs_output_group_m>-LedgerGLLineItem = ls_output_group_s_m-LedgerGLLineItem.
            <fs_output_group_m>-eposta = ls_output_group_s_m-eposta.
*          <fs_output_group>-posting_Date         = ls_output_group_s-posting_Date.
*          <fs_output_group>-posting_Date2         = |{ lr_posting_date[ 1 ]-low }. - { lr_posting_date[ 1 ]-high }|.
            IF lr_posting_date IS INITIAL.
              <fs_output_group_m>-posting_date2         = ''.
            ELSE.
              <fs_output_group_m>-posting_date2         = final_posting_date.
            ENDIF.
            IF ls_output_group_s_m-debit_amount      > 0.
              <fs_output_group_m>-debit_amount       += ls_output_group_s_m-debit_amount.
            ELSEIF ls_output_group_s_m-credit_amount <= 0.
              <fs_output_group_m>-credit_amount      += ls_output_group_s_m-credit_amount.
            ENDIF.
          ENDLOOP.
          lv_total_m = <fs_output_group_m>-debit_amount + <fs_output_group_m>-credit_amount.
          IF lv_total_m > 0.
            <fs_output_group_m>-debit_amount = lv_total_m.
            <fs_output_group_m>-credit_amount = 0.
          ELSEIF lv_total_m < 0.
            <fs_output_group_m>-credit_amount = lv_total_m.
            <fs_output_group_m>-debit_amount = 0.
          ELSE.
            <fs_output_group_m>-credit_amount = 0.
            <fs_output_group_m>-debit_amount = 0.
          ENDIF.

        ENDLOOP.


        "Supplier

        SELECT
             i_journalentryitem~supplier,
             i_journalentryitem~companycode AS company_code,
*           i_journalentryitem~postingDate AS posting_Date,
             i_supplier~suppliername AS supplier_name,
               i_journalentryitem~amountincompanycodecurrency AS debit_amount,
               i_journalentryitem~accountingdocument AS accountingdocument,""
               i_journalentryitem~amountincompanycodecurrency AS credit_amount,
               i_journalentryitem~CompanyCodeCurrency AS currency,
               i_supplier~taxnumber1 AS tax_number1,
               i_supplier~taxnumber2 AS tax_number2,
               i_supplier~phonenumber1 AS telephone_number1,
               i_supplier~faxnumber AS fax_number,
             i_journalentryitem~product AS material,
*               i_SupplierCompany~SupplierHeadOffice AS SupplierHeadOffice,
             i_operationalacctgdocitem~branchaccount AS branchaccount,
              i_journalentryitem~LedgerGLLineItem AS LedgerGLLineItem,
            CASE WHEN i_operationalacctgdocitem~branchaccount IS NOT INITIAL THEN bp3~EmailAddress ELSE i_buspartemailaddresstp_3~EmailAddress END AS eposta
        FROM i_journalentryitem
        INNER JOIN i_supplier ON i_journalentryitem~supplier = i_supplier~supplier
        LEFT JOIN i_operationalacctgdocitem WITH PRIVILEGED ACCESS ON i_journalentryitem~supplier = i_operationalacctgdocitem~supplier
        AND i_journalentryitem~companycode = i_operationalacctgdocitem~companycode
        AND i_journalentryitem~accountingdocument = i_operationalacctgdocitem~accountingdocument
        AND i_journalentryitem~fiscalyear = i_operationalacctgdocitem~fiscalyear
*          INNER JOIN i_SupplierCompany ON i_journalentryitem~Supplier = i_SupplierCompany~Supplier
        LEFT JOIN i_buspartemailaddresstp_3 ON i_journalentryitem~supplier = i_buspartemailaddresstp_3~businesspartner
        LEFT JOIN i_buspartemailaddresstp_3 AS bp3 ON i_operationalacctgdocitem~branchaccount = bp3~businesspartner
        WHERE i_journalentryitem~supplier IN @lr_supplier
          AND i_supplier~suppliername IN @lr_supplier_name
          AND i_journalentryitem~amountincompanycodecurrency IN @lr_debit_amount
          AND i_journalentryitem~CompanyCodeCurrency IN @lr_currency
          AND i_supplier~taxnumber1 IN @lr_tax_number1
          AND i_supplier~taxnumber2 IN @lr_tax_number2
          AND i_supplier~phonenumber1 IN @lr_telephone_number1
          AND i_supplier~faxnumber IN @lr_fax_number
          AND i_journalentryitem~product IN @lr_material
          AND i_journalentryitem~companycode IN @lr_company_code
          AND i_journalentryitem~postingdate IN @lr_posting_date
          AND i_journalentryitem~ledger IN @lr_ledger
          AND i_journalentryitem~clearingjournalentry IS INITIAL
      AND i_journalentryitem~specialglcode <> 'W'    "revised
      AND i_journalentryitem~financialaccounttype = 'K'    "revised
*        AND i_operationalacctgdocitem~branchaccount IN @lr_branchaccount "Do not activate this
          AND i_supplier~organizationbpname2 IN @lr_organizationbpname2
           AND i_journalentryitem~isreversed IS INITIAL
        ORDER BY i_journalentryitem~supplier, i_journalentryitem~accountingdocument, i_operationalacctgdocitem~branchaccount, i_journalentryitem~LedgerGLLineItem
        INTO CORRESPONDING FIELDS OF TABLE @lt_output_s.

        DELETE ADJACENT DUPLICATES FROM lt_output_s COMPARING supplier accountingdocument branchAccount LedgerGLLineItem.

        LOOP AT lt_output_s INTO DATA(ls_output2_s)
          GROUP BY ( supplier = ls_output2_s-supplier branchaccount = ls_output2_s-branchaccount )
          REFERENCE INTO DATA(lr_output_group2_s).

          APPEND INITIAL LINE TO lt_output_group_s ASSIGNING FIELD-SYMBOL(<fs_output_group2_s>).
          <fs_output_group2_s>-supplier = lr_output_group2_s->supplier.
          <fs_output_group2_s>-branchaccount = lr_output_group2_s->branchaccount.

          LOOP AT GROUP lr_output_group2_s INTO DATA(ls_output_group_s2_s).
*          <fs_output_group2_S>-boolean_s = abap_false.
*          <fs_output_group2_S>-boolean_m = abap_true. "Hide Customer fields.
            <fs_output_group2_s>-supplier_name = ls_output_group_s2_s-supplier_name.
            <fs_output_group2_s>-currency = ls_output_group_s2_s-currency.
            <fs_output_group2_s>-tax_number1 = ls_output_group_s2_s-tax_number1.
            <fs_output_group2_s>-tax_number2 = ls_output_group_s2_s-tax_number2.
            <fs_output_group2_s>-telephone_number1 = ls_output_group_s2_s-telephone_number1.
            <fs_output_group2_s>-fax_number = ls_output_group_s2_s-fax_number.
            <fs_output_group2_s>-material = ls_output_group_s2_s-material.
            <fs_output_group2_s>-company_code = ls_output_group_s2_s-company_code.
            <fs_output_group2_s>-organizationbpname2 = ls_output_group_s2_s-organizationbpname2.
            <fs_output_group2_s>-LedgerGLLineItem =  ls_output_group_s2-LedgerGLLineItem.
            <fs_output_group2_s>-eposta =  ls_output_group_s2-eposta.
            IF lr_posting_date IS INITIAL.
              <fs_output_group2_s>-posting_date2         = ''.
            ELSE.
              <fs_output_group2_s>-posting_date2         = final_posting_date.
            ENDIF.
            IF ls_output_group_s2_s-debit_amount > 0.
              <fs_output_group2_s>-debit_amount += ls_output_group_s2_s-debit_amount.
            ELSEIF ls_output_group_s2_s-credit_amount <= 0.
              <fs_output_group2_s>-credit_amount += ls_output_group_s2_s-credit_amount.
            ENDIF.
          ENDLOOP.
          lv_total_s = <fs_output_group2_s>-debit_amount + <fs_output_group2_s>-credit_amount.
          IF lv_total_s > 0.
            <fs_output_group2_s>-debit_amount = lv_total_s.
            <fs_output_group2_s>-credit_amount = 0.
          ELSEIF lv_total_s < 0.
            <fs_output_group2_s>-credit_amount = lv_total_s.
            <fs_output_group2_s>-debit_amount = 0.
          ELSE.
            <fs_output_group2_s>-credit_amount = 0.
            <fs_output_group2_s>-debit_amount = 0.
          ENDIF.
        ENDLOOP.

        APPEND LINES OF lt_output_group_m TO lt_output.
        APPEND LINES OF lt_output_group_s TO lt_output.



        "LAST SELECT COUNT - Both Customer and Supplier
        SELECT customer          ,
                supplier           ,
                branchAccount      ,
                accountingDocument ,
                customer_name      ,
                supplier_name      ,
                debit_amount       ,
                credit_amount      ,
                currency           ,
                tax_number1        ,
                tax_number2        ,
                telephone_number1  ,
                fax_number         ,
                distribution_channel,
                material           ,
                report_type        ,
                boolean_m          ,
                boolean_s          ,
                company_Code       ,
                posting_Date       ,
                posting_Date2      ,
                ledger             ,
                eposta             ,
                OrganizationBPName2
                         FROM @lt_output AS lt_output
                          ORDER BY lt_output~customer", lt_output~supplier
                 INTO CORRESPONDING FIELDS OF TABLE @lt_temp
               UP TO @lv_top ROWS
               OFFSET @lv_skip.

        SELECT COUNT( * )
         FROM @lt_output AS lt_output
                       INTO @lv_cnt.

    ENDCASE.





*    IF sy-subrc IS INITIAL. "Removed this line because if no values returned, I still have to display it. lv_count should be printed.
    TRY.
        "TRY begins
        io_response->set_data( lt_temp ).


        DATA lv_count TYPE int8.
*    lv_count = lines( lt_output ).
        lv_count = lv_cnt.

        IF io_request->is_total_numb_of_rec_requested(  ).
          io_response->set_total_number_of_records( lv_count ).
        ENDIF.


        "CATCH begins
      CATCH cx_root INTO DATA(exception).

        DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_longtext( ).

        DATA(exception_t100_key) = cl_message_helper=>get_latest_t100_exception( exception )->t100key.
        RAISE EXCEPTION TYPE zfi007_cl_root_mus_sat
          EXPORTING
            textid   = VALUE scx_t100key(
            msgid = exception_t100_key-msgid
            msgno = exception_t100_key-msgno
*          attr1 = exception_t100_key-attr
            attr2 = exception_t100_key-attr2
            attr3 = exception_t100_key-attr3
            attr4 = exception_t100_key-attr4
            )
            previous = exception.
    ENDTRY.

*    ENDIF.

  ENDMETHOD.

ENDCLASS.
