CLASS lhc_ZFI007_R_MUS_SAT_MUTABAKAT DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.




  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zfi007_r_mus_sat_mutabakat RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE zfi007_r_mus_sat_mutabakat.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE zfi007_r_mus_sat_mutabakat.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE zfi007_r_mus_sat_mutabakat.

    METHODS read FOR READ
      IMPORTING keys FOR READ zfi007_r_mus_sat_mutabakat RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zfi007_r_mus_sat_mutabakat.
    METHODS print FOR MODIFY
      IMPORTING keys   FOR ACTION zfi007_r_mus_sat_mutabakat~print
      RESULT    result.
    METHODS send_mail FOR MODIFY
      IMPORTING keys FOR ACTION zfi007_r_mus_sat_mutabakat~send_mail.

ENDCLASS.

CLASS lhc_ZFI007_R_MUS_SAT_MUTABAKAT IMPLEMENTATION.


  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD read.
    "We cannot read rows in the report directly like this since we have used custom entity. Because there are no database tables behind it.
*      SELECT * FROM zfi007_r_mus_sat_mutabakat
*          FOR ALL ENTRIES IN @keys
*          WHERE Customer = @keys-%control
*            AND logid          = @keys-logid
*            AND saleref        = @keys-saleref
*          INTO CORRESPONDING FIELDS OF TABLE @result.
*  DATA(result) = VALUE zcl_my_query_provider=>get_my_data( zfi007_r_mus_sat_mutabakat ).
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD print.
    DATA: ls_data     TYPE zfi007_s_mus_sat_binding,
          ls_request  TYPE zfi007_s_mus_sat_request,
          ls_response TYPE zfi007_s_mus_sat_response.

    DATA lo_cls TYPE REF TO zfi007_cl_root_mus_sat.
    lo_cls = NEW #( ).


*   CATCH cx_http_dest_provider_error..

**    TRY.
**        DATA(lo_dest) = cl_http_destination_provider=>create_by_comm_arrangement(
**        comm_scenario = 'write comm senario here'
**        comm_system_id = 'write comm system here'
**        service_id = 'ZFI007_OS_MUS_SAT_MUT_REST'
**        ).
**      CATCH cx_http_dest_provider_error INTO DATA(lx_error).
**    ENDTRY.
**
**    TRY.
**        DATA(lo_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).
**      CATCH cx_web_http_client_error INTO DATA(lx_client_error).
**    ENDTRY.
**
**    DATA(lo_request) = lo_client->get_http_request( ).
**    lo_request->set_header_fields( VALUE #(
**        ( name = 'Accept' value = 'application/json, text/plain, */*' )
**        ( name = 'Content-Type' value = 'application/json;charset=utf-8' )
**    ) ).


*    READ ENTITIES OF zfi007_r_mus_sat_mutabakat IN LOCAL MODE
*        ENTITY zfi007_r_mus_sat_mutabakat
*           FIELDS (
*                       company_Code
*                       tax_number1
*                       tax_number2
*                       telephone_number1
*                       fax_number
*                       posting_Date
*                       debit_amount
*                       credit_amount
*                       ePosta
*            )
*           WITH CORRESPONDING #( keys )
*         RESULT DATA(lt_result)
*         FAILED failed.


*    LOOP AT keys INTO DATA(ls_keys).
*        CATCH cx_rap_query_prov_not_impl.
*        CATCH cx_rap_query_provider.
*      lo_cls->read_partner( partner_no = ls_keys-customer ).
*    ENDLOOP.


*READ table keys into data(ls_keys) with key supplier is not initial.
*IF sy-subrc =



    IF line_exists( keys[ supplier = '' ] ).

      "E-mail SQL query
      SELECT EmailAddress AS mail, companyCode AS companyCode FROM I_BusPartEmailAddressTP_3
      INNER JOIN i_journalEntryItem ON I_BusPartEmailAddressTP_3~BusinessPartner = i_journalEntryItem~Customer
*WHERE i_journalEntryItem~CompanyCode = @ls_keys-%param-company_Code
      INTO TABLE @DATA(lt_mails).

      LOOP AT keys INTO DATA(ls_keys).
SELECT SINGLE companycodeName from i_companycode where companycode = @ls_keys-%param-company_code into @data(lv_company_code_name).
        READ TABLE lt_mails INTO DATA(ls_mails) WITH KEY companyCode = ls_keys-%param-company_Code.
        DATA(lv_ePosta) = ls_mails-mail.
        ls_data = VALUE #(

                             CompanyCodeName = lv_company_code_name
                           Alici        = ls_keys-%param-customer_name
                            VergiDairesi = ls_keys-%param-tax_number1
                             VergiNumarasi = ls_keys-%param-tax_number2
                             Tel = ls_keys-%param-telephone_number1
                             Faks = ls_keys-%param-fax_number
                             Tarih_sys = cl_abap_context_info=>get_system_date( )
                             Tarih = ls_keys-%param-posting_Date
                             ParaMiktari = COND #( WHEN ls_keys-%param-debit_amount > 0 THEN ls_keys-%param-debit_amount WHEN ls_keys-%param-credit_amount < 0 THEN ls_keys-%param-credit_amount ELSE 0 )
                             AlacakVerecek = COND #( WHEN ls_keys-%param-debit_amount > 0 THEN 'BORÇ' WHEN ls_keys-%param-credit_amount < 0 THEN 'ALACAK' ELSE '' )
                             ePosta = lv_ePosta
                            ).
      ENDLOOP.
    ELSE.

      "E-mail SQL query
      SELECT EmailAddress AS mail, companyCode AS companyCode FROM I_BusPartEmailAddressTP_3
      INNER JOIN i_journalEntryItem ON I_BusPartEmailAddressTP_3~BusinessPartner = i_journalEntryItem~Supplier
*WHERE i_journalEntryItem~CompanyCode = @ls_keys-%param-company_Code
      INTO TABLE @lt_mails.

      LOOP AT keys INTO ls_keys.
      SELECT SINGLE companycodeName from i_companycode where companycode = @ls_keys-%param-company_code into @lv_company_code_name.
        READ TABLE lt_mails INTO ls_mails WITH KEY companyCode = ls_keys-%param-company_Code.
        lv_ePosta = ls_mails-mail.
        ls_data = VALUE #(
                             CompanyCodeName = lv_company_code_name
                           Alici        = ls_keys-%param-supplier_name
                            VergiDairesi = ls_keys-%param-tax_number1
                             VergiNumarasi = ls_keys-%param-tax_number2
                             Tel = ls_keys-%param-telephone_number1
                             Faks = ls_keys-%param-fax_number
                             Tarih_sys = cl_abap_context_info=>get_system_date( )
                             Tarih = ls_keys-%param-posting_Date
                             ParaMiktari = COND #( WHEN ls_keys-%param-debit_amount > 0 THEN ls_keys-%param-debit_amount WHEN ls_keys-%param-credit_amount < 0 THEN ls_keys-%param-credit_amount ELSE 0 )
                             AlacakVerecek = COND #( WHEN ls_keys-%param-debit_amount > 0 THEN 'BORÇ' WHEN ls_keys-%param-credit_amount < 0 THEN 'ALACAK' ELSE '' )
                             ePosta = lv_ePosta
                            ).
      ENDLOOP.

    ENDIF.

    TRY.
        CALL TRANSFORMATION zfi007_tr_mus_sat_binding
        SOURCE form = ls_data
        RESULT XML DATA(lv_xml).

      CATCH cx_root INTO DATA(lo_root).
    ENDTRY.

    DATA(lv_base64_data) = cl_web_http_utility=>encode_x_base64( unencoded = lv_xml ).

**    ls_request-xdp_template       = 'ZTEST/Test'.
**    ls_request-xml_data           = lv_base64_data.
**    ls_request-form_type          = 'print'.
**    ls_request-form_locale        = 'tr_TR'.
**    ls_request-tagged_pdf         = 1.
**    ls_request-embed_font         = 0.
**    ls_request-change_not_allowed = abap_false.
**    ls_request-print_not_allowed  = abap_false.

**    TRY.
**        CALL METHOD /ui2/cl_json=>serialize
**          EXPORTING
**            data        = ls_request
**            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
**          RECEIVING
**            r_json      = DATA(lv_body).
**
**      CATCH cx_root INTO DATA(lx_root).
**    ENDTRY.

**    lo_request->set_text(
**      EXPORTING
**        i_text = lv_body
**    ).


**    TRY.
**        DATA(lo_response) = lo_client->execute(
**          i_method = if_web_http_client=>post
***         i_timeout = 0
**        ).
**      CATCH  cx_web_http_client_error INTO lx_client_error.
**    ENDTRY.
**    DATA(lv_response) = lo_response->get_text( ).
**    DATA(ls_status)   = lo_response->get_status( ).

**    TRY.
**        CALL METHOD /ui2/cl_json=>deserialize
**          EXPORTING
**            json          = lv_response
**            assoc_arrays  = abap_true
**            name_mappings = VALUE #( ( json = 'fileContent' abap = 'FILECONTENT' ) )
**          CHANGING
**            data          = ls_response.
**      CATCH cx_root INTO lx_root.
**    ENDTRY.

   TRY.
       zcldobj_cl_ads_util=>call_adobe(
         EXPORTING
           iv_form_name            = 'ZFI007_AF_MUS_SAT_MUTABAKAT'
           iv_template_name        = 'MUS_SAT_MUTABAKAT'
           iv_xml                  = lv_base64_data
           iv_adobe_scenario       = 'ZCLDOBJ_CS_ADS'
           iv_adobe_system         = 'ZCLDOBJ_CSYS_ADS'
           iv_adobe_service_id     = 'ZCLDOBJ_OS_ADS_REST'
*       iv_queue_name           =
*       iv_print_ser_scenario   =
*       iv_print_ser_system     =
*       iv_print_ser_service_id =
         IMPORTING
           ev_pdf                  = data(lv_pdf)
           ev_response_code        = data(lv_res_c)
           ev_response_text        = data(lv_res_t)
       ).
     CATCH cx_http_dest_provider_error.
       "handle exception
   ENDTRY.

    IF lv_pdf IS INITIAL.

    ELSE.
      result = VALUE #( ( %cid_ref = VALUE #( keys[ 1 ]-%cid_ref OPTIONAL )
                         customer = VALUE #( keys[ 1 ]-customer OPTIONAL )
                         supplier = VALUE #( keys[ 1 ]-supplier OPTIONAL )
                         %param = lv_pdf ) ).
*    out->write( ls_response-filecontent ).
    ENDIF.



**    "This code block is for assigning to printer directly without displaying preview.
**    DATA(lv_print_data) = cl_web_http_utility=>decode_x_base64( encoded = ls_response-filecontent ).
**    DATA(lv_qitem_id) = cl_print_queue_utils=>create_queue_itemid( ).
**
**    cl_print_queue_utils=>create_queue_item_by_data(
**      EXPORTING
**        iv_qname            = 'ZBS_TEST_QUEUE'
**        iv_print_data       = lv_print_data
**        iv_name_of_main_doc = 'Test Document'
**        iv_itemid           = lv_qitem_id
**      IMPORTING
**        ev_err_msg          = DATA(lv_err_msg)
**    ).
**    """

  ENDMETHOD.

  METHOD send_mail.

*    DATA lt_lessons TYPE TABLE OF zfi007_r_mus_sat_mutabakat. "zbg_001_dd_lsn.
    DATA lv_sender_mail TYPE string.
    DATA lv_partner_name type name1_gp.
    CONSTANTS c_new TYPE abap_cr_lf VALUE cl_abap_char_utilities=>cr_lf.

    DATA success TYPE abap_boolean.
    DATA sender TYPE cl_bcs_mail_message=>ty_address.
    DATA receivers TYPE cl_bcs_mail_message=>tyt_recipient.


""""""
"For attachment - print seciton
DATA: ls_data     TYPE zfi007_s_mus_sat_binding,
          ls_request  TYPE zfi007_s_mus_sat_request,
          ls_response TYPE zfi007_s_mus_sat_response.

    DATA lo_cls TYPE REF TO zfi007_cl_root_mus_sat.
    lo_cls = NEW #( ).

   IF line_exists( keys[ supplier = '' ] ).

      "E-mail SQL query
      SELECT EmailAddress AS mail, companyCode AS companyCode FROM I_BusPartEmailAddressTP_3
      INNER JOIN i_journalEntryItem ON I_BusPartEmailAddressTP_3~BusinessPartner = i_journalEntryItem~Customer
*WHERE i_journalEntryItem~CompanyCode = @ls_keys-%param-company_Code
      INTO TABLE @DATA(lt_mails).

      LOOP AT keys INTO DATA(ls_keys).
SELECT SINGLE companycodeName from i_companycode where companycode = @ls_keys-%param-company_code into @data(lv_company_code_name).
        READ TABLE lt_mails INTO DATA(ls_mails) WITH KEY companyCode = ls_keys-%param-company_Code.
        DATA(lv_ePosta) = ls_mails-mail.
        ls_data = VALUE #(

                             CompanyCodeName = lv_company_code_name
                           Alici        = ls_keys-%param-customer_name
                            VergiDairesi = ls_keys-%param-tax_number1
                             VergiNumarasi = ls_keys-%param-tax_number2
                             Tel = ls_keys-%param-telephone_number1
                             Faks = ls_keys-%param-fax_number
                             Tarih_sys = cl_abap_context_info=>get_system_date( )
                             Tarih = ls_keys-%param-posting_Date
                             ParaMiktari = COND #( WHEN ls_keys-%param-debit_amount > 0 THEN ls_keys-%param-debit_amount WHEN ls_keys-%param-credit_amount < 0 THEN ls_keys-%param-credit_amount ELSE 0 )
                             AlacakVerecek = COND #( WHEN ls_keys-%param-debit_amount > 0 THEN 'BORÇ' WHEN ls_keys-%param-credit_amount < 0 THEN 'ALACAK' ELSE '' )
                             ePosta = lv_ePosta
                            ).
      ENDLOOP.
    ELSE.

      "E-mail SQL query
      SELECT EmailAddress AS mail, companyCode AS companyCode FROM I_BusPartEmailAddressTP_3
      INNER JOIN i_journalEntryItem ON I_BusPartEmailAddressTP_3~BusinessPartner = i_journalEntryItem~Supplier
*WHERE i_journalEntryItem~CompanyCode = @ls_keys-%param-company_Code
      INTO TABLE @lt_mails.

      LOOP AT keys INTO ls_keys.
      SELECT SINGLE companycodeName from i_companycode where companycode = @ls_keys-%param-company_code into @lv_company_code_name.
        READ TABLE lt_mails INTO ls_mails WITH KEY companyCode = ls_keys-%param-company_Code.
        lv_ePosta = ls_mails-mail.
        ls_data = VALUE #(
                             CompanyCodeName = lv_company_code_name
                           Alici        = ls_keys-%param-supplier_name
                            VergiDairesi = ls_keys-%param-tax_number1
                             VergiNumarasi = ls_keys-%param-tax_number2
                             Tel = ls_keys-%param-telephone_number1
                             Faks = ls_keys-%param-fax_number
                             Tarih_sys = cl_abap_context_info=>get_system_date( )
                             Tarih = ls_keys-%param-posting_Date
                             ParaMiktari = COND #( WHEN ls_keys-%param-debit_amount > 0 THEN ls_keys-%param-debit_amount WHEN ls_keys-%param-credit_amount < 0 THEN ls_keys-%param-credit_amount ELSE 0 )
                             AlacakVerecek = COND #( WHEN ls_keys-%param-debit_amount > 0 THEN 'BORÇ' WHEN ls_keys-%param-credit_amount < 0 THEN 'ALACAK' ELSE '' )
                             ePosta = lv_ePosta
                            ).
      ENDLOOP.

    ENDIF.

    TRY.
        CALL TRANSFORMATION zfi007_tr_mus_sat_binding
        SOURCE form = ls_data
        RESULT XML DATA(lv_xml).

      CATCH cx_root INTO DATA(lo_root).
    ENDTRY.

    DATA(lv_base64_data) = cl_web_http_utility=>encode_x_base64( unencoded = lv_xml ).

  TRY.
       zcldobj_cl_ads_util=>call_adobe(
         EXPORTING
           iv_form_name            = 'ZFI007_AF_MUS_SAT_MUTABAKAT'
           iv_template_name        = 'MUS_SAT_MUTABAKAT'
           iv_xml                  = lv_base64_data
           iv_adobe_scenario       = 'ZCLDOBJ_CS_ADS'
           iv_adobe_system         = 'ZCLDOBJ_CSYS_ADS'
           iv_adobe_service_id     = 'ZCLDOBJ_OS_ADS_REST'
*       iv_queue_name           =
*       iv_print_ser_scenario   =
*       iv_print_ser_system     =
*       iv_print_ser_service_id =
         IMPORTING
           ev_pdf                  = data(lv_pdf)
           ev_response_code        = data(lv_res_c)
           ev_response_text        = data(lv_res_t)
       ).
     CATCH cx_http_dest_provider_error.
       "handle exception
   ENDTRY.


*    IF lv_pdf IS INITIAL.
*
*    ELSE.
*      result = VALUE #( ( %cid_ref = VALUE #( keys[ 1 ]-%cid_ref OPTIONAL )
*                         customer = VALUE #( keys[ 1 ]-customer OPTIONAL )
*                         supplier = VALUE #( keys[ 1 ]-supplier OPTIONAL )
*                         %param = lv_pdf ) ).
*    ENDIF.

"""""
*    LOOP AT keys INTO DATA(ls_keys).
*      READ ENTITIES OF zmg003_dd_student IN LOCAL MODE
*      ENTITY Lecture
*      ALL FIELDS WITH VALUE #( ( lessonuuid = ls_keys-lessonuuid ) )
*      RESULT DATA(lt_result)
*      FAILED DATA(ls_failed)
*      REPORTED DATA(ls_reported).



*      APPEND INITIAL LINE TO lt_lessons ASSIGNING FIELD-SYMBOL(<fs_lesson>).
*      <fs_lesson> = CORRESPONDING #(  lt_result[  1 ] ).
*    ENDLOOP.


    "E-mail SQL query
    SELECT EmailAddress AS mail, companyCode AS companyCode FROM I_BusPartEmailAddressTP_3
    INNER JOIN i_journalEntryItem ON I_BusPartEmailAddressTP_3~BusinessPartner = i_journalEntryItem~Supplier
*    WHERE i_journalEntryItem~CompanyCode = @ls_keys-%param-company_Code
    INTO TABLE @DATA(lt_sender_mails).





    LOOP AT keys INTO ls_keys.
      READ TABLE lt_sender_mails INTO DATA(ls_sender_mails) WITH KEY companyCode = ls_keys-%param-company_Code.
      CASE ls_sender_mails-companycode.
        WHEN 1000.
          lv_sender_mail = 'muhasebe@hesapci.com'.
        WHEN 2000.
          lv_sender_mail = 'muhasebe@adalyapetrol.com.tr'.
        WHEN 3000.
          lv_sender_mail = 'muhasebe@anttechbva.com'.
      ENDCASE.
      sender = lv_sender_mail ##NO_TEXT.
*      sender = 'mustafa.gedikli@nttdata.com' ##NO_TEXT.

      receivers = VALUE #( (
      address = ls_mails-mail
*      address = 'mustafa.gedikli@nttdata.com'
       ) ).
        IF line_exists( keys[ supplier = '' ] ).
       lv_partner_name = ls_keys-%param-customer_name.
       ELSE.
       lv_partner_name = ls_keys-%param-supplier_name.
       ENDIF.
*    ENDLOOP.


    TRY.
        DATA(mail) = cl_bcs_mail_message=>create_instance( ).

        mail->set_sender( sender ).

        LOOP AT receivers INTO DATA(lv_email_address).
          mail->add_recipient( lv_email_address-address ).
        ENDLOOP.

        mail->set_subject( 'Cari Bakiye Mutabakat Hk.' ) ##NO_TEXT.
        mail->set_main( cl_bcs_mail_textpart=>create_instance(

        iv_content = |<h1>Sayın { lv_partner_name }</h1>| &&
                     |<p>Mutabakat mektubu ekte bilginize sunulmuştur.</p>| &&
                     |<p>Saygılarımızla.</p> |
        iv_content_type = 'text/html'  ) ) ##NO_TEXT.

        DATA lv_attch_body TYPE string.

        LOOP AT keys INTO DATA(ls_key).

          "I need to insert adobe form attachment here in order to send as e-mail.
*          lv_attch_body = lv_attch_body &&
*                          | Lecture: { ls_keys-customer }{ c_new }| &&
*                          | Code:    { ls_keys-supplier }{ c_new }| &&
*                          |------------------------------------------ { c_new }|.
        ENDLOOP.

        mail->add_attachment( cl_bcs_mail_textpart=>create_instance(
*        iv_content = lv_attch_body"lv_pdf'in içeriği olacak
*        iv_content_type = 'text/plain' "ya pdf ya da xstiring veya string veya cstring olabilir. "yapay zekayı sor en kötü
        iv_content = lv_pdf"lv_pdf'in içeriği olacak
        iv_content_type = 'xstring' "ya pdf ya da xstiring veya string veya cstring olabilir. "yapay zekayı sor en kötü
        iv_filename = 'Lectures.txt' ) ). ##NO_TEXT.
        mail->send( IMPORTING et_status = DATA(lt_status) ).

        DATA(lv_msg) = new_message_with_text( text = lt_status[ 1 ]-status_response severity = cl_abap_behv=>ms-success ).
        APPEND VALUE #( %msg = lv_msg ) TO reported-zfi007_r_mus_sat_mutabakat.

      CATCH cx_bcs_mail INTO DATA(lx_mail).
        DATA(lx_data) = lx_mail->get_longtext( ).

        lv_msg = new_message_with_text( text = lx_data severity = cl_abap_behv=>ms-error ).
        APPEND VALUE #( %msg = lv_msg ) TO reported-zfi007_r_mus_sat_mutabakat.
    ENDTRY.
    ENDLOOP.
  ENDMETHOD.



ENDCLASS.

CLASS lsc_ZFI007_R_MUS_SAT_MUTABAKAT DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZFI007_R_MUS_SAT_MUTABAKAT IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
