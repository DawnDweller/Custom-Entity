CLASS zcldobj_cl_ads_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      ty_cscn_id          TYPE c LENGTH 30,
      ty_cscn_outb_srv_id TYPE c LENGTH 40,
      ty_cs_id            TYPE c LENGTH 60,
      ty_qname            TYPE c LENGTH 32,
      ty_docname          TYPE c LENGTH 120,
      BEGIN OF ty_queue_name,
        qname        TYPE c LENGTH 32,
        qdescription TYPE c LENGTH 80,
      END OF ty_queue_name,
      tyt_queue_name TYPE TABLE OF ty_queue_name.

    CLASS-METHODS: call_adobe IMPORTING iv_form_name            TYPE string
                                        iv_template_name        TYPE string
                                        iv_xml                  TYPE string
                                        iv_adobe_scenario       TYPE ty_cscn_id
                                        iv_adobe_system         TYPE ty_cs_id
                                        iv_adobe_service_id     TYPE ty_cscn_outb_srv_id
                                        iv_queue_name           TYPE ty_qname OPTIONAL
                                        iv_print_ser_scenario   TYPE ty_cscn_id OPTIONAL
                                        iv_print_ser_system     TYPE ty_cs_id OPTIONAL
                                        iv_print_ser_service_id TYPE ty_cscn_outb_srv_id OPTIONAL
                              EXPORTING ev_pdf                  TYPE string
                                        ev_response_code        TYPE int4
                                        ev_response_text        TYPE string
                              RAISING
                                        cx_http_dest_provider_error,

      send_queue IMPORTING iv_queue_name       TYPE ty_qname
                           iv_pdf              TYPE string
                           iv_document_name    TYPE string OPTIONAL
                           iv_number_of_copies TYPE cl_print_queue_utils=>ty_nr_copies DEFAULT 1
                           iv_pages            TYPE cl_print_queue_utils=>ty_page_count DEFAULT 0
*                           iv_print_ser_scenario   TYPE ty_cscn_id
*                           iv_print_ser_system     TYPE ty_cs_id
*                           iv_print_ser_service_id TYPE ty_cscn_outb_srv_id
                 EXPORTING ev_response_code    TYPE int4
                           ev_response_text    TYPE string,

      get_queue_names EXPORTING et_queue_names TYPE tyt_queue_name
                      RAISING   cx_web_http_client_error
                                cx_http_dest_provider_error
                                /iwbep/cx_cp_remote
                                /iwbep/cx_gateway.

    TYPES:BEGIN OF ty_printcontents,
            object_key    TYPE string,
            document_name TYPE string,
          END OF ty_printcontents.

    TYPES: BEGIN OF ty_printtasks,
             number_of_copies TYPE int4,
             username         TYPE string,
             qname            TYPE string,
             print_contents   TYPE STANDARD TABLE OF ty_printcontents WITH EMPTY KEY,
           END OF ty_printtasks.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcldobj_cl_ads_util IMPLEMENTATION.


  METHOD call_adobe.
    DATA: ls_req      TYPE zcldobj_s_ads_render_body,
          ls_response TYPE zcldobj_s_ads_render_response.
    TRY.
        TRY.
            DATA(lo_dest) = cl_http_destination_provider=>create_by_comm_arrangement(
              comm_scenario  = iv_adobe_scenario
              comm_system_id = iv_adobe_system
              service_id     = iv_adobe_service_id
            ).
          CATCH cx_http_dest_provider_error INTO DATA(lo_error).
            ev_response_code = 500.
            ev_response_text = lo_error->get_text( ).
            EXIT.
        ENDTRY.

        TRY.
            DATA(lo_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).
          CATCH  cx_web_http_client_error INTO DATA(lo_client_error).
            ev_response_code = 500.
            ev_response_text = lo_client_error->get_text( ).
            EXIT.
        ENDTRY.

        DATA(lo_request) = lo_client->get_http_request( ).
        lo_request->set_header_fields( VALUE #(
          ( name = 'Accept' value = 'application/json, text/plain, */*'  )
          ( name = 'Content-Type' value = 'application/json;charset=utf-8'  )
        ) ).

        ls_req-xdp_template       = |{ iv_form_name }/{ iv_template_name }|.
        ls_req-xml_data           = iv_xml.
        ls_req-form_type          = 'print'.
        ls_req-form_locale        = 'tr_TR'.
        ls_req-tagged_pdf         = 1.
        ls_req-embed_font         = 0.
        ls_req-change_not_allowed = abap_false.
        ls_req-print_not_allowed  = abap_false.

        TRY.
            CALL METHOD /ui2/cl_json=>serialize
              EXPORTING
                data        = ls_req
                pretty_name = /ui2/cl_json=>pretty_mode-camel_case
              RECEIVING
                r_json      = DATA(lv_body).

          CATCH cx_root INTO DATA(lc_root).
            DATA(lv_message) = lc_root->get_longtext( ).
        ENDTRY.

        lo_request->set_text(
          EXPORTING
            i_text = lv_body
        ).

        TRY.
            DATA(lo_response) = lo_client->execute(
              i_method = if_web_http_client=>post
*             i_timeout = 0
            ).
          CATCH  cx_web_http_client_error INTO lo_client_error.
            ev_response_code = 500.
            ev_response_text = lo_client_error->get_text( ).
            EXIT.
        ENDTRY.
        DATA(lv_response) = lo_response->get_text( ).
        DATA(ls_status)   = lo_response->get_status( ).

        TRY.
            CALL METHOD /ui2/cl_json=>deserialize
              EXPORTING
                json          = lv_response
                assoc_arrays  = abap_true
                name_mappings = VALUE #( ( json = 'fileContent' abap = 'FILECONTENT' ) )
              CHANGING
                data          = ls_response.
          CATCH cx_root INTO lc_root.
            lv_message = lc_root->get_longtext( ).
        ENDTRY.

        ev_pdf = ls_response-filecontent.

        IF ev_pdf IS INITIAL.
          ev_response_code = 400.
          ev_response_text = 'Çıktı alınırken hata alındı.'.
        ENDIF.

      CATCH cx_fp_fdp_error cx_fp_ads_util INTO DATA(ls_data).
        lv_message = ls_data->get_longtext( ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_queue_names.

*    DATA:
**      lt_business_data TYPE TABLE OF zads_cm_print_queue=>tys_print_queue_info,
*      lo_http_client   TYPE REF TO if_web_http_client,
*      lo_client_proxy  TYPE REF TO /iwbep/if_cp_client_proxy,
*      lo_request       TYPE REF TO /iwbep/if_cp_request_read_list,
*      lo_response      TYPE REF TO /iwbep/if_cp_response_read_lst.
*
*    TRY.
*        " Create http client
*        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
*                                                     comm_scenario  = 'ZADS_CS_PRINT_QUEUE'
*                                                     comm_system_id = 'ZSNC_CSYS_GENERAL'
*                                                     service_id     = 'ZADS_OS_PRINT_QUEUE_REST' ).
*
*        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
*        lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
*          EXPORTING
*             is_proxy_model_key       = VALUE #( repository_id       = 'DEFAULT'
*                                                 proxy_model_id      = 'ZADS_CM_PRINT_QUEUE'
*                                                 proxy_model_version = '0001' )
*            io_http_client             = lo_http_client
*            iv_relative_service_root   = '/sap/opu/odata/sap/API_CLOUD_PRINT_PULL_SRV' ).
*
*        ASSERT lo_http_client IS BOUND.


*        DATA(lo_function_resource) = lo_client_proxy->create_resource_for_function( zads_cm_print_queue=>gcs_function_import-get_print_queues_of_user ).

*        DATA(lo_function_request) = lo_function_resource->create_request( ).

*        DATA(lo_function_response) = lo_function_request->execute( /iwbep/if_cp_request_function=>gcs_http_method-get ).
*
*        lo_function_response->get_business_data( IMPORTING ea_response_data =  lt_business_data ).
*
*        et_queue_names = CORRESPONDING #( lt_business_data ).
*
*      CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
*
*      CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
*
*      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
*
*      CATCH cx_http_dest_provider_error.
*
*    ENDTRY.

  ENDMETHOD.


  METHOD send_queue.
    DATA: ls_printtask  TYPE ty_printtasks,
          lv_print_data TYPE xstring,
          lv_err_msg    TYPE string.

    GET TIME STAMP FIELD DATA(lv_timestamp).

    CONVERT TIME STAMP lv_timestamp TIME ZONE 'TURKEY'
            INTO DATE DATA(lv_date) TIME DATA(lv_time).

    lv_print_data = cl_web_http_utility=>decode_x_base64( encoded = iv_pdf ).
    DATA(lv_qitem_id) = cl_print_queue_utils=>create_queue_itemid( ).

    cl_print_queue_utils=>create_queue_item_by_data(
      EXPORTING
        iv_qname            = iv_queue_name
        iv_print_data       = lv_print_data
        iv_name_of_main_doc = COND #( WHEN iv_document_name IS NOT INITIAL
                                      THEN |{ iv_document_name }_{ lv_date+6(2) }-{ lv_date+4(2) }-{ lv_date(4) }_{ lv_time(2) }{ lv_time+2(2) }{ lv_time+4(2) }|
                                      ELSE |{ iv_queue_name }_{ lv_date+6(2) }-{ lv_date+4(2) }-{ lv_date(4) }_{ lv_time(2) }{ lv_time+2(2) }{ lv_time+4(2) }| )
        iv_itemid           = lv_qitem_id
        iv_pages            = iv_pages
        iv_number_of_copies = iv_number_of_copies
      IMPORTING
        ev_err_msg          = lv_err_msg
    ).

    IF lv_err_msg IS INITIAL.
      ev_response_code = 200.
      ev_response_text = 'Çıktı başarılı bir şekilde yazıcıya aktarıldı.'.
    ELSE.
      ev_response_code = 400.
      ev_response_text = lv_err_msg.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
