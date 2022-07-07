CLASS zdmo_cl_rap_gen_via_event DEFINITION
  PUBLIC

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS : selection_name        TYPE c LENGTH 8   VALUE 'I_VIEW',
                selection_description TYPE c LENGTH 255 VALUE 'RAP business object name'.

    INTERFACES if_oo_adt_classrun.

*    CLASS-METHODS:
    METHODS generate_bo_on_event FOR EVENT generation_event OF zdmo_cl_rap_gen_raise_event
      IMPORTING
        rap_generator_bo_name.

  PROTECTED SECTION.
    METHODS create_transport RETURNING VALUE(lo_transport) TYPE sxco_transport.
  PRIVATE SECTION.

    CONSTANTS:
      co_rap_generator_package TYPE sxco_package VALUE 'ZDMO_RAP_GENERATOR'.



    DATA package_name TYPE sxco_package.
ENDCLASS.



CLASS ZDMO_CL_RAP_GEN_VIA_EVENT IMPLEMENTATION.


  METHOD create_transport.

    "transport creation checks whether being on cloud or on prem

    DATA xco_lib TYPE REF TO zdmo_cl_rap_xco_lib.

    DATA(xco_on_prem_library) = NEW zdmo_cl_rap_xco_on_prem_lib(  ).

    IF xco_on_prem_library->on_premise_branch_is_used(  ) = abap_true.
      xco_lib = NEW zdmo_cl_rap_xco_on_prem_lib(  ).
    ELSE.
      xco_lib = NEW zdmo_cl_rap_xco_cloud_lib(  ).
    ENDIF.


*    DATA(ls_package) = xco_cp_abap_repository=>package->for( co_rap_generator_package )->read( ).

    DATA(ls_package) = xco_lib->get_package( package_name )->read(  ).

    "  DATA(ls_package) = xco_cp_abap_repository=>package->for( co_rap_generator_package )->read( ).
    DATA(lv_transport_layer) = ls_package-property-transport_layer->value.
    DATA(lv_transport_target) = ls_package-property-transport_layer->get_transport_target( )->value.
    DATA(lo_transport_request) = xco_cp_cts=>transports->workbench( lv_transport_target )->create_request( |RAP Generator Application Job Catalog Entry and Job Template| ).



* IF lo_transport_request->get_status( ) = xco_cp_transport=>filter->status( xco_cp_transport=>status->modifiable ).
* DATA(lo_transport_modifiable) = abap_true.
* ENDIF.



    lo_transport = lo_transport_request->value.

  ENDMETHOD.


  METHOD generate_bo_on_event.

    DATA json_string TYPE string.
    DATA messages TYPE string_table.
    DATA(on_prem_xco_lib) = NEW zdmo_cl_rap_xco_on_prem_lib(  ).

    DATA zdmo_test_line TYPE zdmo_test.
    zdmo_test_line-test = 'hugo6'.
    update zdmo_test FROM @zdmo_test_line.
    WAIT up to 10 seconds.
    commit work.
*    assert 1 = 2.
return.
    TRY.
        IF on_prem_xco_lib->on_premise_branch_is_used(  ).
          DATA(application_log) = cl_bali_log=>create_with_header(
                          header = cl_bali_header_setter=>create( object = 'XCO_DEMO'
                                                                  subobject = 'DEMO'
                                                                  external_id = 'External ID' ) )..
        ELSE.
          application_log = cl_bali_log=>create_with_header(
                          header = cl_bali_header_setter=>create( object = zdmo_cl_rap_node=>application_log_object_name
                                                                  subobject = zdmo_cl_rap_node=>application_log_sub_obj1_name
                                                                  external_id = 'External ID' ) ).
        ENDIF.

      CATCH cx_bali_runtime INTO DATA(application_log_exception).

        DATA(bali_log_exception) = application_log_exception->get_text(  ).

        RAISE EXCEPTION TYPE cx_apj_rt_content
          EXPORTING
            previous = application_log_exception.

    ENDTRY.



    SELECT SINGLE * FROM zdmo_r_rapgeneratorbo  WHERE boname = @rap_generator_bo_name
    INTO @DATA(rap_generator_bo).

    IF sy-subrc = 0.
      json_string = rap_generator_bo-jsonstring.
    ELSE.
      RAISE EXCEPTION TYPE zdmo_cx_rap_generator
        EXPORTING
          textid   = zdmo_cx_rap_generator=>root_cause_exception
          mv_value = |BO name { rap_generator_bo_name } not found in ZDMO_r_rapgeneratorbo |.
    ENDIF.

    DATA(xco_on_prem_library) = NEW zdmo_cl_rap_xco_on_prem_lib(  ).

    DATA(todo) = NEW zdmo_cl_rap_node(  ).
    TRY.
        CASE rap_generator_bo-packagelanguageversion.

          WHEN zdmo_cl_rap_node=>package_abap_language_version-standard.
            DATA(rap_generator_on_prem) = zdmo_cl_rap_generator_on_prem=>create_for_on_prem_development( json_string ).
            DATA(framework_messages) = rap_generator_on_prem->generate_bo( ).
            APPEND |RAP BO { rap_generator_on_prem->get_rap_bo_name(  ) }  generated successfully| TO messages.

          WHEN zdmo_cl_rap_node=>package_abap_language_version-abap_for_sap_cloud_platform.

            "If in on premise systems packages with the language version abap_for_sap_cloud_platform are used
            "we have to use the xco_cp libraries for generation and
            "we have to use the xco on prem libraries for reading

            IF xco_on_prem_library->on_premise_branch_is_used(  ) = abap_true.
              DATA(rap_generator) = zdmo_cl_rap_generator=>create_for_on_prem_development( json_string ).
            ELSE.
              rap_generator = zdmo_cl_rap_generator=>create_for_cloud_development( json_string ).
            ENDIF.

            framework_messages = rap_generator->generate_bo( ).
            APPEND |RAP BO { rap_generator->get_rap_bo_name(  ) }  generated successfully| TO messages.

          WHEN OTHERS.
            RAISE EXCEPTION TYPE zdmo_cx_rap_generator
              EXPORTING
                textid   = zdmo_cx_rap_generator=>root_cause_exception
                mv_value = |abap language version '{ rap_generator_bo-packagelanguageversion }' is not supported |.

        ENDCASE.



        APPEND |Messages from framework:| TO messages.
        LOOP AT framework_messages INTO DATA(framework_message).
          APPEND framework_message-message TO messages.

          DATA(application_log_free_text) = cl_bali_free_text_setter=>create(
                            severity = if_bali_constants=>c_severity_status
                            text = CONV #( framework_message-message ) ).
          application_log_free_text->set_detail_level( detail_level = '1' ).
          application_log->add_item( item = application_log_free_text ).

        ENDLOOP.
        cl_bali_log_db=>get_instance( )->save_log(
                                                   log = application_log
                                                   assign_to_current_appl_job = abap_true
                                                   ).
      CATCH zdmo_cx_rap_generator INTO DATA(rap_generator_exception).

        DATA(rap_generator_exception_text) = rap_generator_exception->get_text(  ).

        application_log->add_item( item = cl_bali_exception_setter=>create(
                                     severity = if_bali_constants=>c_severity_error
                                     exception = rap_generator_exception ) ).

        cl_bali_log_db=>get_instance( )->save_log(
                                                   log = application_log
                                                   assign_to_current_appl_job = abap_true
                                                   ).

        RAISE EXCEPTION TYPE cx_apj_rt_content
          EXPORTING
            previous = rap_generator_exception.

    ENDTRY.


  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    "test raise event
    DATA(href) = NEW zdmo_cl_rap_gen_via_event(  ).
*  SET HANDLER href->meth FOR ALL INSTANCES.

    SET HANDLER href->generate_bo_on_event FOR ALL INSTANCES.
    out->write( sy-subrc ).
  ENDMETHOD.
ENDCLASS.
