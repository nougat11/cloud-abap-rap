class zdmo_cl_rap_generator_setup definition
  public
  inheriting from cl_xco_cp_adt_simple_classrun
  final
  create public .

  public section.

    data package_name_of_rap_generator type sxco_package read-only.

    methods constructor raising zdmo_cx_rap_generator.
    methods create_application_log_entry returning value(r_application_log_object_name) type string raising zdmo_cx_rap_generator. "if_bali_object_handler=>ty_object RAISING zdmo_cx_rap_generator.
    methods create_job_catalog_entry returning value(r_job_catalog_name) type string raising zdmo_cx_rap_generator. "TYPE cl_apj_dt_create_content=>ty_catalog_name RAISING zdmo_cx_rap_generator.
    methods create_job_template_entry returning value(r_job_template_name) type string raising zdmo_cx_rap_generator. " TYPE cl_apj_dt_create_content=>ty_template_name RAISING zdmo_cx_rap_generator.
    methods create_service_binding returning value(r_service_binding_mame) type string raising zdmo_cx_rap_generator. " TYPE  sxco_ao_object_name RAISING zdmo_cx_rap_generator.


  protected section.
    methods: main redefinition.
  private section.

    types: begin of t_longtext,
             msgv1(50),
             msgv2(50),
             msgv3(50),
             msgv4(50),
           end of t_longtext.

    data transport_request type sxco_transport .
    data xco_on_prem_library type ref to zdmo_cl_rap_xco_on_prem_lib  .
    data package_of_rap_generator type ref to if_xco_package.
    data xco_lib type ref to zdmo_cl_rap_xco_lib.

    methods create_transport returning value(r_transport_request) type sxco_transport.



endclass.



class zdmo_cl_rap_generator_setup implementation.


  method main.

    try.
        data(application_log_object_name) = create_application_log_entry(  ).
        out->write( |{ application_log_object_name } | ).  ##NO_TEXT
      catch zdmo_cx_rap_generator into data(rap_generator_setup_exception).
        out->write( rap_generator_setup_exception->get_text(  ) ).
    endtry.
    try.
        data(job_catalog_name) = create_job_catalog_entry(  ).
        out->write( |{ job_catalog_name } | ).  ##NO_TEXT
      catch zdmo_cx_rap_generator into rap_generator_setup_exception.
        out->write( rap_generator_setup_exception->get_text(  ) ).
    endtry.
    try.
        data(job_template_name) = create_job_template_entry(  ).
        out->write( |{ job_template_name } | ).  ##NO_TEXT
      catch zdmo_cx_rap_generator into rap_generator_setup_exception.
        out->write( rap_generator_setup_exception->get_text(  ) ).
    endtry.
    try.
        data(service_binding_name) = create_service_binding(  ).
        out->write( |{ service_binding_name } | ).
      catch zdmo_cx_rap_generator into rap_generator_setup_exception.
        out->write( rap_generator_setup_exception->get_text(  ) ).
    endtry.
  endmethod.


  method create_transport.

    data longtext      type t_longtext.
    data transport_request_description type sxco_ar_short_description value 'RAP Generator Application Job Catalog Entry and Job Template'.
    data package_name_to_check type sxco_package  .


    package_name_to_check = package_of_rap_generator->name.
    try.
        while xco_lib->get_package( package_name_to_check )->read( )-property-transport_layer->value = '$SPL'.
          package_name_to_check = xco_lib->get_package( package_name_to_check )->read( )-property-super_package->name.
        endwhile.
        data(transport_target) = xco_lib->get_package( package_name_to_check
          )->read( )-property-transport_layer->get_transport_target( ).
        data(transport_target_name) = transport_target->value.
        r_transport_request = xco_cp_cts=>transports->workbench( transport_target_name )->create_request( transport_request_description )->value.
      catch cx_root into data(exc_getting_transport_target).
        clear r_transport_request.
        longtext = exc_getting_transport_target->get_text( ).
        raise exception new zdmo_cx_rap_generator( textid     = zdmo_cx_rap_generator=>job_scheduling_error
                                                   mv_value   = conv #( longtext-msgv1 )
                                                   mv_value_2 = conv #( longtext-msgv2 )
                                                   previous   = exc_getting_transport_target
                                                   ).
    endtry.
  endmethod.


  method create_job_catalog_entry.

    data longtext      type t_longtext.
    data(lo_dt) = cl_apj_dt_create_content=>get_instance( ).

    clear r_job_catalog_name.

    " Create job catalog entry (corresponds to the former report incl. selection parameters)
    " Provided implementation class iv_class_name shall implement two interfaces:
    " - if_apj_dt_exec_object to provide the definition of all supported selection parameters of the job
    "   (corresponds to the former report selection parameters) and to provide the actual default values
    " - if_apj_rt_exec_object to implement the job execution

    try.
        lo_dt->create_job_cat_entry(
            iv_catalog_name       = zdmo_cl_rap_node=>job_catalog_name
            iv_class_name         = zdmo_cl_rap_node=>job_class_name
            iv_text               = zdmo_cl_rap_node=>job_catalog_text
            iv_catalog_entry_type = cl_apj_dt_create_content=>class_based
            iv_transport_request  = transport_request
            iv_package            = package_of_rap_generator->name
        ).

        r_job_catalog_name = |Job catalog { zdmo_cl_rap_node=>job_catalog_name } created succesfully|. " zdmo_cl_rap_node=>job_catalog_name.

      catch cx_apj_dt_content into data(lx_apj_dt_content).

        if not ( lx_apj_dt_content->if_t100_message~t100key-msgno = cx_apj_dt_content=>cx_object_already_exists-msgno and
                 lx_apj_dt_content->if_t100_message~t100key-msgid = cx_apj_dt_content=>cx_object_already_exists-msgid and
                 lx_apj_dt_content->object = 'ZDMO_RAP_GEN_CATATALOG_ENTRY' ).
          longtext = lx_apj_dt_content->get_text( ).
          raise exception new zdmo_cx_rap_generator( textid     = zdmo_cx_rap_generator=>job_scheduling_error
                                                     mv_value   = conv #( longtext-msgv1 )
                                                     mv_value_2 = conv #( longtext-msgv2 )
                                                     previous   = lx_apj_dt_content
                                                     ).
        else.
          r_job_catalog_name = lx_apj_dt_content->get_text(  ).
        endif.
    endtry.
  endmethod.


  method create_job_template_entry.

    " Create job template (corresponds to the former system selection variant) which is mandatory
    " to select the job later on in the Fiori app to schedule the job
    data lt_parameters type if_apj_dt_exec_object=>tt_templ_val.

    data longtext      type t_longtext.

    clear r_job_template_name.

    data(lo_dt) = cl_apj_dt_create_content=>get_instance( ).
    try.
        lo_dt->create_job_template_entry(
            iv_template_name     = zdmo_cl_rap_node=>job_template_name
            iv_catalog_name      = zdmo_cl_rap_node=>job_catalog_name
            iv_text              = zdmo_cl_rap_node=>job_template_text
            it_parameters        = lt_parameters
            iv_transport_request = transport_request
            iv_package           = package_of_rap_generator->name
        ).

        r_job_template_name = |Job template { zdmo_cl_rap_node=>job_template_name } generated successfully|."zdmo_cl_rap_node=>job_template_name.

      catch cx_apj_dt_content into data(lx_apj_dt_content).
        if  not ( lx_apj_dt_content->if_t100_message~t100key-msgno = cx_apj_dt_content=>cx_object_already_exists-msgno and
                 lx_apj_dt_content->if_t100_message~t100key-msgid = cx_apj_dt_content=>cx_object_already_exists-msgid and
                 lx_apj_dt_content->object = 'ZDMO_RAP_GEN_JOB_TEMPLATE' ).
          longtext = lx_apj_dt_content->get_text( ).
          raise exception new zdmo_cx_rap_generator( textid     = zdmo_cx_rap_generator=>job_scheduling_error
                                                     mv_value   = conv #( longtext-msgv1 )
                                                     mv_value_2 = conv #( longtext-msgv2 )
                                                     previous   = lx_apj_dt_content
                                                     ).
        else.
          r_job_template_name = lx_apj_dt_content->get_text(  ).
        endif.
    endtry.
  endmethod.


  method create_application_log_entry.
    data longtext      type t_longtext.

    clear  r_application_log_object_name.

    if xco_on_prem_library->on_premise_branch_is_used(  ).

      "use xco sample application log object
      r_application_log_object_name =  'Application log object XCO_DEMO will be used'.

    else.

      data(application_log_sub_objects) = value if_bali_object_handler=>ty_tab_subobject(
                                              ( subobject = zdmo_cl_rap_node=>application_log_sub_obj1_name
                                                subobject_text = zdmo_cl_rap_node=>application_log_sub_obj1_text )
                                             "( subobject = '' subobject_text = '' )
                                              ).

      data(lo_log_object) = cl_bali_object_handler=>get_instance( ).

      try.
          lo_log_object->create_object( exporting iv_object = zdmo_cl_rap_node=>application_log_object_name
                                                  iv_object_text = zdmo_cl_rap_node=>application_log_object_text
                                                  it_subobjects = application_log_sub_objects
                                                  iv_package = package_of_rap_generator->name
                                                  iv_transport_request = transport_request ).

          r_application_log_object_name = zdmo_cl_rap_node=>application_log_object_name.

        catch cx_bali_objects into data(lx_bali_objects).

          if  not ( lx_bali_objects->if_t100_message~t100key-msgno = '602' and
                    lx_bali_objects->if_t100_message~t100key-msgid = 'BL' ).
            "MSGID   BL  SYMSGID C   20
            "MSGNO   602 SYMSGNO N   3
            longtext = lx_bali_objects->get_text( ).
            raise exception new zdmo_cx_rap_generator( textid     = zdmo_cx_rap_generator=>job_scheduling_error
                                                       mv_value   = conv #( longtext-msgv1 )
                                                       mv_value_2 = conv #( longtext-msgv2 )
                                                       previous   = lx_bali_objects
                                                       ).

          else.
            r_application_log_object_name = |Application log object { zdmo_cl_rap_node=>application_log_object_name } already exists|.
          endif.
      endtry.

    endif.
  endmethod.


  method constructor.

    super->constructor( ).

    xco_on_prem_library = new zdmo_cl_rap_xco_on_prem_lib(  ).

    "check whether being on cloud or on prem
    if xco_on_prem_library->on_premise_branch_is_used(  ) = abap_true.
      xco_lib = new zdmo_cl_rap_xco_on_prem_lib(  ).
    else.
      xco_lib = new zdmo_cl_rap_xco_cloud_lib(  ).
    endif.

    package_of_rap_generator = xco_lib->get_class( 'ZDMO_CL_RAP_NODE' )->if_xco_ar_object~get_package(  ).
    package_name_of_rap_generator = package_of_rap_generator->name.

    if xco_lib->get_package( package_name_of_rap_generator  )->read( )-property-record_object_changes = abap_true.
      transport_request = create_transport(  ).
    else.
      clear transport_request.
    endif.

**    transport_request = create_transport(  ).



  endmethod.


  method create_service_binding.

    data service_binding_name type sxco_srvb_object_name  value 'ZDMO_UI_RAP_GENERATOR_O2'.
    data service_definition_name type  sxco_srvd_object_name  value 'ZDMO_RAPGENERATORBO'.
    data longtext      type t_longtext.


********************************************************************************
    "cloud
    DATA mo_environment TYPE REF TO if_xco_cp_gen_env_dev_system.
    DATA mo_srvb_put_operation    TYPE REF TO if_xco_cp_gen_d_o_put .
********************************************************************************
    "onpremise
*    data mo_environment           type ref to if_xco_gen_environment .
*    data mo_srvb_put_operation    type ref to if_xco_gen_o_mass_put.
********************************************************************************

    data(service_definition) = xco_lib->get_service_definition( service_definition_name ).

    if service_definition is initial.
      raise exception new zdmo_cx_rap_generator( textid     = zdmo_cx_rap_generator=>job_scheduling_error
                                                 mv_value   = conv #( service_definition_name )
                                                 mv_value_2 = conv #( 'does not exist' )
                                                 ).
    endif.

    data(service_binding) = xco_lib->get_service_binding( service_binding_name ).

    if service_binding->if_xco_ar_object~exists(  ) = abap_true.
      r_service_binding_mame = |Service binding { service_binding_name } does already exist|.
      exit.
    endif.

    try.


**********************************************************************
        "cloud
    mo_environment = xco_cp_generation=>environment->dev_system( transport_request )  .
    mo_srvb_put_operation = mo_environment->create_put_operation( ).
**********************************************************************
        "on premise
*        if transport_request is not initial.
*          mo_environment = xco_generation=>environment->transported( transport_request ).
*        else.
*          mo_environment = xco_generation=>environment->local.
*        endif.
*
*        mo_srvb_put_operation = mo_environment->create_mass_put_operation( ).

**********************************************************************

        data(specification_srvb) = mo_srvb_put_operation->for-srvb->add_object(   service_binding_name
                                        )->set_package( package_name_of_rap_generator
                                        )->create_form_specification( ).

        specification_srvb->set_short_description( |Service binding for RAP Generator| ) ##no_text.

        specification_srvb->set_binding_type( xco_cp_service_binding=>binding_type->odata_v2_ui ).

        specification_srvb->add_service( )->add_version( '0001' )->set_service_definition( service_definition_name ).

        data(result) = mo_srvb_put_operation->execute(  ).


        r_service_binding_mame = |Service binding { service_binding_name } generated successfully|.
*        DATA(findings) = result->findings.
*        DATA(findings_list) = findings->get( ).
      catch cx_root into data(cx_root).

        longtext = cx_root->get_text( ).
        raise exception new zdmo_cx_rap_generator( textid     = zdmo_cx_rap_generator=>root_cause_exception
                                                   mv_value   = conv #( longtext-msgv1 )
                                                   mv_value_2 = conv #( longtext-msgv2 )
                                                   previous   = cx_root
                                                   ).
    endtry.


  endmethod.
endclass.
