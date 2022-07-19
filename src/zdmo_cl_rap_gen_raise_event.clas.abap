CLASS zdmo_cl_rap_gen_raise_event DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    EVENTS:
      generation_event
        EXPORTING
          VALUE(rap_generator_bo_name) TYPE string
        .

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZDMO_CL_RAP_GEN_RAISE_EVENT IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    out->write( 'before raise event'  && sy-uzeit ).    "test raise event
    DATA(href) = NEW zdmo_cl_rap_gen_via_event(  ).
*  SET HANDLER href->meth FOR ALL INSTANCES.

    SET HANDLER href->generate_bo_on_event FOR ALL INSTANCES.
    RAISE EVENT generation_event EXPORTING rap_generator_bo_name = 'Test'.
    out->write( 'after raise event' && sy-uzeit ).
  ENDMETHOD.
ENDCLASS.
