*&---------------------------------------------------------------------*
*& Report ZFORM_PERSONS_RQ
*&---------------------------------------------------------------------*
REPORT zform_persons_rq.

TABLES: zform_persons_1r.

*---------------------------------------------------------------------*
*       Classe de tratamento de eventos do ALV
*---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_toolbar FOR EVENT added_function OF cl_salv_events_table
      IMPORTING e_salv_function.
ENDCLASS.

TYPES: BEGIN OF ty_person_del, "estrutura onde mostra na alv os utilizadores eliminados
         person_number  TYPE zform_persons_1r-person_number,
         name           TYPE zform_persons_1r-name,
         age            TYPE i,
         country_name   TYPE t005t-landx,
         marital_status TYPE zform_persons_1r-marital_status,
         phone_number   TYPE zform_persons_1r-phone_number,
         email          TYPE zform_persons_1r-email,
         gender         TYPE zform_persons_1r-gender,
         deleted        TYPE zform_persons_1r-deleted,
*         created_date   TYPE zform_persons_1r-created_date, para inserir na alv
*         created_by     TYPE zform_persons_1r-created_by,
       END OF ty_person_del.

TYPES: BEGIN OF ty_person,
         person_number  TYPE zform_persons_1r-person_number,
         name           TYPE zform_persons_1r-name,
         age            TYPE i,
         country_name   TYPE t005t-landx,
         marital_status TYPE zform_persons_1r-marital_status,
         phone_number   TYPE zform_persons_1r-phone_number,
         email          TYPE zform_persons_1r-email,
         gender         TYPE zform_persons_1r-gender,
       END OF ty_person.

DATA: gt_persons_delet TYPE STANDARD TABLE OF ty_person_del,
      gt_persons       TYPE STANDARD TABLE OF ty_person,
      lt_data          TYPE STANDARD TABLE OF zform_persons_1r,
      lv_age           TYPE i,
      go_alv           TYPE REF TO cl_salv_table.

SELECT-OPTIONS: s_person FOR zform_persons_1r-person_number,
                s_email  FOR zform_persons_1r-email,
                s_gender FOR zform_persons_1r-gender,
                s_age    FOR lv_age.

PARAMETERS: p_del AS CHECKBOX DEFAULT ' '.


CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_toolbar.
    CASE e_salv_function.
      WHEN 'ADD'.
        PERFORM insert_person.
      WHEN 'UPDATE'.
        PERFORM update_person.
      WHEN 'DELETE'.
        PERFORM delete_person.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*---------------------------------------------------------------------*
*       START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_data.
  PERFORM display_alv.

*---------------------------------------------------------------------*
*       FORM GET_DATA
*---------------------------------------------------------------------*
FORM get_data.
  DATA: lv_marital_desc TYPE string,
        lv_gender_desc  TYPE string,
        ls_line         TYPE zform_persons_1r,
        ls_outp         TYPE ty_person_del,
        ls_outp2        TYPE ty_person.

  CLEAR           gt_persons_delet.
  CLEAR lt_data.

  SELECT *
    FROM zform_persons_1r
   INTO TABLE lt_data
   WHERE person_number IN s_person
     AND email         IN s_email
     AND gender        IN s_gender.

  LOOP AT lt_data INTO ls_line.
    IF p_del = space AND ls_line-deleted = 'X'.
      CONTINUE.
    ENDIF.

    IF ls_line-date_of_birth IS INITIAL.
      CONTINUE.
    ENDIF.

    DATA(lv_birth_year)  = ls_line-date_of_birth+0(4).
    DATA(lv_birth_month) = ls_line-date_of_birth+4(2).
    DATA(lv_birth_day)   = ls_line-date_of_birth+6(2).

    DATA(lv_curr_year)   = sy-datum+0(4).
    DATA(lv_curr_month)  = sy-datum+4(2).
    DATA(lv_curr_day)    = sy-datum+6(2).

    lv_age = lv_curr_year - lv_birth_year.
    IF lv_curr_month < lv_birth_month OR
       ( lv_curr_month = lv_birth_month AND lv_curr_day < lv_birth_day ).
      lv_age = lv_age - 1.
    ENDIF.

    IF NOT lv_age IN s_age.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING ls_line TO ls_outp.
    ls_outp-age = lv_age.
*    ls_outp-created_date = ls_line-created_date.   se quiser mostrar na alv
*    ls_outp-created_by   = ls_line-created_by.

    SELECT SINGLE landx INTO ls_outp-country_name
      FROM t005t
      WHERE land1 = ls_line-country
        AND spras = sy-langu.

    CASE ls_line-marital_status.
      WHEN 'S'. lv_marital_desc = 'Solteiro'.
      WHEN 'C'. lv_marital_desc = 'Casado'.
      WHEN 'D'. lv_marital_desc = 'Divorciado'.
      WHEN 'V'. lv_marital_desc = 'Viúvo'.
      WHEN OTHERS. lv_marital_desc = ''.
    ENDCASE.

    CASE ls_line-gender.
      WHEN 'M'. lv_gender_desc = 'Masculino'.
      WHEN 'F'. lv_gender_desc = 'Feminino'.
      WHEN OTHERS. lv_gender_desc = ''.
    ENDCASE.

    ls_outp-marital_status = lv_marital_desc.
    ls_outp-gender         = lv_gender_desc.

    IF p_del = 'X'.
      APPEND ls_outp TO gt_persons_delet.
      sort gt_persons_delet by person_number DESCENDING.
    ELSE.
      MOVE-CORRESPONDING ls_outp TO ls_outp2.
      APPEND ls_outp2 TO gt_persons.
      sort gt_persons by person_number DESCENDING.
    ENDIF.

  ENDLOOP.
  " Verificação final de resultados
  IF p_del = 'X' AND gt_persons_delet IS INITIAL.
    MESSAGE 'Nenhum registo encontrado com os dados inseridos.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF p_del = '' AND gt_persons IS INITIAL.
    MESSAGE 'Nenhum registo encontrado com os dados inseridos.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM DISPLAY_ALV
*---------------------------------------------------------------------*
FORM display_alv.
  DATA: lo_columns TYPE REF TO cl_salv_columns_table,
        lo_column  TYPE REF TO cl_salv_column_table,
        lo_events  TYPE REF TO cl_salv_events_table,
        go_handler TYPE REF TO lcl_event_handler.
  TRY.
      IF p_del = 'X'.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = go_alv
          CHANGING  t_table      = gt_persons_delet ).
      ELSE.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = go_alv
          CHANGING  t_table      = gt_persons ).
      ENDIF.

      lo_columns = go_alv->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          lo_column ?= lo_columns->get_column( 'AGE' ).
          lo_column->set_short_text( 'Idade' ).
          lo_column->set_medium_text( 'Idade' ).
          lo_column->set_long_text( 'Idade' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      go_alv->set_screen_status(
        pfstatus      = 'ZSTANDARD_RQ'
        report        = sy-repid
        set_functions = go_alv->c_functions_all ).

      CREATE OBJECT go_handler.
      lo_events = go_alv->get_event( ).
      SET HANDLER go_handler->handle_toolbar FOR lo_events.

      go_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_msg).
      MESSAGE lx_msg->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INSERT_PERSON
*&---------------------------------------------------------------------*
FORM insert_person.
  DATA: lt_values   TYPE TABLE OF sval,
        lv_person   TYPE zform_persons_1r-person_number,
        lv_ret_code TYPE c.

  APPEND VALUE #( tabname = 'ZFORM_PERSONS_1R' fieldname = 'NAME' ) TO lt_values.
  APPEND VALUE #( tabname = 'ZFORM_PERSONS_1R' fieldname = 'DATE_OF_BIRTH' ) TO lt_values.
  APPEND VALUE #( tabname = 'ZFORM_PERSONS_1R' fieldname = 'PHONE_NUMBER' ) TO lt_values.
  APPEND VALUE #( tabname = 'ZFORM_PERSONS_1R' fieldname = 'COUNTRY' ) TO lt_values.
  APPEND VALUE #( tabname = 'ZFORM_PERSONS_1R' fieldname = 'MARITAL_STATUS' ) TO lt_values.
  APPEND VALUE #( tabname = 'ZFORM_PERSONS_1R' fieldname = 'EMAIL' ) TO lt_values.
  APPEND VALUE #( tabname = 'ZFORM_PERSONS_1R' fieldname = 'GENDER' ) TO lt_values.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Inserir Pessoa'
    IMPORTING
      returncode      = lv_ret_code
    TABLES
      fields          = lt_values
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc = 0.

*    if lt_values[ 1 ]-value is initial or lt_values[ 2 ]-value is initial or lt_values[ 3 ]-value is initial or lt_values[ 4 ]-value is initial or lt_values[ 5 ]-value is initial or lt_values[ 6 ]-value is initial or
*      lt_values[ 7 ]-value is initial.
*      MESSAGE 'Não foram preenchidos todos os parâmetros' TYPE 'E'.
*      return.
*      endif.

    DATA(lv_missing) = abap_false.

    LOOP AT lt_values INTO DATA(ls_val).
      IF ls_val-value IS INITIAL.
        lv_missing = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_missing = abap_true.
*      MESSAGE 'Todos os campos são obrigatórios' TYPE 'E'.
*      RETURN.
if lv_ret_code ne 'A'.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Atenção'
          txt1  = 'Todos os campos são obrigatórios!'
          txt2  = 'Por favor, preencha todos os dados.'.
      RETURN.
    ENDIF.
    endif.
    SELECT MAX( person_number ) INTO lv_person FROM zform_persons_1r.
    lv_person = lv_person + 1.

    DATA ls_new TYPE zform_persons_1r.
    ls_new-person_number  = lv_person.
    ls_new-name           = lt_values[ 1 ]-value.
    ls_new-date_of_birth  = lt_values[ 2 ]-value.
    ls_new-phone_number   = lt_values[ 3 ]-value.
    ls_new-country        = lt_values[ 4 ]-value.
    ls_new-marital_status = lt_values[ 5 ]-value.
    ls_new-email          = lt_values[ 6 ]-value.
    ls_new-gender         = lt_values[ 7 ]-value.
    ls_new-created_date   = sy-datum.
    ls_new-created_by     = sy-uname.

    INSERT zform_persons_1r FROM ls_new.
    if lv_ret_code eq ''.
    IF sy-subrc = 0.
      COMMIT WORK.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Sucesso'
          txt1  = 'Registos inseridos com sucessos!'
          txt2  = 'Por favor, preencha todos os dados.'.
      PERFORM get_data.
      go_alv->refresh( ).
    ENDIF.
  ENDIF.
  endif.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  UPDATE_PERSON
*&---------------------------------------------------------------------*
FORM update_person.
  DATA: lt_values TYPE TABLE OF sval,
        ls_row    TYPE ty_person_del,
        lt_rows   TYPE salv_t_row.

  " Verifica seleção
  DATA(lo_selections) = go_alv->get_selections( ).
  lt_rows = lo_selections->get_selected_rows( ).

  IF lines( lt_rows ) = 0.
    MESSAGE 'Nenhuma linha selecionada' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  READ TABLE gt_persons_delet INDEX lt_rows[ 1 ] INTO ls_row.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  APPEND VALUE #( tabname = 'ZFORM_PERSONS_1R' fieldname = 'PHONE_NUMBER' value = ls_row-phone_number ) TO lt_values.
  APPEND VALUE #( tabname = 'ZFORM_PERSONS_1R' fieldname = 'MARITAL_STATUS' value = ls_row-marital_status ) TO lt_values.
  APPEND VALUE #( tabname = 'ZFORM_PERSONS_1R' fieldname = 'EMAIL' value = ls_row-email ) TO lt_values.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title = 'Atualizar Pessoa'
    TABLES
      fields      = lt_values.

  IF sy-subrc = 0.
    DATA(lv_phone)   = lt_values[ 1 ]-value.
    DATA(lv_status)  = lt_values[ 2 ]-value.
    DATA(lv_email)   = lt_values[ 3 ]-value.

    UPDATE zform_persons_1r SET
      phone_number   = lv_phone
      marital_status = lv_status
      email          = lv_email
    WHERE person_number = ls_row-person_number.

    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE 'Registo atualizado com sucesso' TYPE 'S'.
      PERFORM get_data.
      go_alv->refresh( ).
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETE_PERSON
*&---------------------------------------------------------------------*
FORM delete_person.
  DATA: ls_row    TYPE ty_person_del,
        lv_answer TYPE c LENGTH 1,
        lt_rows   TYPE salv_t_row.

  " Verifica seleção
  DATA(lo_selections) = go_alv->get_selections( ).
  lt_rows = lo_selections->get_selected_rows( ).

  IF lines( lt_rows ) = 0.
    MESSAGE 'Nenhuma linha selecionada' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  READ TABLE gt_persons_delet INDEX lt_rows[ 1 ] INTO ls_row.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar      = 'Confirmação'
      text_question = |Deseja eliminar o número { ls_row-person_number }?|
      text_button_1 = 'Sim'
      text_button_2 = 'Não'
    IMPORTING
      answer        = lv_answer.

  IF lv_answer = '1'.
    UPDATE zform_persons_1r SET
      deleted      = 'X'
      changed_date = sy-datum
      changed_by   = sy-uname
    WHERE person_number = ls_row-person_number.

    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE 'Registo eliminado com sucesso' TYPE 'S'.
      PERFORM get_data.
      go_alv->refresh( ).
    ENDIF.
  ENDIF.
ENDFORM.
