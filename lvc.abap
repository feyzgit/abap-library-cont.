*&---------------------------------------------------------------------*
*& Report ZFK_P_ISLEM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_p_islem_lvc.

TABLES: zfk_islem.

DATA: gt_ogrenci TYPE TABLE OF zfk_ogrenci,
      gs_ogrenci TYPE  zfk_ogrenci,
      gt_kitap   TYPE TABLE OF zfk_kitap,
      gs_kitap   TYPE  zfk_kitap,
      gt_yazar   TYPE TABLE OF zfk_yazar,
      gs_yazar   TYPE  zfk_yazar,
      gt_tur     TYPE TABLE OF zfk_tur,
      gs_tur     TYPE  zfk_tur,
      gt_islem   TYPE TABLE OF zfk_islem,
      gs_islem   TYPE  zfk_islem,
      gt_report  TYPE TABLE OF zfk_s_islem,
      gs_report  TYPE zfk_s_islem,
      gv_note    TYPE c LENGTH 50.


DATA:
  gt_fieldcat TYPE lvc_t_fcat,
  gs_fieldcat TYPE lvc_s_fcat,
  gs_layout   TYPE lvc_s_layo.



DATA: BEGIN OF gt_ozet OCCURS 0,
        ogrencino    LIKE zfk_s_islem-ogrencino,
        adısoyadı    LIKE zfk_s_islem-adisoyadi,
        toplamkitap  LIKE zfk_s_ozet-toplamkitap,
        kalankitap   LIKE zfk_s_ozet-kalankitap,
        gecikenkitap LIKE zfk_s_ozet-gecikenkitap,
        teslimkitap  LIKE zfk_s_ozet-teslimkitap,
      END OF gt_ozet.
DATA: gs_ozet LIKE LINE OF gt_ozet.






SELECTION-SCREEN BEGIN OF BLOCK b1.
SELECT-OPTIONS: s_ktp_no FOR zfk_islem-kitapno,
                s_ogr_no FOR zfk_islem-ogrencino.

SELECTION-SCREEN END OF BLOCK b1.

LOAD-OF-PROGRAM.

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

START-OF-SELECTION.

  PERFORM f_report_data.

END-OF-SELECTION.

  PERFORM f_merge.
  PERFORM f_display.



FORM f_merge.
  PERFORM f_fill_cat.

*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_structure_name = 'ZFK_S_ISLEM'
*    CHANGING
*      ct_fieldcat      = gt_fieldcat.
CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
  EXPORTING
*   I_BUFFER_ACTIVE  =
    i_structure_name = 'ZFK_S_ISLEM'
*   I_CLIENT_NEVER_DISPLAY       = 'X'
*   I_BYPASSING_BUFFER           =
*   I_INTERNAL_TABNAME           =
  CHANGING
    ct_fieldcat      = gt_fieldcat
*   EXCEPTIONS
*   INCONSISTENT_INTERFACE       = 1
*   PROGRAM_ERROR    = 2
*   OTHERS           = 3
  .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

LOOP AT gt_fieldcat REFERENCE INTO DATA(r_fcat).
  CASE r_fcat->fieldname.
    WHEN 'SELKZ'.
      r_fcat->tech = 'X'.
    WHEN OTHERS.
  ENDCASE.
ENDLOOP.
ENDFORM.

FORM sub_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'GUI_STATUS'.
*    data: ls_extab like LINE OF rt_extab.
*    ls_extab-fcode = '&KTPVER'.
*    APPEND ls_extab to rt_extab.
*
  SET PF-STATUS 'GUI_STATUS' EXCLUDING rt_extab.

ENDFORM.

FORM user_command USING b_ucomm LIKE sy-ucomm
  rs_selfield TYPE slis_selfield.

  CASE b_ucomm.
    WHEN '&BACK' OR '&EXIT' OR '&CANC'.
      LEAVE TO SCREEN 0.

    WHEN '&KTPVER'.
      CLEAR: zfk_islem, gv_note.
      CALL SCREEN 2000 STARTING AT 10 10
                       ENDING AT 70 30.

    WHEN '&KTPTESLIM'.

      DATA: lv_sayac TYPE i.
      CLEAR lv_sayac.
      LOOP AT  gt_report INTO gs_report WHERE selkz = 'X' AND
      kitapteslim IS INITIAL.
        ADD 1 TO lv_sayac.
      ENDLOOP.
      IF lv_sayac EQ 1.
        CALL SCREEN 0200 STARTING AT 10 10
                                 ENDING AT 70 30.
      ELSE.
        IF lv_sayac GT 1.
          MESSAGE 'Lütfen 1 satır seçiniz.' TYPE 'I'.
        ELSE.
          MESSAGE 'Lütfen geçerli bir satır seçiniz.' TYPE 'I'.
        ENDIF.

      ENDIF.

    WHEN '&SIL'.
      READ TABLE gt_report INTO gs_report WITH KEY selkz = 'X'.
      IF sy-subrc = 0.
        DATA: ans TYPE c.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Title POPUP_TO_CONFIRM'
            text_question         = 'işlem silinsin mi?'
            text_button_1         = 'Tamam'
            icon_button_1         = 'ICON_CHECKED'
            text_button_2         = 'Çıkış'
            icon_button_2         = 'ICON_CANCEL'
            display_cancel_button = ' '
            popup_type            = 'ICON_MESSAGE_ERROR'
          IMPORTING
            answer                = ans.
        IF ans = 1.
          LOOP AT gt_report INTO gs_report WHERE selkz = 'X'.
*        READ TABLE gt_report INTO gs_report WITH KEY selkz = 'X'.
*        IF sy-subrc = 0.
            DELETE gt_report WHERE islemno = gs_report-islemno.
            DELETE FROM zfk_islem WHERE islemno = gs_report-islemno .
            MESSAGE 'İşlem silindi.' TYPE 'I'.
*        ENDIF.
          ENDLOOP.
        ELSE.
          RETURN.
        ENDIF.
      ELSE.
        MESSAGE 'Geçerli bir satır seçin.' TYPE 'I'.
      ENDIF.
    WHEN '&DEGIS'.

      DATA: lv_sayac1 TYPE i.
      CLEAR lv_sayac1.
      LOOP AT  gt_report INTO gs_report WHERE selkz = 'X'.
        ADD 1 TO lv_sayac1.
      ENDLOOP.
      IF lv_sayac1 EQ 1.
        CALL SCREEN 0300 STARTING AT 10 10
                                 ENDING AT 70 30.
      ELSE.
        IF lv_sayac1 GT 1.
          MESSAGE 'Lütfen 1 satır seçiniz.' TYPE 'I'.
        ELSE.
          MESSAGE 'Lütfen geçerli bir satır seçiniz.' TYPE 'I'.
        ENDIF.
      ENDIF.

    WHEN '&OZET'.
      PERFORM f_data_ozet.
      PERFORM f_merge_ozet.
      PERFORM f_display_ozet.



  ENDCASE.

*  PERFORM f_report_data.
*  LOOP AT gt_report INTO gs_report.
*    IF gs_report-light = 1.
*      gs_report-color = 'C611'.
*    ENDIF.
*    MODIFY gt_report FROM gs_report.
*  ENDLOOP.


  rs_selfield-refresh = 'X'.
  PERFORM f_light.
ENDFORM.


*&---------------------------------------------------------------------*
*& Module STATUS_2000 OUTPUT
*&------------------------------------------------------------------*
MODULE status_2000 OUTPUT.
  SET PF-STATUS 'GUI_2000'.
  SET TITLEBAR 'TITLE_2000'.
  PERFORM f_proc.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_2000 INPUT.
  CASE sy-ucomm.
    WHEN '&KAYDET'.
      PERFORM f_islem_kaydet.
    WHEN '&IPTAL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  IF zfk_islem-ogrencino IS NOT INITIAL.
    DATA: ogrpuan TYPE c LENGTH 3 .
    READ TABLE gt_ogrenci INTO gs_ogrenci WITH KEY ogrencino =
    zfk_islem-ogrencino.
    ogrpuan = gs_ogrenci-puan.
    CONCATENATE  ogrpuan 'kütüphane puanı var.' INTO gv_note SEPARATED
    BY ' '.
  ELSE.
    CLEAR gv_note.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form F_ISLEM_KAYDET
*&---------------------------------------------------------------------*
FORM f_islem_kaydet .
  DATA kitapsayisi TYPE i.
  SELECT COUNT(*) FROM zfk_islem
  WHERE ogrencino = @zfk_islem-ogrencino AND
                           kitapteslim IS INITIAL INTO @kitapsayisi.

  READ TABLE gt_ogrenci INTO gs_ogrenci WITH KEY ogrencino =
  zfk_islem-ogrencino.
  IF gs_ogrenci-puan < 26 AND kitapsayisi > 1.
    MESSAGE 'Bu öğrenci 2 den fazla kitap alamaz' TYPE 'S' DISPLAY LIKE
    'E'. EXIT.
  ELSEIF gs_ogrenci-puan > 25 AND gs_ogrenci-puan < 51   AND kitapsayisi
  > 3.
    MESSAGE 'Bu öğrenci 4 den fazla kitap alamaz' TYPE 'E'.
  ELSEIF gs_ogrenci-puan > 50 AND gs_ogrenci-puan < 76   AND kitapsayisi
  > 5.
    MESSAGE 'Bu öğrenci 6 den fazla kitap alamaz' TYPE 'E'.
  ENDIF.

  gs_islem-islemno = zfk_islem-islemno.
  gs_islem-ogrencino = zfk_islem-ogrencino.
  gs_islem-kitapno = zfk_islem-kitapno.
  gs_islem-kitapalim = zfk_islem-kitapalim.
  INSERT zfk_islem FROM gs_islem.

  IF sy-subrc EQ 0.
    MESSAGE 'Başarı ile kaydedildi.' TYPE 'I'.
    CLEAR: gs_report.
    MOVE-CORRESPONDING gs_islem TO gs_report.
    SELECT SINGLE b~yazarno ,
                  b~yazaradi,
                  c~turno,
                  c~turtanimi,
                  a~kitapno,
                  a~kitapadi
      FROM zfk_kitap AS a
      INNER JOIN zfk_yazar AS b ON b~yazarno = a~yazarno
      INNER JOIN zfk_tur AS c ON c~turno = a~turno
      INTO (@gs_report-yazarno,@gs_report-yazaradi,@gs_report-turno,
      @gs_report-turtanimi,@gs_report-kitapno,@gs_report-kitapadi)
      WHERE a~kitapno = @gs_islem-kitapno.

    READ TABLE gt_ogrenci INTO gs_ogrenci WITH KEY ogrencino =
    gs_islem-ogrencino.
    IF sy-subrc = 0.
      CONCATENATE gs_ogrenci-ogrenciadi gs_ogrenci-ogrencisoyadi INTO
      gs_report-adisoyadi SEPARATED BY ' '.
    ENDIF.
    gs_report-light = 2.
    APPEND gs_report TO gt_report.

    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_PROC
*&---------------------------------------------------------------------*
FORM f_proc .
*  CLEAR: zfk_islem.
  SELECT MAX( islemno ) FROM zfk_islem INTO zfk_islem-islemno.

  ADD 1 TO zfk_islem-islemno.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY
*&---------------------------------------------------------------------*

FORM f_display .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SUB_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = gt_report
*   EXCEPTIONS
*     PROGRAM_ERROR            = 1
*     OTHERS                   = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.



*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program       = sy-repid
*      i_callback_pf_status_set = 'SUB_PF_STATUS'
*      i_callback_user_command  = 'USER_COMMAND'
*      is_layout                = gs_layout
*      it_fieldcat              = gt_fieldcat
*    TABLES
*      t_outtab                 = gt_report.
ENDFORM.
FORM f_report_data .

  SELECT * INTO TABLE gt_ogrenci FROM zfk_ogrenci.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_report
            FROM zfk_islem AS i
            INNER JOIN  zfk_kitap AS k ON i~kitapno = k~kitapno
            INNER JOIN  zfk_yazar AS y ON k~yazarno = y~yazarno
            INNER JOIN  zfk_tur AS t ON k~turno = t~turno
            WHERE i~kitapno IN s_ktp_no
              AND i~ogrencino IN s_ogr_no
              ORDER BY i~islemno ASCENDING.

  PERFORM f_light.

  LOOP AT gt_report INTO gs_report.
    READ TABLE gt_ogrenci INTO gs_ogrenci WITH KEY ogrencino =
    gs_report-ogrencino.
    IF sy-subrc = 0.
      CONCATENATE gs_ogrenci-ogrenciadi gs_ogrenci-ogrencisoyadi INTO
      gs_report-adisoyadi SEPARATED BY space.
      MODIFY gt_report FROM gs_report.
    ENDIF.
  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'GUI_0200'.
  SET TITLEBAR 'TITLE_0200'.
  PERFORM f_get_data.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0200 INPUT  .
  CASE sy-ucomm .
    WHEN '&KAYDET'.
      UPDATE zfk_islem SET kitapteslim = zfk_islem-kitapteslim
                       WHERE islemno = zfk_islem-islemno .
      LOOP AT gt_report INTO gs_report WHERE selkz = 'X'.
        gs_report-kitapteslim = zfk_islem-kitapteslim.
        gs_report-light = '3'.
        MODIFY gt_report FROM gs_report.
      ENDLOOP.
      MESSAGE 'Kayıt başarılı.' TYPE 'I'.
      LEAVE TO SCREEN 0.
    WHEN '&IPTAL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
FORM f_get_data .

  READ TABLE gt_report INTO gs_report WITH KEY selkz = 'X'.

  IF sy-subrc = 0.
    gs_report-kitapteslim = sy-datum.
    MOVE-CORRESPONDING gs_report TO zfk_islem.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'GUI_0200'.
  SET TITLEBAR 'TITLE_0200'.
  PERFORM f_data.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN '&KAYDET'.
      PERFORM f_new_data.
    WHEN '&IPTAL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

FORM f_data .
  READ TABLE gt_report INTO gs_report WITH KEY selkz = 'X'.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING gs_report TO zfk_islem.
  ENDIF.
ENDFORM.


FORM f_new_data.
  MODIFY zfk_islem.
  CLEAR: gs_report.
  MOVE-CORRESPONDING zfk_islem TO gs_report.
  SELECT SINGLE b~yazarno ,
                b~yazaradi,
                c~turno,
                c~turtanimi,
                a~kitapno,
                a~kitapadi
    FROM zfk_kitap AS a
    INNER JOIN zfk_yazar AS b ON b~yazarno = a~yazarno
    INNER JOIN zfk_tur AS c ON c~turno = a~turno
    INTO (@gs_report-yazarno,@gs_report-yazaradi,@gs_report-turno,
    @gs_report-turtanimi,@gs_report-kitapno,@gs_report-kitapadi)
    WHERE a~kitapno = @zfk_islem-kitapno.

  READ TABLE gt_ogrenci INTO gs_ogrenci WITH KEY ogrencino =
  zfk_islem-ogrencino.
  IF sy-subrc = 0.
    CONCATENATE gs_ogrenci-ogrenciadi gs_ogrenci-ogrencisoyadi INTO
    gs_report-adisoyadi SEPARATED BY ' '.
  ENDIF.
  MODIFY gt_report FROM gs_report TRANSPORTING ogrencino adisoyadi
  kitapno kitapadi yazarno yazaradi turno turtanimi kitapalim
  kitapteslim
  WHERE islemno = zfk_islem-islemno.
*    gs_report-light = 2.
*    APPEND gs_report TO gt_report.
  MESSAGE 'Kayıt güncellendi.' TYPE 'I'.
  LEAVE TO SCREEN 0.
*  ENDIF.

ENDFORM.


FORM f_light.
  LOOP AT gt_report INTO gs_report.
    IF gs_report-kitapteslim IS INITIAL.
      IF sy-datum - gs_report-kitapalim GE 20.
        gs_report-light = 1.
        gs_report-color = 'C610'.
      ELSE.
        gs_report-light = 2.
        gs_report-color = ''.

      ENDIF.
    ELSE.
      gs_report-light = 3.
      gs_report-color = ''.
    ENDIF.
    MODIFY gt_report FROM gs_report.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0400 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
*  SET PF-STATUS 'GUI_0200'.
*  SET TITLEBAR 'TITLE_0200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0400 INPUT.
*  CASE sy-ucomm.
*    WHEN '&IPTAL'.
*      LEAVE TO SCREEN 0.
*  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form F_MERGE_OZET
*&---------------------------------------------------------------------*
FORM f_merge_ozet .
  CLEAR : gs_layout,gt_fieldcat.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = sy-repid
      i_internal_tabname = 'GT_OZET'
      i_inclname         = sy-repid
    CHANGING
      ct_fieldcat        = gt_fieldcat.

  LOOP AT gt_fieldcat INTO gs_fieldcat.
    CASE gs_fieldcat-fieldname.
      WHEN 'TOPLAMKITAP'.
        gs_fieldcat-scrtext_s =
        gs_fieldcat-scrtext_m =
        gs_fieldcat-scrtext_l =
        gs_fieldcat-reptext = 'Toplam Kitap'.
      WHEN 'KALANKITAP'.
        gs_fieldcat-scrtext_s =
        gs_fieldcat-scrtext_m =
        gs_fieldcat-scrtext_l =
        gs_fieldcat-reptext = 'Verilmeyen Kitap'.
      WHEN 'GECIKENKITAP'.
        gs_fieldcat-scrtext_s =
        gs_fieldcat-scrtext_m =
        gs_fieldcat-scrtext_l =
        gs_fieldcat-reptext =
'Geciken Kitap'.
      WHEN 'TESLIMKITAP'.
        gs_fieldcat-scrtext_s =
        gs_fieldcat-scrtext_m =
        gs_fieldcat-scrtext_l =
         gs_fieldcat-reptext = 'Teslim Kitap'.
      WHEN OTHERS.
    ENDCASE.
    MODIFY gt_fieldcat FROM gs_fieldcat.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_OZET
*&---------------------------------------------------------------------*
FORM f_display_ozet .
  gs_layout-grid_title = 'Öğrenci Kİtap Raporu'.
*  gs_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK     = ' '
*     I_BYPASSING_BUFFER    = ' '
*     I_BUFFER_ACTIVE       = ' '
      i_callback_program    = sy-repid
*     i_callback_pf_status_set = 'SUB_PF_STATUS'
*     i_callback_user_command  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME      =
*     I_BACKGROUND_ID       = ' '
*     I_GRID_TITLE          =
*     I_GRID_SETTINGS       =
      is_layout             = gs_layout
      it_fieldcat           = gt_fieldcat
*     IT_EXCLUDING          =
*     IT_SPECIAL_GROUPS     =
*     IT_SORT               =
*     IT_FILTER             =
*     IS_SEL_HIDE           =
*     I_DEFAULT             = 'X'
*     I_SAVE                = ' '
*     IS_VARIANT            =
*     IT_EVENTS             =
*     IT_EVENT_EXIT         =
*     IS_PRINT              =
*     IS_REPREP_ID          =
      i_screen_start_column = 30
      i_screen_start_line   = 05
      i_screen_end_column   = 200
      i_screen_end_line     = 30
*     I_HTML_HEIGHT_TOP     = 0
*     I_HTML_HEIGHT_END     = 0
*     IT_ALV_GRAPHICS       =
*     IT_HYPERLINK          =
*     IT_ADD_FIELDCAT       =
*     IT_EXCEPT_QINFO       =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*     O_PREVIOUS_SRAL_HANDLER  =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab              = gt_ozet.
* EXCEPTIONS
*     PROGRAM_ERROR            = 1
*     OTHERS                   = 2
  .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DATA_OZET
*&---------------------------------------------------------------------*
FORM f_data_ozet .
  TYPES :BEGIN OF ty_secim,
           ogrencino TYPE zfk_ogrenci-ogrencino,
           adisoyadi TYPE zfk_s_islem-adisoyadi,
         END OF ty_secim.

  DATA : lt_secim TYPE STANDARD TABLE OF ty_secim,
         ls_secim TYPE ty_secim.

  CLEAR : gt_ozet[].
  LOOP AT gt_report INTO gs_report WHERE selkz = 'X'.
    CLEAR : ls_secim.
    ls_secim-ogrencino = gs_report-ogrencino.
    ls_secim-adisoyadi = gs_report-adisoyadi.
    APPEND ls_secim TO lt_secim.
  ENDLOOP.
  IF sy-subrc NE 0.
    MOVE-CORRESPONDING gt_report[] TO lt_secim[].
  ENDIF.
  SORT lt_secim BY ogrencino.
  DELETE ADJACENT DUPLICATES FROM lt_secim COMPARING ogrencino.

*  LOOP AT lt_secim INTO ls_secim.
*    CLEAR : gs_ozet.
*    gs_ozet-ogrencino = ls_secim-ogrencino.
*    gs_ozet-adisoyadi = ls_secim-adisoyadi.
**    LOOP AT gt_report INTO gs_report WHERE ogrencino =
*ls_secim-ogrencino.
**      ADD 1 TO gs_ozet-toplamkitap.
**      IF gs_report-light = 1.
**        ADD 1 TO gs_ozet-gecikenkitap.
**        ADD 1 TO gs_ozet-kalankitap.
**      ELSEIF gs_report-light = 2.
**        ADD 1 TO gs_ozet-kalankitap.
**      ELSEIF gs_report-light = 3.
**        ADD 1 TO gs_ozet-teslimkitap.
**      ENDIF.
**    ENDLOOP.
**    APPEND gs_ozet TO gt_ozet.
**  ENDLOOP.
  LOOP AT lt_secim INTO ls_secim.
    CLEAR : gs_ozet.
    gs_ozet-ogrencino = ls_secim-ogrencino.
    gs_ozet-adisoyadi = ls_secim-adisoyadi.

    LOOP AT gt_report INTO gs_report WHERE ogrencino =
    ls_secim-ogrencino.
      gs_ozet-toplamkitap = 1.

      IF gs_report-light = 1.
        gs_ozet-gecikenkitap = 1.
        gs_ozet-kalankitap = 1.
      ELSEIF gs_report-light = 2.
        gs_ozet-kalankitap = 1.
      ELSEIF gs_report-light = 3.
        gs_ozet-teslimkitap = 1.
      ENDIF.
      COLLECT gs_ozet INTO gt_ozet.
      gs_ozet-teslimkitap = ''.
      gs_ozet-kalankitap = ''.
      gs_ozet-gecikenkitap = ''.
    ENDLOOP.
  ENDLOOP.

ENDFORM.


FORM f_fill_cat.

  gs_layout-grid_title = 'işlem structure'.
  gs_layout-excp_fname = 'LIGHT'.
  gs_layout-box_fname = 'SELKZ'.
  gs_layout-cwidth_opt = abap_true.
  gs_layout-info_fname = 'COLOR'.

  gs_fieldcat-fieldname = 'LIGHT'.
  gs_fieldcat-colddictxt = 'M'.

  CLEAR:gs_fieldcat.
  gs_fieldcat-fieldname = 'KITAPNO'.
  gs_fieldcat-emphasize = 'C310'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR:gs_fieldcat.
  gs_fieldcat-fieldname = 'KITAPADI'.
  gs_fieldcat-emphasize = 'C300'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR:gs_fieldcat.
  gs_fieldcat-fieldname = 'YAZARNO'.
  gs_fieldcat-emphasize = 'C311'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR:gs_fieldcat.
  gs_fieldcat-fieldname = 'YAZARADI'.
  gs_fieldcat-emphasize = 'C300'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR:gs_fieldcat.
  gs_fieldcat-fieldname = 'TURNO'.
  gs_fieldcat-emphasize = 'C311'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR:gs_fieldcat.
  gs_fieldcat-fieldname = 'TURTANIMI'.
  gs_fieldcat-emphasize = 'C300'.
  APPEND gs_fieldcat TO gt_fieldcat.
ENDFORM.