CREATE OR REPLACE PACKAGE BODY "PROCESS_DATA" IS

/***************************************************************************************************************/
  
  FUNCTION F_GetFagusFullBlockNo(v_Timestamp In Date, vBlockNo In Number) RETURN Number

    /************************************************************/
    -- Funktion f�r att ommvandla den korta variant av blocknr (4 alt 5 pos) till den fullst�ndiga som f�ds i Fagus i samband med frisl�ppande av order.
    -- Skapad 2011-10-30 av Jan �dman, JPO-Data.
    /************************************************************/
    IS
    v_year    VARCHAR2(4);
    BEGIN
     
      v_year := f_GetYear(vBlockNo, v_Timestamp); 
    return  (v_year||substr(lpad(vBlockNo,5,0),1,2)||lpad(substr(vBlockNo,-3),5,0));
    
    EXCEPTION
           WHEN others THEN
           pdp.errpkg.report_and_go(sqlcode, 'F_FullBlockNo: '||vBlockNo||'- timestamp: '||v_Timestamp||'- sqlerr '||sqlerrm);
           RETURN(-1);

  END F_GetFagusFullBlockNo; -- End of F_GetDCCalculation
-----------------------------------------------------------------------------------------

  FUNCTION  F_GetYear(v_BlockNo In Number, v_Timestamp in Date) 
   RETURN NUMBER 
 /************************************************************/
    -- St�dfunktion �t F_GetFagusFullBlockNo som anv�nds i syfte att fastst�lla r�tt �r fr�n det korta blocknr (4-5 pos) som man registrerar vid PM s� att ett kompletta blocknr kan utvinnas.
    -- Skapad 2011-10-30 av Jan �dman, JPO-Data.
 /************************************************************/
 IS
 v_ar NUMBER;
   BEGIN
    -- om man i okt-december b�rjar k�ra p� ett block som �r planerat till jan-mar n�sta �r s� �kas �ret p� med 1. Notera att l�ngden m�ste vara 4 tecken
    if substr(v_BlockNo, 1,1) in ('1', '2', '3') and substr(v_Timestamp, 6,2) in ('10','11','12') AND length(v_BlockNo) = 4 then
        v_ar := substr(v_Timestamp,3,4)+ 1;
       -- om man i januari-april inte har k�rt klart blocket fr�n f�reg�ende �r s� skall �ret minskas med 1
    elsif substr(v_BlockNo, 1,2) IN ('10','11','12') and substr(v_Timestamp, 6,2) in ('01', '02', '03', '04') AND length(v_BlockNo) = 5 then
        v_ar := substr(v_Timestamp,1,4)- 1;
   else
      v_ar := substr(v_Timestamp,1,4);
    end if;
   
   return v_ar;
   
 EXCEPTION
WHEN OTHERS THEN
   pdp.errpkg.report_and_go(sqlcode, 'F_GetYear: '||sqlerrm);
   return null;
  
  
   END F_GetYear;

/***************************************************************************************************************/
 
  FUNCTION F_GetTagType(vTagName In Varchar2) RETURN Varchar2

    /************************************************************/
    -- Funktion f�r att leta upp Tagtype fr�n taginf-tabellen
    -- Skapad 2011-10-30 av Jan �dman, JPO-Data.
    /************************************************************/
    IS
        v_type    STAGING_USER.dim_taginf.tagtype%type;
    BEGIN
     select tagtype into v_type from STAGING_USER.dim_taginf where tagid = vTagName; 
      
      return v_type;
   
   EXCEPTION
           WHEN others THEN
           pdp.errpkg.report_and_go(sqlcode, 'F_GetTagType: '||sqlerrm);
           RETURN(null);

  END F_GetTagType; 
/*********************************************************************************************************************************/ 
  FUNCTION F_GetHelpTag(vTagName In Varchar2) RETURN Varchar2

    /************************************************************/
    -- Funktion f�r att ta  reda p� eventuell hj�lptag som anv�nds f�r att r�kna ut energif�rbrukning
    -- Skapad 2011-10-30 av Jan �dman, JPO-Data.
    /************************************************************/
    IS
        v_type    STAGING_USER.dim_taginf.tagtype%type;
        v_Calc_Tag_Val_Unit STAGING_USER.dim_taginf.calc_tag_val_unit%type;
        v_HelpTagName STAGING_USER.dim_taginf.tagid%type;
        
  
    BEGIN
        v_type                := F_GetTagType(vTagName);
        v_Calc_Tag_Val_Unit   := F_GetCalcTagValUnit(vTagName);
        
        if v_type = 'Ber�knad kW V�xelstr�m' then
          v_HelpTagName := vTagName||'_RANGE';
        elsif v_type = 'Ber�knad kW Likstr�m' then
          v_HelpTagName := substr(vTagName, 1,instr(vTagName, '_', 1, 3))||'VARVTAL';
        elsif v_type = 'Direktloggad' and (v_Calc_Tag_Val_Unit = 'm3/ton' or v_Calc_Tag_Val_Unit = 'l/ton') then --
          v_HelpTagName := F_GetBruttoTag(f_getmachineid(vTagName));
         end if;
        
   return  v_helptagname;
       
    EXCEPTION
           WHEN others THEN
           pdp.errpkg.report_and_go(sqlcode, 'F_GetHelpTag: '||sqlerrm);
           RETURN(-1);

  END F_GetHelpTag; 
/*********************************************************************************************************************************/ 

  FUNCTION F_GetCalcTagValUnit(vTagName In Varchar2) RETURN Varchar2

    /************************************************************/
    -- Funktion f�r att ta  reda p� vilken enhet som det ber�knade f�ltet skall ha. Anv�nds f�r attt ta reda p� vilka taggar som skall ber�kna fl�de baserat p� produktion
    -- Skapad 2012-02-22 av Jan �dman, JPO-Data.
    /************************************************************/
    IS
       v_retval STAGING_USER.dim_taginf.calc_tag_val_unit%type;
  
    BEGIN
        
        select calc_tag_val_unit into v_retval from STAGING_USER.dim_taginf where tagid = vTagName;  
        return v_retval;
        
    EXCEPTION
           WHEN others THEN
           pdp.errpkg.report_and_go(sqlcode, 'F_GetCalcTagValUnit: '||sqlerrm);
           RETURN(-1);

  END F_GetCalcTagValUnit; 

/*********************************************************************************************************************************/ 
  
  FUNCTION F_GetMachineId(vTagName In Varchar2) RETURN number

    /************************************************************/
    -- Funktion som letar upp maskinid baserat p� tagnamn.
    -- Skapad 2012-01-01 av Jan �dman, JPO-Data.
    /************************************************************/
    IS
        -- Variabler f�r felhantering
        v_retval      number;
  
    BEGIN
        
       select machineid into v_retval from STAGING_USER.dim_taginf where tagid = vTagName;  
       return v_retval;
       
       
    EXCEPTION
           WHEN others THEN
           pdp.errpkg.report_and_go(sqlcode, 'F_GetMachineId: '||sqlerrm);
           RETURN(-1);

  END F_GetMachineId; 

/*********************************************************************************************************************************/ 
  
  FUNCTION F_GetBruttoTag(vMachineId In Number) RETURN Varchar2

    /************************************************************/
    -- Funktion som letar upp bruttotag baserat p� maskinid.
    -- Skapad 2012-02-22 av Jan �dman, JPO-Data.
    /************************************************************/
    IS
        -- Variabler
        v_retval      STAGING_USER.dim_taginf.tagid%type;
  
    BEGIN
        
       select tagid into v_retval from STAGING_USER.dim_taginf where tagdescription = 'Bruttoprod per timma' and machineid = vMachineId ;  
       return v_retval;
       
       
    EXCEPTION
           WHEN others THEN
           pdp.errpkg.report_and_go(sqlcode, 'F_GetBruttoTag: '||sqlerrm);
           RETURN(-1);

  END F_GetBruttoTag; 


/*********************************************************************************************************************************/ 
  FUNCTION F_GetTagVal(vTagName In Varchar2, vTagTimestamp In Date) RETURN number

    /************************************************************/
    -- Funktion som letar upp v�rdet f�r en tag vid en viss timestamp.
    -- Skapad 2012-01-01 av Jan �dman, JPO-Data.
    /************************************************************/
    IS
        v_tag_val     Number;
  
    BEGIN
       
       select tagvalue into v_tag_val from pdp.historian
       where taguserdefinedname = vTagName
       and   tagtimestamp = vTagTimestamp
    and   rownum < 2;
       
       return v_tag_val;
       
    EXCEPTION
           WHEN others THEN
           pdp.errpkg.report_and_go(sqlcode, 'F_GetTagVal: '||sqlerrm);
           RETURN(-1);

  END F_GetTagVal; 

/*********************************************************************************************************************************/ 
 
 PROCEDURE Proc_RefVal_Dim(p_commit_freq_ IN INTEGER DEFAULT 1000, p_no_days_back in INTEGER DEFAULT 1, p_no_days_offset in INTEGER DEFAULT 0)
-- Proceduren anv�nds f�r att h�mta opctaggar fr�n tabellen historian som har genomg�tt ber�kning och formatering till r�tt format.
-- Skapad 2011-11-02 av Jan �dman, JPO-Data.
-- Inparam p_commit_freq_ antal poster i loop innan commit utf�rs
-- Inparam p_no_of_days_back antal dagar bak�t som data skall laddas
-- Inparam p_no_of_days_offset antal dagar bak�t som man skall uyg� fr�n i f�rh�llande till dagens datum

IS

  commit_counter_ INTEGER:=0;
  
  CURSOR c_hamta_RefVal IS
     SELECT MASKINID, TAGTIMESTAMP, BLOCKNR, BRUTTOPROD, 
          HASTIGHET, AR_STOPP, AR_AVBROTT
     FROM   OPC_REF_VAL
     where  TAGTIMESTAMP >=  TO_DATE(TRUNC(SYSDATE - (p_no_days_back + p_no_days_offset))||'06:00:00', 'YYYY-MM-DD HH24:MI:SS')
     AND    TAGTIMESTAMP <   TO_DATE(TRUNC(SYSDATE - p_no_days_offset)||'06:00:00', 'YYYY-MM-DD HH24:MI:SS') 
     AND    BLOCKNR IS NOT NULL;

BEGIN

  -- T�m tabellen innan p�fyllning
  IF UTILITIES_API.Truncate_Table('dim_ref_val') = 0 THEN

           FOR rec_RefVal IN c_hamta_RefVal LOOP
             
    begin
    
                   INSERT
                     INTO    staging_user.dim_ref_val (TAGTIMESTAMP, MASKINID, FAGUSBLOCKNR,
                                                       BRUTTOPROD, HASTIGHET, AR_STOPP, AR_AVBROTT)
                    VALUES                        (rec_RefVal.TAGTIMESTAMP,rec_RefVal.MASKINID, rec_RefVal.BLOCKNR,
                                                      rec_RefVal.BRUTTOPROD, rec_RefVal.HASTIGHET, rec_RefVal.AR_STOPP, rec_RefVal.AR_AVBROTT);
            
                    commit_counter_:= commit_counter_ + 1;
            
                    IF commit_counter_ = p_commit_freq_ THEN
                       COMMIT;
                       commit_counter_:=0;
                    END IF;
     
   exception
   when others then
            pdp.errpkg.report_and_go(sqlcode, 'HamtaRefVal Inre block: '||to_char(rec_RefVal.TAGTIMESTAMP, 'yyyy-mm-dd hh24:mi:ss')||' - Maskinid '||rec_RefVal.MASKINID||' - '||sqlerrm);
   
   end;
     
           END LOOP;
  END IF;

  IF commit_counter_ > 0 THEN
      COMMIT;
  END IF;


EXCEPTION
 WHEN others THEN
         pdp.errpkg.report_and_go(sqlcode, 'HamtaRefVal Yttre block: '||sqlerrm);

END Proc_RefVal_Dim;
---------------------------------------------------------------------------------------------------------------------------------------

PROCEDURE  Proc_Process_facts(p_commit_freq_ IN INTEGER DEFAULT 1000, p_no_of_days_back in integer default 1, p_no_days_offset in integer default 1)
-- Proceduren anv�nds f�r att h�mta data fr�n opc-datainsamling och foramtera och �ven r�kna ut energif�rbrukning .
-- Skapad 2012-01-10 av Jan �dman, JPO-Data.
-- Uppdaterad 2012-04-13 av Jan �dman, JPO-Data.
-- Ny hantering av virtuella taggar, dvs taggar som ber�knas utifr�n andra taggar p.g.a. felaktigheter i m�tsystem m.m. Ordinaire taggar skall beh�llas av sp�rbarhetssk�l


-- Inparam p_commit_freq_ antal poster i loop innan commit utf�rs
-- Inparam p_no_of_days_back antal dagar bak�t som data skall laddas
-- Inparam p_no_of_days_offset antal dagar bak�t som man skall uyg� fr�n i f�rh�llande till dagens datum

IS
--Variables
  commit_counter_ INTEGER:=0;
  v_help_tag_name  VARCHAR2(40);
  v_help_tag_val  NUMBER;
  v_calc_tag_val   NUMBER;
  v_calc_tag_val_unit   VARCHAR2(200);
  v_tag_val        NUMBER;
  v_formula       VARCHAR2(100);
  v_tagname        VARCHAR2(100); 
  v_machine_id     number;
  v_tagtype        varchar2(30);
  v_tagname_inre_loop varchar2(100);

--Corsors
  --CURSOR F�R YTTRE LOOP SOM LETAR SIG IGENOM SAMTLIGA TAGGGAR EN I TAGET
  CURSOR c_hamta_taginfo IS
     SELECT * 
       from staging_user.dim_taginf
    where taggroup <> 'Ref';
--    and  tagid =  'PM1_1_FIA212_MV_VIRT';

 --CURSOR F�R DEN INRE LOOPEN SOM H�MTAR M�TNINGAR FR�N HISTORIAN MED TAGNAMN SOM INPARAM S� ATT EN "TAG" I TAGET BEARBETAS    
 CURSOR c_hamta_timestamp_info (in_tagname varchar2)is 
     SELECT *
    FROM pdp.historian
    WHERE taguserdefinedname  = in_tagname --'PM1_1_FIA217_MV'--
       and   tagtimestamp >=  TO_DATE(TRUNC(SYSDATE - (p_no_of_days_back + p_no_days_offset))||'06:00:00', 'YYYY-MM-DD HH24:MI:SS')
     and   tagtimestamp <   TO_DATE(TRUNC(SYSDATE - p_no_days_offset)||'06:00:00', 'YYYY-MM-DD HH24:MI:SS') 
     and   tagvalue IS NOT NULL;


BEGIN

    -- T�mmer f�rst gamla v�rden i staging arean
    if utilities_api.truncate_table('process_facts') = 0 then

   FOR rec_taginfo IN c_hamta_taginfo LOOP
   
        begin
   
    -- I den yttre loopen h�mtas info om sj�lva tagen som inte f�r�ndras �ver tid
             v_formula := rec_taginfo.formula;
             v_tagtype := f_gettagtype(rec_taginfo.tagid);
             v_machine_id := f_getmachineid(rec_taginfo.tagid);
             v_tagname := rec_taginfo.tagid;
             v_calc_tag_val_unit := F_GetCalcTagValUnit(rec_taginfo.tagid);
             
             -- Hantering av virtuella taggar som m�ste f� tr�ff i inre loop s� att man hittar data i timestamps
             if v_tagname = 'PM1_1_FIA212_MV_VIRT' then
                v_tagname_inre_loop := 'PM1_1_FIA212_MV';
             else 
                v_tagname_inre_loop := v_tagname;
             end if; 
            
             -- Om direktloggad s� finns ingen hj�lptag
             if v_tagtype <> 'Direktloggad' or v_calc_tag_val_unit = 'm3/ton' or v_calc_tag_val_unit = 'l/ton'then
                    v_help_tag_name := f_gethelptag(rec_taginfo.tagid) ;
             end if;
             
               -- I inre loopen h�mtas data fr�n tagv�rdet och h�r g�rs �ven omr�kningar som placeras i calc_tag_val-kolumnen
               FOR REC_TIMESTAMP_INFO IN c_hamta_timestamp_info(v_tagname_inre_loop) LOOP
                
     begin   
     
                      
         
                       if v_tagtype <> 'Direktloggad' or v_calc_tag_val_unit = 'm3/ton' or v_calc_tag_val_unit = 'l/ton'then
                          v_help_tag_val := F_GetTagVal(v_help_tag_name, rec_timestamp_info.tagtimestamp) ;
                       end if;
                       
                      --------------------------------------------------------------------------------------------------------------------------------- 
                      -- H�r hanteras taggar var m�tv�rde av olika anledningar inte st�mmer utan beh�ver ber�knas innan ber�kning enligt formeln utf�rs. Dessa taggar l�ggs
                      -- till i tagtabellen med _VIRT p� slutet i tagid som en virtuell tag d� den gamla beh�lls f�r sp�rbarhetens skull
                      ---------------------------------------------------------------------------------------------------------------------------------
                      -- �vrig �nga PM1 minus Krima�nga----------------------------------------------------
                      if v_tagname = 'PM1_1_FIA212_MV_VIRT' then 
                          -- Formel f�r �vrig �nga PM1 = �vriga �nga PM1 minus �nga Kriman + hantering av skillnader i ing�ende enheter
                          v_tag_val  := f_gettagval('PM1_1_FIA212_MV',rec_timestamp_info.tagtimestamp) - f_gettagval('PM2_30_FIA202_MV',rec_timestamp_info.tagtimestamp)/ 1000;
                      
                     
                      -- Normala taggar--------------------------------------------------------
                      else
                          v_tag_val  := rec_timestamp_info.tagvalue;
                      end if;
                      ------------------------------------------------------------------------------------------------------------------------------------
                      
                       
                       -- Ingen ber�kning
                       if v_formula = 'N/A' then
             
                          v_calc_tag_val := v_tag_val; --Om man inte har n�gon formel som r�knar om det ursprungliga tagv�rdet s� s�tts calc_tag_val till samma
                       
                       else
                        
                          v_calc_tag_val := f_dynamic_calc(v_tag_val, v_help_tag_val,v_formula); /*Anropar funktion som anv�nder dynamisk PL/SQL som exekverar formel**/
                                
                       end if;
                      
        --Block som g�r att det g�r att hantera problem med indata tex dubbletter
        begin
        
                         -- skriver data till tabell i staging arean, ber�knad formaterad och klar att laddas till qlikview
                         insert into staging_user.process_facts(MASKINID_FK, TAGID_FK, TAGTIMESTAMP_FK, TAG_VAL, TAG_VAL_CALC)
                         values (v_machine_id, v_tagname, rec_timestamp_info.tagtimestamp, v_tag_val, v_calc_tag_val);
                         
          EXCEPTION
                        WHEN others THEN
                         pdp.errpkg.report_and_go(sqlcode, 'HamtaProdFacts Insertsats: Tagid'||rec_taginfo.tagid||' - Timestamp '||to_char(rec_timestamp_info.tagtimestamp, 'yyyy-mm-dd hh24:mi:ss')||' - tagval '
              ||v_tag_val||' calc '||v_calc_tag_val||sqlerrm);
       end;
                      -- Slut insertblock
       
       commit_counter_:= commit_counter_ + 1;
                      
                      IF commit_counter_ = p_commit_freq_ THEN
                         COMMIT;
                         commit_counter_:=0;
                      END IF;
            
                      -- Nollst�ller variabler i den inre looopen
                       v_help_tag_val := null;
                       v_tag_val := null;
                    v_calc_tag_val:= null;
        
     EXCEPTION
                  WHEN others THEN
                   pdp.errpkg.report_and_go(sqlcode, 'HamtaProdFacts Inre block: Tagid'||rec_taginfo.tagid||' - Timestamp '||to_char(rec_timestamp_info.tagtimestamp, 'yyyy-mm-dd hh24:mi:ss')||' - tagval '
       ||v_tag_val||' calc '||v_calc_tag_val||sqlerrm);
     
     end; -- Inre block som ser till att loopen forts�tter �ven om n�gon post �r fel
                     
                 END LOOP;
                 --Nollst�ller variabler i den yttre loopen
                 v_formula := null;
                 v_tagtype := null;
                 v_machine_id := null;
                 v_tagname := null;
    
    
  EXCEPTION
         WHEN others THEN
          pdp.errpkg.report_and_go(sqlcode, 'HamtaProdFacts Yttre loop: Tagid'||rec_taginfo.tagid||' '||sqlerrm);
 
 
 end;
   
   
   
   END LOOP; 
    
  
     IF commit_counter_ > 0 THEN
      COMMIT;
     END IF;
  END IF;

EXCEPTION
 WHEN others THEN
   pdp.errpkg.report_and_go(sqlcode, 'HamtaProdFacts Yttre block: '||sqlerrm);

END Proc_Process_facts;

-----------------------------------------------------------------------------------
FUNCTION F_Dynamic_Calc(tag_val in number, help_tag_val in number default null, formula in varchar2) RETURN number

-- Funktion som anv�nder dynamisk pl/sql f�r att kunna hantera formler som anv�nds i tagtabellen f�r att bl.a. r�kna ut kw
IS

  v_string         VARCHAR2(500);
  v_sqlstmt        VARCHAR2(500);
  v_retval         NUMBER;

BEGIN
  
  v_string := formula;
  v_string := replace(v_string, 'ROT(', 'SQRT(');
  v_string := replace(v_string, 'Str�mRange', ':help_tag_val');
  v_string := replace(v_string, 'Str�m', ':tag_val');
  v_string := replace(v_string, 'Fl�de', ':tag_val');
  v_string := replace(v_string, 'Varvtal', ':help_tag_val');
  v_string := replace(v_string, 'Bruttoprod', ':help_tag_val');-- Denna anv�nds fr�mst f�r fl�desbaserade processtaggar 
  v_string := replace(v_string, ',', '.');
  v_sqlstmt := 'select '||v_string||' from dual';
  
  if help_tag_val is null then
    EXECUTE IMMEDIATE v_sqlstmt
    INTO v_retval
    USING tag_val;
  else
    EXECUTE IMMEDIATE v_sqlstmt
    INTO v_retval
    USING tag_val, help_tag_val;
  end if;

  RETURN (v_retval);


EXCEPTION
 WHEN others THEN
  pdp.errpkg.report_and_go(sqlcode, 'F_Dynamic_Calc: '||sqlerrm);  RETURN (-1);

END F_Dynamic_Calc;


/*********************************************************************************************************************************/ 
 
 PROCEDURE Proc_Machine_Dim(p_commit_freq_ IN INTEGER DEFAULT 1000)
-- Proceduren anv�nds f�r att ladda Dimensionsdata kring pappersmaskiner.
-- Skapad 2011-11-03 av Jan �dman, JPO-Data.

IS

  commit_counter_ INTEGER:=0;

  CURSOR c_hamta_Maskin IS
     SELECT MASKINID,
     BENAMNING,
    KVALITETSTYPID
   FROM KASSREG.MASKIN@SLIRPROD;

BEGIN

  -- T�m tabellen innan p�fyllning
  if utilities_api.truncate_table('DIM_MACHINE') = 0 then

           FOR rec_maskin IN c_hamta_Maskin LOOP
             
             INSERT
               INTO STAGING_USER.DIM_MACHINE( MASKINID,
                              MASKINNAMN,
                              KVALITETSTYPID)
              VALUES      (rec_maskin.MASKINID,
                        rec_maskin.BENAMNING,
                        rec_maskin.KVALITETSTYPID);
      
              commit_counter_:= commit_counter_ + 1;
      
              IF commit_counter_ = p_commit_freq_ THEN
                 COMMIT;
                 commit_counter_:=0;
              END IF;
              
              
           END LOOP;
        
         IF commit_counter_ > 0 THEN
          COMMIT;
         END IF;
  end if;

EXCEPTION
 WHEN others THEN
    pdp.errpkg.report_and_go(sqlcode, 'HamtaMaskinDim: '||sqlerrm);

END Proc_Machine_Dim;

 

/*********************************************************************************************************************************/ 


 
 PROCEDURE Proc_Tillv_Block_Dim(p_commit_freq_ IN INTEGER DEFAULT 1000)
-- Proceduren anv�nds f�r att ladda Dimensionsdata kring pappersmaskinernas tillverkningsblock.
-- Skapad 2011-11-03 av Jan �dman, JPO-Data.

IS

  commit_counter_ INTEGER:=0;


  CURSOR c_hamta_ProdBlock IS
     SELECT FAGUSBLOCKNR, ABB_BLOCKNR, F�RG, KVALITET, BEST�LLT, PRODUCERAT,
    KVAR, PACKADE_KG, PACKADE_ENHETER, PLAN_STARTTID,AKT_PRODSTART, AKT_PRODSTOPP
       FROM DW_PRODUCTIONBLOCK@FAGUS;


BEGIN

  -- T�m tabellen innan p�fyllning
  IF UTILITIES_API.Truncate_Table('DIM_PRODBLOCK') = 0 THEN
      
  
     FOR rec_prodblock IN c_hamta_prodblock LOOP
        INSERT INTO STAGING_USER.DIM_PRODBLOCK ( FAGUSBLOCKNR, ABB_BLOCKNR, F�RG, KVALITET, BEST�LLT, PRODUCERAT, KVAR, PACKADE_KG, PACKADE_ENHETER, 
                                       PLAN_STARTTID, AKT_PRODSTART, AKT_PRODSTOPP) 
        VALUES (rec_prodblock.FAGUSBLOCKNR, rec_prodblock.ABB_BLOCKNR, rec_prodblock.F�RG, 
                     rec_prodblock.KVALITET, rec_prodblock.BEST�LLT, rec_prodblock.PRODUCERAT, 
                     rec_prodblock.KVAR, rec_prodblock.PACKADE_KG, rec_prodblock.PACKADE_ENHETER, 
                     rec_prodblock.PLAN_STARTTID, rec_prodblock.AKT_PRODSTART, rec_prodblock.AKT_PRODSTOPP);

         commit_counter_:= commit_counter_ + 1;

          IF commit_counter_ = p_commit_freq_ THEN
             COMMIT;
             commit_counter_:=0;
          END IF;
          
   END LOOP;


    IF commit_counter_ > 0 THEN
       COMMIT;
    END IF;

   END IF;


EXCEPTION
 WHEN others THEN
      pdp.errpkg.report_and_go(sqlcode, 'HamtaTillvBlockDim: '||sqlerrm);
      
END Proc_Tillv_Block_Dim;
------------------------------------------------------------------------------
 PROCEDURE LoadOPCData(p_commit_freq_ IN INTEGER DEFAULT 1000, p_no_of_days_back in integer default 1, p_no_days_offset in integer default 1)
-- Proceduren anv�nds f�r att ladda Dimensionsdata kring pappersmaskinernas tillverkningsblock.
-- Skapad 2011-11-03 av Jan �dman, JPO-Data.

IS

BEGIN

proc_machine_dim(p_commit_freq_);
proc_tillv_block_dim(p_commit_freq_);
proc_refval_dim(p_commit_freq_,p_no_of_days_back,p_no_days_offset);
proc_process_facts(p_commit_freq_,p_no_of_days_back,p_no_days_offset);






END LoadOPCData;



END PROCESS_DATA;
/

