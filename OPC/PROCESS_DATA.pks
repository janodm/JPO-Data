CREATE OR REPLACE PACKAGE "PROCESS_DATA" 
IS

  FUNCTION F_GetHelpTag(vTagName In Varchar2) RETURN Varchar2;
  FUNCTION F_GetBruttoTag(vMachineId In Number) RETURN Varchar2;
  FUNCTION F_GetTagType(vTagName In Varchar2) RETURN Varchar2;
  FUNCTION F_GetMachineId(vTagName In Varchar2) RETURN Number;
  FUNCTION F_GetTagVal(vTagName In Varchar2, vTagTimestamp in Date) RETURN Number;
  FUNCTION F_GetCalcTagValUnit(vTagName In Varchar2) RETURN Varchar2;
  FUNCTION F_GetFagusFullBlockNo(v_Timestamp In Date, vBlockNo In Number) RETURN Number;
  FUNCTION F_GetYear(v_BlockNo In Number, v_Timestamp in Date) RETURN number;
  FUNCTION F_Dynamic_Calc(tag_val in number, help_tag_val in number default null, formula in varchar2) RETURN number;
  
  PROCEDURE Proc_RefVal_Dim(p_commit_freq_ IN INTEGER DEFAULT 1000, p_no_days_back in INTEGER DEFAULT 1, p_no_days_offset in INTEGER DEFAULT 0);
  PROCEDURE Proc_Process_facts(p_commit_freq_ IN INTEGER DEFAULT 1000, p_no_of_days_back in integer default 1, p_no_days_offset in integer default 1);
  PROCEDURE Proc_Machine_Dim(p_commit_freq_ IN INTEGER DEFAULT 1000);
  PROCEDURE Proc_Tillv_Block_Dim(p_commit_freq_ IN INTEGER DEFAULT 1000);
   
  Procedure LoadOPCData(p_commit_freq_ IN INTEGER DEFAULT 1000, p_no_of_days_back in integer default 1, p_no_days_offset in integer default 1);
   
--v_ar||substr(lpad(tambour_rec.BLOCKNR,5,0),1,2)||lpad(substr(tambour_rec.BLOCKNR,-3),5,0)
END PROCESS_DATA;
/

