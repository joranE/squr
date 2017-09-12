-- degree_banner()
(
  select
  	coalesce(d.shrdgmr_term_code_completed,
             d.shrdgmr_term_code_grad)         as grad_term,
  	d.shrdgmr_pidm                             as pidm,
    d.shrdgmr_seq_no                           as seq_no,
    d.shrdgmr_pidm || '-' || d.shrdgmr_seq_no  as deg_key,
    st.sgbstdn_term_code_admit                 as admit_term,
  	d.shrdgmr_camp_code                        as campus_code,
  	d.shrdgmr_levl_code                        as levl_code,
  	to_char(d.shrdgmr_grad_date,'YYYY-MM-DD')  as grad_date,
  	to_char(d.shrdgmr_appl_date,'YYYY-MM-DD')  as appl_date,
  	d.shrdgmr_degs_code                        as degree_status,
  	dv.stvdegc_acat_code                       as acat_code,
    ac.stvacat_desc                            as acat_desc,
  	d.shrdgmr_degc_code                        as degree,
  	dv.stvdegc_desc                            as degree_desc,
  	d.shrdgmr_coll_code_1                      as college,
  	co1.stvcoll_desc                           as college_desc,
    d.shrdgmr_dept_code                        as dept,
    dp.stvdept_desc                            as dept_desc,
    '11'                                       as major_num,
  	d.shrdgmr_majr_code_1                      as major,
  	mv1.stvmajr_desc                           as major_desc
   from
  	shrdgmr d inner join sgbstdn st       on d.shrdgmr_pidm         = st.sgbstdn_pidm
              inner join stvdegc dv       on d.shrdgmr_degc_code    = dv.stvdegc_code
              inner join stvmajr mv1      on d.shrdgmr_majr_code_1  = mv1.stvmajr_code
              inner join stvcoll co1      on d.shrdgmr_coll_code_1  = co1.stvcoll_code
              inner join stvacat ac       on dv.stvdegc_acat_code   = ac.stvacat_code
              left outer join stvdept dp  on d.shrdgmr_dept_code    = dp.stvdept_code
  where
    d.shrdgmr_pidm in @pidm and
    coalesce(d.shrdgmr_term_code_completed,
             d.shrdgmr_term_code_grad) between @min_term and @max_term and 
    st.sgbstdn_term_code_eff = (
      select 
        max(sgbstdn_term_code_eff)
      from
        sgbstdn
      where
        sgbstdn_pidm = d.shrdgmr_pidm and
        sgbstdn_stst_code = 'AS' and
        sgbstdn_term_code_eff <= coalesce(d.shrdgmr_term_code_completed,
                                          d.shrdgmr_term_code_grad)
    ) and
    d.shrdgmr_coll_code_1 is not null and 
    d.shrdgmr_majr_code_1 is not null
)

union

(
  select
  	coalesce(d.shrdgmr_term_code_completed,
             d.shrdgmr_term_code_grad)         as grad_term,
  	d.shrdgmr_pidm                             as pidm,
    d.shrdgmr_seq_no                           as seq_no,
    d.shrdgmr_pidm || '-' || d.shrdgmr_seq_no  as deg_key,
    st.sgbstdn_term_code_admit                 as admit_term,
  	d.shrdgmr_camp_code                        as campus_code,
  	d.shrdgmr_levl_code                        as levl_code,
  	to_char(d.shrdgmr_grad_date,'YYYY-MM-DD')  as grad_date,
  	to_char(d.shrdgmr_appl_date,'YYYY-MM-DD')  as appl_date,
  	d.shrdgmr_degs_code                        as degree_status,
  	dv.stvdegc_acat_code                       as acat_code,
    ac.stvacat_desc                            as acat_desc,
  	d.shrdgmr_degc_code                        as degree,
  	dv.stvdegc_desc                            as degree_desc,
  	d.shrdgmr_coll_code_1                      as college,
  	co1.stvcoll_desc                           as college_desc,
    d.shrdgmr_dept_code                        as dept,
    dp.stvdept_desc                            as dept_desc,
    '12'                                       as major_num,
  	d.shrdgmr_majr_code_1_2                    as major,
  	mv1.stvmajr_desc                           as major_desc
   from
  	shrdgmr d inner join sgbstdn st       on d.shrdgmr_pidm           = st.sgbstdn_pidm
              inner join stvdegc dv       on d.shrdgmr_degc_code      = dv.stvdegc_code
              inner join stvmajr mv1      on d.shrdgmr_majr_code_1_2  = mv1.stvmajr_code
              inner join stvcoll co1      on d.shrdgmr_coll_code_1    = co1.stvcoll_code
              inner join stvacat ac       on dv.stvdegc_acat_code     = ac.stvacat_code
              left outer join stvdept dp  on d.shrdgmr_dept_code_1_2  = dp.stvdept_code
  where
    d.shrdgmr_pidm in @pidm and
    coalesce(d.shrdgmr_term_code_completed,
             d.shrdgmr_term_code_grad) between @min_term and @max_term and 
    st.sgbstdn_term_code_eff = (
      select 
        max(sgbstdn_term_code_eff)
      from
        sgbstdn
      where
        sgbstdn_pidm = d.shrdgmr_pidm and
        sgbstdn_stst_code = 'AS' and
        sgbstdn_term_code_eff <= coalesce(d.shrdgmr_term_code_completed,
                                          d.shrdgmr_term_code_grad)
    ) and
    d.shrdgmr_coll_code_1 is not null and 
    d.shrdgmr_majr_code_1_2 is not null
)

union

(
  select
  	coalesce(d.shrdgmr_term_code_completed,
             d.shrdgmr_term_code_grad)         as grad_term,
  	d.shrdgmr_pidm                             as pidm,
    d.shrdgmr_seq_no                           as seq_no,
    d.shrdgmr_pidm || '-' || d.shrdgmr_seq_no  as deg_key,
    st.sgbstdn_term_code_admit                 as admit_term,
  	d.shrdgmr_camp_code                        as campus_code,
  	d.shrdgmr_levl_code                        as levl_code,
  	to_char(d.shrdgmr_grad_date,'YYYY-MM-DD')  as grad_date,
  	to_char(d.shrdgmr_appl_date,'YYYY-MM-DD')  as appl_date,
  	d.shrdgmr_degs_code                        as degree_status,
  	dv.stvdegc_acat_code                       as acat_code,
    ac.stvacat_desc                            as acat_desc,
  	d.shrdgmr_degc_code                        as degree,
  	dv.stvdegc_desc                            as degree_desc,
  	d.shrdgmr_coll_code_2                      as college,
  	co1.stvcoll_desc                           as college_desc,
    d.shrdgmr_dept_code                        as dept,
    dp.stvdept_desc                            as dept_desc,
    '21'                                       as major_num,
  	d.shrdgmr_majr_code_2                      as major,
  	mv1.stvmajr_desc                           as major_desc
   from
  	shrdgmr d inner join sgbstdn st       on d.shrdgmr_pidm         = st.sgbstdn_pidm
              inner join stvdegc dv       on d.shrdgmr_degc_code    = dv.stvdegc_code
              inner join stvmajr mv1      on d.shrdgmr_majr_code_2  = mv1.stvmajr_code
              inner join stvcoll co1      on d.shrdgmr_coll_code_2  = co1.stvcoll_code
              inner join stvacat ac       on dv.stvdegc_acat_code   = ac.stvacat_code
              left outer join stvdept dp  on d.shrdgmr_dept_code_2  = dp.stvdept_code
  where
    d.shrdgmr_pidm in @pidm and
    coalesce(d.shrdgmr_term_code_completed,
             d.shrdgmr_term_code_grad) between @min_term and @max_term and 
    st.sgbstdn_term_code_eff = (
      select 
        max(sgbstdn_term_code_eff)
      from
        sgbstdn
      where
        sgbstdn_pidm = d.shrdgmr_pidm and
        sgbstdn_stst_code = 'AS' and
        sgbstdn_term_code_eff <= coalesce(d.shrdgmr_term_code_completed,
                                          d.shrdgmr_term_code_grad)
    ) and
    d.shrdgmr_coll_code_2 is not null and 
    d.shrdgmr_majr_code_2 is not null
)

union

(
  select
  	coalesce(d.shrdgmr_term_code_completed,
             d.shrdgmr_term_code_grad)         as grad_term,
  	d.shrdgmr_pidm                             as pidm,
    d.shrdgmr_seq_no                           as seq_no,
    d.shrdgmr_pidm || '-' || d.shrdgmr_seq_no  as deg_key,
    st.sgbstdn_term_code_admit                 as admit_term,
  	d.shrdgmr_camp_code                        as campus_code,
  	d.shrdgmr_levl_code                        as levl_code,
  	to_char(d.shrdgmr_grad_date,'YYYY-MM-DD')  as grad_date,
  	to_char(d.shrdgmr_appl_date,'YYYY-MM-DD')  as appl_date,
  	d.shrdgmr_degs_code                        as degree_status,
  	dv.stvdegc_acat_code                       as acat_code,
    ac.stvacat_desc                            as acat_desc,
  	d.shrdgmr_degc_code                        as degree,
  	dv.stvdegc_desc                            as degree_desc,
  	d.shrdgmr_coll_code_2                      as college,
  	co1.stvcoll_desc                           as college_desc,
    d.shrdgmr_dept_code                        as dept,
    dp.stvdept_desc                            as dept_desc,
    '22'                                       as major_num,
  	d.shrdgmr_majr_code_2_2                    as major,
  	mv1.stvmajr_desc                           as major_desc
   from
  	shrdgmr d inner join sgbstdn st       on d.shrdgmr_pidm           = st.sgbstdn_pidm
              inner join stvdegc dv       on d.shrdgmr_degc_code      = dv.stvdegc_code
              inner join stvmajr mv1      on d.shrdgmr_majr_code_2_2  = mv1.stvmajr_code
              inner join stvcoll co1      on d.shrdgmr_coll_code_2    = co1.stvcoll_code
              inner join stvacat ac       on dv.stvdegc_acat_code     = ac.stvacat_code
              left outer join stvdept dp  on d.shrdgmr_dept_code_2_2  = dp.stvdept_code
  where
    d.shrdgmr_pidm in @pidm and
    coalesce(d.shrdgmr_term_code_completed,
             d.shrdgmr_term_code_grad) between @min_term and @max_term and 
    st.sgbstdn_term_code_eff = (
      select 
        max(sgbstdn_term_code_eff)
      from
        sgbstdn
      where
        sgbstdn_pidm = d.shrdgmr_pidm and
        sgbstdn_stst_code = 'AS' and
        sgbstdn_term_code_eff <= coalesce(d.shrdgmr_term_code_completed,d.shrdgmr_term_code_grad)
    ) and
    d.shrdgmr_coll_code_2 is not null and 
    d.shrdgmr_majr_code_2_2 is not null
)