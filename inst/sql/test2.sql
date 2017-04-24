select *
from
  nbrjobs a
where
  a.nbrjobs_pidm in @pidm and
  a.nbrjobs_effective_date = (
    select max(nbrjobs_effective_date)
    from nbrjobs
    where nbrjobs_pidm = a.nbrjobs_pidm
    )
