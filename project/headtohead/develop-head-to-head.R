if (FALSE) {
  h2hTB = ViewAllHeadToHeadByDriver('jbutton')
  h2hTB = h2hTB %>%
    left_join(h2hTB %>%
                group_by(team, teamMate) %>%
                summarise(maxYear = max(year[year != 'overall'])) %>%
                ungroup() %>%
                arrange(maxYear) %>%
                mutate(index =  (1:n()) %% 2),
              c('team', 'teamMate'))
}

MakeH2HGtTable = function(myDriv1, toFile = FALSE) {
  h2hTB = ViewAllHeadToHeadByDriver(myDriv1)
  # let's flag when the team mate changes
  # don't colour by team, that doesn't change when team mate changes of course
  
  # drop coldumn 6 i think
  h2hTB = h2hTB %>%
    select(-tempCol6)
  
  myLongDriver = with(driverDF, longDriver[driver == myDriv1])
  
  h2hTB$team = toupper(h2hTB$team)
  
  tempColName = paste0('tempCol', 1:5)
  explanation = c('include all dry laps (except behind safety car, inlaps, outlaps, lap 1)',
                  'exclude laps where either driver has a car problem',
                  'exclude laps where either driver is in traffic',
                  'exclude laps where either driver has no incentive to push',
                  'adjust for tyre compound and tyre age')
  for (j in 1:length(tempColName)) {
    names(h2hTB)[names(h2hTB) == tempColName[j]] = explanation[j]
  }
  
  myTab = h2hTB %>%
    mutate_cond(year == 'overall',
                team = '',
                teamMate = '') %>%
    gt %>%
    tab_style(
      style = list(
        cell_text(font = 'Rockwell',
                  size = 'small')
      ),
      locations = cells_body(
        columns = vars(team, year, teamMate)
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(font = 'Franklin Gothic',
                  size = 'medium')
      ),
      locations = cells_body(
        columns = vars(teamMate)
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = 'lightcyan')
      ),
      locations = cells_body(
        rows = (index %% 2) == 0
      )
    )  %>%
    tab_style(
      style = list(
        cell_fill(color = 'khaki1')
      ),
      locations = cells_body(
        rows = (index %% 2) == 1
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = 'bold')
      ),
      location = cells_body(
        columns = vars(year),
        rows = (year == 'overall')
      )
    ) %>%
    cols_hide(columns = vars(isSummary, index)) %>%
    cols_width(vars(year) ~ px(65),
               vars(team) ~ px(80),
               vars('teamMate') ~ 90,
               everything() ~ px(125)) %>%
    tab_header(
      title = md(myLongDriver)
    )
  
  if (toFile) {
    fileOut = paste0(OUTPUTPATH, 'misc/headtohead/', myDriv1, '.png')
    gtsave(data = myTab, filename = fileOut)
  }
  # ok, we're on our way
  return(myTab)
}
