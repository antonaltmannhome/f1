use cleanf1;

create table driver(
	forixId INT not null,
	driver varchar(50) not null,
	adjustedSurname varchar(50) not null,
	surname varchar(50) not null,
	longDriver varchar(50) not null,
	primary key (driver));

create table race(
	race varchar(50) not null,
	date DATE default null,
	circuit varchar(50) not null,
    gotData BOOL default FALSE,
	nlap SMALLINT default null,
	perim INT default null,
	primary key (race));
	
create table racetyre(
	race varchar(50) not null,
	FOREIGN KEY (race)
	REFERENCES race(race),
	tyre varchar(50) not null,
	numberOfLap SMALLINT not null,
	primary key(race, tyre));

create table racedriver(
	race varchar(50) not null,
	CONSTRAINT race_race_fk
	FOREIGN KEY (race)
	REFERENCES race (race),
	driver varchar(50) not null,
	CONSTRAINT driver_driver_fk
	FOREIGN KEY (driver)
	REFERENCES driver (driver),
	car varchar(50) not null,
	classification varchar(50) default null,
	officialFinishingPosition SMALLINT default null,
	startingGrid SMALLINT default null,
	primary key(race, driver));

create table racedriverlap(
	race varchar(50) not null,
    driver varchar(50) not null,
	CONSTRAINT FK
	FOREIGN KEY (race, driver)
	REFERENCES racedriver (race, driver),
	FOREIGN KEY (race, tyre)
	REFERENCES racetyre(race, tyre),
	lap SMALLINT not null,
    sec DEC(10,3) default null,
	leadLap SMALLINT default null,
	inlap BOOL default null,
	outlap BOOL default null,
	stint SMALLINT default null,
	tyre varchar(50) not null,
	tyreLap SMALLINT default null,
	preDeltaDidOt SMALLINT default null,
	preDeltaGotOt SMALLINT default null,
	preDeltaDidLap SMALLINT default null,
	preDeltaGotLap SMALLINT default null,
	isSafetyCar BOOL default null,
	isWet BOOL default null,
	isRed BOOL default null,
	primary key(race, driver, lap));

create table pitstop(
	race varchar(50) not null,
	driver varchar(50) not null,
	endOfLap SMALLINT not null,
	FOREIGN KEY (race, driver)
	REFERENCES racedriver(race, driver),
	pitStopTime DEC(10,3),
	isRedFlagStop BOOL,
	penalty BOOL,
	usedOrNew varchar(5),
	replaceTyre BOOL,
	tyre varchar(50) not null,
	FOREIGN KEY (race, tyre)
	REFERENCES racetyre(race, tyre),
	primary key(race, driver, endOfLap));

create table stint(
	race varchar(50) not null,
	driver varchar(50) not null,
    stint SMALLINT not null,
	startLap SMALLINT not null,
	FOREIGN KEY (race, driver)
	REFERENCES racedriver(race, driver),
	endLap SMALLINT default null,
	usedOrNew varchar(5),
	tyre varchar(50) not null,
	FOREIGN KEY (race, tyre)
	REFERENCES racetyre(race, tyre),
	primary key(race, driver, stint));

create table qualifyingsession(
	race varchar(50) not null,
    session SMALLINT not null,
	isWet BOOL not null,
	isAbandoned BOOL not null,
	FOREIGN KEY (race)
	REFERENCES race(race),
	primary key(race, session));

create table qualifying(
	race varchar(50) not null,
	driver varchar(50) not null,
    session SMALLINT not null,
	sec DEC(10,3) not null,
	FOREIGN KEY (race, driver)
	REFERENCES racedriver(race, driver),
	FOREIGN KEY (race, session)
	REFERENCES qualifyingsession(race, session),
	primary key(race, driver, session));

create table overtaking(
	race varchar(50) not null,
	didOtDriver varchar(50) not null,
    gotOtDriver varchar(50) not null,
    didOtLap SMALLINT not null,
    gotOtLap SMALLINT not null,
	inlap BOOL not null,
	outlap BOOL not null,
	isSafetyCar BOOL not null,
	isSCRestart BOOL not null,
	FOREIGN KEY (race, didOtDriver, didOtLap)
	REFERENCES racedriverlap(race, driver, lap),
   	FOREIGN KEY (race, gotOtDriver, gotOtLap)
	REFERENCES racedriverlap(race, driver, lap),
    primary key(race, didOtDriver, gotOtDriver, didOtLap, gotOtLap));

create table possibleovertaking(
	race varchar(50) not null,
	lap SMALLINT not null,
	driver1 varchar(50) not null,
	driver2 varchar(50) not null,
	timeElapsedDelta DEC(10,3) not null,
	predSecDelta DEC(10,3) not null,
	rankDelta SMALLINT not null,
	gotOvertaken BOOL default null,
	/* why can't i add this, exactly like i have for overtaking? seems like sql only allows one reference like that and overtaking has already done it
	FOREIGN KEY (race, driver1, lap)
	REFERENCES racedriverlap(race, driver, lap),
   	FOREIGN KEY (race, driver2, lap)
	REFERENCES racedriverlap(race, driver, lap), */
	primary key(race, lap, driver1, driver2));

create table finishingpositionprobability(
	race varchar(50) not null,
	driver varchar(50) not null,
	lap SMALLINT not null,
	finPos SMALLINT not null,
	probability DEC(10, 3) default NULL,
	primary key(race, driver, lap, finPos));
