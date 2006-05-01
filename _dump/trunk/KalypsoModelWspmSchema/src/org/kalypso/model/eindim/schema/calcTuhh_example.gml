<?xml version="1.0" encoding="UTF-8"?>
<calculation1DStationaryTuhh gml:id="exampleTuhhWspCalc" xmlns="org.kalypso.model.eindim.tuhh" xmlns:gml="http://www.opengis.net/gml"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="org.kalypso.model.eindim.tuhh calculation1DStationaryTuhh.xsd">
    <user xmlns="org.kalypso.model.eindim">Belger</user>
    <creationDate xmlns="org.kalypso.model.eindim">1972-10-24T10:00:00</creationDate>
    <strand1DStationaryTuhhMember>
        <strand1DStationaryTuhh></strand1DStationaryTuhh>
    </strand1DStationaryTuhhMember>
    <fliessgesetz>darcyOffeneGerinne</fliessgesetz>
    <startStation>1000</startStation>
    <endStation>2000</endStation>
    <calculationModeMember>
        <calculationModeWaterlevel>
            <type></type>
            <runOffEventMember></runOffEventMember>
            <calculationStartConditionMember>
                <calculationStartCondition></calculationStartCondition>
            </calculationStartConditionMember>
            <calculationWaterlevelParameterMember>
                <calculationWaterlevelParameter>
                    <wspIteration>exactIteration</wspIteration>
                    <verzoegerungsverlust>bjoernsen</verzoegerungsverlust>
                    <reibungsverlust>geometricFormula</reibungsverlust>
                    <doCalcBridges>true</doCalcBridges>
                    <doCalcBarrages>true</doCalcBarrages>
                </calculationWaterlevelParameter>
            </calculationWaterlevelParameterMember>
        </calculationModeWaterlevel>
    </calculationModeMember>
</calculation1DStationaryTuhh>