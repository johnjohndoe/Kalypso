<?xml version="1.0" encoding="UTF-8"?>
<CalculationWspmTuhhSteadyState gml:id="exampleTuhhWspCalc" xmlns="org.kalypso.model.wspm.tuhh" xmlns:gml="http://www.opengis.net/gml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="org.kalypso.model.wspm.tuhh ../../Kalypso/KalypsoModelWspmTuhh/src/org/kalypso/model/wspm/tuhh/schemata/wspmTuhhSteadyState.xsd"> 
    <!--xsi:schemaLocation="org.kalypso.model.wspm.tuhh file://C:\Programme_inst\eclipse31\workspace\KalypsoModelWspmTuhh\src\org\kalypso\model\wspm\tuhh\schemata\wspmTuhhSteadyState.xsd">--> 
    <!--xsi:schemaLocation="org.kalypso.model.wspm.tuhh">-->
    <gml:description xmlns="org.kalypso.model.wspm">Tuhh-Beispiel: total schöön</gml:description>
    <gml:name xmlns="org.kalypso.model.wspm">Demo</gml:name>
    <calcCreationMember xmlns="org.kalypso.model.wspm">
        <CalcCreation gml:id="CalcCreation">
            <gml:description xmlns="org.kalypso.model.wspm"></gml:description>
            <gml:name xmlns="org.kalypso.model.wspm"></gml:name>
            <user>Belger</user>
            <creationDate>1972-10-24T10:00:00</creationDate>
        </CalcCreation>
    </calcCreationMember>
    <reachWspmTuhhSteadyStateMember>
        <ReachWspmTuhhSteadyState gml:id="ReachWspmTuhhSteadyState"/>
    </reachWspmTuhhSteadyStateMember>
    <fliessgesetz>darcyOffeneGerinne</fliessgesetz>
    <subReachDefinitionMember>
        <SubReachDefinition gml:id="SubReachDefinition">
            <startStation>1000</startStation>
            <endStation>2000</endStation>
        </SubReachDefinition>
    </subReachDefinitionMember>
    <calculationModeMember>
        <CalculationModeWaterlevel gml:id="CalculationModeWaterlevel">
            <type/>
            <runOffEventMember/>
            <calculationStartConditionMember>
                <CalculationStartCondition gml:id="CalculationStartCondition"/>
            </calculationStartConditionMember>
            <calculationWaterlevelParameterMember>
                <CalculationWaterlevelParameter gml:id="CalculationWaterlevelParameter">
                    <wspIteration>exactIteration</wspIteration>
                    <verzoegerungsverlust>bjoernsen</verzoegerungsverlust>
                    <reibungsverlust>geometricFormula</reibungsverlust>
                    <specialOptionsMember>
                        <SpecialOptions gml:id="SpecialOptions">
                            <doCalcBridges>true</doCalcBridges>
                            <doCalcBarrages>true</doCalcBarrages>
                        </SpecialOptions>
                    </specialOptionsMember>
                </CalculationWaterlevelParameter>
            </calculationWaterlevelParameterMember>
        </CalculationModeWaterlevel>
    </calculationModeMember>
</CalculationWspmTuhhSteadyState>
