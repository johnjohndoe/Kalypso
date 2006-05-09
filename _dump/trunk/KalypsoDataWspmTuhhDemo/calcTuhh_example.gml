<?xml version="1.0" encoding="UTF-8"?>
<CalculationWspmTuhhSteadyState gml:id="exampleTuhhWspCalc" xmlns="org.kalypso.model.wspm.tuhh" xmlns:gml="http://www.opengis.net/gml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="org.kalypso.model.wspm.tuhh">
<!--    xsi:schemaLocation="org.kalypso.model.wspm.tuhh file://C:\Programme_inst\eclipse31\workspace\KalypsoModelWspmTuhh\src\org\kalypso\model\wspm\tuhh\schemata\wspmTuhhSteadyState.xsd"-->
    <calcCreationMember xmlns="org.kalypso.model.wspm">
        <CalcCreation>
            <user>Belger</user>
            <creationDate>1972-10-24T10:00:00</creationDate>
        </CalcCreation>
    </calcCreationMember>
    <reachWspmTuhhSteadyStateMember>
        <ReachWspmTuhhSteadyState/>
    </reachWspmTuhhSteadyStateMember>
    <fliessgesetz>darcyOffeneGerinne</fliessgesetz>
    <subReachDefinitionMember>
        <SubReachDefinition>
            <startStation>1000</startStation>
            <endStation>2000</endStation>
        </SubReachDefinition>
    </subReachDefinitionMember>
    <calculationModeMember>
        <CalculationModeWaterlevel>
            <type/>
            <runOffEventMember/>
            <calculationStartConditionMember>
                <CalculationStartCondition/>
            </calculationStartConditionMember>
            <calculationWaterlevelParameterMember>
                <CalculationWaterlevelParameter>
                    <wspIteration>exactIteration</wspIteration>
                    <verzoegerungsverlust>bjoernsen</verzoegerungsverlust>
                    <reibungsverlust>geometricFormula</reibungsverlust>
                    <specialOptionsMember>
                        <SpecialOptions>
                            <doCalcBridges>true</doCalcBridges>
                            <doCalcBarrages>true</doCalcBarrages>
                        </SpecialOptions>
                    </specialOptionsMember>
                </CalculationWaterlevelParameter>
            </calculationWaterlevelParameterMember>
        </CalculationModeWaterlevel>
    </calculationModeMember>
</CalculationWspmTuhhSteadyState>
