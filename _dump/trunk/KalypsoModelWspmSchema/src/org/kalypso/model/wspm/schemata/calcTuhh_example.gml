<?xml version="1.0" encoding="UTF-8"?>
<calculationWspmTuhhSteadyState gml:id="exampleTuhhWspCalc" xmlns="org.kalypso.model.wspm.tuhh" xmlns:gml="http://www.opengis.net/gml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="org.kalypso.model.wspm.tuhh wspmTuhhSteadyState.xsd">
    <calcCreationMember xmlns="org.kalypso.model.wspm">
        <calcCreation>
            <user>Belger</user>
            <creationDate>1972-10-24T10:00:00</creationDate>
        </calcCreation>
    </calcCreationMember>
    <strandWspmTuhhSteadyStateMember>
        <strandWspmTuhhSteadyState/>
    </strandWspmTuhhSteadyStateMember>
    <fliessgesetz>darcyOffeneGerinne</fliessgesetz>
    <subStrandDefinitionMember>
        <subStrandDefinition>
            <startStation>1000</startStation>
            <endStation>2000</endStation>
        </subStrandDefinition>
    </subStrandDefinitionMember>
    <calculationModeMember>
        <calculationModeWaterlevel>
            <type/>
            <runOffEventMember/>
            <calculationStartConditionMember>
                <calculationStartCondition/>
            </calculationStartConditionMember>
            <calculationWaterlevelParameterMember>
                <calculationWaterlevelParameter>
                    <wspIteration>exactIteration</wspIteration>
                    <verzoegerungsverlust>bjoernsen</verzoegerungsverlust>
                    <reibungsverlust>geometricFormula</reibungsverlust>
                    <specialOptionsMember>
                        <specialOptions>
                            <doCalcBridges>true</doCalcBridges>
                        <doCalcBarrages>true</doCalcBarrages>
                    </specialOptions>
                        </specialOptionsMember>
                </calculationWaterlevelParameter>
            </calculationWaterlevelParameterMember>
        </calculationModeWaterlevel>
    </calculationModeMember>
</calculationWspmTuhhSteadyState>
