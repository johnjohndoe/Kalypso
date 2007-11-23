<?xml version="1.0" encoding="WINDOWS-1252"?>
<nofdp1DModel:HydraulModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:nofdp1DModel="org.kalypso.nofdpidss.1dmodel" xmlns:ns1="org.kalypso.model.wspm.sobek.structures" xmlns:swe="http://www.opengis.net/swe" xs:schemaLocation="org.kalypso.nofdpidss.1dmodel ../../../../../../../../../../KalypsoNofdpIDSSSchema/src/org/kalypso/nofdp/idss/schema/schemata/hydraulModel.xsd" gml:id="root">
 <wspmSobekProj:sobekModelMember xmlns:wspmSobekProj="org.kalypso.model.wspm.sobek.project">
  <wspmSobekProj:SobekModel gml:id="SobekModel1193385975107143">
   <wspmSobek:nodeMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <ns1:SbkWeir gml:id="SbkWeir11954884883965">
     <wspmSobek:name/>
     <wspmSobek:description/>
     <wspmSobek:location>
      <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:32632">
       <gml:coordinates ts="," decimal="." cs=" ">0.0 0.0</gml:coordinates>
      </gml:Point>
     </wspmSobek:location>
     <wspmSobek:uniqueID>weir</wspmSobek:uniqueID>
     <wspmSobek:stationName/>
     <ns1:linksToBranch xlink:href="#Branch1194011748116732"/>
     <ns1:crestWidth>2.0</ns1:crestWidth>
     <ns1:crestHeight>3.0</ns1:crestHeight>
     <ns1:dischargeCoeffCE>1.0</ns1:dischargeCoeffCE>
     <ns1:lateralContractionCoeffCW>1.0</ns1:lateralContractionCoeffCW>
     <ns1:flowDirection>Direction.Both</ns1:flowDirection>
    </ns1:SbkWeir>
   </wspmSobek:nodeMember>
   <wspmSobek:nodeMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <ns1:SbkDatabaseStructure gml:id="SbkDatabaseStructure11954884807393">
     <wspmSobek:name/>
     <wspmSobek:description/>
     <wspmSobek:location>
      <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:32632">
       <gml:coordinates ts="," decimal="." cs=" ">0.0 0.0</gml:coordinates>
      </gml:Point>
     </wspmSobek:location>
     <wspmSobek:uniqueID>databaseStructure</wspmSobek:uniqueID>
     <wspmSobek:stationName/>
     <ns1:linksToBranch xlink:href="#Branch1194011748116732"/>
     <ns1:crestHeight>123.0</ns1:crestHeight>
     <ns1:numOfGateValues>1</ns1:numOfGateValues>
     <ns1:secondAxisValueType>Sobek.RiverDatabaseStructure.SecondAxisValue.h2</ns1:secondAxisValueType>
     <ns1:interpolationType>Sobek.RiverDatabaseStructure.InterpolationType.Linear</ns1:interpolationType>
     <ns1:databaseMember>
      <wspmSobekCommon:Table xmlns:wspmSobekCommon="org.kalypso.model.wspm.sobek.common" gml:id="Table11955735127861">
       <om:result xmlns:om="http://www.opengis.net/om"/>
       <wspmSobekCommon:interpolationType>Interpolation.Continuous</wspmSobekCommon:interpolationType>
       <wspmSobekCommon:interpolationPeriod>No</wspmSobekCommon:interpolationPeriod>
       <wspmSobekCommon:interpolationValue>0</wspmSobekCommon:interpolationValue>
      </wspmSobekCommon:Table>
     </ns1:databaseMember>
     <ns1:databaseUsageMember>
      <wspmSobekCommon:Table xmlns:wspmSobekCommon="org.kalypso.model.wspm.sobek.common" gml:id="Table119557351422310">
       <om:result xmlns:om="http://www.opengis.net/om"/>
       <wspmSobekCommon:interpolationType>Interpolation.Continuous</wspmSobekCommon:interpolationType>
       <wspmSobekCommon:interpolationPeriod>No</wspmSobekCommon:interpolationPeriod>
       <wspmSobekCommon:interpolationValue>0</wspmSobekCommon:interpolationValue>
      </wspmSobekCommon:Table>
     </ns1:databaseUsageMember>
    </ns1:SbkDatabaseStructure>
   </wspmSobek:nodeMember>
   <wspmSobek:nodeMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <ns1:SbkGeneralStructure gml:id="SbkGeneralStructure11954884708337">
     <wspmSobek:name/>
     <wspmSobek:description/>
     <wspmSobek:location>
      <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:32632">
       <gml:coordinates ts="," decimal="." cs=" ">0.0 0.0</gml:coordinates>
      </gml:Point>
     </wspmSobek:location>
     <wspmSobek:uniqueID>generalStructure</wspmSobek:uniqueID>
     <wspmSobek:stationName/>
     <ns1:linksToBranch xlink:href="#Branch1194011748116732"/>
     <ns1:widthUpstream>1.0</ns1:widthUpstream>
     <ns1:widthStructureLeft>2.0</ns1:widthStructureLeft>
     <ns1:widthStructure>3.0</ns1:widthStructure>
     <ns1:widthStructureRight>4.0</ns1:widthStructureRight>
     <ns1:widthDownstream>5.0</ns1:widthDownstream>
     <ns1:bedLevelUpstream>6.0</ns1:bedLevelUpstream>
     <ns1:bedLevelStructureLeft>7.0</ns1:bedLevelStructureLeft>
     <ns1:bedLevelStructure>9.0</ns1:bedLevelStructure>
     <ns1:bedLevelStructureRight>10.0</ns1:bedLevelStructureRight>
     <ns1:bedLevelDownstream>11.0</ns1:bedLevelDownstream>
     <ns1:gateHeight>12.0</ns1:gateHeight>
     <ns1:posFreeGateFlowCoeff>1.0</ns1:posFreeGateFlowCoeff>
     <ns1:posDrownedGateFlowCoeff>1.0</ns1:posDrownedGateFlowCoeff>
     <ns1:posFreeWeirFlowCoeff>1.0</ns1:posFreeWeirFlowCoeff>
     <ns1:posDrownedWeirFlowCoeff>1.0</ns1:posDrownedWeirFlowCoeff>
     <ns1:posContractionCoefficient>1.0</ns1:posContractionCoefficient>
     <ns1:negFreeGateFlowCoeff>1.0</ns1:negFreeGateFlowCoeff>
     <ns1:negDrownedGateFlowCoeff>1.0</ns1:negDrownedGateFlowCoeff>
     <ns1:negFreeWeirFlowCoeff>1.0</ns1:negFreeWeirFlowCoeff>
     <ns1:negDrownedWeirFlowCoeff>1.0</ns1:negDrownedWeirFlowCoeff>
     <ns1:negContractionCoefficient>1.0</ns1:negContractionCoefficient>
     <ns1:extraResistence>1.0</ns1:extraResistence>
    </ns1:SbkGeneralStructure>
   </wspmSobek:nodeMember>
   <wspmSobek:nodeMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <ns1:SbkPump gml:id="SbkPump11954884659112">
     <wspmSobek:name/>
     <wspmSobek:description/>
     <wspmSobek:location>
      <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:32632">
       <gml:coordinates ts="," decimal="." cs=" ">0.0 0.0</gml:coordinates>
      </gml:Point>
     </wspmSobek:location>
     <wspmSobek:uniqueID>pump</wspmSobek:uniqueID>
     <wspmSobek:stationName/>
     <ns1:linksToBranch xlink:href="#Branch1194011748116732"/>
     <ns1:flowDirection>Direction.Positive</ns1:flowDirection>
     <ns1:pumpControl>PumpControl.Downward</ns1:pumpControl>
     <ns1:reductionType>ReductionFactor.Constant</ns1:reductionType>
     <ns1:reductionConstant>1.0</ns1:reductionConstant>
     <ns1:capacity>2.0</ns1:capacity>
     <ns1:switchOnLevelSuctionSide>3.0</ns1:switchOnLevelSuctionSide>
     <ns1:switchOffLevelSuctionSide>4.0</ns1:switchOffLevelSuctionSide>
     <ns1:switchOnLevelPressureSide>5.0</ns1:switchOnLevelPressureSide>
     <ns1:switchOffLevelPressureSide>6.0</ns1:switchOffLevelPressureSide>
    </ns1:SbkPump>
   </wspmSobek:nodeMember>
   <wspmSobek:nodeMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <ns1:SbkRiverWeir gml:id="SbkRiverWeir11954884514583">
     <wspmSobek:name/>
     <wspmSobek:description/>
     <wspmSobek:location>
      <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:32632">
       <gml:coordinates ts="," decimal="." cs=" ">0.0 0.0</gml:coordinates>
      </gml:Point>
     </wspmSobek:location>
     <wspmSobek:uniqueID>RiverWeir</wspmSobek:uniqueID>
     <wspmSobek:stationName/>
     <ns1:linksToBranch xlink:href="#Branch1194011748116732"/>
     <ns1:crestHeight>1.0</ns1:crestHeight>
     <ns1:crestWidth>2.0</ns1:crestWidth>
     <ns1:crestShape>Sobek.Structures.RiverWeir.CrestShape.Broad</ns1:crestShape>
     <ns1:posCorrectionCoeff>1.0</ns1:posCorrectionCoeff>
     <ns1:posSubmergeLimit>1.0</ns1:posSubmergeLimit>
     <ns1:posReductionFactorsMember>
      <wspmSobekCommon:Table xmlns:wspmSobekCommon="org.kalypso.model.wspm.sobek.common" gml:id="Table119557350766110">
       <om:result xmlns:om="http://www.opengis.net/om"/>
       <wspmSobekCommon:interpolationType>Interpolation.Continuous</wspmSobekCommon:interpolationType>
       <wspmSobekCommon:interpolationPeriod>No</wspmSobekCommon:interpolationPeriod>
       <wspmSobekCommon:interpolationValue>0</wspmSobekCommon:interpolationValue>
      </wspmSobekCommon:Table>
     </ns1:posReductionFactorsMember>
     <ns1:negCorrectionCoeff>1.0</ns1:negCorrectionCoeff>
     <ns1:negSubmergeLimit>1.0</ns1:negSubmergeLimit>
     <ns1:negReductionFactorsMember>
      <wspmSobekCommon:Table xmlns:wspmSobekCommon="org.kalypso.model.wspm.sobek.common" gml:id="Table119557350652011">
       <om:result xmlns:om="http://www.opengis.net/om"/>
       <wspmSobekCommon:interpolationType>Interpolation.Continuous</wspmSobekCommon:interpolationType>
       <wspmSobekCommon:interpolationPeriod>No</wspmSobekCommon:interpolationPeriod>
       <wspmSobekCommon:interpolationValue>0</wspmSobekCommon:interpolationValue>
      </wspmSobekCommon:Table>
     </ns1:negReductionFactorsMember>
    </ns1:SbkRiverWeir>
   </wspmSobek:nodeMember>
   <wspmSobek:nodeMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <ns1:SbkCompoundStructure gml:id="SbkCompoundStructure11952145785351">
     <wspmSobek:name/>
     <wspmSobek:description/>
     <wspmSobek:location>
      <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:32632">
       <gml:coordinates ts="," decimal="." cs=" ">0.0 0.0</gml:coordinates>
      </gml:Point>
     </wspmSobek:location>
     <wspmSobek:uniqueID>sbkCompuondUnique</wspmSobek:uniqueID>
     <wspmSobek:stationName/>
     <ns1:linksToBranch xlink:href="#Branch1194011748116732"/>
     <ns1:abstractSbkCompoundStructureNodeMember>
      <ns1:SbkGeneralStructure gml:id="SbkGeneralStructure11952145893321">
       <wspmSobek:name>teilDerCStruct</wspmSobek:name>
       <wspmSobek:description/>
       <wspmSobek:location>
        <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:32632">
         <gml:coordinates ts="," decimal="." cs=" ">0.0 0.0</gml:coordinates>
        </gml:Point>
       </wspmSobek:location>
       <wspmSobek:uniqueID>genStruccture2</wspmSobek:uniqueID>
       <wspmSobek:stationName/>
       <ns1:linksToBranch xlink:href="#Branch1194011748116732"/>
       <ns1:widthUpstream>2.0</ns1:widthUpstream>
       <ns1:widthStructureLeft>3.0</ns1:widthStructureLeft>
       <ns1:widthStructure>3.0</ns1:widthStructure>
       <ns1:widthStructureRight>4.0</ns1:widthStructureRight>
       <ns1:widthDownstream>6.0</ns1:widthDownstream>
       <ns1:bedLevelUpstream>8.0</ns1:bedLevelUpstream>
       <ns1:bedLevelStructureLeft>1.0</ns1:bedLevelStructureLeft>
       <ns1:bedLevelStructure>2.0</ns1:bedLevelStructure>
       <ns1:bedLevelStructureRight>3.0</ns1:bedLevelStructureRight>
       <ns1:bedLevelDownstream>4.0</ns1:bedLevelDownstream>
       <ns1:gateHeight>4.0</ns1:gateHeight>
       <ns1:posFreeGateFlowCoeff>1.0</ns1:posFreeGateFlowCoeff>
       <ns1:posDrownedGateFlowCoeff>1.0</ns1:posDrownedGateFlowCoeff>
       <ns1:posFreeWeirFlowCoeff>1.0</ns1:posFreeWeirFlowCoeff>
       <ns1:posDrownedWeirFlowCoeff>1.0</ns1:posDrownedWeirFlowCoeff>
       <ns1:posContractionCoefficient>1.0</ns1:posContractionCoefficient>
       <ns1:negFreeGateFlowCoeff>1.0</ns1:negFreeGateFlowCoeff>
       <ns1:negDrownedGateFlowCoeff>1.0</ns1:negDrownedGateFlowCoeff>
       <ns1:negFreeWeirFlowCoeff>1.0</ns1:negFreeWeirFlowCoeff>
       <ns1:negDrownedWeirFlowCoeff>1.0</ns1:negDrownedWeirFlowCoeff>
       <ns1:negContractionCoefficient>1.0</ns1:negContractionCoefficient>
       <ns1:extraResistence>1.0</ns1:extraResistence>
      </ns1:SbkGeneralStructure>
     </ns1:abstractSbkCompoundStructureNodeMember>
     <ns1:abstractSbkCompoundStructureNodeMember>
      <ns1:SbkRiverWeir gml:id="SbkRiverWeir11952145838632">
       <wspmSobek:name>teilDerCStruct</wspmSobek:name>
       <wspmSobek:description/>
       <wspmSobek:location>
        <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:32632">
         <gml:coordinates ts="," decimal="." cs=" ">0.0 0.0</gml:coordinates>
        </gml:Point>
       </wspmSobek:location>
       <wspmSobek:uniqueID>riverWeir</wspmSobek:uniqueID>
       <wspmSobek:stationName/>
       <ns1:linksToBranch xlink:href="#Branch1194011748116732"/>
       <ns1:crestHeight>2.0</ns1:crestHeight>
       <ns1:crestWidth>3.0</ns1:crestWidth>
       <ns1:crestShape>Sobek.Structures.RiverWeir.CrestShape.Broad</ns1:crestShape>
       <ns1:posCorrectionCoeff>4.0</ns1:posCorrectionCoeff>
       <ns1:posSubmergeLimit>5.0</ns1:posSubmergeLimit>
       <ns1:posReductionFactorsMember>
        <wspmSobekCommon:Table xmlns:wspmSobekCommon="org.kalypso.model.wspm.sobek.common" gml:id="Table119557352277013">
         <om:result xmlns:om="http://www.opengis.net/om"/>
         <wspmSobekCommon:interpolationType>Interpolation.Continuous</wspmSobekCommon:interpolationType>
         <wspmSobekCommon:interpolationPeriod>No</wspmSobekCommon:interpolationPeriod>
         <wspmSobekCommon:interpolationValue>0</wspmSobekCommon:interpolationValue>
        </wspmSobekCommon:Table>
       </ns1:posReductionFactorsMember>
       <ns1:negCorrectionCoeff>6.0</ns1:negCorrectionCoeff>
       <ns1:negSubmergeLimit>6.0</ns1:negSubmergeLimit>
       <ns1:negReductionFactorsMember>
        <wspmSobekCommon:Table xmlns:wspmSobekCommon="org.kalypso.model.wspm.sobek.common" gml:id="Table119557352441114">
         <om:result xmlns:om="http://www.opengis.net/om"/>
         <wspmSobekCommon:interpolationType>Interpolation.Continuous</wspmSobekCommon:interpolationType>
         <wspmSobekCommon:interpolationPeriod>No</wspmSobekCommon:interpolationPeriod>
         <wspmSobekCommon:interpolationValue>0</wspmSobekCommon:interpolationValue>
        </wspmSobekCommon:Table>
       </ns1:negReductionFactorsMember>
      </ns1:SbkRiverWeir>
     </ns1:abstractSbkCompoundStructureNodeMember>
    </ns1:SbkCompoundStructure>
   </wspmSobek:nodeMember>
   <wspmSobek:branchMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <wspmSobek:Branch gml:id="Branch1194011748116732">
     <wspmSobek:name>b_00001</wspmSobek:name>
     <wspmSobek:description/>
     <wspmSobek:river/>
     <wspmSobek:uniqueID>b_00001</wspmSobek:uniqueID>
     <wspmSobek:riverLine>
      <gml:LineString xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:32632">
       <gml:coordinates ts="," decimal="." cs=" ">499389.54629169765 5507282.684919135,499370.53303131077 5507220.488915126,499414.4531218657 5507113.225249332,499412.8398515239 5506930.77924623</gml:coordinates>
      </gml:LineString>
     </wspmSobek:riverLine>
     <wspmSobek:length>363.39756343527745</wspmSobek:length>
    </wspmSobek:Branch>
   </wspmSobek:branchMember>
   <wspmSobek:lastfallMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <wspmSobek:Lastfall gml:id="Lastfall119401177992983">
     <wspmSobek:name>neuer test</wspmSobek:name>
     <wspmSobek:description/>
     <wspmSobek:simulationBegin>1997-07-02</wspmSobek:simulationBegin>
     <wspmSobek:simulationEnd>2003-07-02</wspmSobek:simulationEnd>
     <wspmSobek:preSimulationTime>12</wspmSobek:preSimulationTime>
     <wspmSobek:simulationTimestep>10</wspmSobek:simulationTimestep>
     <wspmSobek:resultTimeStepAsMultiple>3</wspmSobek:resultTimeStepAsMultiple>
    </wspmSobek:Lastfall>
   </wspmSobek:lastfallMember>
  </wspmSobekProj:SobekModel>
 </wspmSobekProj:sobekModelMember>
 <nofdp1DModel:lastUsedLanduseAttrib/>
</nofdp1DModel:HydraulModel>
