<?xml version="1.0" encoding="WINDOWS-1252"?>
<nofdp1DModel:HydraulModel xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:nofdp1DModel="org.kalypso.nofdpidss.1dmodel" xmlns:ns1="org.kalypso.model.wspm.sobek.structures" gml:id="root">
 <wspmSobekProj:sobekModelMember xmlns:wspmSobekProj="org.kalypso.model.wspm.sobek.project">
  <wspmSobekProj:SobekModel gml:id="SobekModel11936714728091">
   <wspmSobek:nodeMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <ns1:SbkCompoundStructureNode gml:id="SbkCompoundStructureNode11936715940592">
     <wspmSobek:name/>
     <wspmSobek:description/>
     <wspmSobek:location>
      <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:swe="http://www.opengis.net/swe" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
       <gml:coordinates ts="," decimal="." cs=" ">0.0 0.0</gml:coordinates>
      </gml:Point>
     </wspmSobek:location>
     <wspmSobek:uniqueID>ersdr</wspmSobek:uniqueID>
     <wspmSobek:stationName>sf</wspmSobek:stationName>
     <ns1:linksToBranch xlink:href="#Branch11936759340907"/>
     <ns1:abstractSbkCompoundStructureNodeMember>
      <ns1:SbkDatabaseStructure gml:id="SbkDatabaseStructure11936716102312">
       <wspmSobek:name/>
       <wspmSobek:description/>
       <wspmSobek:location/>
       <wspmSobek:uniqueID/>
       <wspmSobek:stationName/>
       <ns1:crestHeight/>
       <ns1:numOfGateValues>1</ns1:numOfGateValues>
       <ns1:secondHeightRelative/>
       <ns1:interpolationType>Sobek.RiverDatabaseStructure.InterpolationType.Lineair</ns1:interpolationType>
       <ns1:database/>
       <ns1:databaseUsage/>
      </ns1:SbkDatabaseStructure>
     </ns1:abstractSbkCompoundStructureNodeMember>
     <ns1:abstractSbkCompoundStructureNodeMember>
      <ns1:SbkRiverPump gml:id="SbkRiverPump11936716073714">
       <wspmSobek:name/>
       <wspmSobek:description/>
       <wspmSobek:location/>
       <wspmSobek:uniqueID/>
       <wspmSobek:stationName/>
      </ns1:SbkRiverPump>
     </ns1:abstractSbkCompoundStructureNodeMember>
     <ns1:abstractSbkCompoundStructureNodeMember>
      <ns1:SbkGeneralStructure gml:id="SbkGeneralStructure11936716044503">
       <wspmSobek:name/>
       <wspmSobek:description/>
       <wspmSobek:location/>
       <wspmSobek:uniqueID/>
       <wspmSobek:stationName/>
       <ns1:widthUpstream/>
       <ns1:widthStructureLeft/>
       <ns1:widthStructure/>
       <ns1:widthStructureRight/>
       <ns1:widthDownstream/>
       <ns1:bedLevelUpstream/>
       <ns1:bedLevelStructureLeft/>
       <ns1:bedLevelStructure/>
       <ns1:bedLevelStructureRight/>
       <ns1:bedLevelDownstream/>
       <ns1:gateHeight/>
       <ns1:posFreeGateFlowCoeff>1.0</ns1:posFreeGateFlowCoeff>
       <ns1:posDrownedGateFlowCoeff>1.0</ns1:posDrownedGateFlowCoeff>
       <ns1:posFreeWeirFlowCoeff>1.0</ns1:posFreeWeirFlowCoeff>
       <ns1:posDrownedWeirFlowCoeff>1.0</ns1:posDrownedWeirFlowCoeff>
       <ns1:posContractionCoefficient>1.0</ns1:posContractionCoefficient>
       <ns1:negFreeGateFlowCoeff>1.0</ns1:negFreeGateFlowCoeff>
       <ns1:negDrownedGateFlowCoeff>1.0</ns1:negDrownedGateFlowCoeff>
       <ns1:negFreeWeirFlowCoeff>1.0</ns1:negFreeWeirFlowCoeff>
       <ns1:negDrownedWeirFlowCoeff>1.0</ns1:negDrownedWeirFlowCoeff>
       <ns1:negContractionCoeff>1.0</ns1:negContractionCoeff>
       <ns1:extraResistence>1.0</ns1:extraResistence>
      </ns1:SbkGeneralStructure>
     </ns1:abstractSbkCompoundStructureNodeMember>
     <ns1:abstractSbkCompoundStructureNodeMember>
      <ns1:SbkRiverAdvancedWeir gml:id="SbkRiverAdvancedWeir11936716014655">
       <wspmSobek:name/>
       <wspmSobek:description/>
       <wspmSobek:location/>
       <wspmSobek:uniqueID/>
       <wspmSobek:stationName/>
      </ns1:SbkRiverAdvancedWeir>
     </ns1:abstractSbkCompoundStructureNodeMember>
     <ns1:abstractSbkCompoundStructureNodeMember>
      <ns1:SbkRiverWeir gml:id="SbkRiverWeir11936715988564">
       <wspmSobek:name/>
       <wspmSobek:description/>
       <wspmSobek:location/>
       <wspmSobek:uniqueID/>
       <wspmSobek:stationName/>
       <ns1:crestHeight/>
       <ns1:crestWidth/>
       <ns1:crestShape>0.0</ns1:crestShape>
       <ns1:posCorrectionCoeff/>
       <ns1:posSubmergeLimit/>
       <ns1:posReductionFactors/>
       <ns1:negCorrectionCoeff/>
       <ns1:negSubmergeLimit/>
       <ns1:negReductionFactors/>
      </ns1:SbkRiverWeir>
     </ns1:abstractSbkCompoundStructureNodeMember>
    </ns1:SbkCompoundStructureNode>
   </wspmSobek:nodeMember>
   <wspmSobek:nodeMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <ns1:SbkRiverWeir gml:id="SbkRiverWeir11936714767460">
     <wspmSobek:name>riverWeir</wspmSobek:name>
     <wspmSobek:description>riverWeir</wspmSobek:description>
     <wspmSobek:location>
      <gml:Point xmlns:ns3="http://www.w3.org/2001/SMIL20/" xmlns:swe="http://www.opengis.net/swe" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:ns4="http://www.isotc211.org/2005/gmd" xmlns:ns5="http://www.isotc211.org/2005/gco" xmlns:ns6="http://www.isotc211.org/2005/gts" xmlns:ns7="http://www.isotc211.org/2005/gss" xmlns:ns8="http://www.isotc211.org/2005/gsr" xmlns:ns9="http://www.w3.org/2001/SMIL20/Language" srsName="EPSG:31467">
       <gml:coordinates ts="," decimal="." cs=" ">0.0 0.0</gml:coordinates>
      </gml:Point>
     </wspmSobek:location>
     <wspmSobek:uniqueID>riverWeir</wspmSobek:uniqueID>
     <wspmSobek:stationName>riverWeir</wspmSobek:stationName>
     <ns1:linksToBranch xlink:href="#Branch11936759340907"/>
     <ns1:crestHeight>12.0</ns1:crestHeight>
     <ns1:crestWidth>12.0</ns1:crestWidth>
     <ns1:crestShape>0.0</ns1:crestShape>
     <ns1:posCorrectionCoeff/>
     <ns1:posSubmergeLimit/>
     <ns1:posReductionFactors/>
     <ns1:negCorrectionCoeff/>
     <ns1:negSubmergeLimit/>
     <ns1:negReductionFactors/>
    </ns1:SbkRiverWeir>
   </wspmSobek:nodeMember>
   <wspmSobek:branchMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <wspmSobek:Branch gml:id="Branch11936759340907">
     <wspmSobek:name>sdf</wspmSobek:name>
     <wspmSobek:description>saf</wspmSobek:description>
     <wspmSobek:river>dsf</wspmSobek:river>
     <wspmSobek:uniqueID>231</wspmSobek:uniqueID>
     <wspmSobek:riverLine/>
     <wspmSobek:length>12.0</wspmSobek:length>
    </wspmSobek:Branch>
   </wspmSobek:branchMember>
  </wspmSobekProj:SobekModel>
 </wspmSobekProj:sobekModelMember>
 <nofdp1DModel:lastUsedLanduseAttrib/>
</nofdp1DModel:HydraulModel>
