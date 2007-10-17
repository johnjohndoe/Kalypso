<?xml version="1.0" encoding="WINDOWS-1252"?>
<nofdp1DModel:HydraulModel xmlns:xs="http://www.w3.org/2001/XMLSchema-instance"
 xs:schemaLocation="org.kalypso.nofdpidss.1dmodel ../../workspace/KalypsoNofdpIDSSSchema/src/org/kalypso/nofdp/idss/schema/schemata/hydraulModel.xsd"
 xmlns:nofdp1DModel="org.kalypso.nofdpidss.1dmodel"
 xmlns:xlink="http://www.w3.org/1999/xlink"
 xmlns:gml="http://www.opengis.net/gml"
 gml:id="root">
 <nofdp1DModel:roughnessCategoryMember>
  <nofdp1DModel:RoughnessCategory gml:id="RoughnessCategory11925355132076">
   <nofdp1DModel:name>roughness category</nofdp1DModel:name>
   <nofdp1DModel:description/>
   <nofdp1DModel:roughnessClassList/>
  </nofdp1DModel:RoughnessCategory>
 </nofdp1DModel:roughnessCategoryMember>
 <nofdp1DModel:roughnessClassMember xmlns:nofdpCommons="org.kalypso.nofdpidss.commons">
  <nofdp1DModel:RoughnessClass gml:id="RoughnessClass11925355153792">
   <nofdp1DModel:name/>
   <nofdp1DModel:description/>
   <nofdp1DModel:kstValue>0.1</nofdp1DModel:kstValue>
   <nofdp1DModel:recommendedValueRange/>
  </nofdp1DModel:RoughnessClass>
 </nofdp1DModel:roughnessClassMember>
 <nofdp1DModel:waterBodyMember>
  <nofdp1DModel:WaterBody gml:id="WaterBody11925355177851"/>
 </nofdp1DModel:waterBodyMember>
 <wspmSobekProj:sobekModelMember xmlns:wspmSobekProj="org.kalypso.model.wspm.sobek.project">
  <wspmSobekProj:SobekModel gml:id="SobekModel11925332915821">
   <wspmSobek:nodeMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <wspmSobek:BoundaryConditionNode gml:id="BoundaryConditionNode11925333415983">
     <wspmSobek:name/>
     <wspmSobek:description/>
     <wspmSobek:location/>
     <wspmSobek:uniqueID/>
     <wspmSobek:stationName>asdf</wspmSobek:stationName>
     <wspmSobek:isActiveCalcPoint>true</wspmSobek:isActiveCalcPoint>
     <wspmSobek:bcType>bc_q</wspmSobek:bcType>
    </wspmSobek:BoundaryConditionNode>
   </wspmSobek:nodeMember>
  
   <wspmSobek:nodeMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <nofdp1DModel:WeirNode gml:id="WeirNode11925333319883">
     <wspmSobek:name/>
     <wspmSobek:description/>
     <wspmSobek:location/>
     <wspmSobek:uniqueID/>
     <wspmSobek:stationName>mmm</wspmSobek:stationName>
     <nofdp1DModel:linksToBranch/>
     <nofdp1DModel:crestWidth>2</nofdp1DModel:crestWidth>
     <nofdp1DModel:crestHeight>3</nofdp1DModel:crestHeight>
     <nofdp1DModel:dischargeCoeffCE>1.0</nofdp1DModel:dischargeCoeffCE>
     <nofdp1DModel:lateralContractionCoeffCW>1.0</nofdp1DModel:lateralContractionCoeffCW>
     <nofdp1DModel:flowDirection>Direction.Both</nofdp1DModel:flowDirection>
    </nofdp1DModel:WeirNode>
   </wspmSobek:nodeMember>
   <wspmSobek:nodeMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <nofdp1DModel:RetardingBasinNode gml:id="RetardingBasinNode11925333281132">
     <wspmSobek:name/>
     <wspmSobek:description/>
     <wspmSobek:location/>
     <wspmSobek:uniqueID/>
     <wspmSobek:stationName>daf</wspmSobek:stationName>
     <nofdp1DModel:linksToBranch/>
     <nofdp1DModel:isControlledRetardingBasin>false</nofdp1DModel:isControlledRetardingBasin>
     <nofdp1DModel:crestWidth>12</nofdp1DModel:crestWidth>
     <nofdp1DModel:crestHeight>34</nofdp1DModel:crestHeight>
     <nofdp1DModel:bottomLevel>45</nofdp1DModel:bottomLevel>
     <nofdp1DModel:storageArea>45</nofdp1DModel:storageArea>
     <nofdp1DModel:calculatedVolume>34</nofdp1DModel:calculatedVolume>
    </nofdp1DModel:RetardingBasinNode>
   </wspmSobek:nodeMember>
   <wspmSobek:nodeMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <nofdp1DModel:PolderNode gml:id="PolderNode11925332980510">
     <wspmSobek:name/>
     <wspmSobek:description/>
     <wspmSobek:location/>
     <wspmSobek:uniqueID/>
     <wspmSobek:stationName>fds</wspmSobek:stationName>
     <nofdp1DModel:linksToBranch/>
     <nofdp1DModel:crestWidth>34</nofdp1DModel:crestWidth>
     <nofdp1DModel:crestHeight>34</nofdp1DModel:crestHeight>
     <nofdp1DModel:dischargeCoeffCE>1.0</nofdp1DModel:dischargeCoeffCE>
     <nofdp1DModel:lateralContractionCoeffCW>1.0</nofdp1DModel:lateralContractionCoeffCW>
     <nofdp1DModel:flowDirection>Direction.Both</nofdp1DModel:flowDirection>
     <nofdp1DModel:bottomLevel>23</nofdp1DModel:bottomLevel>
     <nofdp1DModel:storageArea>34</nofdp1DModel:storageArea>
     <nofdp1DModel:capacity>45</nofdp1DModel:capacity>
     <nofdp1DModel:switchOnLevelPressureSide>45</nofdp1DModel:switchOnLevelPressureSide>
     <nofdp1DModel:switchOffLevelPressureSide>45</nofdp1DModel:switchOffLevelPressureSide>
     <nofdp1DModel:calculatedVolume>45</nofdp1DModel:calculatedVolume>
    </nofdp1DModel:PolderNode>
   </wspmSobek:nodeMember>
   <wspmSobek:branchMember xmlns:wspmSobek="org.kalypso.model.wspm.sobek">
    <wspmSobek:Branch gml:id="Branch11925333476131">
     
     <wspmSobek:name/>
     <wspmSobek:description/>

     <wspmSobek:river/>
     
     <wspmSobek:uniqueID/>
     
     <wspmSobek:riverLine/>
     
     <wspmSobek:length>23</wspmSobek:length>
     <wspmSobek:upperConnectionNode/>
     <wspmSobek:lowerConnectionNode/>
    </wspmSobek:Branch>
   </wspmSobek:branchMember>
  </wspmSobekProj:SobekModel>
 </wspmSobekProj:sobekModelMember>
</nofdp1DModel:HydraulModel>
