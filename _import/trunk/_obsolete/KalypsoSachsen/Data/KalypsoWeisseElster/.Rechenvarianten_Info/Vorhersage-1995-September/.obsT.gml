<?xml version="1.0" encoding="UTF-8"?>
<MappingCollection fid="ID1" xmlns="http://org.kalypso.updateObservationMapping" xmlns:gml="http://www.opengis.net/gml" xmlns:obslink="obslink.zml.kalypso.org" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
<!-- bisher gibt es nur eine virtuelle Station--> 
  <mappingMember>
    <MappingObservation fid="MappingObservation1">
      <gml:description/>
      <gml:name>virtuelle Temperaturmessstelle</gml:name>
      <point>
		<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
          <gml:coordinates cs="," decimal="." ts=" ">5298301.50233064,5642440.883135041</gml:coordinates>
        </gml:Point>
      </point>
      <inObservationLink>
            <TimeseriesLink linktype="zml" ns1:href="project:/.model/zeitreihen/T_leer.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>        
      </inObservationLink>
      <outObservationLink>
            <TimeseriesLink linktype="zml" ns1:href="Ombrometer/T_virtuell.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>        
      </outObservationLink>
    </MappingObservation>
  </mappingMember>  
</MappingCollection>
