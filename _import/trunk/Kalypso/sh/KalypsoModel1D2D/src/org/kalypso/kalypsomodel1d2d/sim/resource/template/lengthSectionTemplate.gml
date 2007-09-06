<?xml version="1.0" encoding="windows-1252"?>
<om:Observation gml:id="LengthSectionResult" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="
    http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd
    http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
    http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd
    http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
    http://www.seegrid.csiro.au/xml/st http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/simpleTypeDerivation.xsd
    "
    xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:gml="http://www.opengis.net/gml"
    xmlns:om="http://www.opengis.net/om" xmlns:swe="http://www.opengis.net/swe">
    <gml:name>Längsschnitt</gml:name>
    <om:time/>
    <om:procedure>
        <om:ObservationProcedure gml:id="proc_wspm_ls">
            <gml:description>WSPM TUHH Längsschnitt Ergebnis</gml:description>
            <gml:name>WSPM-TUHH-LS</gml:name>
            <om:method/>
        </om:ObservationProcedure>
    </om:procedure>
    <om:observedProperty>
        <swe:Phenomenon gml:id="phen_wspm_ls">
            <gml:name>WSPM-TUHH-LS</gml:name>
        </swe:Phenomenon>
    </om:observedProperty>
    <om:featureOfInterest/>
    <om:resultDefinition>
        <swe:RecordDefinition recordLength="25" gml:id="rd">
            <gml:name/>
            <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionStation"/>
            <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionRunOff"/>
            <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionGround"/>
            <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionWaterlevel"/>
        </swe:RecordDefinition>
    </om:resultDefinition>
    <om:result></om:result>
</om:Observation>