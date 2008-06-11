<?xml version="1.0" encoding="UTF-8"?>
<om:Observation xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="
    http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd
    http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
    http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd
    http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
    org.kalypso.gml.om ../schema_om.xsd
    "
    xmlns:kom="org.kalypso.gml.om" xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om"
    xmlns:swe="http://www.opengis.net/swe">

    <!-- hier werden mehrere Ereignissen in eine Observation abgelegt
    Alternativ: mehrere Observations pro Ereignis -->

    <gml:name>Abflussereignis</gml:name>
    <om:time/>
    <om:procedure xlink:href="dict_procedure.xml#wsp_q_result"/>
    <om:observedProperty/>
    <om:featureOfInterest/>
    <om:resultDefinition>
        <swe:RecordDefinition recordLength="4" gml:id="rd">
            <gml:name/>
            <swe:component xlink:href="dict_components.xml#stat"/>
            <swe:component>
                <swe:ItemDefinition gml:id="hq5">
                    <gml:name>hq5</gml:name>
                    <swe:property xlink:href="dict_phenomenon.xml#q"/>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal" xmlns:st="http://www.seegrid.csiro.au/xml/st">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#qm/s"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component xlink:href="dict_components.xml#hq10"/>
            <swe:component xlink:href="dict_components.xml#hq20"/>
        </swe:RecordDefinition>
    </om:resultDefinition>
    <om:result><![CDATA[
58.4500    2.580     3.690     3.698
58.4930    2.370     3.700     3.706
58.5500    2.610     3.709     3.715
58.6000    2.760     3.719     3.724
    ]]>
    </om:result>
</om:Observation>
