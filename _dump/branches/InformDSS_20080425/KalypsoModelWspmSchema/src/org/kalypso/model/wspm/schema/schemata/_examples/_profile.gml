<?xml version="1.0" encoding="UTF-8"?>
<profile:profile xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
     xsi:schemaLocation="
     http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd
     http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
     http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd
     http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
     org.kalypso.model.wspmprofile ../profile.xsd
     "
     xmlns:profile="org.kalypso.model.wspmprofile" xmlns:xlink="http://www.w3.org/1999/xlink"
     xmlns:gml="http://www.opengis.net/gml" xmlns:om="http://www.opengis.net/om"
     xmlns:swe="http://www.opengis.net/swe">

     <gml:metaDataProperty>
          <profile:metaData>
               <name>Kommentar</name>
               <value>
                    <![CDATA[
Henneberger Wehr
]]>
               </value>
          </profile:metaData>
     </gml:metaDataProperty>
     <gml:metaDataProperty>
          <profile:metaData>
               <name>Header</name>
               <value type="WSPWIN_Header">
                    <![CDATA[
L:\ug_werr3\vermessung\050725\Prof184_50_66.d66
                                     
                                        wehr
                                    
                                        0
                                        Werra
Werra vermess                           0
QUERPROFIL 572
STATION KM 223.2694
                 
01.01.00
B-1 0 0 0 0 0 0    
 
]]>
               </value>
          </profile:metaData>
     </gml:metaDataProperty>
     <gml:name>Querprofil Station KM 223.2694</gml:name>
     <om:time>
          <gml:TimeInstant>
               <gml:timePosition>2000-01-01T00:00:00Z</gml:timePosition>
          </gml:TimeInstant>
     </om:time>
     <om:procedure xlink:href="dict_procedure.xml#querprofil"/>
     <!-- die Information richtet sich an den Benutzer -->
     <om:observedProperty xlink:href="dict_phenomenon.xml#querprofil_pasche"/>
     <om:featureOfInterest/>
     <om:resultDefinition>
          <swe:RecordDefinition gml:id="rd" recordLength="10">
               <gml:name/>
               <swe:component xlink:href="dict_components.xml#breite"/>
               <swe:component xlink:href="dict_components.xml#hoehe"/>
               <swe:component xlink:href="dict_components.xml#markierung-trennflaeche"/>
               <swe:component xlink:href="dict_components.xml#markierung-durchstroemtebereiche"/>
               <swe:component xlink:href="dict_components.xml#rauheit-ks"/>
               <swe:component xlink:href="dict_components.xml#gkk-rechtswert"/>
               <swe:component xlink:href="dict_components.xml#gkk-hochwert"/>
          </swe:RecordDefinition>
     </om:resultDefinition>
     <om:result>
          <![CDATA[
-82.4100 286.9600 none true 0.3500 4387568.5026 5604102.3846
-80.4800 287.2500 none false 0.0200 4387569.3529 5604104.1172
-75.4400 285.7700 low false 0.0200 4387571.5732 5604108.6418
-71.3700 283.9000 none false 0.0500 4387573.3663 5604112.2955
-38.2300 283.6500 none false 0.0500 4387587.9663 5604142.0461
-11.1900 283.2500 none false 0.0200 4387599.8789 5604166.3207
 -8.7700 283.1770 none false 0.0200 4387600.9450 5604168.4932
 -8.4100 283.2060 none false 0.0200 4387601.1036 5604168.8163
 -7.3500 283.1770 none false 0.0200 4387601.2358 5604169.0857
 -7.3000 283.2060 none false 0.0200 4387601.4100 5604169.4407
 -7.2400 283.1500 none false 0.0500 4387601.6190 5604169.8667
 -6.9800 283.1600 none false 0.0500 4387601.7336 5604170.1001
 -0.9100 283.1400 none false 0.0200 4387604.4078 5604175.5493
 -0.8100 283.1480 none false 0.0200 4387604.4518 5604175.6391
 -0.7700 283.1480 none false 0.0200 4387604.4694 5604175.6750
 0.0000 283.1480 none false 0.0200 4387604.8087 5604176.3662
 0.2300 283.1480 none false 0.0200 4387604.9100 5604176.5727
 0.2900 283.1770 none false 0.0200 4387604.9364 5604176.6266
 0.5400 283.1200 none false 0.0500 4387605.0466 5604176.8510
 5.6600 283.1600 high false 0.0200 4387607.3022 5604181.4473
 6.3300 288.2900 none false 0.0200 4387607.5974 5604182.0488
 6.3800 288.4800 none false 0.0500 4387607.6194 5604182.0937
 9.9300 288.5700 none false 0.0500 4387609.1834 5604185.2806
 9.9400 289.4200 none false 0.2000 4387609.1878 5604185.2896
 19.8000 289.4600 none true 0.2000 4387613.5316 5604194.1412
 ]]>
     </om:result>
     <profileStation uom="dict_uom.xml#m">223.2694</profileStation>
     <profileMember>
          <om:Observation>
               <gml:name>Maulprofil</gml:name>
               <om:time/>
               <om:procedure/>
               <om:observedProperty/>
               <om:featureOfInterest/>
               <om:resultDefinition xlink:href="dict_resultdef.xml#maulprofil"/>
               <om:result> 3.2233 4.2342 5.2344 6.2344 8.2342 9.1234 34.2342 </om:result>
          </om:Observation>
     </profileMember>
</profile:profile>
