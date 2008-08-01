<?xml version="1.0" encoding="UTF-8"?>
<om:Observation xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="
    http://www.opengis.net/om http://dev.bjoernsen.de/ogc/schema/om/1.0.30/om.xsd
    http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd
    http://www.w3.org/1999/xlink http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/xlink/xlinks.xsd
    http://www.opengis.net/swe http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/swe.xsd
    http://www.seegrid.csiro.au/xml/st http://dev.bjoernsen.de/ogc/schema/sweCommon/1.0.30/simpleTypeDerivation.xsd
    http://www.ksp.org/om ../../schemas/original/kalypso/omExtensions.xsd
    "
    xmlns:kom="http://www.ksp.org/om" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:st="http://www.seegrid.csiro.au/xml/st" xmlns:gml="http://www.opengis.net/gml"
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
            <swe:component>
                <swe:ItemDefinition gml:id="Stat">
                    <gml:name>Station</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-stat">
                            <gml:name>Stationierung</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="4"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#km"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="Kenn">
                    <gml:name>Kennung</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-kenn">
                            <gml:name>Kennung</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:Word>
                            <swe:restriction>
                                <st:pattern value="n|b|w|t|k|e|m|i"/>
                            </swe:restriction>
                            <swe:classification>foo</swe:classification>
                        </swe:Word>
                        <!--swe:Word>
                         <swe:classification gml:remoteSchema="dict_kennung.xml"/>
                    </swe:Word-->
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="Abfluss">
                    <gml:name>Abfluss</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-abfluss">
                            <gml:name>Abfluss</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#m3s"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="Sohle">
                    <gml:name>Sohlhöhe</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-sohle">
                            <gml:name>Sohlhöhe</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="h_WSP">
                    <gml:name>Höhe WSP</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-h_wsp">
                            <gml:name>Höhe WSP</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="hen">
                    <gml:name>Energielinie</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-hen">
                            <gml:name>Energielinie</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="h_BV">
                    <gml:name>Bordvoll Höhe</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-h_bv">
                            <gml:name>Bordvoll Höhe</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="Boe_li">
                    <gml:name>Böschung (links)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-boe_li">
                            <gml:name>Böschung (links)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="Boe_re">
                    <gml:name>Böschung (rechts)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-boe_re">
                            <gml:name>Böschung (rechts)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="v_m">
                    <gml:name>Mittlere Geschwindigkeit</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-v_m">
                            <gml:name>Mittlere Geschwindigkeit</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#m_s"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="tau_fl">
                    <gml:name>Sohlschubspannung (Flussschlauch)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-tau_fl">
                            <gml:name>Sohlschubspannung (Flussschlauch)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="2"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#N_m2"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="lamb_li">
                    <gml:name>lambda (links)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-lamb_li">
                            <gml:name>lambda (links)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="4"/>
                            </st:restriction>
                            <swe:noScale>true</swe:noScale>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="lamb_fl">
                    <gml:name>lambda (Flussschlauch)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-lam_fl">
                            <gml:name>lambda (Flussschlauch)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="4"/>
                            </st:restriction>
                            <swe:noScale>true</swe:noScale>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="lamb_re">
                    <gml:name>lambda (rechts)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-lam_re">
                            <gml:name>lambda (rechts)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="4"/>
                            </st:restriction>
                            <swe:noScale>true</swe:noScale>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="f_li">
                    <gml:name>Fläche (links)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-fl_li">
                            <gml:name>Fläche (links)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#m2"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="f_fl">
                    <gml:name>Fläche (Flussschlauch)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-f_fl">
                            <gml:name>Fläche (Flussschlauch)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#m2"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="f_re">
                    <gml:name>Fläche (rechts)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-f_re">
                            <gml:name>Fläche (rechts)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#m2"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="br_li">
                    <gml:name>Breite (links)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-br_li">
                            <gml:name>Breite (links)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="br_fl">
                    <gml:name>Breite (Flussschlauch)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-br_fl">
                            <gml:name>Breite (Flussschlauch)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="br_re">
                    <gml:name>Breite (rechts)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-br_re">
                            <gml:name>Breite (rechts)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="0.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="WehrOK">
                    <gml:name>Wehr (Oberkante)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-wehrok">
                            <gml:name>Wehr (Oberkante)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="-1000.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="BrueckOK">
                    <gml:name>Brücke (Oberkante)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-brueckok">
                            <gml:name>Brücke (Oberkante)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="-1000.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="BrueckUK">
                    <gml:name>Brücke (Unterkante)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-brueckuk">
                            <gml:name>Brücke (Unterkante)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="-1000.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#mNN"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="BrueckB">
                    <gml:name>Brücke (Breite in Fließrichtung)</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-brueckb">
                            <gml:name>Brücke (Breite in Fließrichtung)</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="-1000.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
            <swe:component>
                <swe:ItemDefinition gml:id="RohrDN">
                    <gml:name>Rohrdurchmesser</gml:name>
                    <swe:property>
                        <swe:Phenomenon gml:id="phen-rohrdn">
                            <gml:name>Rohrdurchmesser</gml:name>
                        </swe:Phenomenon>
                    </swe:property>
                    <swe:representation>
                        <swe:SimpleType>
                            <st:restriction base="decimal">
                                <st:minInclusive value="-1000.0"/>
                                <st:fractionDigits value="3"/>
                            </st:restriction>
                            <gml:unitOfMeasure uom="dict_uom.xml#m"/>
                        </swe:SimpleType>
                    </swe:representation>
                </swe:ItemDefinition>
            </swe:component>
        </swe:RecordDefinition>
    </om:resultDefinition>
    <om:result><![CDATA[
     0.0130    n     2.200     3.841     4.229     4.298     4.339     4.899     4.339   1.164   22.19  0.0000  0.1310  0.0000     0.000     1.890     0.000     0.000     5.597     0.000  -999.999  -999.999  -999.999  -999.999  -999.999
     0.0590    n     2.200     4.140     4.572     4.657     4.161     4.161     4.235   1.281   26.27  0.0903  0.1223  0.0000     0.104     1.373     0.241     0.515     3.400     0.958  -999.999  -999.999  -999.999  -999.999  -999.999
     0.1590    n     2.200     4.313     5.141     5.192     4.700     4.700     4.700   0.957   12.44  0.1215  0.0892  0.1223     0.184     1.792     0.323     0.835     2.700     1.279  -999.999  -999.999  -999.999  -999.999  -999.999
     0.2050    n     2.000     4.362     5.246     5.279     4.927     4.927     4.969   0.787    6.54  0.1183  0.0781  0.1175     0.088     2.388     0.066     0.554     3.165     0.479  -999.999  -999.999  -999.999  -999.999  -999.999
     0.3660    n     2.000     4.822     5.525     5.561     4.822     4.895     4.822   0.821    9.87  0.1099  0.1039  0.0000     0.304     1.866     0.267     0.965     2.900     0.755  -999.999  -999.999  -999.999  -999.999  -999.999
     0.4750    n     2.000     5.053     5.839     5.910     5.400     5.400     5.429   1.045   17.02  0.1222  0.0925  2.7463     0.105     1.543     0.266     0.480     2.365     0.832  -999.999  -999.999  -999.999  -999.999  -999.999
     0.6320    n     2.000     5.355     6.285     6.311     6.772     6.772     6.772   0.720    6.63  0.0000  0.1023  0.0000     0.000     2.777     0.000     0.000     4.963     0.000  -999.999  -999.999  -999.999  -999.999  -999.999
     0.7200    n     1.800     5.401     6.377     6.389     5.597     6.272     5.597   0.460    2.66  0.1121  0.0837  0.1996     0.010     3.272     0.632     0.200     4.135     1.606  -999.999  -999.999  -999.999  -999.999  -999.999
     0.7360    n     1.800     5.425     6.383     6.399     6.040     6.040     6.051   0.548    3.62  0.1112  0.0889  0.1152     0.103     3.046     0.136     0.602     4.020     0.820  -999.999  -999.999  -999.999  -999.999  -999.999
     0.7490    n     1.800     5.257     6.388     6.413     6.040     6.040     6.112   0.679    4.96  0.1159  0.0791  0.1161     0.082     2.470     0.101     0.469     2.829     0.730  -999.999  -999.999  -999.999  -999.999  -999.999
     0.8620    n     1.800     5.450     6.482     6.503     5.949     5.949     6.000   0.607    4.28  0.1083  0.0738  0.1228     0.417     2.317     0.232     1.081     2.643     0.964  -999.999  -999.999  -999.999  -999.999  -999.999
     0.9820    n     1.800     5.441     6.568     6.588     6.000     6.059     6.000   0.581    3.87  0.1285  0.0739  0.1280     0.285     2.488     0.327     1.118     2.651     1.149  -999.999  -999.999  -999.999  -999.999  -999.999
     1.1740    n     1.700     5.740     6.716     6.739     6.249     6.249     6.250   0.635    4.89  0.1146  0.0766  0.1253     0.382     2.084     0.212     1.169     2.588     0.911  -999.999  -999.999  -999.999  -999.999  -999.999
     1.2210    n     1.700     5.961     6.785     6.827     6.265     6.265     6.379   0.866   10.87  0.1119  0.0983  0.0000     0.253     1.545     0.164     0.971     2.190     0.838  -999.999  -999.999  -999.999  -999.999  -999.999
     1.2440    n     1.700     5.905     6.843     6.868     6.200     6.200     6.200   0.673    6.07  0.1275  0.0842  0.1112     0.357     1.808     0.362     1.112     2.150     1.128  -999.999  -999.999  -999.999  -999.999  -999.999
     1.3530    n     1.700     6.099     6.989     7.020     6.400     6.425     6.400   0.751    7.22  0.1206  0.0833  0.1140     0.158     1.720     0.387     0.565     2.227     1.315  -999.999  -999.999  -999.999  -999.999  -999.999
     1.3640    n     1.700     6.251     7.018     7.038     7.769     7.769     7.769   0.622    6.31  0.0000  0.1304  0.0000     0.000     2.732     0.000     0.000     6.983     0.000  -999.999  -999.999  -999.999  -999.999  -999.999
     1.4580    n     1.700     5.852     7.136     7.150     6.200     6.200     6.200   0.432    3.89  0.1013  0.1411  0.1824     1.279     2.281     0.374    10.369     1.950     0.898  -999.999  -999.999  -999.999  -999.999  -999.999
     1.6090    n     1.700     6.320     7.240     7.260     6.565     6.624     6.565   0.590    4.01  0.0694  0.0819  0.1040     0.256     2.403     0.223     4.861     2.828     0.909  -999.999  -999.999  -999.999  -999.999  -999.999
     1.6790    n     1.700     6.306     7.291     7.307     6.600     6.600     6.600   0.517    3.35  0.1249  0.0741  0.1279     0.437     2.308     0.541     1.265     2.600     1.567  -999.999  -999.999  -999.999  -999.999  -999.999
     1.7050    n     1.700     6.356     7.307     7.332     6.700     6.700     6.700   0.651    4.94  0.1181  0.0725  0.1263     0.212     1.991     0.408     0.700     2.300     1.344  -999.999  -999.999  -999.999  -999.999  -999.999
     1.7210    n     1.700     6.257     7.326     7.346     6.700     6.700     6.700   0.599    4.74  0.1297  0.0825  0.1141     0.338     2.056     0.443     1.406     2.300     1.415  -999.999  -999.999  -999.999  -999.999  -999.999
     1.8030    n     1.700     6.523     7.398     7.420     6.869     6.900     6.869   0.621    4.88  0.1380  0.0837  0.1147     0.195     2.211     0.332     0.782     2.748     1.254  -999.999  -999.999  -999.999  -999.999  -999.999
     1.8930    n     1.700     6.719     7.498     7.528     7.000     7.000     7.000   0.734    6.88  0.1163  0.0855  0.1138     0.140     1.901     0.274     0.561     2.700     1.101  -999.999  -999.999  -999.999  -999.999  -999.999
     1.9060    n     1.700     6.819     7.522     7.551     8.000     8.000     8.000   0.748    8.35  0.0000  0.1195  0.0000     0.000     2.274     0.000     0.000     5.242     0.000  -999.999  -999.999  -999.999  -999.999  -999.999
     1.9360    n     1.700     6.436     7.577     7.585     6.442     6.442     6.446   0.345    1.24  0.0979  0.0853  0.0000     0.834     2.558     1.530     1.597     2.254     2.282  -999.999  -999.999  -999.999  -999.999  -999.999
     1.9790    n     1.700     6.766     7.592     7.607     7.183     7.183     7.578   0.548    3.46  0.1175  0.0886  0.0000     0.096     3.008     0.000     0.469     4.700     0.052  -999.999  -999.999  -999.999  -999.999  -999.999
     2.0510    n     1.700     6.680     7.635     7.657     6.938     7.035     6.938   0.618    3.66  0.1164  0.0665  0.1154     0.130     2.435     0.187     0.434     2.857     0.537  -999.999  -999.999  -999.999  -999.999  -999.999
     2.1500    n     1.700     6.823     7.707     7.725     7.082     7.082     7.181   0.556    3.76  0.1183  0.0740  0.1164     0.164     2.098     0.792     0.526     2.748     1.751  -999.999  -999.999  -999.999  -999.999  -999.999
     2.1720    n     1.700     7.169     7.730     7.756     7.396     7.396     7.500   0.697    7.20  0.1188  0.1098  0.1173     0.082     2.281     0.077     0.490     4.871     0.669  -999.999  -999.999  -999.999  -999.999  -999.999
]]>
    </om:result>
</om:Observation>
