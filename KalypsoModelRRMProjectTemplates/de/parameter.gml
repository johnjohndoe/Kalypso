<?xml version="1.0" encoding="UTF-8"?>
<ns4:Parameter xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xmlns:ns4="http://www.tuhh.de/parameter" xs:schemaLocation="http://www.tuhh.de/parameter null  project:/.model/schema/parameter.xsd" gml:fid="Parameter0">
 <ns4:snowMember>
  <ns4:Snow gml:fid="Snow_beispieltyp">
   <gml:name>beispieltyp</gml:name>
   <ns4:xwwo>0.2</ns4:xwwo>
   <ns4:xwwmax>0.45</ns4:xwwmax>
   <ns4:xsnotem>0.25</ns4:xsnotem>
   <ns4:xsnorad>0.35</ns4:xsnorad>
   <ns4:xh0>0.0</ns4:xh0>
  </ns4:Snow>
 </ns4:snowMember>
 <ns4:soilLayerMember>
  <ns4:SoilLayer gml:fid="SoilLayer_mS">
   <gml:name>mS</gml:name>
   <ns4:typwp>4.5</ns4:typwp>
   <ns4:typfk>13.5</ns4:typfk>
   <ns4:typbfm>41.5</ns4:typbfm>
   <ns4:typkf>6530.0</ns4:typkf>
   <ns4:typbf0>0.75</ns4:typbf0>
  </ns4:SoilLayer>
 </ns4:soilLayerMember>
 <ns4:soilLayerMember>
  <ns4:SoilLayer gml:fid="SoilLayer_mSgsld2">
   <gml:name>mSgsld2</gml:name>
   <ns4:typwp>4.0</ns4:typwp>
   <ns4:typfk>15.0</ns4:typfk>
   <ns4:typbfm>41.0</ns4:typbfm>
   <ns4:typkf>5810.0</ns4:typkf>
   <ns4:typbf0>0.75</ns4:typbf0>
  </ns4:SoilLayer>
 </ns4:soilLayerMember>
 <ns4:soiltypeMember>
  <ns4:Soiltype gml:fid="Soiltype_2mS8mSg6">
   <gml:description>Mittelsand über Torf über Prof. 6</gml:description>
   <gml:name>2mS8mSg6</gml:name>
   <ns4:soilLayerParameterMember>
    <ns4:SoilLayerParameter gml:fid="SoilLayerParameter0">
     <ns4:soilLayerLink xlink:href="#SoilLayer_mS"/>
     <ns4:xtief>2.0</ns4:xtief>
     <ns4:xret>false</ns4:xret>
    </ns4:SoilLayerParameter>
   </ns4:soilLayerParameterMember>
   <ns4:soilLayerParameterMember>
    <ns4:SoilLayerParameter gml:fid="SoilLayerParameter1">
     <ns4:soilLayerLink xlink:href="#SoilLayer_mSgsld2"/>
     <ns4:xtief>8.0</ns4:xtief>
     <ns4:xret>false</ns4:xret>
    </ns4:SoilLayerParameter>
   </ns4:soilLayerParameterMember>
   <ns4:soilLayerParameterMember>
    <ns4:SoilLayerParameter gml:fid="SoilLayerParameter2">
     <ns4:soilLayerLink xlink:href="#SoilLayer_mSgsld2"/>
     <ns4:xtief>5.0</ns4:xtief>
     <ns4:xret>false</ns4:xret>
    </ns4:SoilLayerParameter>
   </ns4:soilLayerParameterMember>
  </ns4:Soiltype>
 </ns4:soiltypeMember>
 <ns4:landuseMember>
  <ns4:Landuse gml:fid="Landuse_ALB_111">
   <gml:description>Gebäude- und Freifläche;öffentliche Zwecke;öffentlich (Verwaltung)</gml:description>
   <gml:name>ALB_111</gml:name>
   <ns4:idealLandUsePeriodLink xlink:href="#IdealLandUse_1"/>
   <ns4:sealingLink xlink:href="#Sealing7"/>
  </ns4:Landuse>
 </ns4:landuseMember>
 <ns4:idealLandUseMember>
  <ns4:IdealLandUse gml:fid="IdealLandUse_1">
   <gml:description>als Mischtyp Garten</gml:description>
   <gml:name>natürliche Flächen von Bebauungstypen</gml:name>
   <idealLandUseZML xmlns="http://www.tuhh.de/parameter">&lt;?xml version="1.0" encoding="UTF-8" standalone="yes"?&gt;
&lt;ns5:observation editable="true" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:ns5="zml.kalypso.org"&gt;
    &lt;ns5:name&gt;Bebauung&lt;/ns5:name&gt;
    &lt;ns5:metadataList&gt;
        &lt;ns5:metadata value="Bebauung" name="Name"/&gt;
        &lt;ns5:metadata value="UTC" name="Zeitzone"/&gt;
    &lt;/ns5:metadataList&gt;
    &lt;ns5:axis unit="-" type="KC" name="Cropfactor" key="false" datatype="TYPE=double"&gt;
        &lt;ns5:valueArray separator=";"&gt;1.0;1.0;1.0;1.0;1.0;1.05;1.1;1.1;1.1;1.1;1.1;1.0;1.0&lt;/ns5:valueArray&gt;
    &lt;/ns5:axis&gt;
    &lt;ns5:axis unit="mm" type="LAI" name="Interzeptionspeicherinhalt" key="false" datatype="TYPE=double"&gt;
        &lt;ns5:valueArray separator=";"&gt;2.0;2.0;2.0;2.0;2.0;2.0;2.0;2.0;2.0;2.0;2.0;2.0;2.0&lt;/ns5:valueArray&gt;
    &lt;/ns5:axis&gt;
    &lt;ns5:axis unit="m" type="WT" name="Wurzeltiefe" key="false" datatype="TYPE=double"&gt;
        &lt;ns5:valueArray separator=";"&gt;0.6;0.6;0.6;0.6;0.6;0.65;0.65;0.65;0.65;0.7;0.65;0.65;0.6&lt;/ns5:valueArray&gt;
    &lt;/ns5:axis&gt;
    &lt;ns5:axis unit="d" type="date" name="Datum" key="true" datatype="TYPE=date#FORMAT=yyyy-MM-dd'T'HH:mm:ss"&gt;
        &lt;ns5:valueArray separator=";"&gt;2000-11-15T11:00:00;2000-12-15T11:00:00;2001-01-15T11:00:00;2001-02-15T11:00:00;2001-03-15T11:00:00;2001-04-15T10:00:00;2001-05-15T10:00:00;2001-06-15T10:00:00;2001-07-15T10:00:00;2001-08-15T10:00:00;2001-09-15T10:00:00;2001-10-15T10:00:00;2001-11-15T11:00:00&lt;/ns5:valueArray&gt;
    &lt;/ns5:axis&gt;
&lt;/ns5:observation&gt;

</idealLandUseZML>
  </ns4:IdealLandUse>
 </ns4:idealLandUseMember>
 <ns4:sealingMember>
  <ns4:Sealing gml:fid="Sealing7">
   <gml:description>Verwaltung</gml:description>
   <gml:name>Klasse7</gml:name>
   <ns4:m_vers>0.782</ns4:m_vers>
  </ns4:Sealing>
 </ns4:sealingMember>
</ns4:Parameter>
