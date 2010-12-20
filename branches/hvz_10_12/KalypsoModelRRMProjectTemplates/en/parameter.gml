<?xml version="1.0" encoding="UTF-8"?>
<Parameter fid="Parameter0" xmlns:zmlinline="inline.zml.kalypso.org" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns="http://www.tuhh.de/parameter" xmlns:obslink="obslink.zml.kalypso.org" xmlns:gml="http://www.opengis.net/gml" xsi:schemaLocation="http://www.tuhh.de/parameter null  project:/.model/schema/parameter.xsd">
  <snowMember>
    <Snow fid="Snow_beispieltyp">
      <gml:name>beispieltyp</gml:name>
      <xwwo>0.2</xwwo>
      <xwwmax>0.45</xwwmax>
      <xsnotem>0.25</xsnotem>
      <xsnorad>0.35</xsnorad>
      <xh0>0.0</xh0>
    </Snow>
  </snowMember>
  <soilLayerMember>
    <SoilLayer fid="SoilLayer_mS">
      <gml:name>mS</gml:name>
      <typkap>mSld2</typkap>
      <typwp>4.5</typwp>
      <typfk>13.5</typfk>
      <typbfm>41.5</typbfm>
      <typkf>6530.0</typkf>
      <typbf0>0.75</typbf0>
    </SoilLayer>
  </soilLayerMember>
  <soilLayerMember>
    <SoilLayer fid="SoilLayer_mSgsld2">
      <gml:name>mSgsld2</gml:name>
      <typkap>mSgld2</typkap>
      <typwp>4.0</typwp>
      <typfk>15.0</typfk>
      <typbfm>41.0</typbfm>
      <typkf>5810.0</typkf>
      <typbf0>0.75</typbf0>
    </SoilLayer>
  </soilLayerMember>
  <soiltypeMember>
    <Soiltype fid="Soiltype_2mS8mSg6">
      <gml:description>Mittelsand über Torf über Prof. 6</gml:description>
      <gml:name>2mS8mSg6</gml:name>
      <soilLayerParameterMember>
        <SoilLayerParameter fid="SoilLayerParameter0">
          <soilLayerLink xlink:href="#SoilLayer_mS"/>
          <xtief>2.0</xtief>
          <xret>1.0</xret>
        </SoilLayerParameter>
      </soilLayerParameterMember>
      <soilLayerParameterMember>
        <SoilLayerParameter fid="SoilLayerParameter1">
          <soilLayerLink xlink:href="#SoilLayer_mSgsld2"/>
          <xtief>8.0</xtief>
          <xret>1.0</xret>
        </SoilLayerParameter>
      </soilLayerParameterMember>
      <soilLayerParameterMember>
        <SoilLayerParameter fid="SoilLayerParameter2">
          <soilLayerLink xlink:href="#SoilLayer_mSgsld2"/>
          <xtief>5.0</xtief>
          <xret>0.0</xret>
        </SoilLayerParameter>
      </soilLayerParameterMember>
    </Soiltype>
  </soiltypeMember>
  <landuseMember>
    <Landuse fid="Landuse_ALB_111">
      <gml:description>Gebäude- und Freifläche;öffentliche Zwecke;öffentlich (Verwaltung)</gml:description>
      <gml:name>ALB_111</gml:name>
      <idealLandUsePeriodLink xlink:href="#IdealLandUse_1"/>
      <sealingLink xlink:href="#Sealing7"/>
    </Landuse>
  </landuseMember>
  <idealLandUseMember>
    <IdealLandUse fid="IdealLandUse_1">
      <gml:description>als Mischtyp Garten</gml:description>
      <gml:name>natürliche Flächen von Bebauungstypen</gml:name>
      <idealLandUseZML>&lt;?xml version="1.0" encoding="UTF-8" standalone="yes"?&gt;
&lt;observation editable="true" xmlns="zml.kalypso.org"&gt;
    &lt;name&gt;Bebauung&lt;/name&gt;
    &lt;metadataList&gt;
        &lt;metadata name="Name" value="Bebauung"/&gt;
    &lt;/metadataList&gt;
    &lt;axis datatype="TYPE=date#FORMAT=yyyy-MM-dd'T'HH:mm:ss" key="true" name="Datum" type="date" unit="d"&gt;
        &lt;valueArray separator=";"&gt;2000-11-15T12:00:00;2000-12-15T12:00:00;2001-01-15T12:00:00;2001-02-15T12:00:00;2001-03-15T12:00:00;2001-04-15T12:00:00;2001-05-15T12:00:00;2001-06-15T12:00:00;2001-07-15T12:00:00;2001-08-15T12:00:00;2001-09-15T12:00:00;2001-10-15T12:00:00;2001-11-15T12:00:00&lt;/valueArray&gt;
    &lt;/axis&gt;
    &lt;axis datatype="TYPE=double" key="false" name="Wurzeltiefe" type="WT" unit="m"&gt;
        &lt;valueArray separator=";"&gt;0.6;0.6;0.6;0.6;0.6;0.65;0.65;0.65;0.65;0.7;0.65;0.65;0.6&lt;/valueArray&gt;
    &lt;/axis&gt;
    &lt;axis datatype="TYPE=double" key="false" name="Interzeptionspeicherinhalt" type="LAI" unit="mm"&gt;
        &lt;valueArray separator=";"&gt;2.0;2.0;2.0;2.0;2.0;2.0;2.0;2.0;2.0;2.0;2.0;2.0;2.0&lt;/valueArray&gt;
    &lt;/axis&gt;
    &lt;axis datatype="TYPE=double" key="false" name="Cropfactor" type="KC" unit="-"&gt;
        &lt;valueArray separator=";"&gt;1.0;1.0;1.0;1.0;1.0;1.05;1.1;1.1;1.1;1.1;1.1;1.0;1.0&lt;/valueArray&gt;
    &lt;/axis&gt;
&lt;/observation&gt;

</idealLandUseZML>
    </IdealLandUse>
  </idealLandUseMember>
  <sealingMember>
    <Sealing fid="Sealing7">
      <gml:description>Verwaltung</gml:description>
      <gml:name>Klasse7</gml:name>
      <m_vers>0.782</m_vers>
    </Sealing>
  </sealingMember>
</Parameter>
