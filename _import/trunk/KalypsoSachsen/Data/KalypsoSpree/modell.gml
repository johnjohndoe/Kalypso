<?xml version="1.0" encoding="UTF-8"?>
<SpreeModell fid="root" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns="org.kalypso.spree.modell" xmlns:gml="http://www.opengis.net/gml" xsi:schemaLocation="project:/.model/schema/modell.xsd">
  <PegelCollectionAssociation>
    <PegelCollection fid="EZC_1">
      <PegelMember>
        <Einzugsgebiet fid="EZ_1">
          <Name>Schirgiswalde</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5460570.0,5660599.999999915</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582010?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_SCHIRG.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;W&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen/QX_SCHIRG.zml" ns1:type="simple"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen/QV_SCHIRG.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;Q&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582010.P1_MW" ns1:type="simple"/>
          </Wasserstand_vorhersageAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>70.0</BodenfeuchteMax>
          <Ausdehnung/>
          <Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_SCHIRG.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Niederschlag_gemessen>
          <Niederschlag_prognose>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_SCHIRG.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Niederschlag_prognose>
          <Niederschlag_rechnung>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/PA_SCHIRG.zml" ns1:type="simple"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_2">
          <Name>Bautzen</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5458610.0,5670079.999999915</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582030?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_BAUTZWB.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;W&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen/QX_BAUTZWB.zml" ns1:type="simple"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen/QV_BAUTZWB.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;Q&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582030.P1_MW" ns1:type="simple"/>
          </Wasserstand_vorhersageAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>130.0</BodenfeuchteMax>
          <Ausdehnung/>
          <Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_BAUTZWB.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Niederschlag_gemessen>
          <Niederschlag_prognose>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_BAUTZWB.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Niederschlag_prognose>
          <Niederschlag_rechnung>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/PA_BAUTZWB.zml" ns1:type="simple"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_3">
          <Name>Gröditz 1</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5473800.0,5674169.999999914</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...583121?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_GROEDI.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;W&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QX_GROEDI.zml" ns1:type="simple"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QV_GROEDI.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;Q&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...583121.P1_MW" ns1:type="simple"/>
          </Wasserstand_vorhersageAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>70.0</BodenfeuchteMax>
          <Ausdehnung/>
          <Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_GROEDI.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Niederschlag_gemessen>
          <Niederschlag_prognose>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_GROEDI.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Niederschlag_prognose>
          <Niederschlag_rechnung>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/PA_GROEDI.zml" ns1:type="simple"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_4">
          <Name>Jänkendorf</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5487000.0,5679729.999999914</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...583250?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_JAENKD.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;W&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QX_JAENKD.zml" ns1:type="simple"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QV_JAENKD.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;Q&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...583250.P1_MW" ns1:type="simple"/>
          </Wasserstand_vorhersageAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>200.0</BodenfeuchteMax>
          <Ausdehnung/>
          <Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_JAENKD.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Niederschlag_gemessen>
          <Niederschlag_prognose>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_JAENKD.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Niederschlag_prognose>
          <Niederschlag_rechnung>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/PA_JAENKD.zml" ns1:type="simple"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_5">
          <Name>Särichen</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5492110.0,5681829.999999915</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...583290?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_SAERI.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;W&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QX_SAERI.zml" ns1:type="simple"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QV_SAERI.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;Q&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...583290.P1_MW" ns1:type="simple"/>
          </Wasserstand_vorhersageAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>310.0</BodenfeuchteMax>
          <Ausdehnung/>
          <Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_SAERI.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Niederschlag_gemessen>
          <Niederschlag_prognose>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_SAERI.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Niederschlag_prognose>
          <Niederschlag_rechnung>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/PA_SAERI.zml" ns1:type="simple"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <FlusslaufModell fid="FM_1">
          <Name>Lieske</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5467400.0,5688519.999999912</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582060?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_LIESKE.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;W&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QX_LIESKE.zml" ns1:type="simple"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QV_LIESKE.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;Q&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582060.P1_MW" ns1:type="simple"/>
          </Wasserstand_vorhersageAblage>
          <Korrektur_Faktor>1.0</Korrektur_Faktor>
          <Korrektur_Niveau>0.0</Korrektur_Niveau>
          <Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
          <KorrekturEmpfehlungLaufzeit>0.0</KorrekturEmpfehlungLaufzeit>
        </FlusslaufModell>
      </PegelMember>
      <PegelMember>
        <FlusslaufModell fid="FM_2">
          <Name>Boxberg</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5470540.0,5696839.999999913</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...583200?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_BOXBRG.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;W&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QX_BOXBRG.zml" ns1:type="simple"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QV_BOXBRG.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;Q&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...583200.P1_MW" ns1:type="simple"/>
          </Wasserstand_vorhersageAblage>
          <Korrektur_Faktor>1.0</Korrektur_Faktor>
          <Korrektur_Niveau>0.0</Korrektur_Niveau>
          <Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
          <KorrekturEmpfehlungLaufzeit>-1.0</KorrekturEmpfehlungLaufzeit>
        </FlusslaufModell>
      </PegelMember>
      <PegelMember>
        <FlusslaufModell fid="FM_3">
          <Name>Sprey</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5466760.0,5699709.999999911</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582080?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_SPREY.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;W&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QX_SPREY.zml" ns1:type="simple"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QV_SPREY.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;Q&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582080.P1_MW" ns1:type="simple"/>
          </Wasserstand_vorhersageAblage>
          <Korrektur_Faktor>1.0</Korrektur_Faktor>
          <Korrektur_Niveau>0.0</Korrektur_Niveau>
          <Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
          <KorrekturEmpfehlungLaufzeit>0.0</KorrekturEmpfehlungLaufzeit>
        </FlusslaufModell>
      </PegelMember>
      <PegelMember>
        <FlusslaufModell fid="FM_4">
          <Name>Spreewitz</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5458780.0,5708639.999999911</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582820?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_SPWITZ.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;W&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QX_SPWITZ.zml" ns1:type="simple"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QV_SPWITZ.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;Q&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582820.P1_MW" ns1:type="simple"/>
          </Wasserstand_vorhersageAblage>
          <Korrektur_Faktor>1.0</Korrektur_Faktor>
          <Korrektur_Niveau>0.0</Korrektur_Niveau>
          <Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
          <KorrekturEmpfehlungLaufzeit>0.0</KorrekturEmpfehlungLaufzeit>
        </FlusslaufModell>
      </PegelMember>
      <PegelMember>
        <FlusslaufModell fid="FM_5">
          <Name>Spremberg</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5456440.000035925,5716010.004180241</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="project:/.model/zeitreihen/W_leer.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_SPREMB.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;W&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QX_SPREMB.zml" ns1:type="simple"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QV_SPREMB.zml?&lt;filter&gt;&lt;wqFilter xmlns=&quot;filters.zml.kalypso.org&quot; type=&quot;Q&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="" ns1:type="simple"/>
          </Wasserstand_vorhersageAblage>
          <Korrektur_Faktor>1.0</Korrektur_Faktor>
          <Korrektur_Niveau>0.0</Korrektur_Niveau>
          <Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
          <KorrekturEmpfehlungLaufzeit>0.0</KorrekturEmpfehlungLaufzeit>
        </FlusslaufModell>
      </PegelMember>
    </PegelCollection>
  </PegelCollectionAssociation>
  <TalsperreCollectionAssociation>
    <TalsperreCollection fid="TSC_1">
      <TalsperreMember>
        <Talsperre fid="TS_QUITZDORF">
          <Name>Quitzdorf</Name>
          <Anfangsstauvolumen>0.0</Anfangsstauvolumen>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5482460.0,5683899.999999912</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abgabe_eingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QV_TSQUITZ.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Abgabe_eingang>
          <Abgabe>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/QV_TSQUITZ.zml" ns1:type="simple"/>
          </Abgabe>
          <Abgabe_plausibilisiert>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./Ergebnisse/zeitreihen/QP_TSQUITZ.zml" ns1:type="simple"/>
          </Abgabe_plausibilisiert>
          <Zufluss_berechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./Ergebnisse/zeitreihen/Q_TSQUITZ.zml" ns1:type="simple"/>
          </Zufluss_berechnet>
          <Stauinhalt>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./Ergebnisse/zeitreihen/V_TSQUITZ.zml" ns1:type="simple"/>
          </Stauinhalt>
        </Talsperre>
      </TalsperreMember>
      <TalsperreMember>
        <Talsperre fid="TS_BAUTZEN">
          <Name>Bautzen</Name>
          <Anfangsstauvolumen>0.0</Anfangsstauvolumen>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5463280.0,5675799.999999914</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abgabe_eingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QV_TSBAUTZ.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Abgabe_eingang>
          <Abgabe>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/QV_TSBAUTZ.zml" ns1:type="simple"/>
          </Abgabe>
          <Abgabe_plausibilisiert>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./Ergebnisse/zeitreihen/QP_TSBAUTZ.zml" ns1:type="simple"/>
          </Abgabe_plausibilisiert>
          <Zufluss_berechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./Ergebnisse/zeitreihen/Q_TSBAUTZ.zml" ns1:type="simple"/>
          </Zufluss_berechnet>
          <Stauinhalt>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./Ergebnisse/zeitreihen/V_TSBAUTZ.zml" ns1:type="simple"/>
          </Stauinhalt>
        </Talsperre>
      </TalsperreMember>
    </TalsperreCollection>
  </TalsperreCollectionAssociation>
  <VorgabeCollectionAssociation>
    <VorgabeCollection fid="FC_1">
      <VorgabeMember>
        <Flutung fid="F_1">
          <Name>ÜL RLK</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5453429.999999999,5711859.99999991</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss_eingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QV_RLKETTE.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Abfluss_eingang>
          <Abfluss>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/QV_RLKETTE.zml" ns1:type="simple"/>
          </Abfluss>
        </Flutung>
      </VorgabeMember>
      <VorgabeMember>
        <Flutung fid="F_2">
          <Name>ZL RL Lohsa II</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5466790,5697390</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss_eingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QV_LOHSA.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Abfluss_eingang>
          <Abfluss>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/QV_LOHSA.zml" ns1:type="simple"/>
          </Abfluss>
        </Flutung>
      </VorgabeMember>
      <VorgabeMember>
        <Flutung fid="F_3">
          <Name>ZL RL Bärwalde</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5466277,5692313</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss_eingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QV_BWALDE.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Abfluss_eingang>
          <Abfluss>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/QV_BWALDE.zml" ns1:type="simple"/>
          </Abfluss>
        </Flutung>
      </VorgabeMember>
      <VorgabeMember>
        <Flutung fid="F_4">
          <Name>Abzweig Kleine Spree</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5467640.0,5681839.999999913</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss_eingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QV_SPWIESE.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Abfluss_eingang>
          <Abfluss>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/QV_SPWIESE.zml" ns1:type="simple"/>
          </Abfluss>
        </Flutung>
      </VorgabeMember>
      <VorgabeMember>
        <Zufluss fid="Z_1">
          <Name>Burgneudorf</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5457600.0,5707319.999999911</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss_eingang>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QP_BURGNEU.zml?&lt;filter&gt;&lt;interpolationFilter xmlns=&quot;filters.zml.kalypso.org&quot; calendarField=&quot;HOUR_OF_DAY&quot; amount=&quot;3&quot; forceFill=&quot;true&quot; defaultValue=&quot;0.0&quot; defaultStatus=&quot;2&quot;/&gt;&lt;/filter&gt;" ns1:type="simple"/>
          </Abfluss_eingang>
          <Abfluss>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" linktype="zml" timeaxis="Datum" valueaxis="Wert" ns1:actuate="onRequest" ns1:href="./zeitreihen/QP_BURGNEU.zml" ns1:type="simple"/>
          </Abfluss>
        </Zufluss>
      </VorgabeMember>
    </VorgabeCollection>
  </VorgabeCollectionAssociation>
</SpreeModell>
