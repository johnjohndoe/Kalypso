<?xml version="1.0" encoding="UTF-8"?>
<SpreeModell fid="root" xmlns:gml="http://www.opengis.net/gml" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="project:/.model/schema/modell.xsd">
  <PegelCollectionAssociation>
    <PegelCollection fid="EZC_1">
      <PegelMember>
        <Einzugsgebiet fid="EZ_1">
          <Name>Schirgiswalde</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.437256873558024,51.08040338685404</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/W_SCHIRG.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_SCHIRG.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen/QP_SCHIRG.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_prognoseAblage>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen/QP_SCHIRG.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_prognoseAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>70.0</BodenfeuchteMax>
          <Ausdehnung/>
          <Niederschlag_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_SCHIRG.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_gemessen>
          <Niederschlag_prognose>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_SCHIRG.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_prognose>
          <Niederschlag_rechnung>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/PA_SCHIRG.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_2">
          <Name>Bautzen</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.408197495081502,51.16548358289859</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/W_BAUTZWB.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_BAUTZWB.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen/QP_BAUTZWB.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_prognoseAblage>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen/QP_BAUTZWB.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_prognoseAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>130.0</BodenfeuchteMax>
          <Ausdehnung/>
          <Niederschlag_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_BAUTZWB.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_gemessen>
          <Niederschlag_prognose>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_BAUTZWB.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_prognose>
          <Niederschlag_rechnung>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/PA_BAUTZWB.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_3">
          <Name>Gröditz 1</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.62508257367659,51.2031475145876</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/W_GROEDI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_GROEDI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_GROEDI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_prognoseAblage>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_GROEDI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_prognoseAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>70.0</BodenfeuchteMax>
          <Ausdehnung/>
          <Niederschlag_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_GROEDI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_gemessen>
          <Niederschlag_prognose>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_GROEDI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_prognose>
          <Niederschlag_rechnung>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/PA_GROEDI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_4">
          <Name>Jänkendorf</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.813769036234167,51.253582013997914</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/W_JAENKD.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_JAENKD.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_JAENKD.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_prognoseAblage>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_JAENKD.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_prognoseAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>200.0</BodenfeuchteMax>
          <Ausdehnung/>
          <Niederschlag_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_JAENKD.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_gemessen>
          <Niederschlag_prognose>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_JAENKD.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_prognose>
          <Niederschlag_rechnung>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/PA_JAENKD.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_5">
          <Name>Särichen</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.88692561760173,51.27255353885482</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/W_SAERI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_SAERI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_SAERI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_prognoseAblage>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_SAERI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_prognoseAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>310.0</BodenfeuchteMax>
          <Ausdehnung/>
          <Niederschlag_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_SAERI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_gemessen>
          <Niederschlag_prognose>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/PA_SAERI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_prognose>
          <Niederschlag_rechnung>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/PA_SAERI.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <FlusslaufModell fid="FM_1">
          <Name>Lieske</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.532194783483517,51.33181354139722</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/W_LIESKE.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_LIESKE.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_LIESKE.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_prognoseAblage>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_LIESKE.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_prognoseAblage>
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
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.576562666413441,51.40677424585018</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/W_BOXBRG.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_BOXBRG.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_BOXBRG.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_prognoseAblage>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_BOXBRG.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_prognoseAblage>
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
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.521964471920874,51.432363723905716</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/W_SPREY.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_SPREY.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_SPREY.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_prognoseAblage>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_SPREY.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_prognoseAblage>
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
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.406166144736858,51.512108877614835</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/W_SPWITZ.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_SPWITZ.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_SPWITZ.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_prognoseAblage>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_SPWITZ.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_prognoseAblage>
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
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.38314223515295,51.55066172469371</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessenEingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/W_SPREMB.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessenEingang>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/W_SPREMB.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_SPREMB.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_prognoseAblage>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./ergebnisse/zeitreihen//QP_SPREMB.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_prognoseAblage>
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
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.748527499155514,51.290946380065435</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abgabe_eingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QV_TSQUITZ.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe_eingang>
          <Abgabe>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/QV_TSQUITZ.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe>
          <Stauinhalt>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/V_TSQUITZ.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Stauinhalt>
        </Talsperre>
      </TalsperreMember>
      <TalsperreMember>
        <Talsperre fid="TS_BAUTZEN">
          <Name>Bautzen</Name>
          <Anfangsstauvolumen>0.0</Anfangsstauvolumen>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.47438224624109,51.21722084046126</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abgabe_eingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QV_TSBAUTZ.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe_eingang>
          <Abgabe>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/QV_TSBAUTZ.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe>
          <Stauinhalt>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/V_TSBAUTZ.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Stauinhalt>
        </Talsperre>
      </TalsperreMember>
    </TalsperreCollection>
  </TalsperreCollectionAssociation>
  <FlutungCollectionAssociation>
    <FlutungCollection fid="FC_1">
      <FlutungMember>
        <Flutung fid="F_1">
          <Name>Überleitung Restlochkette</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.328671484926812,51.54063639386712</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abgabe_eingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QV_RLKETTE.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe_eingang>
          <Abgabe>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/QV_RLKETTE.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe>
        </Flutung>
      </FlutungMember>
      <FlutungMember>
        <Flutung fid="F_2">
          <Name>Zuleiter Restloch Lohsa II</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.503652093785098,51.42219258740152</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abgabe_eingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QV_LOHSA.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe_eingang>
          <Abgabe>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/QV_LOHSA.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe>
        </Flutung>
      </FlutungMember>
      <FlutungMember>
        <Flutung fid="F_3">
          <Name>Zuleiter Restloch Bärwalde</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.522181052271417,51.411599213373556</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abgabe_eingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QV_BWALDE.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe_eingang>
          <Abgabe>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/QV_BWALDE.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe>
        </Flutung>
      </FlutungMember>
      <FlutungMember>
        <Flutung fid="F_4">
          <Name>Abzweig Kleine Spree</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.536244148685535,51.27177959930271</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abgabe_eingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QV_SPWIESE.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe_eingang>
          <Abgabe>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/QV_SPWIESE.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe>
        </Flutung>
      </FlutungMember>
    </FlutungCollection>
  </FlutungCollectionAssociation>
  <ZuflussCollectionAssociation>
    <ZuflussCollection fid="ZC_1">
      <ZuflussMember>
        <Zufluss fid="Z_1">
          <Name>Burgneudorf</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#4326">
              <gml:coordinates cs="," decimal="." ts=" ">14.389326189879572,51.5001565935295</gml:coordinates>
            </gml:Point>
          </Ort>
          <Zugabe_eingang>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="kalypso-ocs:Spree://2004/QP_BURGNEU.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Zugabe_eingang>
          <Zugabe>
            <TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./zeitreihen/QP_BURGNEU.zml" ns1:type="simple" timeaxis="Datum" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Zugabe>
        </Zufluss>
      </ZuflussMember>
    </ZuflussCollection>
  </ZuflussCollectionAssociation>
</SpreeModell>
