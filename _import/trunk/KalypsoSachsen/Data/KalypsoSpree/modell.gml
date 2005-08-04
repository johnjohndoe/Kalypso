<?xml version="1.0" encoding="UTF-8"?>
<SpreeModell fid="root" xmlns="org.kalypso.spree.modell" xmlns:gml="http://www.opengis.net/gml" xmlns:obslink="obslink.zml.kalypso.org" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="org.kalypso.spree.modell http://lfug-kv-01:8080/KalypsoConf/schemata/spree/modell.xsd">
  <gml:description/>
  <gml:name/>
  <PegelCollectionAssociation>
    <PegelCollection fid="EZC_1">
      <PegelMember>
        <Einzugsgebiet fid="EZ_1">
          <Name>Schirgiswalde</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5460570.00016158,5660600.021143925</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/W_SCHIRG.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen/QX_SCHIRG.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen/QV_SCHIRG.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink linktype="zml" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582010.P1_MW" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersageAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>70.0</BodenfeuchteMax>
          <Niederschlag_rechnung>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/PA_SCHIRG.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_2">
          <Name>Bautzen</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5458610.000169803,5670080.021103631</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/W_BAUTZWB.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen/QX_BAUTZWB.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen/QV_BAUTZWB.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink linktype="zml" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582030.P1_MW" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersageAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>130.0</BodenfeuchteMax>
          <Niederschlag_rechnung>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/PA_BAUTZWB.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_3">
          <Name>Gröditz 1</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5473800.000107538,5674170.021085799</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/W_GROEDI.zml" valueaxis="Wert" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QX_GROEDI.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QV_GROEDI.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink linktype="zml" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...583121.P1_MW" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersageAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>1.0</Bodenfeuchte>
          <BodenfeuchteMax>70.0</BodenfeuchteMax>
          <Niederschlag_rechnung>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/PA_GROEDI.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_4">
          <Name>Jänkendorf</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5487000.000053392,5679730.021061616</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/W_JAENKD.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QX_JAENKD.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QV_JAENKD.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink linktype="zml" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...583250.P1_MW" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersageAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>200.0</BodenfeuchteMax>
          <Niederschlag_rechnung>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/PA_JAENKD.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_5">
          <Name>Särichen</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5492110.000032412,5681830.021052472</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/W_SAERI.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QX_SAERI.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QV_SAERI.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink linktype="zml" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...583290.P1_MW" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersageAblage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>0.0</Bodenfeuchte>
          <BodenfeuchteMax>310.0</BodenfeuchteMax>
          <Niederschlag_rechnung>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/PA_SAERI.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <FlusslaufModell fid="FM_1">
          <Name>Lieske</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5467400.000134027,5688520.021023534</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/W_LIESKE.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QX_LIESKE.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QV_LIESKE.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink linktype="zml" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582060.P1_MW" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
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
              <gml:coordinates cs="," decimal="." ts=" ">5470540.00012123,5696840.020986796</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/W_BOXBRG.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QX_BOXBRG.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QV_BOXBRG.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink linktype="zml" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...583200.P1_MW" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
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
              <gml:coordinates cs="," decimal="." ts=" ">5466760.000136828,5699710.020974114</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/W_SPREY.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QX_SPREY.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QV_SPREY.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink linktype="zml" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582080.P1_MW" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
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
              <gml:coordinates cs="," decimal="." ts=" ">5458780.000169841,5708640.020934349</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/W_SPWITZ.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QX_SPWITZ.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QV_SPWITZ.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink linktype="zml" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582820.P1_MW" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
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
              <gml:coordinates cs="," decimal="." ts=" ">5456440.000215545,5716010.025081432</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/W_SPREMB.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QX_SPREMB.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen//QV_SPREMB.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Wasserstand_vorhersage>
          <Wasserstand_vorhersageAblage>
            <TimeseriesLink linktype="zml" ns1:href="kalypso-ocs:psicompact://HN.6_SP.02PG...582090.P1_MW" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
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
              <gml:coordinates cs="," decimal="." ts=" ">5482460.000072073,5683900.021043521</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abgabe>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/QV_TSQUITZ.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe>
          <Abgabe_plausibilisiert>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen/QP_TSQUITZ.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe_plausibilisiert>
          <Zufluss_berechnet>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen/Q_TSQUITZ.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Zufluss_berechnet>
          <Stauinhalt>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen/V_TSQUITZ.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Stauinhalt>
        </Talsperre>
      </TalsperreMember>
      <TalsperreMember>
        <Talsperre fid="TS_BAUTZEN">
          <Name>Bautzen</Name>
          <Anfangsstauvolumen>0.0</Anfangsstauvolumen>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5463280.000150746,5675800.02107895</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abgabe>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/QV_TSBAUTZ.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe>
          <Abgabe_plausibilisiert>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen/QP_TSBAUTZ.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abgabe_plausibilisiert>
          <Zufluss_berechnet>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen/Q_TSBAUTZ.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Zufluss_berechnet>
          <Stauinhalt>
            <TimeseriesLink linktype="zml" ns1:href="./Ergebnisse/Zeitreihen/V_TSBAUTZ.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
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
              <gml:coordinates cs="," decimal="." ts=" ">5453430.000191948,5711860.020919969</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/QV_RLKETTE.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abfluss>
        </Flutung>
      </VorgabeMember>
      <VorgabeMember>
        <Flutung fid="F_2">
          <Name>ZL RL Lohsa II</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5466790.00013667,5697390.020984512</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/QV_LOHSA.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abfluss>
        </Flutung>
      </VorgabeMember>
      <VorgabeMember>
        <Flutung fid="F_3">
          <Name>ZL RL Bärwalde</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5466277.000138702,5692313.021006971</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/QV_BWALDE.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abfluss>
        </Flutung>
      </VorgabeMember>
      <VorgabeMember>
        <Flutung fid="F_4">
          <Name>Abzweig Kleine Spree</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5467640.000132938,5681840.021052703</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/QV_SPWIESE.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abfluss>
        </Flutung>
      </VorgabeMember>
      <VorgabeMember>
        <Zufluss fid="Z_1">
          <Name>Burgneudorf</Name>
          <Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5457600.000174678,5707320.020940305</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss>
            <TimeseriesLink linktype="zml" ns1:href="Zeitreihen/QP_BURGNEU.zml" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
          </Abfluss>
        </Zufluss>
      </VorgabeMember>
    </VorgabeCollection>
  </VorgabeCollectionAssociation>
</SpreeModell>
