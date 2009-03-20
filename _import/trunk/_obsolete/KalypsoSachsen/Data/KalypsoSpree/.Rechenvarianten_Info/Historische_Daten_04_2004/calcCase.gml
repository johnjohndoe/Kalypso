<?xml version="1.0" encoding="UTF-8"?>
<SpreeModell fid="root" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:obslink="obslink.zml.kalypso.org" xmlns="org.kalypso.spree.modell" xmlns:gml="http://www.opengis.net/gml" xsi:schemaLocation="org.kalypso.spree.modell project:/.model/schema/modell.xsd">
  <PegelCollectionAssociation>
    <PegelCollection fid="EZC_1">
      <PegelMember>
        <Einzugsgebiet fid="EZ_1">
          <Name>Schirgiswalde</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5460570.000096948,5660600.012686323</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/W_SCHIRG.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen/QX_SCHIRG.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen/QV_SCHIRG.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_vorhersage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>9.11</Bodenfeuchte>
          <BodenfeuchteMax>70.0</BodenfeuchteMax>
          <Niederschlag_rechnung>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/PA_SCHIRG.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_2">
          <Name>Bautzen</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5458610.000101882,5670080.012662144</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/W_BAUTZWB.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen/QX_BAUTZWB.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen/QV_BAUTZWB.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_vorhersage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>35.85</Bodenfeuchte>
          <BodenfeuchteMax>130.0</BodenfeuchteMax>
          <Niederschlag_rechnung>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/PA_BAUTZWB.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_3">
          <Name>Gröditz 1</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5473800.000064522,5674170.012651444</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/W_GROEDI.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QX_GROEDI.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QV_GROEDI.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_vorhersage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>11.44</Bodenfeuchte>
          <BodenfeuchteMax>70.0</BodenfeuchteMax>
          <Niederschlag_rechnung>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/PA_GROEDI.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_4">
          <Name>Jänkendorf</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5487000.000032035,5679730.012636937</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/W_JAENKD.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QX_JAENKD.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QV_JAENKD.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_vorhersage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>12.51</Bodenfeuchte>
          <BodenfeuchteMax>200.0</BodenfeuchteMax>
          <Niederschlag_rechnung>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/PA_JAENKD.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <Einzugsgebiet fid="EZ_5">
          <Name>Särichen</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5492110.000019447,5681830.012631449</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen//W_SAERI.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QX_SAERI.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QV_SAERI.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_vorhersage>
          <BodenfeuchteMin>0.01</BodenfeuchteMin>
          <Bodenfeuchte>17.88</Bodenfeuchte>
          <BodenfeuchteMax>310.0</BodenfeuchteMax>
          <Niederschlag_rechnung>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/PA_SAERI.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Niederschlag_rechnung>
        </Einzugsgebiet>
      </PegelMember>
      <PegelMember>
        <FlusslaufModell fid="FM_1">
          <Name>Lieske</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5467400.000080416,5688520.012614087</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/W_LIESKE.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QX_LIESKE.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QV_LIESKE.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_vorhersage>
          <Korrektur_Faktor>1.0</Korrektur_Faktor>
          <Korrektur_Niveau>0.0</Korrektur_Niveau>
          <Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
          <KorrekturEmpfehlungLaufzeit>-99.0</KorrekturEmpfehlungLaufzeit>
        </FlusslaufModell>
      </PegelMember>
      <PegelMember>
        <FlusslaufModell fid="FM_2">
          <Name>Boxberg</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5470540.000072738,5696840.012592045</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/W_BOXBRG.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QX_BOXBRG.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QV_BOXBRG.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_vorhersage>
          <Korrektur_Faktor>1.0</Korrektur_Faktor>
          <Korrektur_Niveau>0.0</Korrektur_Niveau>
          <Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
          <KorrekturEmpfehlungLaufzeit>2.0</KorrekturEmpfehlungLaufzeit>
        </FlusslaufModell>
      </PegelMember>
      <PegelMember>
        <FlusslaufModell fid="FM_3">
          <Name>Sprey</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5466760.000082097,5699710.012584431</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/W_SPREY.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QX_SPREY.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QV_SPREY.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_vorhersage>
          <Korrektur_Faktor>1.0</Korrektur_Faktor>
          <Korrektur_Niveau>0.0</Korrektur_Niveau>
          <Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
          <KorrekturEmpfehlungLaufzeit>-4.0</KorrekturEmpfehlungLaufzeit>
        </FlusslaufModell>
      </PegelMember>
      <PegelMember>
        <FlusslaufModell fid="FM_4">
          <Name>Spreewitz</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5458780.000101904,5708640.012560573</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/W_SPWITZ.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QX_SPWITZ.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QV_SPWITZ.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_vorhersage>
          <Korrektur_Faktor>1.0</Korrektur_Faktor>
          <Korrektur_Niveau>0.0</Korrektur_Niveau>
          <Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
          <KorrekturEmpfehlungLaufzeit>-99.0</KorrekturEmpfehlungLaufzeit>
        </FlusslaufModell>
      </PegelMember>
      <PegelMember>
        <FlusslaufModell fid="FM_5">
          <Name>Spremberg</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5456440.000143697,5716010.016720953</gml:coordinates>
            </gml:Point>
          </Ort>
          <Wasserstand_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/W_SPREMB.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gemessen>
          <Wasserstand_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QX_SPREMB.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_gerechnet>
          <Wasserstand_vorhersage>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen//QV_SPREMB.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Wasserstand_vorhersage>
          <Korrektur_Faktor>1.0</Korrektur_Faktor>
          <Korrektur_Niveau>0.0</Korrektur_Niveau>
          <Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
          <KorrekturEmpfehlungLaufzeit>-99.0</KorrekturEmpfehlungLaufzeit>
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
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5482460.000043244,5683900.012626079</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abgabe>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/QV_TSQUITZ.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Abgabe>
          <Abgabe_plausibilisiert>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen/QP_TSQUITZ.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Abgabe_plausibilisiert>
          <Zufluss_berechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen/Q_TSQUITZ.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Zufluss_berechnet>
          <Stauinhalt>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen/V_TSQUITZ.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Stauinhalt>
        </Talsperre>
      </TalsperreMember>
      <TalsperreMember>
        <Talsperre fid="TS_BAUTZEN">
          <Name>Bautzen</Name>
          <Anfangsstauvolumen>0.0</Anfangsstauvolumen>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5463280.000090447,5675800.012647334</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abgabe>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/QV_TSBAUTZ.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Abgabe>
          <Abgabe_plausibilisiert>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen/QP_TSBAUTZ.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Abgabe_plausibilisiert>
          <Zufluss_berechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen/Q_TSBAUTZ.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Zufluss_berechnet>
          <Stauinhalt>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Ergebnisse/Zeitreihen/V_TSBAUTZ.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
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
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5453430.000115169,5711860.012551947</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/QV_RLKETTE.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Abfluss>
        </Flutung>
      </VorgabeMember>
      <VorgabeMember>
        <Flutung fid="F_2">
          <Name>ZL RL Lohsa II</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5466790.000082002,5697390.012590707</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/QV_LOHSA.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Abfluss>
        </Flutung>
      </VorgabeMember>
      <VorgabeMember>
        <Flutung fid="F_3">
          <Name>ZL RL Bärwalde</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5466277.000083221,5692313.012604181</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/QV_BWALDE.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Abfluss>
        </Flutung>
      </VorgabeMember>
      <VorgabeMember>
        <Flutung fid="F_4">
          <Name>Abzweig Kleine Spree</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5467640.000079762,5681840.012631588</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/QV_SPWIESE.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Abfluss>
        </Flutung>
      </VorgabeMember>
      <VorgabeMember>
        <Zufluss fid="Z_1">
          <Name>Burgneudorf</Name>
          <Ort>
            <gml:Point srsName="EPSG:31469">
              <gml:coordinates cs="," decimal="." ts=" ">5457600.000104807,5707320.012564149</gml:coordinates>
            </gml:Point>
          </Ort>
          <Abfluss>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:actuate="onRequest" ns1:href="./Zeitreihen/QP_BURGNEU.zml" ns1:type="simple" linktype="zml" timeaxis="Datum" valueaxis="Wert"/>
          </Abfluss>
        </Zufluss>
      </VorgabeMember>
    </VorgabeCollection>
  </VorgabeCollectionAssociation>
</SpreeModell>
