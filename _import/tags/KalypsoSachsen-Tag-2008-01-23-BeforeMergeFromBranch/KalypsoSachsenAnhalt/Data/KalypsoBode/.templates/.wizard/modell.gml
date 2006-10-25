<?xml version="1.0" encoding="UTF-8"?>
<TubigModell fid="root" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:tubigpegel="org.kalypso.tubig.pegel" xmlns="org.kalypso.tubig.modell" xmlns:tubigcommon="org.kalypso.tubig.common" xmlns:tubigspeicher="org.kalypso.tubig.speicher" xmlns:tubigombrometer="org.kalypso.tubig.ombrometer" xmlns:obslink="obslink.zml.kalypso.org" xmlns:gml="http://www.opengis.net/gml" xsi:schemaLocation="org.kalypso.tubig.modell project:/.model/schema/modell.xsd">
  <tubigpegel:PegelCollectionAssociation>
    <tubigpegel:PegelCollection fid="PegelCollection0">
      <gml:boundedBy>
        <gml:Box>
          <gml:coord>
            <gml:X>0.0</gml:X>
            <gml:Y>0.0</gml:Y>
          </gml:coord>
          <gml:coord>
            <gml:X>0.0</gml:X>
            <gml:Y>0.0</gml:Y>
          </gml:coord>
        </gml:Box>
      </gml:boundedBy>
      <tubigpegel:PegelMember>
        <tubigpegel:WasserlaufModell fid="WLM_Staßfurt">
          <tubigpegel:Name>Staßfurt</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5265120.666353023,5752160.263210346</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579085</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>stas</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Staßfurt.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Staßfurt.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Hadmersleben -> Stassfurt]]></tubigpegel:Kommentar>
          <tubigpegel:XAW>1.9599</tubigpegel:XAW>
          <tubigpegel:XEW>36.4273</tubigpegel:XEW>
          <tubigpegel:FK1>0.5199</tubigpegel:FK1>
          <tubigpegel:FK2>0.6138</tubigpegel:FK2>
          <tubigpegel:NN1>9.0</tubigpegel:NN1>
          <tubigpegel:NN2>11.0</tubigpegel:NN2>
          <tubigpegel:Laufzeit>2</tubigpegel:Laufzeit>
          <tubigpegel:LaufzeitDefault>2</tubigpegel:LaufzeitDefault>
          <tubigpegel:DM1>1.8548</tubigpegel:DM1>
          <tubigpegel:DM2>8.869</tubigpegel:DM2>
        </tubigpegel:WasserlaufModell>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:WasserlaufModell fid="WLM_Wegeleben">
          <tubigpegel:Name>Wegeleben</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5237817.466836101,5757262.4569166275</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579049</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>wege</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Wegeleben.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Wegeleben.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Ditfurt+Hausneindorf -> Wegeleben]]></tubigpegel:Kommentar>
          <tubigpegel:XAW>0.7505</tubigpegel:XAW>
          <tubigpegel:XEW>-0.3906</tubigpegel:XEW>
          <tubigpegel:FK1>0.0916</tubigpegel:FK1>
          <tubigpegel:FK2>0.8595</tubigpegel:FK2>
          <tubigpegel:NN1>4.0</tubigpegel:NN1>
          <tubigpegel:NN2>4.0</tubigpegel:NN2>
          <tubigpegel:Laufzeit>0</tubigpegel:Laufzeit>
          <tubigpegel:LaufzeitDefault>0</tubigpegel:LaufzeitDefault>
          <tubigpegel:DM1>0.8713</tubigpegel:DM1>
          <tubigpegel:DM2>0.8818</tubigpegel:DM2>
        </tubigpegel:WasserlaufModell>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:WasserlaufModell fid="WLM_Ditfurt">
          <tubigpegel:Name>Ditfurt</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5238766.71591927,5751133.2589417035</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579040</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>ditf</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Ditfurt.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Ditfurt.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Wendefurt->Ditfurt]]></tubigpegel:Kommentar>
          <tubigpegel:XAW>0.5024</tubigpegel:XAW>
          <tubigpegel:XEW>0.1037</tubigpegel:XEW>
          <tubigpegel:FK1>0.6356</tubigpegel:FK1>
          <tubigpegel:FK2>0.8137</tubigpegel:FK2>
          <tubigpegel:NN1>3.0</tubigpegel:NN1>
          <tubigpegel:NN2>3.0</tubigpegel:NN2>
          <tubigpegel:Laufzeit>3</tubigpegel:Laufzeit>
          <tubigpegel:LaufzeitDefault>3</tubigpegel:LaufzeitDefault>
          <tubigpegel:DM1>0.6561</tubigpegel:DM1>
          <tubigpegel:DM2>0.703</tubigpegel:DM2>
        </tubigpegel:WasserlaufModell>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:WasserlaufModell fid="WLM_Nienhagen">
          <tubigpegel:Name>Nienhagen</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5236078.022653717,5763414.323225588</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579745</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>nien</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Nienhagen.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Nienhagen.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Mahndorf -> Nienhagen]]></tubigpegel:Kommentar>
          <tubigpegel:XAW>0.794</tubigpegel:XAW>
          <tubigpegel:XEW>-2.2129</tubigpegel:XEW>
          <tubigpegel:FK1>0.0015</tubigpegel:FK1>
          <tubigpegel:FK2>0.7874</tubigpegel:FK2>
          <tubigpegel:NN1>3.0</tubigpegel:NN1>
          <tubigpegel:NN2>3.0</tubigpegel:NN2>
          <tubigpegel:Laufzeit>0</tubigpegel:Laufzeit>
          <tubigpegel:LaufzeitDefault>0</tubigpegel:LaufzeitDefault>
          <tubigpegel:DM1>0.9809</tubigpegel:DM1>
          <tubigpegel:DM2>0.8134</tubigpegel:DM2>
        </tubigpegel:WasserlaufModell>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:WasserlaufModell fid="WLM_Neugattersleben">
          <tubigpegel:Name>Neugattersleben</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5273131.126460996,5750908.695825978</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579090</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>neug</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>false</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Neugattersleben.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Neugattersleben.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Stassfurt -> Neugattersleben]]></tubigpegel:Kommentar>
          <tubigpegel:XAW>2.3493</tubigpegel:XAW>
          <tubigpegel:XEW>2.4818</tubigpegel:XEW>
          <tubigpegel:FK1>0.0027</tubigpegel:FK1>
          <tubigpegel:FK2>0.8972</tubigpegel:FK2>
          <tubigpegel:NN1>3.0</tubigpegel:NN1>
          <tubigpegel:NN2>3.0</tubigpegel:NN2>
          <tubigpegel:Laufzeit>2</tubigpegel:Laufzeit>
          <tubigpegel:LaufzeitDefault>2</tubigpegel:LaufzeitDefault>
          <tubigpegel:DM1>0.2038</tubigpegel:DM1>
          <tubigpegel:DM2>0.1701</tubigpegel:DM2>
        </tubigpegel:WasserlaufModell>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:WasserlaufModell fid="WLM_Hadmersleben">
          <tubigpegel:Name>Hadmersleben</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5247443.86571238,5769896.309583677</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579070</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>hadm</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Hadmersleben.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Hadmersleben.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Wegeleben+Nienhagen+Oschersleben -> Hadmersleben]]></tubigpegel:Kommentar>
          <tubigpegel:XAW>0.8004</tubigpegel:XAW>
          <tubigpegel:XEW>-0.1929</tubigpegel:XEW>
          <tubigpegel:FK1>0.5975</tubigpegel:FK1>
          <tubigpegel:FK2>0.8898</tubigpegel:FK2>
          <tubigpegel:NN1>2.0</tubigpegel:NN1>
          <tubigpegel:NN2>2.0</tubigpegel:NN2>
          <tubigpegel:Laufzeit>2</tubigpegel:Laufzeit>
          <tubigpegel:LaufzeitDefault>2</tubigpegel:LaufzeitDefault>
          <tubigpegel:DM1>0.6524</tubigpegel:DM1>
          <tubigpegel:DM2>0.6507</tubigpegel:DM2>
        </tubigpegel:WasserlaufModell>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:WasserlaufModell fid="WLM_Hausneindorf">
          <tubigpegel:Name>Hausneindorf</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5242962.026694506,5751581.344781446</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579620</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>haus</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Hausneindorf.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Hausneindorf.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Meisdorf->Hausneindorf]]></tubigpegel:Kommentar>
          <tubigpegel:XAW>0.5884</tubigpegel:XAW>
          <tubigpegel:XEW>-1.1278</tubigpegel:XEW>
          <tubigpegel:FK1>0.1325</tubigpegel:FK1>
          <tubigpegel:FK2>0.8517</tubigpegel:FK2>
          <tubigpegel:NN1>2.0</tubigpegel:NN1>
          <tubigpegel:NN2>3.0</tubigpegel:NN2>
          <tubigpegel:Laufzeit>5</tubigpegel:Laufzeit>
          <tubigpegel:LaufzeitDefault>5</tubigpegel:LaufzeitDefault>
          <tubigpegel:DM1>0.459</tubigpegel:DM1>
          <tubigpegel:DM2>0.1653</tubigpegel:DM2>
        </tubigpegel:WasserlaufModell>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:Niederschlagsgebiet fid="NSGebiet_Meisdorf">
          <tubigpegel:Name>Meisdorf</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5243204.612193139,5735094.576465873</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579610</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>meis</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Meisdorf.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Meisdorf.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Gebiet/>
          <tubigpegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Meisdorf.zml" linktype="zml"/>
          </tubigpegel:Niederschlag>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewMeisdorf_Elend">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Elend"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewMeisdorf_Ditfurt">
              <tubigpegel:faktor>0.2</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Ditfurt"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewMeisdorf_Hasselfelde">
              <tubigpegel:faktor>0.1</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Hasselfelde"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewMeisdorf_Mahndorf">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Mahndorf"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewMeisdorf_Silberhütte">
              <tubigpegel:faktor>0.7</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Silberhütte"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewMeisdorf_Weferlingen">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Weferlingen"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
        </tubigpegel:Niederschlagsgebiet>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:Niederschlagsgebiet fid="NSGebiet_Elend">
          <tubigpegel:Name>Elend</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5202064.849923498,5743158.864709639</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579305</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>elen</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Elend.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Elend.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Gebiet/>
          <tubigpegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Elend.zml" linktype="zml"/>
          </tubigpegel:Niederschlag>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewElend_Elend">
              <tubigpegel:faktor>1.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Elend"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewElend_Ditfurt">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Ditfurt"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewElend_Hasselfelde">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Hasselfelde"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewElend_Mahndorf">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Mahndorf"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewElend_Silberhütte">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Silberhütte"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewElend_Weferlingen">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Weferlingen"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
        </tubigpegel:Niederschlagsgebiet>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:Niederschlagsgebiet fid="NSGebiet_Hasselfelde">
          <tubigpegel:Name>Hasselfelde</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5213844.941062603,5736803.356648105</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>4421200</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>hass</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Hasselfelde.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Hasselfelde.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Gebiet/>
          <tubigpegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Hasselfelde.zml" linktype="zml"/>
          </tubigpegel:Niederschlag>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewHasselfelde_Elend">
              <tubigpegel:faktor>0.1</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Elend"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewHasselfelde_Ditfurt">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Ditfurt"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewHasselfelde_Hasselfelde">
              <tubigpegel:faktor>0.8</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Hasselfelde"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewHasselfelde_Mahndorf">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Mahndorf"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewHasselfelde_Silberhütte">
              <tubigpegel:faktor>0.1</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Silberhütte"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewHasselfelde_Weferlingen">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Weferlingen"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
        </tubigpegel:Niederschlagsgebiet>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:Niederschlagsgebiet fid="NSGebiet_Mahndorf">
          <tubigpegel:Name>Mahndorf</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5222223.011338177,5757795.57484032</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579712</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>mahn</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Mahndorf.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Mahndorf.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Gebiet/>
          <tubigpegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Mahndorf.zml" linktype="zml"/>
          </tubigpegel:Niederschlag>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewMahndorf_Elend">
              <tubigpegel:faktor>0.2</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Elend"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewMahndorf_Ditfurt">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Ditfurt"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewMahndorf_Hasselfelde">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Hasselfelde"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewMahndorf_Mahndorf">
              <tubigpegel:faktor>0.8</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Mahndorf"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewMahndorf_Silberhütte">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Silberhütte"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewMahndorf_Weferlingen">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Weferlingen"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
        </tubigpegel:Niederschlagsgebiet>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:Niederschlagsgebiet fid="NSGebiet_Silberhütte">
          <tubigpegel:Name>Silberhütte</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5229948.558568242,5729148.945125754</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579605</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>silb</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Silberhütte.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Silberhütte.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Gebiet/>
          <tubigpegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Silberhütte.zml" linktype="zml"/>
          </tubigpegel:Niederschlag>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewSilberhütte_Elend">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Elend"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewSilberhütte_Ditfurt">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Ditfurt"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewSilberhütte_Hasselfelde">
              <tubigpegel:faktor>0.3</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Hasselfelde"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewSilberhütte_Mahndorf">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Mahndorf"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewSilberhütte_Silberhütte">
              <tubigpegel:faktor>0.7</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Silberhütte"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewSilberhütte_Weferlingen">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Weferlingen"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
        </tubigpegel:Niederschlagsgebiet>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:Niederschlagsgebiet fid="NSGebiet_Königshütte_WB">
          <tubigpegel:Name>Königshütte-WB</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5207708.385630242,5742265.26812559</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579209</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>koen</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Königshütte.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Königshütte.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Gebiet/>
          <tubigpegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Königshütte.zml" linktype="zml"/>
          </tubigpegel:Niederschlag>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewKönigshütteWB_Elend">
              <tubigpegel:faktor>0.8</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Elend"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewKönigshütteWB_Ditfurt">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Ditfurt"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewKönigshütteWB_Hasselfelde">
              <tubigpegel:faktor>0.2</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Hasselfelde"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewKönigshütteWB_Mahndorf">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Mahndorf"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewKönigshütteWB_Silberhütte">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Silberhütte"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewKönigshütteWB_Weferlingen">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Weferlingen"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
        </tubigpegel:Niederschlagsgebiet>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:Niederschlagsgebiet fid="NSGebiet_Steinerne_Renne">
          <tubigpegel:Name>Steinerne Renne</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5205705.650895029,5751283.956602088</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579705</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>stei</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Steinerne_Renne.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Steinerne_Renne.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Gebiet/>
          <tubigpegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Steinerne_Renne.zml" linktype="zml"/>
          </tubigpegel:Niederschlag>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewStRenne_Elend">
              <tubigpegel:faktor>0.8</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Elend"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewStRenne_Ditfurt">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Ditfurt"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewStRenne_Hasselfelde">
              <tubigpegel:faktor>0.2</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Hasselfelde"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewStRenne_Mahndorf">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Mahndorf"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewStRenne_Silberhütte">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Silberhütte"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewStRenne_Weferlingen">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Weferlingen"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
        </tubigpegel:Niederschlagsgebiet>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:Niederschlagsgebiet fid="NSGebiet_Oschersleben">
          <tubigpegel:Name>Oschersleben</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5241071.153292574,5772263.0450663855</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579810</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>osch</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Oschersleben.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Oschersleben.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Gebiet/>
          <tubigpegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Oschersleben.zml" linktype="zml"/>
          </tubigpegel:Niederschlag>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewOschersleben_Elend">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Elend"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewOschersleben_Ditfurt">
              <tubigpegel:faktor>0.2</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Ditfurt"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewOschersleben_Hasselfelde">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Hasselfelde"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewOschersleben_Mahndorf">
              <tubigpegel:faktor>0.1</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Mahndorf"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewOschersleben_Silberhütte">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Silberhütte"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewOschersleben_Weferlingen">
              <tubigpegel:faktor>0.7</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Weferlingen"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
        </tubigpegel:Niederschlagsgebiet>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:Niederschlagsgebiet fid="NSGebiet_Trautenstein">
          <tubigpegel:Name>Trautenstein</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5209130.667362792,5737147.612297329</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579405</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>trau</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Trautenstein.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Trautenstein.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Gebiet/>
          <tubigpegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Trautenstein.zml" linktype="zml"/>
          </tubigpegel:Niederschlag>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewTrautenstein_Elend">
              <tubigpegel:faktor>0.4</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Elend"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewTrautenstein_Ditfurt">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Ditfurt"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewTrautenstein_Hasselfelde">
              <tubigpegel:faktor>0.6</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Hasselfelde"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewTrautenstein_Mahndorf">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Mahndorf"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewTrautenstein_Silberhütte">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Silberhütte"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewTrautenstein_Weferlingen">
              <tubigpegel:faktor>0.0</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Weferlingen"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
        </tubigpegel:Niederschlagsgebiet>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:Pegel fid="Pegel_Thale">
          <tubigpegel:Name>Thale</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5225486.406048773,5741212.732772261</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579020</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>thal</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Thale.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Thale.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
        </tubigpegel:Pegel>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:Pegel fid="Pegel_Wendefurth">
          <tubigpegel:Name>Wendefurth</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5218308.349121872,5742049.203931116</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>579006</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>wend</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>false</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Wendefurth.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
        </tubigpegel:Pegel>
      </tubigpegel:PegelMember>
    </tubigpegel:PegelCollection>
  </tubigpegel:PegelCollectionAssociation>
  <tubigombrometer:OmbrometerCollectionAssociation>
    <tubigombrometer:OmbrometerCollection fid="OmbrometerCollection0">
      <gml:boundedBy>
        <gml:Box>
          <gml:coord>
            <gml:X>0.0</gml:X>
            <gml:Y>0.0</gml:Y>
          </gml:coord>
          <gml:coord>
            <gml:X>0.0</gml:X>
            <gml:Y>0.0</gml:Y>
          </gml:coord>
        </gml:Box>
      </gml:boundedBy>
      <tubigombrometer:OmbrometerMember>
        <tubigombrometer:Ombrometer fid="Ombro_Weferlingen">
          <tubigombrometer:Name>Weferlingen</tubigombrometer:Name>
          <tubigombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5231071.410767844,5805231.416728718</gml:coordinates>
            </gml:Point>
          </tubigombrometer:Ort>
          <tubigombrometer:Messstellennummer>440010</tubigombrometer:Messstellennummer>
          <tubigombrometer:Kurz_Name>wefe</tubigombrometer:Kurz_Name>
          <tubigombrometer:Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Weferlingen.zml" linktype="zml"/>
          </tubigombrometer:Niederschlag_gemessen>
        </tubigombrometer:Ombrometer>
      </tubigombrometer:OmbrometerMember>
      <tubigombrometer:OmbrometerMember>
        <tubigombrometer:Ombrometer fid="Ombro_Ditfurt">
          <tubigombrometer:Name>Ditfurt</tubigombrometer:Name>
          <tubigombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5238766.71591927,5751133.2589417035</gml:coordinates>
            </gml:Point>
          </tubigombrometer:Ort>
          <tubigombrometer:Messstellennummer>579040</tubigombrometer:Messstellennummer>
          <tubigombrometer:Kurz_Name>ditf</tubigombrometer:Kurz_Name>
          <tubigombrometer:Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Ditfurt.zml" linktype="zml"/>
          </tubigombrometer:Niederschlag_gemessen>
        </tubigombrometer:Ombrometer>
      </tubigombrometer:OmbrometerMember>
      <tubigombrometer:OmbrometerMember>
        <tubigombrometer:Ombrometer fid="Ombro_Elend">
          <tubigombrometer:Name>Elend</tubigombrometer:Name>
          <tubigombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5202064.849923498,5743158.864709639</gml:coordinates>
            </gml:Point>
          </tubigombrometer:Ort>
          <tubigombrometer:Messstellennummer>579305</tubigombrometer:Messstellennummer>
          <tubigombrometer:Kurz_Name>elen</tubigombrometer:Kurz_Name>
          <tubigombrometer:Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Elend.zml" linktype="zml"/>
          </tubigombrometer:Niederschlag_gemessen>
        </tubigombrometer:Ombrometer>
      </tubigombrometer:OmbrometerMember>
      <tubigombrometer:OmbrometerMember>
        <tubigombrometer:Ombrometer fid="Ombro_Hasselfelde">
          <tubigombrometer:Name>Hasselfelde</tubigombrometer:Name>
          <tubigombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5213844.941062603,5736803.356648105</gml:coordinates>
            </gml:Point>
          </tubigombrometer:Ort>
          <tubigombrometer:Messstellennummer>579504</tubigombrometer:Messstellennummer>
          <tubigombrometer:Kurz_Name>hass</tubigombrometer:Kurz_Name>
          <tubigombrometer:Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Hasselfelde.zml" linktype="zml"/>
          </tubigombrometer:Niederschlag_gemessen>
        </tubigombrometer:Ombrometer>
      </tubigombrometer:OmbrometerMember>
      <tubigombrometer:OmbrometerMember>
        <tubigombrometer:Ombrometer fid="Ombro_Silberhütte">
          <tubigombrometer:Name>Silberhütte</tubigombrometer:Name>
          <tubigombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5229948.558568242,5729148.945125754</gml:coordinates>
            </gml:Point>
          </tubigombrometer:Ort>
          <tubigombrometer:Messstellennummer>579605</tubigombrometer:Messstellennummer>
          <tubigombrometer:Kurz_Name>silb</tubigombrometer:Kurz_Name>
          <tubigombrometer:Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Silberhütte.zml" linktype="zml"/>
          </tubigombrometer:Niederschlag_gemessen>
        </tubigombrometer:Ombrometer>
      </tubigombrometer:OmbrometerMember>
      <tubigombrometer:OmbrometerMember>
        <tubigombrometer:Ombrometer fid="Ombro_Mahndorf">
          <tubigombrometer:Name>Mahndorf</tubigombrometer:Name>
          <tubigombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5222223.011338177,5757795.57484032</gml:coordinates>
            </gml:Point>
          </tubigombrometer:Ort>
          <tubigombrometer:Messstellennummer>579712</tubigombrometer:Messstellennummer>
          <tubigombrometer:Kurz_Name>mahn</tubigombrometer:Kurz_Name>
          <tubigombrometer:Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Mahndorf.zml" linktype="zml"/>
          </tubigombrometer:Niederschlag_gemessen>
        </tubigombrometer:Ombrometer>
      </tubigombrometer:OmbrometerMember>
    </tubigombrometer:OmbrometerCollection>
  </tubigombrometer:OmbrometerCollectionAssociation>
  <tubigspeicher:SpeicherCollectionAssociation>
    <tubigspeicher:SpeicherCollection fid="SpeicherCollection0">
      <gml:boundedBy>
        <gml:Box>
          <gml:coord>
            <gml:X>0.0</gml:X>
            <gml:Y>0.0</gml:Y>
          </gml:coord>
          <gml:coord>
            <gml:X>0.0</gml:X>
            <gml:Y>0.0</gml:Y>
          </gml:coord>
        </gml:Box>
      </gml:boundedBy>
      <tubigspeicher:SpeicherMember>
        <tubigspeicher:Speicher fid="Speicher_KalteBode">
          <tubigspeicher:Name>HWR Kalte Bode</tubigspeicher:Name>
          <tubigspeicher:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5206105.143908598,5743473.354427688</gml:coordinates>
            </gml:Point>
          </tubigspeicher:Ort>
          <tubigspeicher:Messstellennummer>579320</tubigspeicher:Messstellennummer>
          <tubigspeicher:Kurz_Name>kabo</tubigspeicher:Kurz_Name>
          <tubigspeicher:Abgabe>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicherabgabe/Kalte_Bode.zml" linktype="zml"/>
          </tubigspeicher:Abgabe>
          <tubigspeicher:Zufluss_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicherzufluss/Kalte_Bode.zml" linktype="zml"/>
          </tubigspeicher:Zufluss_gerechnet>
          <tubigspeicher:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicher/Messung/Kalte_Bode.zml" linktype="zml"/>
          </tubigspeicher:Ganglinie_gemessen>
          <tubigspeicher:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicher/Vorhersage/Kalte_Bode.zml" linktype="zml"/>
          </tubigspeicher:Ganglinie_gerechnet>
          <tubigspeicher:StauraumParameter>
            <tubigspeicher:Stauraum fid="Stauraum_kabo">
              <tubigspeicher:Totraum>0.2</tubigspeicher:Totraum>
              <tubigspeicher:Reserveraum>0.34</tubigspeicher:Reserveraum>
              <tubigspeicher:Betriebsraum>
                <tubigcommon:MonatCollection fid="Stauraum_kabo_Jahr">
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_kabo_Jan">
                      <tubigcommon:Name>Januar</tubigcommon:Name>
                      <tubigcommon:Nummer>1</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_kabo_Feb">
                      <tubigcommon:Name>Februar</tubigcommon:Name>
                      <tubigcommon:Nummer>2</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_kabo_Mrz">
                      <tubigcommon:Name>März</tubigcommon:Name>
                      <tubigcommon:Nummer>3</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_kabo_Apr">
                      <tubigcommon:Name>April</tubigcommon:Name>
                      <tubigcommon:Nummer>4</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_kabo_Mai">
                      <tubigcommon:Name>Mai</tubigcommon:Name>
                      <tubigcommon:Nummer>5</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.29</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_kabo_Jun">
                      <tubigcommon:Name>Juni</tubigcommon:Name>
                      <tubigcommon:Nummer>6</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.29</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_kabo_Jul">
                      <tubigcommon:Name>Juli</tubigcommon:Name>
                      <tubigcommon:Nummer>7</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.29</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_kabo_Aug">
                      <tubigcommon:Name>August</tubigcommon:Name>
                      <tubigcommon:Nummer>8</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.29</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_kabo_Sep">
                      <tubigcommon:Name>September</tubigcommon:Name>
                      <tubigcommon:Nummer>9</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.29</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_kabo_Okt">
                      <tubigcommon:Name>Oktober</tubigcommon:Name>
                      <tubigcommon:Nummer>10</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.29</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_kabo_Nov">
                      <tubigcommon:Name>November</tubigcommon:Name>
                      <tubigcommon:Nummer>11</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_kabo_Dez">
                      <tubigcommon:Name>Dezember</tubigcommon:Name>
                      <tubigcommon:Nummer>12</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                </tubigcommon:MonatCollection>
              </tubigspeicher:Betriebsraum>
              <tubigspeicher:Stauraum>4.47</tubigspeicher:Stauraum>
            </tubigspeicher:Stauraum>
          </tubigspeicher:StauraumParameter>
          <tubigspeicher:MindestabgabeParameter>
            <tubigspeicher:Mindestabgabe fid="MinAbgabe_kabo">
              <tubigspeicher:Mindestabgabe>
                <tubigcommon:MonatCollection fid="MinAbgabe_kabo_Jahr">
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_kabo_Jan">
                      <tubigcommon:Name>Januar</tubigcommon:Name>
                      <tubigcommon:Nummer>1</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.05</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_kabo_Feb">
                      <tubigcommon:Name>Februar</tubigcommon:Name>
                      <tubigcommon:Nummer>2</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.05</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_kabo_Mrz">
                      <tubigcommon:Name>März</tubigcommon:Name>
                      <tubigcommon:Nummer>3</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.05</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_kabo_Apr">
                      <tubigcommon:Name>April</tubigcommon:Name>
                      <tubigcommon:Nummer>4</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.05</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_kabo_Mai">
                      <tubigcommon:Name>Mai</tubigcommon:Name>
                      <tubigcommon:Nummer>5</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.05</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_kabo_Jun">
                      <tubigcommon:Name>Juni</tubigcommon:Name>
                      <tubigcommon:Nummer>6</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.05</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_kabo_Jul">
                      <tubigcommon:Name>Juli</tubigcommon:Name>
                      <tubigcommon:Nummer>7</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.05</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_kabo_Aug">
                      <tubigcommon:Name>August</tubigcommon:Name>
                      <tubigcommon:Nummer>8</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.05</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_kabo_Sep">
                      <tubigcommon:Name>September</tubigcommon:Name>
                      <tubigcommon:Nummer>9</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.05</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_kabo_Okt">
                      <tubigcommon:Name>Oktober</tubigcommon:Name>
                      <tubigcommon:Nummer>10</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.05</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_kabo_Nov">
                      <tubigcommon:Name>November</tubigcommon:Name>
                      <tubigcommon:Nummer>11</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.05</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_kabo_Dez">
                      <tubigcommon:Name>Dezember</tubigcommon:Name>
                      <tubigcommon:Nummer>12</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.05</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                </tubigcommon:MonatCollection>
              </tubigspeicher:Mindestabgabe>
            </tubigspeicher:Mindestabgabe>
          </tubigspeicher:MindestabgabeParameter>
          <tubigspeicher:MaximalabgabeParameter>
            <tubigspeicher:Maximalabgabe fid="MaxAbgabe_kabo">
              <tubigspeicher:Maximalabgabe>
                <tubigcommon:MonatCollection fid="MaxAbgabe_kabo_Jahr">
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_kabo_Jan">
                      <tubigcommon:Name>Januar</tubigcommon:Name>
                      <tubigcommon:Nummer>1</tubigcommon:Nummer>
                      <tubigcommon:Wert>35.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_kabo_Feb">
                      <tubigcommon:Name>Februar</tubigcommon:Name>
                      <tubigcommon:Nummer>2</tubigcommon:Nummer>
                      <tubigcommon:Wert>35.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_kabo_Mrz">
                      <tubigcommon:Name>März</tubigcommon:Name>
                      <tubigcommon:Nummer>3</tubigcommon:Nummer>
                      <tubigcommon:Wert>35.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_kabo_Apr">
                      <tubigcommon:Name>April</tubigcommon:Name>
                      <tubigcommon:Nummer>4</tubigcommon:Nummer>
                      <tubigcommon:Wert>35.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_kabo_Mai">
                      <tubigcommon:Name>Mai</tubigcommon:Name>
                      <tubigcommon:Nummer>5</tubigcommon:Nummer>
                      <tubigcommon:Wert>35.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_kabo_Jun">
                      <tubigcommon:Name>Juni</tubigcommon:Name>
                      <tubigcommon:Nummer>6</tubigcommon:Nummer>
                      <tubigcommon:Wert>35.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_kabo_Jul">
                      <tubigcommon:Name>Juli</tubigcommon:Name>
                      <tubigcommon:Nummer>7</tubigcommon:Nummer>
                      <tubigcommon:Wert>35.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_kabo_Aug">
                      <tubigcommon:Name>August</tubigcommon:Name>
                      <tubigcommon:Nummer>8</tubigcommon:Nummer>
                      <tubigcommon:Wert>35.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_kabo_Sep">
                      <tubigcommon:Name>September</tubigcommon:Name>
                      <tubigcommon:Nummer>9</tubigcommon:Nummer>
                      <tubigcommon:Wert>35.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_kabo_Okt">
                      <tubigcommon:Name>Oktober</tubigcommon:Name>
                      <tubigcommon:Nummer>10</tubigcommon:Nummer>
                      <tubigcommon:Wert>35.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_kabo_Nov">
                      <tubigcommon:Name>November</tubigcommon:Name>
                      <tubigcommon:Nummer>11</tubigcommon:Nummer>
                      <tubigcommon:Wert>35.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_kabo_Dez">
                      <tubigcommon:Name>Dezember</tubigcommon:Name>
                      <tubigcommon:Nummer>12</tubigcommon:Nummer>
                      <tubigcommon:Wert>35.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                </tubigcommon:MonatCollection>
              </tubigspeicher:Maximalabgabe>
            </tubigspeicher:Maximalabgabe>
          </tubigspeicher:MaximalabgabeParameter>
          <tubigspeicher:TrinkwasserParameter>
            <tubigspeicher:Trinkwasser fid="Trinkwasser_kabo">
              <tubigspeicher:Trinkwasser>
                <tubigcommon:MonatCollection fid="Trinkwasser_kabo_Jahr">
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_kabo_Jan">
                      <tubigcommon:Name>Januar</tubigcommon:Name>
                      <tubigcommon:Nummer>1</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_kabo_Feb">
                      <tubigcommon:Name>Februar</tubigcommon:Name>
                      <tubigcommon:Nummer>2</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_kabo_Mrz">
                      <tubigcommon:Name>März</tubigcommon:Name>
                      <tubigcommon:Nummer>3</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_kabo_Apr">
                      <tubigcommon:Name>April</tubigcommon:Name>
                      <tubigcommon:Nummer>4</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_kabo_Mai">
                      <tubigcommon:Name>Mai</tubigcommon:Name>
                      <tubigcommon:Nummer>5</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_kabo_Jun">
                      <tubigcommon:Name>Juni</tubigcommon:Name>
                      <tubigcommon:Nummer>6</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_kabo_Jul">
                      <tubigcommon:Name>Juli</tubigcommon:Name>
                      <tubigcommon:Nummer>7</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_kabo_Aug">
                      <tubigcommon:Name>August</tubigcommon:Name>
                      <tubigcommon:Nummer>8</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_kabo_Sep">
                      <tubigcommon:Name>September</tubigcommon:Name>
                      <tubigcommon:Nummer>9</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_kabo_Okt">
                      <tubigcommon:Name>Oktober</tubigcommon:Name>
                      <tubigcommon:Nummer>10</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_kabo_Nov">
                      <tubigcommon:Name>November</tubigcommon:Name>
                      <tubigcommon:Nummer>11</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_kabo_Dez">
                      <tubigcommon:Name>Dezember</tubigcommon:Name>
                      <tubigcommon:Nummer>12</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                </tubigcommon:MonatCollection>
              </tubigspeicher:Trinkwasser>
            </tubigspeicher:Trinkwasser>
          </tubigspeicher:TrinkwasserParameter>
          <tubigspeicher:EACollectionAssociation>
            <tubigspeicher:EACollection fid="EACollection_kabo">
              <gml:boundedBy>
                <gml:Box>
                  <gml:coord>
                    <gml:X>0.0</gml:X>
                    <gml:Y>0.0</gml:Y>
                  </gml:coord>
                  <gml:coord>
                    <gml:X>0.0</gml:X>
                    <gml:Y>0.0</gml:Y>
                  </gml:coord>
                </gml:Box>
              </gml:boundedBy>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_1">
                  <tubigspeicher:Höhe>446.35</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>0.0</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>0.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_2">
                  <tubigspeicher:Höhe>451.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>0.2</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>8.36</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_3">
                  <tubigspeicher:Höhe>454.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>0.54</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>10.68</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung>Stauziel Winter</tubigspeicher:Bemerkung>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_4">
                  <tubigspeicher:Höhe>456.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>0.83</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>11.89</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung>Stauziel Sommer</tubigspeicher:Bemerkung>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_5">
                  <tubigspeicher:Höhe>458.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>1.21</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>13.11</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_6">
                  <tubigspeicher:Höhe>460.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>1.73</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>14.32</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_7">
                  <tubigspeicher:Höhe>462.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>2.38</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>15.27</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_8">
                  <tubigspeicher:Höhe>464.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>3.2</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>16.23</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_9">
                  <tubigspeicher:Höhe>466.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>4.2</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>17.18</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_10">
                  <tubigspeicher:Höhe>466.5</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>4.47</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>17.42</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung>Vollstau</tubigspeicher:Bemerkung>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_11">
                  <tubigspeicher:Höhe>466.7</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>4.56</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>17.52</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>10.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_12">
                  <tubigspeicher:Höhe>467.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>4.75</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>17.66</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>42.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_kabo_13">
                  <tubigspeicher:Höhe>467.4</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>4.95</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>17.85</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>112.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
            </tubigspeicher:EACollection>
          </tubigspeicher:EACollectionAssociation>
        </tubigspeicher:Speicher>
      </tubigspeicher:SpeicherMember>
      <tubigspeicher:SpeicherMember>
        <tubigspeicher:Speicher fid="Speicher_Rappbode">
          <tubigspeicher:Name>TS Rappbode</tubigspeicher:Name>
          <tubigspeicher:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5216625.661087754,5744001.801473191</gml:coordinates>
            </gml:Point>
          </tubigspeicher:Ort>
          <tubigspeicher:Messstellennummer>579430</tubigspeicher:Messstellennummer>
          <tubigspeicher:Kurz_Name>rapp</tubigspeicher:Kurz_Name>
          <tubigspeicher:Abgabe>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicherabgabe/Rappbode.zml" linktype="zml"/>
          </tubigspeicher:Abgabe>
          <tubigspeicher:Zufluss_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicherzufluss/Rappbode.zml" linktype="zml"/>
          </tubigspeicher:Zufluss_gerechnet>
          <tubigspeicher:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicher/Messung/Rappbode.zml" linktype="zml"/>
          </tubigspeicher:Ganglinie_gemessen>
          <tubigspeicher:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicher/Vorhersage/Rappbode.zml" linktype="zml"/>
          </tubigspeicher:Ganglinie_gerechnet>
          <tubigspeicher:StauraumParameter>
            <tubigspeicher:Stauraum fid="Stauraum_rapp">
              <tubigspeicher:Totraum>0.34</tubigspeicher:Totraum>
              <tubigspeicher:Reserveraum>3.63</tubigspeicher:Reserveraum>
              <tubigspeicher:Betriebsraum>
                <tubigcommon:MonatCollection fid="Stauraum_rapp_Jahr">
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_rapp_Jan">
                      <tubigcommon:Name>Januar</tubigcommon:Name>
                      <tubigcommon:Nummer>1</tubigcommon:Nummer>
                      <tubigcommon:Wert>91.03</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_rapp_Feb">
                      <tubigcommon:Name>Februar</tubigcommon:Name>
                      <tubigcommon:Nummer>2</tubigcommon:Nummer>
                      <tubigcommon:Wert>91.03</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_rapp_Mrz">
                      <tubigcommon:Name>März</tubigcommon:Name>
                      <tubigcommon:Nummer>3</tubigcommon:Nummer>
                      <tubigcommon:Wert>91.03</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_rapp_Apr">
                      <tubigcommon:Name>April</tubigcommon:Name>
                      <tubigcommon:Nummer>4</tubigcommon:Nummer>
                      <tubigcommon:Wert>91.03</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_rapp_Mai">
                      <tubigcommon:Name>Mai</tubigcommon:Name>
                      <tubigcommon:Nummer>5</tubigcommon:Nummer>
                      <tubigcommon:Wert>105.11</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_rapp_Jun">
                      <tubigcommon:Name>Juni</tubigcommon:Name>
                      <tubigcommon:Nummer>6</tubigcommon:Nummer>
                      <tubigcommon:Wert>105.11</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_rapp_Jul">
                      <tubigcommon:Name>Juli</tubigcommon:Name>
                      <tubigcommon:Nummer>7</tubigcommon:Nummer>
                      <tubigcommon:Wert>105.11</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_rapp_Aug">
                      <tubigcommon:Name>August</tubigcommon:Name>
                      <tubigcommon:Nummer>8</tubigcommon:Nummer>
                      <tubigcommon:Wert>105.11</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_rapp_Sep">
                      <tubigcommon:Name>September</tubigcommon:Name>
                      <tubigcommon:Nummer>9</tubigcommon:Nummer>
                      <tubigcommon:Wert>105.11</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_rapp_Okt">
                      <tubigcommon:Name>Oktober</tubigcommon:Name>
                      <tubigcommon:Nummer>10</tubigcommon:Nummer>
                      <tubigcommon:Wert>105.11</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_rapp_Nov">
                      <tubigcommon:Name>November</tubigcommon:Name>
                      <tubigcommon:Nummer>11</tubigcommon:Nummer>
                      <tubigcommon:Wert>91.03</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_rapp_Dez">
                      <tubigcommon:Name>Dezember</tubigcommon:Name>
                      <tubigcommon:Nummer>12</tubigcommon:Nummer>
                      <tubigcommon:Wert>91.03</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                </tubigcommon:MonatCollection>
              </tubigspeicher:Betriebsraum>
              <tubigspeicher:Stauraum>109.08</tubigspeicher:Stauraum>
            </tubigspeicher:Stauraum>
          </tubigspeicher:StauraumParameter>
          <tubigspeicher:MindestabgabeParameter>
            <tubigspeicher:Mindestabgabe fid="MinAbgabe_rapp">
              <tubigspeicher:Mindestabgabe>
                <tubigcommon:MonatCollection fid="MinAbgabe_rapp_Jahr">
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_rapp_Jan">
                      <tubigcommon:Name>Januar</tubigcommon:Name>
                      <tubigcommon:Nummer>1</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.5</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_rapp_Feb">
                      <tubigcommon:Name>Februar</tubigcommon:Name>
                      <tubigcommon:Nummer>2</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.5</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_rapp_Mrz">
                      <tubigcommon:Name>März</tubigcommon:Name>
                      <tubigcommon:Nummer>3</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.5</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_rapp_Apr">
                      <tubigcommon:Name>April</tubigcommon:Name>
                      <tubigcommon:Nummer>4</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.5</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_rapp_Mai">
                      <tubigcommon:Name>Mai</tubigcommon:Name>
                      <tubigcommon:Nummer>5</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.5</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_rapp_Jun">
                      <tubigcommon:Name>Juni</tubigcommon:Name>
                      <tubigcommon:Nummer>6</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.5</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_rapp_Jul">
                      <tubigcommon:Name>Juli</tubigcommon:Name>
                      <tubigcommon:Nummer>7</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.5</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_rapp_Aug">
                      <tubigcommon:Name>August</tubigcommon:Name>
                      <tubigcommon:Nummer>8</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.5</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_rapp_Sep">
                      <tubigcommon:Name>September</tubigcommon:Name>
                      <tubigcommon:Nummer>9</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.5</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_rapp_Okt">
                      <tubigcommon:Name>Oktober</tubigcommon:Name>
                      <tubigcommon:Nummer>10</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.5</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_rapp_Nov">
                      <tubigcommon:Name>November</tubigcommon:Name>
                      <tubigcommon:Nummer>11</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.5</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_rapp_Dez">
                      <tubigcommon:Name>Dezember</tubigcommon:Name>
                      <tubigcommon:Nummer>12</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.5</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                </tubigcommon:MonatCollection>
              </tubigspeicher:Mindestabgabe>
            </tubigspeicher:Mindestabgabe>
          </tubigspeicher:MindestabgabeParameter>
          <tubigspeicher:MaximalabgabeParameter>
            <tubigspeicher:Maximalabgabe fid="MaxAbgabe_rapp">
              <tubigspeicher:Maximalabgabe>
                <tubigcommon:MonatCollection fid="MaxAbgabe_rapp_Jahr">
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_rapp_Jan">
                      <tubigcommon:Name>Januar</tubigcommon:Name>
                      <tubigcommon:Nummer>1</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_rapp_Feb">
                      <tubigcommon:Name>Februar</tubigcommon:Name>
                      <tubigcommon:Nummer>2</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_rapp_Mrz">
                      <tubigcommon:Name>März</tubigcommon:Name>
                      <tubigcommon:Nummer>3</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_rapp_Apr">
                      <tubigcommon:Name>April</tubigcommon:Name>
                      <tubigcommon:Nummer>4</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_rapp_Mai">
                      <tubigcommon:Name>Mai</tubigcommon:Name>
                      <tubigcommon:Nummer>5</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_rapp_Jun">
                      <tubigcommon:Name>Juni</tubigcommon:Name>
                      <tubigcommon:Nummer>6</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_rapp_Jul">
                      <tubigcommon:Name>Juli</tubigcommon:Name>
                      <tubigcommon:Nummer>7</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_rapp_Aug">
                      <tubigcommon:Name>August</tubigcommon:Name>
                      <tubigcommon:Nummer>8</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_rapp_Sep">
                      <tubigcommon:Name>September</tubigcommon:Name>
                      <tubigcommon:Nummer>9</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_rapp_Okt">
                      <tubigcommon:Name>Oktober</tubigcommon:Name>
                      <tubigcommon:Nummer>10</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_rapp_Nov">
                      <tubigcommon:Name>November</tubigcommon:Name>
                      <tubigcommon:Nummer>11</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_rapp_Dez">
                      <tubigcommon:Name>Dezember</tubigcommon:Name>
                      <tubigcommon:Nummer>12</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                </tubigcommon:MonatCollection>
              </tubigspeicher:Maximalabgabe>
            </tubigspeicher:Maximalabgabe>
          </tubigspeicher:MaximalabgabeParameter>
          <tubigspeicher:TrinkwasserParameter>
            <tubigspeicher:Trinkwasser fid="Trinkwasser_rapp">
              <tubigspeicher:Trinkwasser>
                <tubigcommon:MonatCollection fid="Trinkwasser_rapp_Jahr">
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_rapp_Jan">
                      <tubigcommon:Name>Januar</tubigcommon:Name>
                      <tubigcommon:Nummer>1</tubigcommon:Nummer>
                      <tubigcommon:Wert>1.2</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_rapp_Feb">
                      <tubigcommon:Name>Februar</tubigcommon:Name>
                      <tubigcommon:Nummer>2</tubigcommon:Nummer>
                      <tubigcommon:Wert>1.2</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_rapp_Mrz">
                      <tubigcommon:Name>März</tubigcommon:Name>
                      <tubigcommon:Nummer>3</tubigcommon:Nummer>
                      <tubigcommon:Wert>1.2</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_rapp_Apr">
                      <tubigcommon:Name>April</tubigcommon:Name>
                      <tubigcommon:Nummer>4</tubigcommon:Nummer>
                      <tubigcommon:Wert>1.2</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_rapp_Mai">
                      <tubigcommon:Name>Mai</tubigcommon:Name>
                      <tubigcommon:Nummer>5</tubigcommon:Nummer>
                      <tubigcommon:Wert>1.2</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_rapp_Jun">
                      <tubigcommon:Name>Juni</tubigcommon:Name>
                      <tubigcommon:Nummer>6</tubigcommon:Nummer>
                      <tubigcommon:Wert>1.2</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_rapp_Jul">
                      <tubigcommon:Name>Juli</tubigcommon:Name>
                      <tubigcommon:Nummer>7</tubigcommon:Nummer>
                      <tubigcommon:Wert>1.2</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_rapp_Aug">
                      <tubigcommon:Name>August</tubigcommon:Name>
                      <tubigcommon:Nummer>8</tubigcommon:Nummer>
                      <tubigcommon:Wert>1.2</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_rapp_Sep">
                      <tubigcommon:Name>September</tubigcommon:Name>
                      <tubigcommon:Nummer>9</tubigcommon:Nummer>
                      <tubigcommon:Wert>1.2</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_rapp_Okt">
                      <tubigcommon:Name>Oktober</tubigcommon:Name>
                      <tubigcommon:Nummer>10</tubigcommon:Nummer>
                      <tubigcommon:Wert>1.2</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_rapp_Nov">
                      <tubigcommon:Name>November</tubigcommon:Name>
                      <tubigcommon:Nummer>11</tubigcommon:Nummer>
                      <tubigcommon:Wert>1.2</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_rapp_Dez">
                      <tubigcommon:Name>Dezember</tubigcommon:Name>
                      <tubigcommon:Nummer>12</tubigcommon:Nummer>
                      <tubigcommon:Wert>1.2</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                </tubigcommon:MonatCollection>
              </tubigspeicher:Trinkwasser>
            </tubigspeicher:Trinkwasser>
          </tubigspeicher:TrinkwasserParameter>
          <tubigspeicher:EACollectionAssociation>
            <tubigspeicher:EACollection fid="EACollection_rapp">
              <gml:boundedBy>
                <gml:Box>
                  <gml:coord>
                    <gml:X>0.0</gml:X>
                    <gml:Y>0.0</gml:Y>
                  </gml:coord>
                  <gml:coord>
                    <gml:X>0.0</gml:X>
                    <gml:Y>0.0</gml:Y>
                  </gml:coord>
                </gml:Box>
              </gml:boundedBy>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_rapp_1">
                  <tubigspeicher:Höhe>332.8</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>0.0</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>0.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_rapp_2">
                  <tubigspeicher:Höhe>345.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>0.335</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>29.8</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung>ZT=tiefstes Absenkziel = ZS TS Wendefurth Sommer</tubigspeicher:Bemerkung>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_rapp_3">
                  <tubigspeicher:Höhe>360.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>2.67</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>44.5</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_rapp_4">
                  <tubigspeicher:Höhe>364.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>3.97</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>47.7</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_rapp_5">
                  <tubigspeicher:Höhe>384.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>18.41</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>61.1</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_rapp_6">
                  <tubigspeicher:Höhe>393.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>30.38</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>66.3</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_rapp_7">
                  <tubigspeicher:Höhe>408.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>60.34</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>74.1</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_rapp_8">
                  <tubigspeicher:Höhe>421.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>99.44</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>80.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_rapp_9">
                  <tubigspeicher:Höhe>421.5</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>101.19</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>80.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung>ZS = Stauziel Winter</tubigspeicher:Bemerkung>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_rapp_10">
                  <tubigspeicher:Höhe>423.6</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>109.08</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>80.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung>ZS = Stauziel Sommer = Vollstau</tubigspeicher:Bemerkung>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_rapp_11">
                  <tubigspeicher:Höhe>424.1</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>111.08</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>80.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>37.2</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_rapp_12">
                  <tubigspeicher:Höhe>424.68</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>113.4</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>80.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>118.1</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung>höchstes Stauziel</tubigspeicher:Bemerkung>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
            </tubigspeicher:EACollection>
          </tubigspeicher:EACollectionAssociation>
        </tubigspeicher:Speicher>
      </tubigspeicher:SpeicherMember>
      <tubigspeicher:SpeicherMember>
        <tubigspeicher:Speicher fid="Speicher_Wendefurth">
          <tubigspeicher:Name>TS Wendefurth</tubigspeicher:Name>
          <tubigspeicher:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5218272.876245457,5742160.857450546</gml:coordinates>
            </gml:Point>
          </tubigspeicher:Ort>
          <tubigspeicher:Messstellennummer>579005</tubigspeicher:Messstellennummer>
          <tubigspeicher:Kurz_Name>wend</tubigspeicher:Kurz_Name>
          <tubigspeicher:Abgabe>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicherabgabe/Wendefurth.zml" linktype="zml"/>
          </tubigspeicher:Abgabe>
          <tubigspeicher:Zufluss_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicherzufluss/Wendefurth.zml" linktype="zml"/>
          </tubigspeicher:Zufluss_gerechnet>
          <tubigspeicher:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicher/Messung/Wendefurth.zml" linktype="zml"/>
          </tubigspeicher:Ganglinie_gemessen>
          <tubigspeicher:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicher/Vorhersage/Wendefurth.zml" linktype="zml"/>
          </tubigspeicher:Ganglinie_gerechnet>
          <tubigspeicher:StauraumParameter>
            <tubigspeicher:Stauraum fid="Stauraum_wend">
              <tubigspeicher:Totraum>0.049</tubigspeicher:Totraum>
              <tubigspeicher:Reserveraum>1.021</tubigspeicher:Reserveraum>
              <tubigspeicher:Betriebsraum>
                <tubigcommon:MonatCollection fid="Stauraum_wend_Jahr">
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_wend_Jan">
                      <tubigcommon:Name>Januar</tubigcommon:Name>
                      <tubigcommon:Nummer>1</tubigcommon:Nummer>
                      <tubigcommon:Wert>3.83</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_wend_Feb">
                      <tubigcommon:Name>Februar</tubigcommon:Name>
                      <tubigcommon:Nummer>2</tubigcommon:Nummer>
                      <tubigcommon:Wert>3.83</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_wend_Mrz">
                      <tubigcommon:Name>März</tubigcommon:Name>
                      <tubigcommon:Nummer>3</tubigcommon:Nummer>
                      <tubigcommon:Wert>3.83</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_wend_Apr">
                      <tubigcommon:Name>April</tubigcommon:Name>
                      <tubigcommon:Nummer>4</tubigcommon:Nummer>
                      <tubigcommon:Wert>3.83</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_wend_Mai">
                      <tubigcommon:Name>Mai</tubigcommon:Name>
                      <tubigcommon:Nummer>5</tubigcommon:Nummer>
                      <tubigcommon:Wert>5.08</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_wend_Jun">
                      <tubigcommon:Name>Juni</tubigcommon:Name>
                      <tubigcommon:Nummer>6</tubigcommon:Nummer>
                      <tubigcommon:Wert>5.08</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_wend_Jul">
                      <tubigcommon:Name>Juli</tubigcommon:Name>
                      <tubigcommon:Nummer>7</tubigcommon:Nummer>
                      <tubigcommon:Wert>5.08</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_wend_Aug">
                      <tubigcommon:Name>August</tubigcommon:Name>
                      <tubigcommon:Nummer>8</tubigcommon:Nummer>
                      <tubigcommon:Wert>5.08</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_wend_Sep">
                      <tubigcommon:Name>September</tubigcommon:Name>
                      <tubigcommon:Nummer>9</tubigcommon:Nummer>
                      <tubigcommon:Wert>5.08</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_wend_Okt">
                      <tubigcommon:Name>Oktober</tubigcommon:Name>
                      <tubigcommon:Nummer>10</tubigcommon:Nummer>
                      <tubigcommon:Wert>5.08</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_wend_Nov">
                      <tubigcommon:Name>November</tubigcommon:Name>
                      <tubigcommon:Nummer>11</tubigcommon:Nummer>
                      <tubigcommon:Wert>3.83</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Stauraum_wend_Dez">
                      <tubigcommon:Name>Dezember</tubigcommon:Name>
                      <tubigcommon:Nummer>12</tubigcommon:Nummer>
                      <tubigcommon:Wert>3.83</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                </tubigcommon:MonatCollection>
              </tubigspeicher:Betriebsraum>
              <tubigspeicher:Stauraum>10.335</tubigspeicher:Stauraum>
            </tubigspeicher:Stauraum>
          </tubigspeicher:StauraumParameter>
          <tubigspeicher:MindestabgabeParameter>
            <tubigspeicher:Mindestabgabe fid="MinAbgabe_wend">
              <tubigspeicher:Mindestabgabe>
                <tubigcommon:MonatCollection fid="MinAbgabe_wend_Jahr">
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_wend_Jan">
                      <tubigcommon:Name>Januar</tubigcommon:Name>
                      <tubigcommon:Nummer>1</tubigcommon:Nummer>
                      <tubigcommon:Wert>2.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_wend_Feb">
                      <tubigcommon:Name>Februar</tubigcommon:Name>
                      <tubigcommon:Nummer>2</tubigcommon:Nummer>
                      <tubigcommon:Wert>2.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_wend_Mrz">
                      <tubigcommon:Name>März</tubigcommon:Name>
                      <tubigcommon:Nummer>3</tubigcommon:Nummer>
                      <tubigcommon:Wert>2.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_wend_Apr">
                      <tubigcommon:Name>April</tubigcommon:Name>
                      <tubigcommon:Nummer>4</tubigcommon:Nummer>
                      <tubigcommon:Wert>2.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_wend_Mai">
                      <tubigcommon:Name>Mai</tubigcommon:Name>
                      <tubigcommon:Nummer>5</tubigcommon:Nummer>
                      <tubigcommon:Wert>2.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_wend_Jun">
                      <tubigcommon:Name>Juni</tubigcommon:Name>
                      <tubigcommon:Nummer>6</tubigcommon:Nummer>
                      <tubigcommon:Wert>2.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_wend_Jul">
                      <tubigcommon:Name>Juli</tubigcommon:Name>
                      <tubigcommon:Nummer>7</tubigcommon:Nummer>
                      <tubigcommon:Wert>2.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_wend_Aug">
                      <tubigcommon:Name>August</tubigcommon:Name>
                      <tubigcommon:Nummer>8</tubigcommon:Nummer>
                      <tubigcommon:Wert>2.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_wend_Sep">
                      <tubigcommon:Name>September</tubigcommon:Name>
                      <tubigcommon:Nummer>9</tubigcommon:Nummer>
                      <tubigcommon:Wert>2.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_wend_Okt">
                      <tubigcommon:Name>Oktober</tubigcommon:Name>
                      <tubigcommon:Nummer>10</tubigcommon:Nummer>
                      <tubigcommon:Wert>2.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_wend_Nov">
                      <tubigcommon:Name>November</tubigcommon:Name>
                      <tubigcommon:Nummer>11</tubigcommon:Nummer>
                      <tubigcommon:Wert>2.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MinAbgabe_wend_Dez">
                      <tubigcommon:Name>Dezember</tubigcommon:Name>
                      <tubigcommon:Nummer>12</tubigcommon:Nummer>
                      <tubigcommon:Wert>2.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                </tubigcommon:MonatCollection>
              </tubigspeicher:Mindestabgabe>
            </tubigspeicher:Mindestabgabe>
          </tubigspeicher:MindestabgabeParameter>
          <tubigspeicher:MaximalabgabeParameter>
            <tubigspeicher:Maximalabgabe fid="MaxAbgabe_wend">
              <tubigspeicher:Maximalabgabe>
                <tubigcommon:MonatCollection fid="MaxAbgabe_wend_Jahr">
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_wend_Jan">
                      <tubigcommon:Name>Januar</tubigcommon:Name>
                      <tubigcommon:Nummer>1</tubigcommon:Nummer>
                      <tubigcommon:Wert>40.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_wend_Feb">
                      <tubigcommon:Name>Februar</tubigcommon:Name>
                      <tubigcommon:Nummer>2</tubigcommon:Nummer>
                      <tubigcommon:Wert>40.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_wend_Mrz">
                      <tubigcommon:Name>März</tubigcommon:Name>
                      <tubigcommon:Nummer>3</tubigcommon:Nummer>
                      <tubigcommon:Wert>40.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_wend_Apr">
                      <tubigcommon:Name>April</tubigcommon:Name>
                      <tubigcommon:Nummer>4</tubigcommon:Nummer>
                      <tubigcommon:Wert>40.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_wend_Mai">
                      <tubigcommon:Name>Mai</tubigcommon:Name>
                      <tubigcommon:Nummer>5</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_wend_Jun">
                      <tubigcommon:Name>Juni</tubigcommon:Name>
                      <tubigcommon:Nummer>6</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_wend_Jul">
                      <tubigcommon:Name>Juli</tubigcommon:Name>
                      <tubigcommon:Nummer>7</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_wend_Aug">
                      <tubigcommon:Name>August</tubigcommon:Name>
                      <tubigcommon:Nummer>8</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_wend_Sep">
                      <tubigcommon:Name>September</tubigcommon:Name>
                      <tubigcommon:Nummer>9</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_wend_Okt">
                      <tubigcommon:Name>Oktober</tubigcommon:Name>
                      <tubigcommon:Nummer>10</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_wend_Nov">
                      <tubigcommon:Name>November</tubigcommon:Name>
                      <tubigcommon:Nummer>11</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="MaxAbgabe_wend_Dez">
                      <tubigcommon:Name>Dezember</tubigcommon:Name>
                      <tubigcommon:Nummer>12</tubigcommon:Nummer>
                      <tubigcommon:Wert>25.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                </tubigcommon:MonatCollection>
              </tubigspeicher:Maximalabgabe>
            </tubigspeicher:Maximalabgabe>
          </tubigspeicher:MaximalabgabeParameter>
          <tubigspeicher:TrinkwasserParameter>
            <tubigspeicher:Trinkwasser fid="Trinkwasser_wend">
              <tubigspeicher:Trinkwasser>
                <tubigcommon:MonatCollection fid="Trinkwasser_wend_Jahr">
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_wend_Jan">
                      <tubigcommon:Name>Januar</tubigcommon:Name>
                      <tubigcommon:Nummer>1</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_wend_Feb">
                      <tubigcommon:Name>Februar</tubigcommon:Name>
                      <tubigcommon:Nummer>2</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_wend_Mrz">
                      <tubigcommon:Name>März</tubigcommon:Name>
                      <tubigcommon:Nummer>3</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_wend_Apr">
                      <tubigcommon:Name>April</tubigcommon:Name>
                      <tubigcommon:Nummer>4</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_wend_Mai">
                      <tubigcommon:Name>Mai</tubigcommon:Name>
                      <tubigcommon:Nummer>5</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_wend_Jun">
                      <tubigcommon:Name>Juni</tubigcommon:Name>
                      <tubigcommon:Nummer>6</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_wend_Jul">
                      <tubigcommon:Name>Juli</tubigcommon:Name>
                      <tubigcommon:Nummer>7</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_wend_Aug">
                      <tubigcommon:Name>August</tubigcommon:Name>
                      <tubigcommon:Nummer>8</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_wend_Sep">
                      <tubigcommon:Name>September</tubigcommon:Name>
                      <tubigcommon:Nummer>9</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_wend_Okt">
                      <tubigcommon:Name>Oktober</tubigcommon:Name>
                      <tubigcommon:Nummer>10</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_wend_Nov">
                      <tubigcommon:Name>November</tubigcommon:Name>
                      <tubigcommon:Nummer>11</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                  <tubigcommon:MonatMember>
                    <tubigcommon:Monat fid="Trinkwasser_wend_Dez">
                      <tubigcommon:Name>Dezember</tubigcommon:Name>
                      <tubigcommon:Nummer>12</tubigcommon:Nummer>
                      <tubigcommon:Wert>0.0</tubigcommon:Wert>
                    </tubigcommon:Monat>
                  </tubigcommon:MonatMember>
                </tubigcommon:MonatCollection>
              </tubigspeicher:Trinkwasser>
            </tubigspeicher:Trinkwasser>
          </tubigspeicher:TrinkwasserParameter>
          <tubigspeicher:EACollectionAssociation>
            <tubigspeicher:EACollection fid="EACollection_wend">
              <gml:boundedBy>
                <gml:Box>
                  <gml:coord>
                    <gml:X>0.0</gml:X>
                    <gml:Y>0.0</gml:Y>
                  </gml:coord>
                  <gml:coord>
                    <gml:X>0.0</gml:X>
                    <gml:Y>0.0</gml:Y>
                  </gml:coord>
                </gml:Box>
              </gml:boundedBy>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_1">
                  <tubigspeicher:Höhe>322.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>0.0</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>0.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_2">
                  <tubigspeicher:Höhe>328.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>0.049</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>28.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung>ZT=tiefstes Absenkziel</tubigspeicher:Bemerkung>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_3">
                  <tubigspeicher:Höhe>330.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>0.155</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>34.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_4">
                  <tubigspeicher:Höhe>332.8</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>0.453</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>40.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung>Schieberachse Umlaufstollen TS Rappbode</tubigspeicher:Bemerkung>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_5">
                  <tubigspeicher:Höhe>336.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>1.025</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>46.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_6">
                  <tubigspeicher:Höhe>338.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>1.554</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>50.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_7">
                  <tubigspeicher:Höhe>340.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>2.202</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>53.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_8">
                  <tubigspeicher:Höhe>342.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>2.956</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>56.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_9">
                  <tubigspeicher:Höhe>344.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>3.848</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>58.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_10">
                  <tubigspeicher:Höhe>346.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>4.884</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>60.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung>GA begrenzt auf 30 m3/s</tubigspeicher:Bemerkung>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_11">
                  <tubigspeicher:Höhe>348.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>6.038</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>60.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_12">
                  <tubigspeicher:Höhe>350.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>7.288</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>60.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_13">
                  <tubigspeicher:Höhe>351.9</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>8.535</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>60.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>0.0</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung>Vollstau</tubigspeicher:Bemerkung>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_14">
                  <tubigspeicher:Höhe>352.0</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>8.602</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>60.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>3.7</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_15">
                  <tubigspeicher:Höhe>352.2</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>8.737</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>60.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>18.7</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_16">
                  <tubigspeicher:Höhe>352.4</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>8.873</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>60.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>40.4</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung/>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
              <tubigspeicher:EAMember>
                <tubigspeicher:EA fid="EA_wend_17">
                  <tubigspeicher:Höhe>352.9</tubigspeicher:Höhe>
                  <tubigspeicher:Inhalt>9.215</tubigspeicher:Inhalt>
                  <tubigspeicher:Grundablass>60.0</tubigspeicher:Grundablass>
                  <tubigspeicher:Überlauf>114.1</tubigspeicher:Überlauf>
                  <tubigspeicher:Bemerkung>ZH=höchstes Stauziel</tubigspeicher:Bemerkung>
                </tubigspeicher:EA>
              </tubigspeicher:EAMember>
            </tubigspeicher:EACollection>
          </tubigspeicher:EACollectionAssociation>
        </tubigspeicher:Speicher>
      </tubigspeicher:SpeicherMember>
      <tubigspeicher:SpeicherMember>
        <tubigspeicher:Überleitung fid="Überleitung_Rappbode">
          <tubigspeicher:Name>TS Königshütte</tubigspeicher:Name>
          <tubigspeicher:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5210386.315687956,5741854.463837401</gml:coordinates>
            </gml:Point>
          </tubigspeicher:Ort>
          <tubigspeicher:Messstellennummer>579000</tubigspeicher:Messstellennummer>
          <tubigspeicher:Kurz_Name>urapp</tubigspeicher:Kurz_Name>
          <tubigspeicher:Abgabe>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicherabgabe/Königshütte.zml" linktype="zml"/>
          </tubigspeicher:Abgabe>
          <tubigspeicher:Zufluss_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Speicherzufluss/Königshütte.zml" linktype="zml"/>
          </tubigspeicher:Zufluss_gerechnet>
        </tubigspeicher:Überleitung>
      </tubigspeicher:SpeicherMember>
    </tubigspeicher:SpeicherCollection>
  </tubigspeicher:SpeicherCollectionAssociation>
</TubigModell>
