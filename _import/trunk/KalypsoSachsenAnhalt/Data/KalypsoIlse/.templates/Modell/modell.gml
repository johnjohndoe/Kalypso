<?xml version="1.0" encoding="UTF-8"?>
<BodeModell fid="root" xmlns:bodecommon="org.kalypso.bode.common" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:bodeombrometer="org.kalypso.bode.ombrometer" xmlns="org.kalypso.bode.modell" xmlns:obslink="obslink.zml.kalypso.org" xmlns:bodespeicher="org.kalypso.bode.speicher" xmlns:gml="http://www.opengis.net/gml" xmlns:bodepegel="org.kalypso.bode.pegel" xsi:schemaLocation="org.kalypso.bode.modell project:/.model/schema/modell.xsd">
  <bodepegel:PegelCollectionAssociation>
    <bodepegel:PegelCollection fid="PegelCollection0">
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
      <bodepegel:PegelMember>
        <bodepegel:Niederschlagsgebiet fid="NSGebiet_Ilsenburg">
          <bodepegel:Name>Ilsenburg</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">4407420,5746440</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:Messstellennummer>444205</bodepegel:Messstellennummer>
          <bodepegel:Kurz_Name>ilse</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Ilsenburg.zml" linktype="zml"/>
          </bodepegel:Ganglinie_gemessen>
          <bodepegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Ilsenburg.zml" linktype="zml"/>
          </bodepegel:Ganglinie_gerechnet>
          <bodepegel:Gebiet/>
          <bodepegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Ilsenburg.zml" linktype="zml"/>
          </bodepegel:Niederschlag>
        </bodepegel:Niederschlagsgebiet>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Niederschlagsgebiet fid="NSGebiet_Bühne_Hoppenstedt">
          <bodepegel:Name>Bühne-Hoppenstedt</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">4407550,5763200</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:Messstellennummer>444210</bodepegel:Messstellennummer>
          <bodepegel:Kurz_Name>beuh</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Bühne_Hoppenstedt.zml" linktype="zml"/>
          </bodepegel:Ganglinie_gemessen>
          <bodepegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Bühne_Hoppenstedt" linktype="zml"/>
          </bodepegel:Ganglinie_gerechnet>
          <bodepegel:Gebiet/>
          <bodepegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Bühne_Hoppenstedt.zml" linktype="zml"/>
          </bodepegel:Niederschlag>
        </bodepegel:Niederschlagsgebiet>
      </bodepegel:PegelMember>
    </bodepegel:PegelCollection>
  </bodepegel:PegelCollectionAssociation>
  <bodeombrometer:OmbrometerCollectionAssociation>
    <bodeombrometer:OmbrometerCollection fid="OmbrometerCollection0">
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
      <bodeombrometer:OmbrometerMember>
        <bodeombrometer:Ombrometer fid="Ombro_Weferlingen">
          <bodeombrometer:Name>Weferlingen</bodeombrometer:Name>
          <bodeombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5231071.410767844,5805231.416728718</gml:coordinates>
            </gml:Point>
          </bodeombrometer:Ort>
          <bodeombrometer:Messstellennummer>440010</bodeombrometer:Messstellennummer>
          <bodeombrometer:Kurz_Name>wefe</bodeombrometer:Kurz_Name>
          <bodeombrometer:Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Weferlingen.zml" linktype="zml"/>
          </bodeombrometer:Niederschlag_gemessen>
        </bodeombrometer:Ombrometer>
      </bodeombrometer:OmbrometerMember>
      <bodeombrometer:OmbrometerMember>
        <bodeombrometer:Ombrometer fid="Ombro_Ditfurt">
          <bodeombrometer:Name>Ditfurt</bodeombrometer:Name>
          <bodeombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5238766.71591927,5751133.2589417035</gml:coordinates>
            </gml:Point>
          </bodeombrometer:Ort>
          <bodeombrometer:Messstellennummer>579040</bodeombrometer:Messstellennummer>
          <bodeombrometer:Kurz_Name>ditf</bodeombrometer:Kurz_Name>
          <bodeombrometer:Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Ditfurt.zml" linktype="zml"/>
          </bodeombrometer:Niederschlag_gemessen>
        </bodeombrometer:Ombrometer>
      </bodeombrometer:OmbrometerMember>
      <bodeombrometer:OmbrometerMember>
        <bodeombrometer:Ombrometer fid="Ombro_Elend">
          <bodeombrometer:Name>Elend</bodeombrometer:Name>
          <bodeombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5202064.849923498,5743158.864709639</gml:coordinates>
            </gml:Point>
          </bodeombrometer:Ort>
          <bodeombrometer:Messstellennummer>579305</bodeombrometer:Messstellennummer>
          <bodeombrometer:Kurz_Name>elen</bodeombrometer:Kurz_Name>
          <bodeombrometer:Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Elend.zml" linktype="zml"/>
          </bodeombrometer:Niederschlag_gemessen>
        </bodeombrometer:Ombrometer>
      </bodeombrometer:OmbrometerMember>
      <bodeombrometer:OmbrometerMember>
        <bodeombrometer:Ombrometer fid="Ombro_Mahndorf">
          <bodeombrometer:Name>Mahndorf</bodeombrometer:Name>
          <bodeombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5222223.011338177,5757795.57484032</gml:coordinates>
            </gml:Point>
          </bodeombrometer:Ort>
          <bodeombrometer:Messstellennummer>579712</bodeombrometer:Messstellennummer>
          <bodeombrometer:Kurz_Name>mahn</bodeombrometer:Kurz_Name>
          <bodeombrometer:Niederschlag_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Mahndorf.zml" linktype="zml"/>
          </bodeombrometer:Niederschlag_gemessen>
        </bodeombrometer:Ombrometer>
      </bodeombrometer:OmbrometerMember>
    </bodeombrometer:OmbrometerCollection>
  </bodeombrometer:OmbrometerCollectionAssociation>
  <bodespeicher:SpeicherCollectionAssociation>
    <bodespeicher:SpeicherCollection fid="SpeicherCollection0">
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
    </bodespeicher:SpeicherCollection>
  </bodespeicher:SpeicherCollectionAssociation>
</BodeModell>
