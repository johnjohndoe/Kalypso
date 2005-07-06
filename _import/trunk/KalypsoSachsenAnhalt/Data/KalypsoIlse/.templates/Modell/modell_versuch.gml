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
        <tubigpegel:Niederschlagsgebiet fid="NSGebiet_Ilsenburg">
          <tubigpegel:Name>Ilsenburg</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5200769.515575188,5754512.981025437</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>444205</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>ilse</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Ilsenburg.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Ilsenburg.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Gebiet/>
          <tubigpegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Ilsenburg.zml" linktype="zml"/>
          </tubigpegel:Niederschlag>
          <tubigpegel:gewichtung>
            <tubigpegel:GewichtungsElement fid="GewIlsenburgElend">
              <tubigpegel:faktor>0.9</tubigpegel:faktor>
              <tubigpegel:ombrometerMember xlink:href="#Ombro_Elend"/>
            </tubigpegel:GewichtungsElement>
          </tubigpegel:gewichtung>
        </tubigpegel:Niederschlagsgebiet>
      </tubigpegel:PegelMember>
      <tubigpegel:PegelMember>
        <tubigpegel:Niederschlagsgebiet fid="NSGebiet_Bühne_Hoppenstedt">
          <tubigpegel:Name>Bühne-Hoppenstedt</tubigpegel:Name>
          <tubigpegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5201591.510535771,5771269.946257283</gml:coordinates>
            </gml:Point>
          </tubigpegel:Ort>
          <tubigpegel:Messstellennummer>444210</tubigpegel:Messstellennummer>
          <tubigpegel:Kurz_Name>bueh</tubigpegel:Kurz_Name>
          <tubigpegel:istEingabePegel>true</tubigpegel:istEingabePegel>
          <tubigpegel:istErgebnisPegel>true</tubigpegel:istErgebnisPegel>
          <tubigpegel:Ganglinie_gemessen>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Messung/Bühne_Hoppenstedt.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gemessen>
          <tubigpegel:Ganglinie_gerechnet>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Pegel/Vorhersage/Bühne_Hoppenstedt.zml" linktype="zml"/>
          </tubigpegel:Ganglinie_gerechnet>
          <tubigpegel:Gebiet/>
          <tubigpegel:Niederschlag>
            <TimeseriesLink xmlns:ns1="http://www.w3.org/1999/xlink" xmlns="obslink.zml.kalypso.org" ns1:href="./Zeitreihen/Gebietsniederschlag/Bühne_Hoppenstedt.zml" linktype="zml"/>
          </tubigpegel:Niederschlag>
        </tubigpegel:Niederschlagsgebiet>
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
    </tubigspeicher:SpeicherCollection>
  </tubigspeicher:SpeicherCollectionAssociation>
</TubigModell>
