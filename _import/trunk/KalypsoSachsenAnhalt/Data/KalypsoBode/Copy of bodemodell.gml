<?xml version="1.0" encoding="UTF-8"?>
<BodeModell fid="root" xmlns="org.kalypso.bode.modell" xmlns:bodecommon="org.kalypso.bode.common" xmlns:bodeombrometer="org.kalypso.bode.ombrometer" xmlns:bodepegel="org.kalypso.bode.pegel" xmlns:bodespeicher="org.kalypso.bode.speicher" xmlns:gml="http://www.opengis.net/gml" xmlns:obslink="obslink.zml.kalypso.org" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation=".model/schema/bodemodell.xsd">
  <bodepegel:PegelCollectionAssociation>
    <bodepegel:PegelCollection fid="PegelCollection">
      <bodepegel:PegelMember>
        <bodepegel:Pegel fid="Pegel0">
          <bodepegel:Name>uhu</bodepegel:Name>
          <bodepegel:Ort/>
          <bodepegel:istEingabePegel/>
          <bodepegel:istErgebnisPegel/>
        </bodepegel:Pegel>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Pegel fid="pegel1">
          <bodepegel:Name>Monika</bodepegel:Name>
          <bodepegel:Ort/>
          <bodepegel:WISKI_Name>fährt bald in Urlaub</bodepegel:WISKI_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>false</bodepegel:istErgebnisPegel>
        </bodepegel:Pegel>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Pegel fid="pegel2">
          <bodepegel:Name>Hallo</bodepegel:Name>
          <bodepegel:Ort/>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>false</bodepegel:istErgebnisPegel>
        </bodepegel:Pegel>
      </bodepegel:PegelMember>
    </bodepegel:PegelCollection>
  </bodepegel:PegelCollectionAssociation>
  <bodeombrometer:OmbrometerCollectionAssociation>
    <bodeombrometer:OmbrometerCollection fid="ombroColl"/>
  </bodeombrometer:OmbrometerCollectionAssociation>
  <bodespeicher:SpeicherCollectionAssociation>
    <bodespeicher:SpeicherCollection fid="speicherColl"/>
  </bodespeicher:SpeicherCollectionAssociation>
</BodeModell>
