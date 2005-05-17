<?xml version="1.0" encoding="UTF-8"?>
<SaaleModell fid="root" xmlns="org.kalypso.saale.modell" xmlns:common="org.kalypso.saale.common" xmlns:pegel="org.kalypso.saale.pegel" xmlns:steuerung="org.kalypso.saale.steuerung" xmlns:wlm="org.kalypso.saale.wlm" xmlns:temp="org.kalypso.saale.temp" xmlns:speicher="org.kalypso.saale.speicher" xmlns:gml="http://www.opengis.net/gml" xmlns:obslink="obslink.zml.kalypso.org" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="org.kalypso.saale.modell file:///D:/Kalypso/KalypsoSaalePlugin/src/org/kalypso/lhwsachsenanhalt/saale/schemata/saalemodell.xsd">
  <steuerung:SteuerungCollectionAssociation>
    <steuerung:SteuerungCollection fid="Steuerung"/>
  </steuerung:SteuerungCollectionAssociation>
  <pegel:PegelCollectionAssociation>
    <pegel:PegelCollection fid="Pegel"/>
  </pegel:PegelCollectionAssociation>
  <wlm:WLMCollectionAssociation>
    <wlm:WLMCollection fid="Wasserlaufmodelle"/>
  </wlm:WLMCollectionAssociation>
  <temp:TempCollectionAssociation>
    <temp:TempCollection fid="Temperatur"/>
  </temp:TempCollectionAssociation>
  <speicher:SpeicherCollectionAssociation>
    <speicher:SpeicherCollection fid="Speicher"/>
  </speicher:SpeicherCollectionAssociation>
</SaaleModell>
