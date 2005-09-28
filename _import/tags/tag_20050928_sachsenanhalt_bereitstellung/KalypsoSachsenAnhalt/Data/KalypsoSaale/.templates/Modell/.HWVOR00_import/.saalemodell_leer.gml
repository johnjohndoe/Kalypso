<?xml version="1.0" encoding="UTF-8"?>
<SaaleModell fid="root" xmlns="org.kalypso.saale.modell" xmlns:common="org.kalypso.saale.common" xmlns:pegel="org.kalypso.saale.pegel" xmlns:steuerung="org.kalypso.saale.steuerung" xmlns:wlm="org.kalypso.saale.wlm" xmlns:ombrometer="org.kalypso.saale.ombrometer" xmlns:speicher="org.kalypso.saale.speicher" xmlns:gml="http://www.opengis.net/gml" xmlns:obslink="obslink.zml.kalypso.org" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="org.kalypso.saale.modell file:///D:/Kalypso/KalypsoSaalePlugin/src/org/kalypso/lhwsachsenanhalt/saale/schemata/saalemodell.xsd">
  <pegel:PegelCollectionAssociation>
    <pegel:PegelCollection fid="Pegel"/>
  </pegel:PegelCollectionAssociation>
  <wlm:WLMCollectionAssociation>
    <wlm:WLMCollection fid="Wasserlaufmodelle"/>
  </wlm:WLMCollectionAssociation>
  <speicher:SpeicherCollectionAssociation>
    <speicher:SpeicherCollection fid="Speicher"/>
  </speicher:SpeicherCollectionAssociation>
  <ombrometer:OmbrometerCollectionAssociation>
    <ombrometer:OmbrometerCollection fid="Ombrometer"/>
  </ombrometer:OmbrometerCollectionAssociation>
</SaaleModell>
