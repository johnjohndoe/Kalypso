<?xml version="1.0" encoding="UTF-8"?>
<BodeModell fid="root" xmlns="org.kalypso.bode.modell" xmlns:bodecommon="org.kalypso.bode.common" xmlns:bodeombrometer="org.kalypso.bode.ombrometer" xmlns:bodepegel="org.kalypso.bode.pegel" xmlns:bodespeicher="org.kalypso.bode.speicher" xmlns:gml="http://www.opengis.net/gml" xmlns:obslink="obslink.zml.kalypso.org" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="org.kalypso.bode.modell .model/schema/bodemodell.xsd">
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
        <bodepegel:WasserlaufModell fid="WLM_Staßfurt">
          <bodepegel:Name>Staßfurt</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5265120.665000831,5752160.234277211</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579085</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>stas</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Hadmersleben -> Stassfurt]]></bodepegel:Kommentar>
          <bodepegel:XAW>1.9599</bodepegel:XAW>
          <bodepegel:XEW>36.4273</bodepegel:XEW>
          <bodepegel:FK1>0.5199</bodepegel:FK1>
          <bodepegel:FK2>0.6138</bodepegel:FK2>
          <bodepegel:NN1>9.0</bodepegel:NN1>
          <bodepegel:NN2>11.0</bodepegel:NN2>
          <bodepegel:Laufzeit>2</bodepegel:Laufzeit>
          <bodepegel:LaufzeitDefault>2</bodepegel:LaufzeitDefault>
          <bodepegel:DM1>1.8548</bodepegel:DM1>
          <bodepegel:DM2>8.869</bodepegel:DM2>
        </bodepegel:WasserlaufModell>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:WasserlaufModell fid="WLM_Wegeleben">
          <bodepegel:Name>Wegeleben</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5237817.465321006,5757262.428122178</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579049</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>wege</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Ditfurt+Hausneindorf -> Wegeleben]]></bodepegel:Kommentar>
          <bodepegel:XAW>0.7505</bodepegel:XAW>
          <bodepegel:XEW>-0.3906</bodepegel:XEW>
          <bodepegel:FK1>0.0916</bodepegel:FK1>
          <bodepegel:FK2>0.8595</bodepegel:FK2>
          <bodepegel:NN1>4.0</bodepegel:NN1>
          <bodepegel:NN2>4.0</bodepegel:NN2>
          <bodepegel:Laufzeit>0</bodepegel:Laufzeit>
          <bodepegel:LaufzeitDefault>0</bodepegel:LaufzeitDefault>
          <bodepegel:DM1>0.8713</bodepegel:DM1>
          <bodepegel:DM2>0.8818</bodepegel:DM2>
        </bodepegel:WasserlaufModell>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:WasserlaufModell fid="WLM_Ditfurt">
          <bodepegel:Name>Ditfurt</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5238766.714411308,5751133.230100412</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579040</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>ditf</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Wendefurt->Ditfurt]]></bodepegel:Kommentar>
          <bodepegel:XAW>0.5024</bodepegel:XAW>
          <bodepegel:XEW>0.1037</bodepegel:XEW>
          <bodepegel:FK1>0.6356</bodepegel:FK1>
          <bodepegel:FK2>0.8137</bodepegel:FK2>
          <bodepegel:NN1>3.0</bodepegel:NN1>
          <bodepegel:NN2>3.0</bodepegel:NN2>
          <bodepegel:Laufzeit>3</bodepegel:Laufzeit>
          <bodepegel:LaufzeitDefault>3</bodepegel:LaufzeitDefault>
          <bodepegel:DM1>0.6561</bodepegel:DM1>
          <bodepegel:DM2>0.703</bodepegel:DM2>
        </bodepegel:WasserlaufModell>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:WasserlaufModell fid="WLM_Niendorf">
          <bodepegel:Name>Niendorf</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5236078.021126529,5763414.294482665</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579745</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>nien</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Mahndorf -> Nienhagen]]></bodepegel:Kommentar>
          <bodepegel:XAW>0.794</bodepegel:XAW>
          <bodepegel:XEW>-2.2129</bodepegel:XEW>
          <bodepegel:FK1>0.0015</bodepegel:FK1>
          <bodepegel:FK2>0.7874</bodepegel:FK2>
          <bodepegel:NN1>3.0</bodepegel:NN1>
          <bodepegel:NN2>3.0</bodepegel:NN2>
          <bodepegel:Laufzeit>0</bodepegel:Laufzeit>
          <bodepegel:LaufzeitDefault>0</bodepegel:LaufzeitDefault>
          <bodepegel:DM1>0.9809</bodepegel:DM1>
          <bodepegel:DM2>0.8134</bodepegel:DM2>
        </bodepegel:WasserlaufModell>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:WasserlaufModell fid="WLM_Neugattersleben">
          <bodepegel:Name>Neugattersleben</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5273131.125155065,5750908.666864051</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579090</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>neug</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>false</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Stassfurt -> Neugattersleben]]></bodepegel:Kommentar>
          <bodepegel:XAW>2.3493</bodepegel:XAW>
          <bodepegel:XEW>2.4818</bodepegel:XEW>
          <bodepegel:FK1>0.0027</bodepegel:FK1>
          <bodepegel:FK2>0.8972</bodepegel:FK2>
          <bodepegel:NN1>3.0</bodepegel:NN1>
          <bodepegel:NN2>3.0</bodepegel:NN2>
          <bodepegel:Laufzeit>2</bodepegel:Laufzeit>
          <bodepegel:LaufzeitDefault>2</bodepegel:LaufzeitDefault>
          <bodepegel:DM1>0.2038</bodepegel:DM1>
          <bodepegel:DM2>0.1701</bodepegel:DM2>
        </bodepegel:WasserlaufModell>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:WasserlaufModell fid="WLM_Hadmersleben">
          <bodepegel:Name>Hadmersleben</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5247443.864253307,5769896.280832737</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579070</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>hadm</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Wegeleben+Nienhagen+Oschersleben -> Hadmersleben]]></bodepegel:Kommentar>
          <bodepegel:XAW>0.8004</bodepegel:XAW>
          <bodepegel:XEW>-0.1929</bodepegel:XEW>
          <bodepegel:FK1>0.5975</bodepegel:FK1>
          <bodepegel:FK2>0.8898</bodepegel:FK2>
          <bodepegel:NN1>2.0</bodepegel:NN1>
          <bodepegel:NN2>2.0</bodepegel:NN2>
          <bodepegel:Laufzeit>2</bodepegel:Laufzeit>
          <bodepegel:LaufzeitDefault>2</bodepegel:LaufzeitDefault>
          <bodepegel:DM1>0.6524</bodepegel:DM1>
          <bodepegel:DM2>0.6507</bodepegel:DM2>
        </bodepegel:WasserlaufModell>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:WasserlaufModell fid="WLM_Hausneindorf">
          <bodepegel:Name>Hausneindorf</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5242962.025211956,5751581.315923652</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579620</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>haus</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Kommentar><![CDATA[M_WLM-Parameter fuer Abschnitt Meisdorf->Hausneindorf]]></bodepegel:Kommentar>
          <bodepegel:XAW>0.5884</bodepegel:XAW>
          <bodepegel:XEW>-1.1278</bodepegel:XEW>
          <bodepegel:FK1>0.1325</bodepegel:FK1>
          <bodepegel:FK2>0.8517</bodepegel:FK2>
          <bodepegel:NN1>2.0</bodepegel:NN1>
          <bodepegel:NN2>3.0</bodepegel:NN2>
          <bodepegel:Laufzeit>5</bodepegel:Laufzeit>
          <bodepegel:LaufzeitDefault>5</bodepegel:LaufzeitDefault>
          <bodepegel:DM1>0.459</bodepegel:DM1>
          <bodepegel:DM2>0.1653</bodepegel:DM2>
        </bodepegel:WasserlaufModell>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Niederschlagsgebiet fid="NSGebiet_Meisdorf">
          <bodepegel:Name>Meisdorf</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5243204.610715386,5735094.547495468</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579610</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>meis</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Gebiet/>
          <bodepegel:Niederschlag_gemessenEingang/>
          <bodepegel:Niederschlag_prognoseEingang/>
          <bodepegel:Niederschlag_gesamt/>
        </bodepegel:Niederschlagsgebiet>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Niederschlagsgebiet fid="NSGebiet_Elend">
          <bodepegel:Name>Elend</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5202064.848171497,5743158.836068862</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579305</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>elen</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Gebiet/>
          <bodepegel:Niederschlag_gemessenEingang/>
          <bodepegel:Niederschlag_prognoseEingang/>
          <bodepegel:Niederschlag_gesamt/>
        </bodepegel:Niederschlagsgebiet>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Niederschlagsgebiet fid="NSGebiet_Hasselfelde">
          <bodepegel:Name>Hasselfelde</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5213844.939397652,5736803.327862106</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>4421200</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>hass</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Gebiet/>
          <bodepegel:Niederschlag_gemessenEingang/>
          <bodepegel:Niederschlag_prognoseEingang/>
          <bodepegel:Niederschlag_gesamt/>
        </bodepegel:Niederschlagsgebiet>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Niederschlagsgebiet fid="NSGebiet_Mahndorf">
          <bodepegel:Name>Mahndorf</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5222223.009723585,5757795.546140475</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579712</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>mahn</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Gebiet/>
          <bodepegel:Niederschlag_gemessenEingang/>
          <bodepegel:Niederschlag_prognoseEingang/>
          <bodepegel:Niederschlag_gesamt/>
        </bodepegel:Niederschlagsgebiet>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Niederschlagsgebiet fid="NSGebiet_Silberhütte">
          <bodepegel:Name>Silberhütte</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5229948.557010742,5729148.916181304</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579605</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>silb</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Gebiet/>
          <bodepegel:Niederschlag_gemessenEingang/>
          <bodepegel:Niederschlag_prognoseEingang/>
          <bodepegel:Niederschlag_gesamt/>
        </bodepegel:Niederschlagsgebiet>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Niederschlagsgebiet fid="NSGebiet_Königshütte">
          <bodepegel:Name>Königshütte</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5207708.383920273,5742265.239427504</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579209</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>koen</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Gebiet/>
          <bodepegel:Niederschlag_gemessenEingang/>
          <bodepegel:Niederschlag_prognoseEingang/>
          <bodepegel:Niederschlag_gesamt/>
        </bodepegel:Niederschlagsgebiet>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Niederschlagsgebiet fid="NSGebiet_Steinerne_Renne">
          <bodepegel:Name>Steinerne Renne</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5205705.649167206,5751283.927985462</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579705</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>stei</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Gebiet/>
          <bodepegel:Niederschlag_gemessenEingang/>
          <bodepegel:Niederschlag_prognoseEingang/>
          <bodepegel:Niederschlag_gesamt/>
        </bodepegel:Niederschlagsgebiet>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Niederschlagsgebiet fid="NSGebiet_Oschersleben">
          <bodepegel:Name>Oschersleben</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5241071.151794376,5772263.016360395</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579810</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>osch</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Gebiet/>
          <bodepegel:Niederschlag_gemessenEingang/>
          <bodepegel:Niederschlag_prognoseEingang/>
          <bodepegel:Niederschlag_gesamt/>
        </bodepegel:Niederschlagsgebiet>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Niederschlagsgebiet fid="NSGebiet_Trautenstein">
          <bodepegel:Name>Trautenstein</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5209130.6656647865,5737147.58355137</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579405</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>trau</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
          <bodepegel:Gebiet/>
          <bodepegel:Niederschlag_gemessenEingang/>
          <bodepegel:Niederschlag_prognoseEingang/>
          <bodepegel:Niederschlag_gesamt/>
        </bodepegel:Niederschlagsgebiet>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Pegel fid="Pegel_Thale">
          <bodepegel:Name>Thale</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5225486.404459895,5741212.703936191</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579020</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>thal</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
        </bodepegel:Pegel>
      </bodepegel:PegelMember>
      <bodepegel:PegelMember>
        <bodepegel:Pegel fid="Pegel_Wendefurth">
          <bodepegel:Name>Wendefurth</bodepegel:Name>
          <bodepegel:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5218308.347485623,5742049.175148535</gml:coordinates>
            </gml:Point>
          </bodepegel:Ort>
          <bodepegel:WISKI_Name>579006</bodepegel:WISKI_Name>
          <bodepegel:Kurz_Name>wend</bodepegel:Kurz_Name>
          <bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
          <bodepegel:istErgebnisPegel>false</bodepegel:istErgebnisPegel>
        </bodepegel:Pegel>
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
              <gml:coordinates cs="," decimal="." ts=" ">5231071.409199598,5805231.388312708</gml:coordinates>
            </gml:Point>
          </bodeombrometer:Ort>
          <bodeombrometer:WISKI_Name>440010</bodeombrometer:WISKI_Name>
          <bodeombrometer:Kurz_Name>wefe</bodeombrometer:Kurz_Name>
        </bodeombrometer:Ombrometer>
      </bodeombrometer:OmbrometerMember>
      <bodeombrometer:OmbrometerMember>
        <bodeombrometer:Ombrometer fid="Ombro_Ditfurt">
          <bodeombrometer:Name>Ditfurt</bodeombrometer:Name>
          <bodeombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5238766.714411308,5751133.230100412</gml:coordinates>
            </gml:Point>
          </bodeombrometer:Ort>
          <bodeombrometer:WISKI_Name>579040</bodeombrometer:WISKI_Name>
          <bodeombrometer:Kurz_Name>ditf</bodeombrometer:Kurz_Name>
        </bodeombrometer:Ombrometer>
      </bodeombrometer:OmbrometerMember>
      <bodeombrometer:OmbrometerMember>
        <bodeombrometer:Ombrometer fid="Ombro_Elend">
          <bodeombrometer:Name>Elend</bodeombrometer:Name>
          <bodeombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5202064.848171497,5743158.836068862</gml:coordinates>
            </gml:Point>
          </bodeombrometer:Ort>
          <bodeombrometer:WISKI_Name>579305</bodeombrometer:WISKI_Name>
          <bodeombrometer:Kurz_Name>elen</bodeombrometer:Kurz_Name>
        </bodeombrometer:Ombrometer>
      </bodeombrometer:OmbrometerMember>
      <bodeombrometer:OmbrometerMember>
        <bodeombrometer:Ombrometer fid="Ombro_Hasselfelde">
          <bodeombrometer:Name>Hasselfelde</bodeombrometer:Name>
          <bodeombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5213844.939397652,5736803.327862106</gml:coordinates>
            </gml:Point>
          </bodeombrometer:Ort>
          <bodeombrometer:WISKI_Name>579504</bodeombrometer:WISKI_Name>
          <bodeombrometer:Kurz_Name>hass</bodeombrometer:Kurz_Name>
        </bodeombrometer:Ombrometer>
      </bodeombrometer:OmbrometerMember>
      <bodeombrometer:OmbrometerMember>
        <bodeombrometer:Ombrometer fid="Ombro_Silberhütte">
          <bodeombrometer:Name>Silberhütte</bodeombrometer:Name>
          <bodeombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5229948.557010742,5729148.916181304</gml:coordinates>
            </gml:Point>
          </bodeombrometer:Ort>
          <bodeombrometer:WISKI_Name>579605</bodeombrometer:WISKI_Name>
          <bodeombrometer:Kurz_Name>silb</bodeombrometer:Kurz_Name>
        </bodeombrometer:Ombrometer>
      </bodeombrometer:OmbrometerMember>
      <bodeombrometer:OmbrometerMember>
        <bodeombrometer:Ombrometer fid="Ombro_Mahndorf">
          <bodeombrometer:Name>Mahndorf</bodeombrometer:Name>
          <bodeombrometer:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5222223.009723585,5757795.546140475</gml:coordinates>
            </gml:Point>
          </bodeombrometer:Ort>
          <bodeombrometer:WISKI_Name>579712</bodeombrometer:WISKI_Name>
          <bodeombrometer:Kurz_Name>mahn</bodeombrometer:Kurz_Name>
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
      <bodespeicher:SpeicherMember>
        <bodespeicher:Speicher fid="Speicher_KalteBode">
          <bodespeicher:Name>HWR Kalte Bode</bodespeicher:Name>
          <bodespeicher:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5206105.142186547,5743473.325752091</gml:coordinates>
            </gml:Point>
          </bodespeicher:Ort>
          <bodespeicher:WISKI_Name>579320</bodespeicher:WISKI_Name>
          <bodespeicher:Kurz_Name>kabo</bodespeicher:Kurz_Name>
          <bodespeicher:Abgabe_Eingang/>
          <bodespeicher:Abgabe/>
          <bodespeicher:Ganglinie_gemessenEingang/>
          <bodespeicher:Ganglinie_gemessen/>
          <bodespeicher:Ganglinie_gerechnet/>
          <bodespeicher:Ganglinie_prognoseAblage/>
          <bodespeicher:StauraumParameter>
            <bodespeicher:Stauraum fid="Stauraum_kabo">
              <bodespeicher:Totraum>0.0</bodespeicher:Totraum>
              <bodespeicher:Reserveraum>0.0</bodespeicher:Reserveraum>
              <bodespeicher:Betriebsraum>
                <bodecommon:MonatCollection fid="Stauraum_kabo_Jahr">
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_kabo_Jan">
                      <bodecommon:Name>Januar</bodecommon:Name>
                      <bodecommon:Nummer>1</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_kabo_Feb">
                      <bodecommon:Name>Februar</bodecommon:Name>
                      <bodecommon:Nummer>2</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_kabo_Mrz">
                      <bodecommon:Name>März</bodecommon:Name>
                      <bodecommon:Nummer>3</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_kabo_Apr">
                      <bodecommon:Name>April</bodecommon:Name>
                      <bodecommon:Nummer>4</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_kabo_Mai">
                      <bodecommon:Name>Mai</bodecommon:Name>
                      <bodecommon:Nummer>5</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_kabo_Jun">
                      <bodecommon:Name>Juni</bodecommon:Name>
                      <bodecommon:Nummer>6</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_kabo_Jul">
                      <bodecommon:Name>Juli</bodecommon:Name>
                      <bodecommon:Nummer>7</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_kabo_Aug">
                      <bodecommon:Name>August</bodecommon:Name>
                      <bodecommon:Nummer>8</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_kabo_Sep">
                      <bodecommon:Name>September</bodecommon:Name>
                      <bodecommon:Nummer>9</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_kabo_Okt">
                      <bodecommon:Name>Oktober</bodecommon:Name>
                      <bodecommon:Nummer>10</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_kabo_Nov">
                      <bodecommon:Name>November</bodecommon:Name>
                      <bodecommon:Nummer>11</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_kabo_Dez">
                      <bodecommon:Name>Dezember</bodecommon:Name>
                      <bodecommon:Nummer>12</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                </bodecommon:MonatCollection>
              </bodespeicher:Betriebsraum>
              <bodespeicher:Stauraum>0.0</bodespeicher:Stauraum>
            </bodespeicher:Stauraum>
          </bodespeicher:StauraumParameter>
          <bodespeicher:MindestabgabeParameter>
            <bodespeicher:Mindestabgabe fid="MinAbgabe_kabo">
              <bodespeicher:Mindestabgabe>
                <bodecommon:MonatCollection fid="MinAbgabe_kabo_Jahr">
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_kabo_Jan">
                      <bodecommon:Name>Januar</bodecommon:Name>
                      <bodecommon:Nummer>1</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_kabo_Feb">
                      <bodecommon:Name>Februar</bodecommon:Name>
                      <bodecommon:Nummer>2</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_kabo_Mrz">
                      <bodecommon:Name>März</bodecommon:Name>
                      <bodecommon:Nummer>3</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_kabo_Apr">
                      <bodecommon:Name>April</bodecommon:Name>
                      <bodecommon:Nummer>4</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_kabo_Mai">
                      <bodecommon:Name>Mai</bodecommon:Name>
                      <bodecommon:Nummer>5</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_kabo_Jun">
                      <bodecommon:Name>Juni</bodecommon:Name>
                      <bodecommon:Nummer>6</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_kabo_Jul">
                      <bodecommon:Name>Juli</bodecommon:Name>
                      <bodecommon:Nummer>7</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_kabo_Aug">
                      <bodecommon:Name>August</bodecommon:Name>
                      <bodecommon:Nummer>8</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_kabo_Sep">
                      <bodecommon:Name>September</bodecommon:Name>
                      <bodecommon:Nummer>9</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_kabo_Okt">
                      <bodecommon:Name>Oktober</bodecommon:Name>
                      <bodecommon:Nummer>10</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_kabo_Nov">
                      <bodecommon:Name>November</bodecommon:Name>
                      <bodecommon:Nummer>11</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_kabo_Dez">
                      <bodecommon:Name>Dezember</bodecommon:Name>
                      <bodecommon:Nummer>12</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                </bodecommon:MonatCollection>
              </bodespeicher:Mindestabgabe>
            </bodespeicher:Mindestabgabe>
          </bodespeicher:MindestabgabeParameter>
          <bodespeicher:MaximalabgabeParameter>
            <bodespeicher:Maximalabgabe fid="MaxAbgabe_kabo">
              <bodespeicher:Maximalabgabe>
                <bodecommon:MonatCollection fid="MaxAbgabe_kabo_Jahr">
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_kabo_Jan">
                      <bodecommon:Name>Januar</bodecommon:Name>
                      <bodecommon:Nummer>1</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_kabo_Feb">
                      <bodecommon:Name>Februar</bodecommon:Name>
                      <bodecommon:Nummer>2</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_kabo_Mrz">
                      <bodecommon:Name>März</bodecommon:Name>
                      <bodecommon:Nummer>3</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_kabo_Apr">
                      <bodecommon:Name>April</bodecommon:Name>
                      <bodecommon:Nummer>4</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_kabo_Mai">
                      <bodecommon:Name>Mai</bodecommon:Name>
                      <bodecommon:Nummer>5</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_kabo_Jun">
                      <bodecommon:Name>Juni</bodecommon:Name>
                      <bodecommon:Nummer>6</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_kabo_Jul">
                      <bodecommon:Name>Juli</bodecommon:Name>
                      <bodecommon:Nummer>7</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_kabo_Aug">
                      <bodecommon:Name>August</bodecommon:Name>
                      <bodecommon:Nummer>8</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_kabo_Sep">
                      <bodecommon:Name>September</bodecommon:Name>
                      <bodecommon:Nummer>9</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_kabo_Okt">
                      <bodecommon:Name>Oktober</bodecommon:Name>
                      <bodecommon:Nummer>10</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_kabo_Nov">
                      <bodecommon:Name>November</bodecommon:Name>
                      <bodecommon:Nummer>11</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_kabo_Dez">
                      <bodecommon:Name>Dezember</bodecommon:Name>
                      <bodecommon:Nummer>12</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                </bodecommon:MonatCollection>
              </bodespeicher:Maximalabgabe>
            </bodespeicher:Maximalabgabe>
          </bodespeicher:MaximalabgabeParameter>
          <bodespeicher:TrinkwasserParameter>
            <bodespeicher:Trinkwasser fid="Trinkwasser_kabo">
              <bodespeicher:Trinkwasser>
                <bodecommon:MonatCollection fid="Trinkwasser_kabo_Jahr">
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_kabo_Jan">
                      <bodecommon:Name>Januar</bodecommon:Name>
                      <bodecommon:Nummer>1</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_kabo_Feb">
                      <bodecommon:Name>Februar</bodecommon:Name>
                      <bodecommon:Nummer>2</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_kabo_Mrz">
                      <bodecommon:Name>März</bodecommon:Name>
                      <bodecommon:Nummer>3</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_kabo_Apr">
                      <bodecommon:Name>April</bodecommon:Name>
                      <bodecommon:Nummer>4</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_kabo_Mai">
                      <bodecommon:Name>Mai</bodecommon:Name>
                      <bodecommon:Nummer>5</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_kabo_Jun">
                      <bodecommon:Name>Juni</bodecommon:Name>
                      <bodecommon:Nummer>6</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_kabo_Jul">
                      <bodecommon:Name>Juli</bodecommon:Name>
                      <bodecommon:Nummer>7</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_kabo_Aug">
                      <bodecommon:Name>August</bodecommon:Name>
                      <bodecommon:Nummer>8</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_kabo_Sep">
                      <bodecommon:Name>September</bodecommon:Name>
                      <bodecommon:Nummer>9</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_kabo_Okt">
                      <bodecommon:Name>Oktober</bodecommon:Name>
                      <bodecommon:Nummer>10</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_kabo_Nov">
                      <bodecommon:Name>November</bodecommon:Name>
                      <bodecommon:Nummer>11</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_kabo_Dez">
                      <bodecommon:Name>Dezember</bodecommon:Name>
                      <bodecommon:Nummer>12</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                </bodecommon:MonatCollection>
              </bodespeicher:Trinkwasser>
            </bodespeicher:Trinkwasser>
          </bodespeicher:TrinkwasserParameter>
          <bodespeicher:EACollectionAssociation>
            <bodespeicher:EACollection fid="EACollection_kabo">
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
              <bodespeicher:EAMember>
                <bodespeicher:EA fid="EA_kabo_0">
                  <bodespeicher:Höhe>0.0</bodespeicher:Höhe>
                  <bodespeicher:Inhalt>0.0</bodespeicher:Inhalt>
                  <bodespeicher:Grundablass>0.0</bodespeicher:Grundablass>
                  <bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
                  <bodespeicher:Bemerkung/>
                </bodespeicher:EA>
              </bodespeicher:EAMember>
            </bodespeicher:EACollection>
          </bodespeicher:EACollectionAssociation>
        </bodespeicher:Speicher>
      </bodespeicher:SpeicherMember>
      <bodespeicher:SpeicherMember>
        <bodespeicher:Speicher fid="Speicher_Rappbode">
          <bodespeicher:Name>TS Rappbode</bodespeicher:Name>
          <bodespeicher:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5216625.659439575,5744001.772716318</gml:coordinates>
            </gml:Point>
          </bodespeicher:Ort>
          <bodespeicher:WISKI_Name>579430</bodespeicher:WISKI_Name>
          <bodespeicher:Kurz_Name>rapp</bodespeicher:Kurz_Name>
          <bodespeicher:Abgabe_Eingang/>
          <bodespeicher:Abgabe/>
          <bodespeicher:Ganglinie_gemessenEingang/>
          <bodespeicher:Ganglinie_gemessen/>
          <bodespeicher:Ganglinie_gerechnet/>
          <bodespeicher:Ganglinie_prognoseAblage/>
          <bodespeicher:StauraumParameter>
            <bodespeicher:Stauraum fid="Stauraum_rapp">
              <bodespeicher:Totraum>0.0</bodespeicher:Totraum>
              <bodespeicher:Reserveraum>0.0</bodespeicher:Reserveraum>
              <bodespeicher:Betriebsraum>
                <bodecommon:MonatCollection fid="Stauraum_rapp_Jahr">
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_rapp_Jan">
                      <bodecommon:Name>Januar</bodecommon:Name>
                      <bodecommon:Nummer>1</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_rapp_Feb">
                      <bodecommon:Name>Februar</bodecommon:Name>
                      <bodecommon:Nummer>2</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_rapp_Mrz">
                      <bodecommon:Name>März</bodecommon:Name>
                      <bodecommon:Nummer>3</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_rapp_Apr">
                      <bodecommon:Name>April</bodecommon:Name>
                      <bodecommon:Nummer>4</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_rapp_Mai">
                      <bodecommon:Name>Mai</bodecommon:Name>
                      <bodecommon:Nummer>5</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_rapp_Jun">
                      <bodecommon:Name>Juni</bodecommon:Name>
                      <bodecommon:Nummer>6</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_rapp_Jul">
                      <bodecommon:Name>Juli</bodecommon:Name>
                      <bodecommon:Nummer>7</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_rapp_Aug">
                      <bodecommon:Name>August</bodecommon:Name>
                      <bodecommon:Nummer>8</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_rapp_Sep">
                      <bodecommon:Name>September</bodecommon:Name>
                      <bodecommon:Nummer>9</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_rapp_Okt">
                      <bodecommon:Name>Oktober</bodecommon:Name>
                      <bodecommon:Nummer>10</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_rapp_Nov">
                      <bodecommon:Name>November</bodecommon:Name>
                      <bodecommon:Nummer>11</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_rapp_Dez">
                      <bodecommon:Name>Dezember</bodecommon:Name>
                      <bodecommon:Nummer>12</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                </bodecommon:MonatCollection>
              </bodespeicher:Betriebsraum>
              <bodespeicher:Stauraum>0.0</bodespeicher:Stauraum>
            </bodespeicher:Stauraum>
          </bodespeicher:StauraumParameter>
          <bodespeicher:MindestabgabeParameter>
            <bodespeicher:Mindestabgabe fid="MinAbgabe_rapp">
              <bodespeicher:Mindestabgabe>
                <bodecommon:MonatCollection fid="MinAbgabe_rapp_Jahr">
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_rapp_Jan">
                      <bodecommon:Name>Januar</bodecommon:Name>
                      <bodecommon:Nummer>1</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_rapp_Feb">
                      <bodecommon:Name>Februar</bodecommon:Name>
                      <bodecommon:Nummer>2</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_rapp_Mrz">
                      <bodecommon:Name>März</bodecommon:Name>
                      <bodecommon:Nummer>3</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_rapp_Apr">
                      <bodecommon:Name>April</bodecommon:Name>
                      <bodecommon:Nummer>4</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_rapp_Mai">
                      <bodecommon:Name>Mai</bodecommon:Name>
                      <bodecommon:Nummer>5</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_rapp_Jun">
                      <bodecommon:Name>Juni</bodecommon:Name>
                      <bodecommon:Nummer>6</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_rapp_Jul">
                      <bodecommon:Name>Juli</bodecommon:Name>
                      <bodecommon:Nummer>7</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_rapp_Aug">
                      <bodecommon:Name>August</bodecommon:Name>
                      <bodecommon:Nummer>8</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_rapp_Sep">
                      <bodecommon:Name>September</bodecommon:Name>
                      <bodecommon:Nummer>9</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_rapp_Okt">
                      <bodecommon:Name>Oktober</bodecommon:Name>
                      <bodecommon:Nummer>10</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_rapp_Nov">
                      <bodecommon:Name>November</bodecommon:Name>
                      <bodecommon:Nummer>11</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_rapp_Dez">
                      <bodecommon:Name>Dezember</bodecommon:Name>
                      <bodecommon:Nummer>12</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                </bodecommon:MonatCollection>
              </bodespeicher:Mindestabgabe>
            </bodespeicher:Mindestabgabe>
          </bodespeicher:MindestabgabeParameter>
          <bodespeicher:MaximalabgabeParameter>
            <bodespeicher:Maximalabgabe fid="MaxAbgabe_rapp">
              <bodespeicher:Maximalabgabe>
                <bodecommon:MonatCollection fid="MaxAbgabe_rapp_Jahr">
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_rapp_Jan">
                      <bodecommon:Name>Januar</bodecommon:Name>
                      <bodecommon:Nummer>1</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_rapp_Feb">
                      <bodecommon:Name>Februar</bodecommon:Name>
                      <bodecommon:Nummer>2</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_rapp_Mrz">
                      <bodecommon:Name>März</bodecommon:Name>
                      <bodecommon:Nummer>3</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_rapp_Apr">
                      <bodecommon:Name>April</bodecommon:Name>
                      <bodecommon:Nummer>4</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_rapp_Mai">
                      <bodecommon:Name>Mai</bodecommon:Name>
                      <bodecommon:Nummer>5</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_rapp_Jun">
                      <bodecommon:Name>Juni</bodecommon:Name>
                      <bodecommon:Nummer>6</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_rapp_Jul">
                      <bodecommon:Name>Juli</bodecommon:Name>
                      <bodecommon:Nummer>7</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_rapp_Aug">
                      <bodecommon:Name>August</bodecommon:Name>
                      <bodecommon:Nummer>8</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_rapp_Sep">
                      <bodecommon:Name>September</bodecommon:Name>
                      <bodecommon:Nummer>9</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_rapp_Okt">
                      <bodecommon:Name>Oktober</bodecommon:Name>
                      <bodecommon:Nummer>10</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_rapp_Nov">
                      <bodecommon:Name>November</bodecommon:Name>
                      <bodecommon:Nummer>11</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_rapp_Dez">
                      <bodecommon:Name>Dezember</bodecommon:Name>
                      <bodecommon:Nummer>12</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                </bodecommon:MonatCollection>
              </bodespeicher:Maximalabgabe>
            </bodespeicher:Maximalabgabe>
          </bodespeicher:MaximalabgabeParameter>
          <bodespeicher:TrinkwasserParameter>
            <bodespeicher:Trinkwasser fid="Trinkwasser_rapp">
              <bodespeicher:Trinkwasser>
                <bodecommon:MonatCollection fid="Trinkwasser_rapp_Jahr">
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_rapp_Jan">
                      <bodecommon:Name>Januar</bodecommon:Name>
                      <bodecommon:Nummer>1</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_rapp_Feb">
                      <bodecommon:Name>Februar</bodecommon:Name>
                      <bodecommon:Nummer>2</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_rapp_Mrz">
                      <bodecommon:Name>März</bodecommon:Name>
                      <bodecommon:Nummer>3</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_rapp_Apr">
                      <bodecommon:Name>April</bodecommon:Name>
                      <bodecommon:Nummer>4</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_rapp_Mai">
                      <bodecommon:Name>Mai</bodecommon:Name>
                      <bodecommon:Nummer>5</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_rapp_Jun">
                      <bodecommon:Name>Juni</bodecommon:Name>
                      <bodecommon:Nummer>6</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_rapp_Jul">
                      <bodecommon:Name>Juli</bodecommon:Name>
                      <bodecommon:Nummer>7</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_rapp_Aug">
                      <bodecommon:Name>August</bodecommon:Name>
                      <bodecommon:Nummer>8</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_rapp_Sep">
                      <bodecommon:Name>September</bodecommon:Name>
                      <bodecommon:Nummer>9</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_rapp_Okt">
                      <bodecommon:Name>Oktober</bodecommon:Name>
                      <bodecommon:Nummer>10</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_rapp_Nov">
                      <bodecommon:Name>November</bodecommon:Name>
                      <bodecommon:Nummer>11</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_rapp_Dez">
                      <bodecommon:Name>Dezember</bodecommon:Name>
                      <bodecommon:Nummer>12</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                </bodecommon:MonatCollection>
              </bodespeicher:Trinkwasser>
            </bodespeicher:Trinkwasser>
          </bodespeicher:TrinkwasserParameter>
          <bodespeicher:EACollectionAssociation>
            <bodespeicher:EACollection fid="EACollection_rapp">
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
              <bodespeicher:EAMember>
                <bodespeicher:EA fid="EA_rapp_0">
                  <bodespeicher:Höhe>0.0</bodespeicher:Höhe>
                  <bodespeicher:Inhalt>0.0</bodespeicher:Inhalt>
                  <bodespeicher:Grundablass>0.0</bodespeicher:Grundablass>
                  <bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
                  <bodespeicher:Bemerkung/>
                </bodespeicher:EA>
              </bodespeicher:EAMember>
            </bodespeicher:EACollection>
          </bodespeicher:EACollectionAssociation>
        </bodespeicher:Speicher>
      </bodespeicher:SpeicherMember>
      <bodespeicher:SpeicherMember>
        <bodespeicher:Speicher fid="Speicher_Wendefurth">
          <bodespeicher:Name>TS Wendefurth</bodespeicher:Name>
          <bodespeicher:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5218272.874608937,5742160.828668987</gml:coordinates>
            </gml:Point>
          </bodespeicher:Ort>
          <bodespeicher:WISKI_Name>579005</bodespeicher:WISKI_Name>
          <bodespeicher:Kurz_Name>wend</bodespeicher:Kurz_Name>
          <bodespeicher:Abgabe_Eingang/>
          <bodespeicher:Abgabe/>
          <bodespeicher:Ganglinie_gemessenEingang/>
          <bodespeicher:Ganglinie_gemessen/>
          <bodespeicher:Ganglinie_gerechnet/>
          <bodespeicher:Ganglinie_prognoseAblage/>
          <bodespeicher:StauraumParameter>
            <bodespeicher:Stauraum fid="Stauraum_wend">
              <bodespeicher:Totraum>0.0</bodespeicher:Totraum>
              <bodespeicher:Reserveraum>0.0</bodespeicher:Reserveraum>
              <bodespeicher:Betriebsraum>
                <bodecommon:MonatCollection fid="Stauraum_wend_Jahr">
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_wend_Jan">
                      <bodecommon:Name>Januar</bodecommon:Name>
                      <bodecommon:Nummer>1</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_wend_Feb">
                      <bodecommon:Name>Februar</bodecommon:Name>
                      <bodecommon:Nummer>2</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_wend_Mrz">
                      <bodecommon:Name>März</bodecommon:Name>
                      <bodecommon:Nummer>3</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_wend_Apr">
                      <bodecommon:Name>April</bodecommon:Name>
                      <bodecommon:Nummer>4</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_wend_Mai">
                      <bodecommon:Name>Mai</bodecommon:Name>
                      <bodecommon:Nummer>5</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_wend_Jun">
                      <bodecommon:Name>Juni</bodecommon:Name>
                      <bodecommon:Nummer>6</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_wend_Jul">
                      <bodecommon:Name>Juli</bodecommon:Name>
                      <bodecommon:Nummer>7</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_wend_Aug">
                      <bodecommon:Name>August</bodecommon:Name>
                      <bodecommon:Nummer>8</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_wend_Sep">
                      <bodecommon:Name>September</bodecommon:Name>
                      <bodecommon:Nummer>9</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_wend_Okt">
                      <bodecommon:Name>Oktober</bodecommon:Name>
                      <bodecommon:Nummer>10</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_wend_Nov">
                      <bodecommon:Name>November</bodecommon:Name>
                      <bodecommon:Nummer>11</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Stauraum_wend_Dez">
                      <bodecommon:Name>Dezember</bodecommon:Name>
                      <bodecommon:Nummer>12</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                </bodecommon:MonatCollection>
              </bodespeicher:Betriebsraum>
              <bodespeicher:Stauraum>0.0</bodespeicher:Stauraum>
            </bodespeicher:Stauraum>
          </bodespeicher:StauraumParameter>
          <bodespeicher:MindestabgabeParameter>
            <bodespeicher:Mindestabgabe fid="MinAbgabe_wend">
              <bodespeicher:Mindestabgabe>
                <bodecommon:MonatCollection fid="MinAbgabe_wend_Jahr">
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_wend_Jan">
                      <bodecommon:Name>Januar</bodecommon:Name>
                      <bodecommon:Nummer>1</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_wend_Feb">
                      <bodecommon:Name>Februar</bodecommon:Name>
                      <bodecommon:Nummer>2</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_wend_Mrz">
                      <bodecommon:Name>März</bodecommon:Name>
                      <bodecommon:Nummer>3</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_wend_Apr">
                      <bodecommon:Name>April</bodecommon:Name>
                      <bodecommon:Nummer>4</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_wend_Mai">
                      <bodecommon:Name>Mai</bodecommon:Name>
                      <bodecommon:Nummer>5</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_wend_Jun">
                      <bodecommon:Name>Juni</bodecommon:Name>
                      <bodecommon:Nummer>6</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_wend_Jul">
                      <bodecommon:Name>Juli</bodecommon:Name>
                      <bodecommon:Nummer>7</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_wend_Aug">
                      <bodecommon:Name>August</bodecommon:Name>
                      <bodecommon:Nummer>8</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_wend_Sep">
                      <bodecommon:Name>September</bodecommon:Name>
                      <bodecommon:Nummer>9</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_wend_Okt">
                      <bodecommon:Name>Oktober</bodecommon:Name>
                      <bodecommon:Nummer>10</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_wend_Nov">
                      <bodecommon:Name>November</bodecommon:Name>
                      <bodecommon:Nummer>11</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MinAbgabe_wend_Dez">
                      <bodecommon:Name>Dezember</bodecommon:Name>
                      <bodecommon:Nummer>12</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                </bodecommon:MonatCollection>
              </bodespeicher:Mindestabgabe>
            </bodespeicher:Mindestabgabe>
          </bodespeicher:MindestabgabeParameter>
          <bodespeicher:MaximalabgabeParameter>
            <bodespeicher:Maximalabgabe fid="MaxAbgabe_wend">
              <bodespeicher:Maximalabgabe>
                <bodecommon:MonatCollection fid="MaxAbgabe_wend_Jahr">
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_wend_Jan">
                      <bodecommon:Name>Januar</bodecommon:Name>
                      <bodecommon:Nummer>1</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_wend_Feb">
                      <bodecommon:Name>Februar</bodecommon:Name>
                      <bodecommon:Nummer>2</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_wend_Mrz">
                      <bodecommon:Name>März</bodecommon:Name>
                      <bodecommon:Nummer>3</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_wend_Apr">
                      <bodecommon:Name>April</bodecommon:Name>
                      <bodecommon:Nummer>4</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_wend_Mai">
                      <bodecommon:Name>Mai</bodecommon:Name>
                      <bodecommon:Nummer>5</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_wend_Jun">
                      <bodecommon:Name>Juni</bodecommon:Name>
                      <bodecommon:Nummer>6</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_wend_Jul">
                      <bodecommon:Name>Juli</bodecommon:Name>
                      <bodecommon:Nummer>7</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_wend_Aug">
                      <bodecommon:Name>August</bodecommon:Name>
                      <bodecommon:Nummer>8</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_wend_Sep">
                      <bodecommon:Name>September</bodecommon:Name>
                      <bodecommon:Nummer>9</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_wend_Okt">
                      <bodecommon:Name>Oktober</bodecommon:Name>
                      <bodecommon:Nummer>10</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_wend_Nov">
                      <bodecommon:Name>November</bodecommon:Name>
                      <bodecommon:Nummer>11</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="MaxAbgabe_wend_Dez">
                      <bodecommon:Name>Dezember</bodecommon:Name>
                      <bodecommon:Nummer>12</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                </bodecommon:MonatCollection>
              </bodespeicher:Maximalabgabe>
            </bodespeicher:Maximalabgabe>
          </bodespeicher:MaximalabgabeParameter>
          <bodespeicher:TrinkwasserParameter>
            <bodespeicher:Trinkwasser fid="Trinkwasser_wend">
              <bodespeicher:Trinkwasser>
                <bodecommon:MonatCollection fid="Trinkwasser_wend_Jahr">
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_wend_Jan">
                      <bodecommon:Name>Januar</bodecommon:Name>
                      <bodecommon:Nummer>1</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_wend_Feb">
                      <bodecommon:Name>Februar</bodecommon:Name>
                      <bodecommon:Nummer>2</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_wend_Mrz">
                      <bodecommon:Name>März</bodecommon:Name>
                      <bodecommon:Nummer>3</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_wend_Apr">
                      <bodecommon:Name>April</bodecommon:Name>
                      <bodecommon:Nummer>4</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_wend_Mai">
                      <bodecommon:Name>Mai</bodecommon:Name>
                      <bodecommon:Nummer>5</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_wend_Jun">
                      <bodecommon:Name>Juni</bodecommon:Name>
                      <bodecommon:Nummer>6</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_wend_Jul">
                      <bodecommon:Name>Juli</bodecommon:Name>
                      <bodecommon:Nummer>7</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_wend_Aug">
                      <bodecommon:Name>August</bodecommon:Name>
                      <bodecommon:Nummer>8</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_wend_Sep">
                      <bodecommon:Name>September</bodecommon:Name>
                      <bodecommon:Nummer>9</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_wend_Okt">
                      <bodecommon:Name>Oktober</bodecommon:Name>
                      <bodecommon:Nummer>10</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_wend_Nov">
                      <bodecommon:Name>November</bodecommon:Name>
                      <bodecommon:Nummer>11</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                  <bodecommon:MonatMember>
                    <bodecommon:Monat fid="Trinkwasser_wend_Dez">
                      <bodecommon:Name>Dezember</bodecommon:Name>
                      <bodecommon:Nummer>12</bodecommon:Nummer>
                      <bodecommon:Wert>0.0</bodecommon:Wert>
                    </bodecommon:Monat>
                  </bodecommon:MonatMember>
                </bodecommon:MonatCollection>
              </bodespeicher:Trinkwasser>
            </bodespeicher:Trinkwasser>
          </bodespeicher:TrinkwasserParameter>
          <bodespeicher:EACollectionAssociation>
            <bodespeicher:EACollection fid="EACollection_wend">
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
              <bodespeicher:EAMember>
                <bodespeicher:EA fid="EA_wend_0">
                  <bodespeicher:Höhe>0.0</bodespeicher:Höhe>
                  <bodespeicher:Inhalt>0.0</bodespeicher:Inhalt>
                  <bodespeicher:Grundablass>0.0</bodespeicher:Grundablass>
                  <bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
                  <bodespeicher:Bemerkung/>
                </bodespeicher:EA>
              </bodespeicher:EAMember>
            </bodespeicher:EACollection>
          </bodespeicher:EACollectionAssociation>
        </bodespeicher:Speicher>
      </bodespeicher:SpeicherMember>
      <bodespeicher:SpeicherMember>
        <bodespeicher:Überleitung fid="Überleitung_Rappbode">
          <bodespeicher:Name>Überleitung Rappbode</bodespeicher:Name>
          <bodespeicher:Ort>
            <gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
              <gml:coordinates cs="," decimal="." ts=" ">5210386.313997258,5741854.435113958</gml:coordinates>
            </gml:Point>
          </bodespeicher:Ort>
          <bodespeicher:WISKI_Name>579000</bodespeicher:WISKI_Name>
          <bodespeicher:Kurz_Name>???</bodespeicher:Kurz_Name>
          <bodespeicher:Abgabe_Eingang/>
          <bodespeicher:Abgabe/>
        </bodespeicher:Überleitung>
      </bodespeicher:SpeicherMember>
    </bodespeicher:SpeicherCollection>
  </bodespeicher:SpeicherCollectionAssociation>
</BodeModell>
