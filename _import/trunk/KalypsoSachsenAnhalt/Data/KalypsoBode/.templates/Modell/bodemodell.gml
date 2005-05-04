<?xml version="1.0" encoding="UTF-8"?>
<!-- mit relativem Pfad: wird von XML-Spy verstanden
<BodeModell fid="root" xmlns="org.kalypso.bode.modell" xmlns:bodecommon="org.kalypso.bode.common" xmlns:bodeombrometer="org.kalypso.bode.ombrometer" xmlns:bodepegel="org.kalypso.bode.pegel" xmlns:bodespeicher="org.kalypso.bode.speicher" xmlns:gml="http://www.opengis.net/gml" xmlns:obslink="obslink.zml.kalypso.org" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="org.kalypso.bode.modell project:/.model/schema/bodemodell.xsd">
<BodeModell fid="root" xmlns="org.kalypso.bode.modell" xmlns:bodecommon="org.kalypso.bode.common" xmlns:bodeombrometer="org.kalypso.bode.ombrometer" xmlns:bodepegel="org.kalypso.bode.pegel" xmlns:bodespeicher="org.kalypso.bode.speicher" xmlns:gml="http://www.opengis.net/gml" xmlns:obslink="obslink.zml.kalypso.org" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="org.kalypso.bode.modell C:\Programme_inst\eclipse\runtime-workspace\KalypsoBode\.model\schema\bodemodell.xsd">
-->
<BodeModell fid="root" xmlns="org.kalypso.bode.modell" xmlns:bodecommon="org.kalypso.bode.common" xmlns:bodeombrometer="org.kalypso.bode.ombrometer" xmlns:bodepegel="org.kalypso.bode.pegel" xmlns:bodespeicher="org.kalypso.bode.speicher" xmlns:gml="http://www.opengis.net/gml" xmlns:obslink="obslink.zml.kalypso.org" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="org.kalypso.bode.modell project:/.model/schema/bodemodell.xsd">


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
							<gml:coordinates cs="," decimal="." ts=" ">5265120.666353023,5752160.263210346</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579085</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>stas</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Staßfurt.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Staßfurt.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
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
							<gml:coordinates cs="," decimal="." ts=" ">5237817.466836101,5757262.4569166275</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579049</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>wege</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Wegeleben.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Wegeleben.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
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
							<gml:coordinates cs="," decimal="." ts=" ">5238766.71591927,5751133.2589417035</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579040</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>ditf</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Ditfurt.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Ditfurt.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
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
				<bodepegel:WasserlaufModell fid="WLM_Nienhagen">
					<bodepegel:Name>Nienhagen</bodepegel:Name>
					<bodepegel:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5236078.022653717,5763414.323225588</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579745</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>nien</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Nienhagen.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Nienhagen.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
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
							<gml:coordinates cs="," decimal="." ts=" ">5273131.126460996,5750908.695825978</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579090</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>neug</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>false</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Nienhagen.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
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
							<gml:coordinates cs="," decimal="." ts=" ">5247443.86571238,5769896.309583677</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579070</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>hadm</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Hadmersleben.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Hadmersleben.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
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
							<gml:coordinates cs="," decimal="." ts=" ">5242962.026694506,5751581.344781446</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579620</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>haus</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Hausneindorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Hausneindorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
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
							<gml:coordinates cs="," decimal="." ts=" ">5243204.612193139,5735094.576465873</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579610</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>meis</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Meisdorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Meisdorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
					<bodepegel:Gebiet/>
					<bodepegel:Niederschlag>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Meisdorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Niederschlag>
				</bodepegel:Niederschlagsgebiet>
			</bodepegel:PegelMember>
			<bodepegel:PegelMember>
				<bodepegel:Niederschlagsgebiet fid="NSGebiet_Elend">
					<bodepegel:Name>Elend</bodepegel:Name>
					<bodepegel:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5202064.849923498,5743158.864709639</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579305</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>elen</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Elend.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Elend.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
					<bodepegel:Gebiet/>
					<bodepegel:Niederschlag>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Elend.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Niederschlag>
				</bodepegel:Niederschlagsgebiet>
			</bodepegel:PegelMember>
			<bodepegel:PegelMember>
				<bodepegel:Niederschlagsgebiet fid="NSGebiet_Hasselfelde">
					<bodepegel:Name>Hasselfelde</bodepegel:Name>
					<bodepegel:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5213844.941062603,5736803.356648105</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>4421200</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>hass</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Hasselfelde.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Hasselfelde.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
					<bodepegel:Gebiet/>
					<bodepegel:Niederschlag>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Hasselfelde.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Niederschlag>
				</bodepegel:Niederschlagsgebiet>
			</bodepegel:PegelMember>
			<bodepegel:PegelMember>
				<bodepegel:Niederschlagsgebiet fid="NSGebiet_Mahndorf">
					<bodepegel:Name>Mahndorf</bodepegel:Name>
					<bodepegel:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5222223.011338177,5757795.57484032</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579712</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>mahn</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Mahndorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Mahndorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
					<bodepegel:Gebiet/>
					<bodepegel:Niederschlag>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Mahndorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Niederschlag>
				</bodepegel:Niederschlagsgebiet>
			</bodepegel:PegelMember>
			<bodepegel:PegelMember>
				<bodepegel:Niederschlagsgebiet fid="NSGebiet_Silberhütte">
					<bodepegel:Name>Silberhütte</bodepegel:Name>
					<bodepegel:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5229948.558568242,5729148.945125754</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579605</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>silb</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Silberhütte.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Silberhütte.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
					<bodepegel:Gebiet/>
					<bodepegel:Niederschlag>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Silberhütte.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Niederschlag>
				</bodepegel:Niederschlagsgebiet>
			</bodepegel:PegelMember>
			<bodepegel:PegelMember>
				<bodepegel:Niederschlagsgebiet fid="NSGebiet_Königshütte">
					<bodepegel:Name>Königshütte</bodepegel:Name>
					<bodepegel:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5207708.385630242,5742265.26812559</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579209</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>koen</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Königshütte.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Königshütte.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
					<bodepegel:Gebiet/>
					<bodepegel:Niederschlag>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Königshütte.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Niederschlag>
				</bodepegel:Niederschlagsgebiet>
			</bodepegel:PegelMember>
			<bodepegel:PegelMember>
				<bodepegel:Niederschlagsgebiet fid="NSGebiet_Steinerne_Renne">
					<bodepegel:Name>Steinerne Renne</bodepegel:Name>
					<bodepegel:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5205705.650895029,5751283.956602088</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579705</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>stei</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Steinerne_Renne.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Steinerne_Renne.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
					<bodepegel:Gebiet/>
					<bodepegel:Niederschlag>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Steinerne_Renne.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Niederschlag>
				</bodepegel:Niederschlagsgebiet>
			</bodepegel:PegelMember>
			<bodepegel:PegelMember>
				<bodepegel:Niederschlagsgebiet fid="NSGebiet_Oschersleben">
					<bodepegel:Name>Oschersleben</bodepegel:Name>
					<bodepegel:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5241071.153292574,5772263.0450663855</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579810</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>osch</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Oschersleben.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Oschersleben.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
					<bodepegel:Gebiet/>
					<bodepegel:Niederschlag>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Oschersleben.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Niederschlag>
				</bodepegel:Niederschlagsgebiet>
			</bodepegel:PegelMember>
			<bodepegel:PegelMember>
				<bodepegel:Niederschlagsgebiet fid="NSGebiet_Trautenstein">
					<bodepegel:Name>Trautenstein</bodepegel:Name>
					<bodepegel:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5209130.667362792,5737147.612297329</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579405</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>trau</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Trautenstein.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Trautenstein.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
					<bodepegel:Gebiet/>
					<bodepegel:Niederschlag>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Trautenstein.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Niederschlag>
				</bodepegel:Niederschlagsgebiet>
			</bodepegel:PegelMember>
			<bodepegel:PegelMember>
				<bodepegel:Pegel fid="Pegel_Thale">
					<bodepegel:Name>Thale</bodepegel:Name>
					<bodepegel:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5225486.406048773,5741212.732772261</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579020</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>thal</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>true</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Thale.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
					<bodepegel:Ganglinie_gerechnet >
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Vorhersage/Thale.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gerechnet>
				</bodepegel:Pegel>
			</bodepegel:PegelMember>
			<bodepegel:PegelMember>
				<bodepegel:Pegel fid="Pegel_Wendefurth">
					<bodepegel:Name>Wendefurth</bodepegel:Name>
					<bodepegel:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5218308.349121872,5742049.203931116</gml:coordinates>
						</gml:Point>
					</bodepegel:Ort>
					<bodepegel:Messstellennummer>579006</bodepegel:Messstellennummer>
					<bodepegel:Kurz_Name>wend</bodepegel:Kurz_Name>
					<bodepegel:istEingabePegel>true</bodepegel:istEingabePegel>
					<bodepegel:istErgebnisPegel>false</bodepegel:istErgebnisPegel>
					<bodepegel:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Pegel/Messung/Wendefurth.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodepegel:Ganglinie_gemessen>
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
							<gml:coordinates cs="," decimal="." ts=" ">5231071.410767844,5805231.416728718</gml:coordinates>
						</gml:Point>
					</bodeombrometer:Ort>
					<bodeombrometer:Messstellennummer>440010</bodeombrometer:Messstellennummer>
					<bodeombrometer:Kurz_Name>wefe</bodeombrometer:Kurz_Name>
					<bodeombrometer:Niederschlag_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Weferlingen.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
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
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Ditfurt.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
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
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Elend.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodeombrometer:Niederschlag_gemessen>
				</bodeombrometer:Ombrometer>
			</bodeombrometer:OmbrometerMember>
			<bodeombrometer:OmbrometerMember>
				<bodeombrometer:Ombrometer fid="Ombro_Hasselfelde">
					<bodeombrometer:Name>Hasselfelde</bodeombrometer:Name>
					<bodeombrometer:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5213844.941062603,5736803.356648105</gml:coordinates>
						</gml:Point>
					</bodeombrometer:Ort>
					<bodeombrometer:Messstellennummer>579504</bodeombrometer:Messstellennummer>
					<bodeombrometer:Kurz_Name>hass</bodeombrometer:Kurz_Name>
					<bodeombrometer:Niederschlag_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Hasselfelde.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodeombrometer:Niederschlag_gemessen>
				</bodeombrometer:Ombrometer>
			</bodeombrometer:OmbrometerMember>
			<bodeombrometer:OmbrometerMember>
				<bodeombrometer:Ombrometer fid="Ombro_Silberhütte">
					<bodeombrometer:Name>Silberhütte</bodeombrometer:Name>
					<bodeombrometer:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5229948.558568242,5729148.945125754</gml:coordinates>
						</gml:Point>
					</bodeombrometer:Ort>
					<bodeombrometer:Messstellennummer>579605</bodeombrometer:Messstellennummer>
					<bodeombrometer:Kurz_Name>silb</bodeombrometer:Kurz_Name>
					<bodeombrometer:Niederschlag_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Silberhütte.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
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
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Gebietsniederschlag/Messung_Stationen/Mahndorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
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
			<bodespeicher:SpeicherMember>
				<bodespeicher:Speicher fid="Speicher_KalteBode">
					<bodespeicher:Name>HWR Kalte Bode</bodespeicher:Name>
					<bodespeicher:Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5206105.143908598,5743473.354427688</gml:coordinates>
						</gml:Point>
					</bodespeicher:Ort>
					<bodespeicher:Messstellennummer>579320</bodespeicher:Messstellennummer>
					<bodespeicher:Kurz_Name>kabo</bodespeicher:Kurz_Name>
					<bodespeicher:Abgabe>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicherabgabe/Kalte_Bode.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Abgabe>
					<bodespeicher:Zufluss_gerechnet>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicherzufluss/Kalte_Bode.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Zufluss_gerechnet>
					<bodespeicher:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicher/Messung/Kalte_Bode.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Ganglinie_gemessen>
					<bodespeicher:Ganglinie_gerechnet>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicher/Vorhersage/Kalte_Bode.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Ganglinie_gerechnet>
					<bodespeicher:StauraumParameter>
						<bodespeicher:Stauraum fid="Stauraum_kabo">
							<bodespeicher:Totraum>0.2</bodespeicher:Totraum>
							<bodespeicher:Reserveraum>0.34</bodespeicher:Reserveraum>
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
											<bodecommon:Wert>0.29</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_kabo_Jun">
											<bodecommon:Name>Juni</bodecommon:Name>
											<bodecommon:Nummer>6</bodecommon:Nummer>
											<bodecommon:Wert>0.29</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_kabo_Jul">
											<bodecommon:Name>Juli</bodecommon:Name>
											<bodecommon:Nummer>7</bodecommon:Nummer>
											<bodecommon:Wert>0.29</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_kabo_Aug">
											<bodecommon:Name>August</bodecommon:Name>
											<bodecommon:Nummer>8</bodecommon:Nummer>
											<bodecommon:Wert>0.29</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_kabo_Sep">
											<bodecommon:Name>September</bodecommon:Name>
											<bodecommon:Nummer>9</bodecommon:Nummer>
											<bodecommon:Wert>0.29</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_kabo_Okt">
											<bodecommon:Name>Oktober</bodecommon:Name>
											<bodecommon:Nummer>10</bodecommon:Nummer>
											<bodecommon:Wert>0.29</bodecommon:Wert>
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
							<bodespeicher:Stauraum>4.47</bodespeicher:Stauraum>
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
											<bodecommon:Wert>0.05</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_kabo_Feb">
											<bodecommon:Name>Februar</bodecommon:Name>
											<bodecommon:Nummer>2</bodecommon:Nummer>
											<bodecommon:Wert>0.05</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_kabo_Mrz">
											<bodecommon:Name>März</bodecommon:Name>
											<bodecommon:Nummer>3</bodecommon:Nummer>
											<bodecommon:Wert>0.05</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_kabo_Apr">
											<bodecommon:Name>April</bodecommon:Name>
											<bodecommon:Nummer>4</bodecommon:Nummer>
											<bodecommon:Wert>0.05</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_kabo_Mai">
											<bodecommon:Name>Mai</bodecommon:Name>
											<bodecommon:Nummer>5</bodecommon:Nummer>
											<bodecommon:Wert>0.05</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_kabo_Jun">
											<bodecommon:Name>Juni</bodecommon:Name>
											<bodecommon:Nummer>6</bodecommon:Nummer>
											<bodecommon:Wert>0.05</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_kabo_Jul">
											<bodecommon:Name>Juli</bodecommon:Name>
											<bodecommon:Nummer>7</bodecommon:Nummer>
											<bodecommon:Wert>0.05</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_kabo_Aug">
											<bodecommon:Name>August</bodecommon:Name>
											<bodecommon:Nummer>8</bodecommon:Nummer>
											<bodecommon:Wert>0.05</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_kabo_Sep">
											<bodecommon:Name>September</bodecommon:Name>
											<bodecommon:Nummer>9</bodecommon:Nummer>
											<bodecommon:Wert>0.05</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_kabo_Okt">
											<bodecommon:Name>Oktober</bodecommon:Name>
											<bodecommon:Nummer>10</bodecommon:Nummer>
											<bodecommon:Wert>0.05</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_kabo_Nov">
											<bodecommon:Name>November</bodecommon:Name>
											<bodecommon:Nummer>11</bodecommon:Nummer>
											<bodecommon:Wert>0.05</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_kabo_Dez">
											<bodecommon:Name>Dezember</bodecommon:Name>
											<bodecommon:Nummer>12</bodecommon:Nummer>
											<bodecommon:Wert>0.05</bodecommon:Wert>
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
											<bodecommon:Wert>35.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_kabo_Feb">
											<bodecommon:Name>Februar</bodecommon:Name>
											<bodecommon:Nummer>2</bodecommon:Nummer>
											<bodecommon:Wert>35.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_kabo_Mrz">
											<bodecommon:Name>März</bodecommon:Name>
											<bodecommon:Nummer>3</bodecommon:Nummer>
											<bodecommon:Wert>35.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_kabo_Apr">
											<bodecommon:Name>April</bodecommon:Name>
											<bodecommon:Nummer>4</bodecommon:Nummer>
											<bodecommon:Wert>35.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_kabo_Mai">
											<bodecommon:Name>Mai</bodecommon:Name>
											<bodecommon:Nummer>5</bodecommon:Nummer>
											<bodecommon:Wert>35.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_kabo_Jun">
											<bodecommon:Name>Juni</bodecommon:Name>
											<bodecommon:Nummer>6</bodecommon:Nummer>
											<bodecommon:Wert>35.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_kabo_Jul">
											<bodecommon:Name>Juli</bodecommon:Name>
											<bodecommon:Nummer>7</bodecommon:Nummer>
											<bodecommon:Wert>35.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_kabo_Aug">
											<bodecommon:Name>August</bodecommon:Name>
											<bodecommon:Nummer>8</bodecommon:Nummer>
											<bodecommon:Wert>35.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_kabo_Sep">
											<bodecommon:Name>September</bodecommon:Name>
											<bodecommon:Nummer>9</bodecommon:Nummer>
											<bodecommon:Wert>35.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_kabo_Okt">
											<bodecommon:Name>Oktober</bodecommon:Name>
											<bodecommon:Nummer>10</bodecommon:Nummer>
											<bodecommon:Wert>35.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_kabo_Nov">
											<bodecommon:Name>November</bodecommon:Name>
											<bodecommon:Nummer>11</bodecommon:Nummer>
											<bodecommon:Wert>35.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_kabo_Dez">
											<bodecommon:Name>Dezember</bodecommon:Name>
											<bodecommon:Nummer>12</bodecommon:Nummer>
											<bodecommon:Wert>35.0</bodecommon:Wert>
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
								<bodespeicher:EA fid="EA_kabo_1">
									<bodespeicher:Höhe>446.35</bodespeicher:Höhe>
									<bodespeicher:Inhalt>0.0</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>0.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_kabo_2">
									<bodespeicher:Höhe>451.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>0.2</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>8.36</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_kabo_3">
									<bodespeicher:Höhe>454.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>0.54</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>10.68</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung>Stauziel Winter</bodespeicher:Bemerkung>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_kabo_4">
									<bodespeicher:Höhe>456.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>0.83</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>11.89</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung>Stauziel Sommer</bodespeicher:Bemerkung>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_kabo_5">
									<bodespeicher:Höhe>458.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>1.21</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>13.11</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_kabo_6">
									<bodespeicher:Höhe>460.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>1.73</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>14.32</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_kabo_7">
									<bodespeicher:Höhe>462.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>2.38</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>15.27</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_kabo_8">
									<bodespeicher:Höhe>464.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>3.2</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>16.23</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_kabo_9">
									<bodespeicher:Höhe>466.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>4.2</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>17.18</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_kabo_10">
									<bodespeicher:Höhe>466.5</bodespeicher:Höhe>
									<bodespeicher:Inhalt>4.47</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>17.42</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung>Vollstau</bodespeicher:Bemerkung>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_kabo_11">
									<bodespeicher:Höhe>466.7</bodespeicher:Höhe>
									<bodespeicher:Inhalt>4.56</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>17.52</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>10.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_kabo_12">
									<bodespeicher:Höhe>467.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>4.75</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>17.66</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>42.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_kabo_13">
									<bodespeicher:Höhe>467.4</bodespeicher:Höhe>
									<bodespeicher:Inhalt>4.95</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>17.85</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>112.0</bodespeicher:Überlauf>
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
							<gml:coordinates cs="," decimal="." ts=" ">5216625.661087754,5744001.801473191</gml:coordinates>
						</gml:Point>
					</bodespeicher:Ort>
					<bodespeicher:Messstellennummer>579430</bodespeicher:Messstellennummer>
					<bodespeicher:Kurz_Name>rapp</bodespeicher:Kurz_Name>
					<bodespeicher:Abgabe>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicherabgabe/Rappbode.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Abgabe>
					<bodespeicher:Zufluss_gerechnet>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicherzufluss/Rappbode.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Zufluss_gerechnet>
					<bodespeicher:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicher/Messung/Rappbode.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Ganglinie_gemessen>
					<bodespeicher:Ganglinie_gerechnet>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicher/Vorhersage/Rappbode.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Ganglinie_gerechnet>

					<bodespeicher:StauraumParameter>
						<bodespeicher:Stauraum fid="Stauraum_rapp">
							<bodespeicher:Totraum>0.34</bodespeicher:Totraum>
							<bodespeicher:Reserveraum>3.63</bodespeicher:Reserveraum>
							<bodespeicher:Betriebsraum>
								<bodecommon:MonatCollection fid="Stauraum_rapp_Jahr">
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_rapp_Jan">
											<bodecommon:Name>Januar</bodecommon:Name>
											<bodecommon:Nummer>1</bodecommon:Nummer>
											<bodecommon:Wert>91.03</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_rapp_Feb">
											<bodecommon:Name>Februar</bodecommon:Name>
											<bodecommon:Nummer>2</bodecommon:Nummer>
											<bodecommon:Wert>91.03</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_rapp_Mrz">
											<bodecommon:Name>März</bodecommon:Name>
											<bodecommon:Nummer>3</bodecommon:Nummer>
											<bodecommon:Wert>91.03</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_rapp_Apr">
											<bodecommon:Name>April</bodecommon:Name>
											<bodecommon:Nummer>4</bodecommon:Nummer>
											<bodecommon:Wert>91.03</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_rapp_Mai">
											<bodecommon:Name>Mai</bodecommon:Name>
											<bodecommon:Nummer>5</bodecommon:Nummer>
											<bodecommon:Wert>105.11</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_rapp_Jun">
											<bodecommon:Name>Juni</bodecommon:Name>
											<bodecommon:Nummer>6</bodecommon:Nummer>
											<bodecommon:Wert>105.11</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_rapp_Jul">
											<bodecommon:Name>Juli</bodecommon:Name>
											<bodecommon:Nummer>7</bodecommon:Nummer>
											<bodecommon:Wert>105.11</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_rapp_Aug">
											<bodecommon:Name>August</bodecommon:Name>
											<bodecommon:Nummer>8</bodecommon:Nummer>
											<bodecommon:Wert>105.11</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_rapp_Sep">
											<bodecommon:Name>September</bodecommon:Name>
											<bodecommon:Nummer>9</bodecommon:Nummer>
											<bodecommon:Wert>105.11</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_rapp_Okt">
											<bodecommon:Name>Oktober</bodecommon:Name>
											<bodecommon:Nummer>10</bodecommon:Nummer>
											<bodecommon:Wert>105.11</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_rapp_Nov">
											<bodecommon:Name>November</bodecommon:Name>
											<bodecommon:Nummer>11</bodecommon:Nummer>
											<bodecommon:Wert>91.03</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_rapp_Dez">
											<bodecommon:Name>Dezember</bodecommon:Name>
											<bodecommon:Nummer>12</bodecommon:Nummer>
											<bodecommon:Wert>91.03</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
								</bodecommon:MonatCollection>
							</bodespeicher:Betriebsraum>
							<bodespeicher:Stauraum>109.08</bodespeicher:Stauraum>
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
											<bodecommon:Wert>0.5</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_rapp_Feb">
											<bodecommon:Name>Februar</bodecommon:Name>
											<bodecommon:Nummer>2</bodecommon:Nummer>
											<bodecommon:Wert>0.5</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_rapp_Mrz">
											<bodecommon:Name>März</bodecommon:Name>
											<bodecommon:Nummer>3</bodecommon:Nummer>
											<bodecommon:Wert>0.5</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_rapp_Apr">
											<bodecommon:Name>April</bodecommon:Name>
											<bodecommon:Nummer>4</bodecommon:Nummer>
											<bodecommon:Wert>0.5</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_rapp_Mai">
											<bodecommon:Name>Mai</bodecommon:Name>
											<bodecommon:Nummer>5</bodecommon:Nummer>
											<bodecommon:Wert>0.5</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_rapp_Jun">
											<bodecommon:Name>Juni</bodecommon:Name>
											<bodecommon:Nummer>6</bodecommon:Nummer>
											<bodecommon:Wert>0.5</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_rapp_Jul">
											<bodecommon:Name>Juli</bodecommon:Name>
											<bodecommon:Nummer>7</bodecommon:Nummer>
											<bodecommon:Wert>0.5</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_rapp_Aug">
											<bodecommon:Name>August</bodecommon:Name>
											<bodecommon:Nummer>8</bodecommon:Nummer>
											<bodecommon:Wert>0.5</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_rapp_Sep">
											<bodecommon:Name>September</bodecommon:Name>
											<bodecommon:Nummer>9</bodecommon:Nummer>
											<bodecommon:Wert>0.5</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_rapp_Okt">
											<bodecommon:Name>Oktober</bodecommon:Name>
											<bodecommon:Nummer>10</bodecommon:Nummer>
											<bodecommon:Wert>0.5</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_rapp_Nov">
											<bodecommon:Name>November</bodecommon:Name>
											<bodecommon:Nummer>11</bodecommon:Nummer>
											<bodecommon:Wert>0.5</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_rapp_Dez">
											<bodecommon:Name>Dezember</bodecommon:Name>
											<bodecommon:Nummer>12</bodecommon:Nummer>
											<bodecommon:Wert>0.5</bodecommon:Wert>
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
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_rapp_Feb">
											<bodecommon:Name>Februar</bodecommon:Name>
											<bodecommon:Nummer>2</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_rapp_Mrz">
											<bodecommon:Name>März</bodecommon:Name>
											<bodecommon:Nummer>3</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_rapp_Apr">
											<bodecommon:Name>April</bodecommon:Name>
											<bodecommon:Nummer>4</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_rapp_Mai">
											<bodecommon:Name>Mai</bodecommon:Name>
											<bodecommon:Nummer>5</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_rapp_Jun">
											<bodecommon:Name>Juni</bodecommon:Name>
											<bodecommon:Nummer>6</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_rapp_Jul">
											<bodecommon:Name>Juli</bodecommon:Name>
											<bodecommon:Nummer>7</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_rapp_Aug">
											<bodecommon:Name>August</bodecommon:Name>
											<bodecommon:Nummer>8</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_rapp_Sep">
											<bodecommon:Name>September</bodecommon:Name>
											<bodecommon:Nummer>9</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_rapp_Okt">
											<bodecommon:Name>Oktober</bodecommon:Name>
											<bodecommon:Nummer>10</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_rapp_Nov">
											<bodecommon:Name>November</bodecommon:Name>
											<bodecommon:Nummer>11</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_rapp_Dez">
											<bodecommon:Name>Dezember</bodecommon:Name>
											<bodecommon:Nummer>12</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
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
											<bodecommon:Wert>1.2</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Trinkwasser_rapp_Feb">
											<bodecommon:Name>Februar</bodecommon:Name>
											<bodecommon:Nummer>2</bodecommon:Nummer>
											<bodecommon:Wert>1.2</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Trinkwasser_rapp_Mrz">
											<bodecommon:Name>März</bodecommon:Name>
											<bodecommon:Nummer>3</bodecommon:Nummer>
											<bodecommon:Wert>1.2</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Trinkwasser_rapp_Apr">
											<bodecommon:Name>April</bodecommon:Name>
											<bodecommon:Nummer>4</bodecommon:Nummer>
											<bodecommon:Wert>1.2</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Trinkwasser_rapp_Mai">
											<bodecommon:Name>Mai</bodecommon:Name>
											<bodecommon:Nummer>5</bodecommon:Nummer>
											<bodecommon:Wert>1.2</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Trinkwasser_rapp_Jun">
											<bodecommon:Name>Juni</bodecommon:Name>
											<bodecommon:Nummer>6</bodecommon:Nummer>
											<bodecommon:Wert>1.2</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Trinkwasser_rapp_Jul">
											<bodecommon:Name>Juli</bodecommon:Name>
											<bodecommon:Nummer>7</bodecommon:Nummer>
											<bodecommon:Wert>1.2</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Trinkwasser_rapp_Aug">
											<bodecommon:Name>August</bodecommon:Name>
											<bodecommon:Nummer>8</bodecommon:Nummer>
											<bodecommon:Wert>1.2</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Trinkwasser_rapp_Sep">
											<bodecommon:Name>September</bodecommon:Name>
											<bodecommon:Nummer>9</bodecommon:Nummer>
											<bodecommon:Wert>1.2</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Trinkwasser_rapp_Okt">
											<bodecommon:Name>Oktober</bodecommon:Name>
											<bodecommon:Nummer>10</bodecommon:Nummer>
											<bodecommon:Wert>1.2</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Trinkwasser_rapp_Nov">
											<bodecommon:Name>November</bodecommon:Name>
											<bodecommon:Nummer>11</bodecommon:Nummer>
											<bodecommon:Wert>1.2</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Trinkwasser_rapp_Dez">
											<bodecommon:Name>Dezember</bodecommon:Name>
											<bodecommon:Nummer>12</bodecommon:Nummer>
											<bodecommon:Wert>1.2</bodecommon:Wert>
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
								<bodespeicher:EA fid="EA_rapp_1">
									<bodespeicher:Höhe>332.8</bodespeicher:Höhe>
									<bodespeicher:Inhalt>0.0</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>0.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_rapp_2">
									<bodespeicher:Höhe>345.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>0.335</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>29.8</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung>ZT=tiefstes Absenkziel = ZS TS Wendefurth Sommer</bodespeicher:Bemerkung>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_rapp_3">
									<bodespeicher:Höhe>360.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>2.67</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>44.5</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_rapp_4">
									<bodespeicher:Höhe>364.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>3.97</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>47.7</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_rapp_5">
									<bodespeicher:Höhe>384.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>18.41</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>61.1</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_rapp_6">
									<bodespeicher:Höhe>393.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>30.38</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>66.3</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_rapp_7">
									<bodespeicher:Höhe>408.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>60.34</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>74.1</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_rapp_8">
									<bodespeicher:Höhe>421.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>99.44</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>80.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_rapp_9">
									<bodespeicher:Höhe>421.5</bodespeicher:Höhe>
									<bodespeicher:Inhalt>101.19</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>80.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung>ZS = Stauziel Winter</bodespeicher:Bemerkung>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_rapp_10">
									<bodespeicher:Höhe>423.6</bodespeicher:Höhe>
									<bodespeicher:Inhalt>109.08</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>80.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung>ZS = Stauziel Sommer = Vollstau</bodespeicher:Bemerkung>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_rapp_11">
									<bodespeicher:Höhe>424.1</bodespeicher:Höhe>
									<bodespeicher:Inhalt>111.08</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>80.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>37.2</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_rapp_12">
									<bodespeicher:Höhe>424.68</bodespeicher:Höhe>
									<bodespeicher:Inhalt>113.4</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>80.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>118.1</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung>höchstes Stauziel</bodespeicher:Bemerkung>
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
							<gml:coordinates cs="," decimal="." ts=" ">5218272.876245457,5742160.857450546</gml:coordinates>
						</gml:Point>
					</bodespeicher:Ort>
					<bodespeicher:Messstellennummer>579005</bodespeicher:Messstellennummer>
					<bodespeicher:Kurz_Name>wend</bodespeicher:Kurz_Name>
					<bodespeicher:Abgabe>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicherabgabe/Wendefurth.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Abgabe>
					<bodespeicher:Zufluss_gerechnet>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicherzufluss/Wendefurth.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Zufluss_gerechnet>
					<bodespeicher:Ganglinie_gemessen>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicher/Messung/Wendefurth.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Ganglinie_gemessen>
					<bodespeicher:Ganglinie_gerechnet>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicher/Vorhersage/Wendefurth.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Ganglinie_gerechnet>
					<bodespeicher:StauraumParameter>
						<bodespeicher:Stauraum fid="Stauraum_wend">
							<bodespeicher:Totraum>0.049</bodespeicher:Totraum>
							<bodespeicher:Reserveraum>1.021</bodespeicher:Reserveraum>
							<bodespeicher:Betriebsraum>
								<bodecommon:MonatCollection fid="Stauraum_wend_Jahr">
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_wend_Jan">
											<bodecommon:Name>Januar</bodecommon:Name>
											<bodecommon:Nummer>1</bodecommon:Nummer>
											<bodecommon:Wert>3.83</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_wend_Feb">
											<bodecommon:Name>Februar</bodecommon:Name>
											<bodecommon:Nummer>2</bodecommon:Nummer>
											<bodecommon:Wert>3.83</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_wend_Mrz">
											<bodecommon:Name>März</bodecommon:Name>
											<bodecommon:Nummer>3</bodecommon:Nummer>
											<bodecommon:Wert>3.83</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_wend_Apr">
											<bodecommon:Name>April</bodecommon:Name>
											<bodecommon:Nummer>4</bodecommon:Nummer>
											<bodecommon:Wert>3.83</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_wend_Mai">
											<bodecommon:Name>Mai</bodecommon:Name>
											<bodecommon:Nummer>5</bodecommon:Nummer>
											<bodecommon:Wert>5.08</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_wend_Jun">
											<bodecommon:Name>Juni</bodecommon:Name>
											<bodecommon:Nummer>6</bodecommon:Nummer>
											<bodecommon:Wert>5.08</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_wend_Jul">
											<bodecommon:Name>Juli</bodecommon:Name>
											<bodecommon:Nummer>7</bodecommon:Nummer>
											<bodecommon:Wert>5.08</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_wend_Aug">
											<bodecommon:Name>August</bodecommon:Name>
											<bodecommon:Nummer>8</bodecommon:Nummer>
											<bodecommon:Wert>5.08</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_wend_Sep">
											<bodecommon:Name>September</bodecommon:Name>
											<bodecommon:Nummer>9</bodecommon:Nummer>
											<bodecommon:Wert>5.08</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_wend_Okt">
											<bodecommon:Name>Oktober</bodecommon:Name>
											<bodecommon:Nummer>10</bodecommon:Nummer>
											<bodecommon:Wert>5.08</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_wend_Nov">
											<bodecommon:Name>November</bodecommon:Name>
											<bodecommon:Nummer>11</bodecommon:Nummer>
											<bodecommon:Wert>3.83</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="Stauraum_wend_Dez">
											<bodecommon:Name>Dezember</bodecommon:Name>
											<bodecommon:Nummer>12</bodecommon:Nummer>
											<bodecommon:Wert>3.83</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
								</bodecommon:MonatCollection>
							</bodespeicher:Betriebsraum>
							<bodespeicher:Stauraum>10.335</bodespeicher:Stauraum>
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
											<bodecommon:Wert>2.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_wend_Feb">
											<bodecommon:Name>Februar</bodecommon:Name>
											<bodecommon:Nummer>2</bodecommon:Nummer>
											<bodecommon:Wert>2.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_wend_Mrz">
											<bodecommon:Name>März</bodecommon:Name>
											<bodecommon:Nummer>3</bodecommon:Nummer>
											<bodecommon:Wert>2.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_wend_Apr">
											<bodecommon:Name>April</bodecommon:Name>
											<bodecommon:Nummer>4</bodecommon:Nummer>
											<bodecommon:Wert>2.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_wend_Mai">
											<bodecommon:Name>Mai</bodecommon:Name>
											<bodecommon:Nummer>5</bodecommon:Nummer>
											<bodecommon:Wert>2.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_wend_Jun">
											<bodecommon:Name>Juni</bodecommon:Name>
											<bodecommon:Nummer>6</bodecommon:Nummer>
											<bodecommon:Wert>2.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_wend_Jul">
											<bodecommon:Name>Juli</bodecommon:Name>
											<bodecommon:Nummer>7</bodecommon:Nummer>
											<bodecommon:Wert>2.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_wend_Aug">
											<bodecommon:Name>August</bodecommon:Name>
											<bodecommon:Nummer>8</bodecommon:Nummer>
											<bodecommon:Wert>2.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_wend_Sep">
											<bodecommon:Name>September</bodecommon:Name>
											<bodecommon:Nummer>9</bodecommon:Nummer>
											<bodecommon:Wert>2.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_wend_Okt">
											<bodecommon:Name>Oktober</bodecommon:Name>
											<bodecommon:Nummer>10</bodecommon:Nummer>
											<bodecommon:Wert>2.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_wend_Nov">
											<bodecommon:Name>November</bodecommon:Name>
											<bodecommon:Nummer>11</bodecommon:Nummer>
											<bodecommon:Wert>2.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MinAbgabe_wend_Dez">
											<bodecommon:Name>Dezember</bodecommon:Name>
											<bodecommon:Nummer>12</bodecommon:Nummer>
											<bodecommon:Wert>2.0</bodecommon:Wert>
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
											<bodecommon:Wert>40.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_wend_Feb">
											<bodecommon:Name>Februar</bodecommon:Name>
											<bodecommon:Nummer>2</bodecommon:Nummer>
											<bodecommon:Wert>40.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_wend_Mrz">
											<bodecommon:Name>März</bodecommon:Name>
											<bodecommon:Nummer>3</bodecommon:Nummer>
											<bodecommon:Wert>40.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_wend_Apr">
											<bodecommon:Name>April</bodecommon:Name>
											<bodecommon:Nummer>4</bodecommon:Nummer>
											<bodecommon:Wert>40.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_wend_Mai">
											<bodecommon:Name>Mai</bodecommon:Name>
											<bodecommon:Nummer>5</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_wend_Jun">
											<bodecommon:Name>Juni</bodecommon:Name>
											<bodecommon:Nummer>6</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_wend_Jul">
											<bodecommon:Name>Juli</bodecommon:Name>
											<bodecommon:Nummer>7</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_wend_Aug">
											<bodecommon:Name>August</bodecommon:Name>
											<bodecommon:Nummer>8</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_wend_Sep">
											<bodecommon:Name>September</bodecommon:Name>
											<bodecommon:Nummer>9</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_wend_Okt">
											<bodecommon:Name>Oktober</bodecommon:Name>
											<bodecommon:Nummer>10</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_wend_Nov">
											<bodecommon:Name>November</bodecommon:Name>
											<bodecommon:Nummer>11</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
										</bodecommon:Monat>
									</bodecommon:MonatMember>
									<bodecommon:MonatMember>
										<bodecommon:Monat fid="MaxAbgabe_wend_Dez">
											<bodecommon:Name>Dezember</bodecommon:Name>
											<bodecommon:Nummer>12</bodecommon:Nummer>
											<bodecommon:Wert>25.0</bodecommon:Wert>
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
								<bodespeicher:EA fid="EA_wend_1">
									<bodespeicher:Höhe>322.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>0.0</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>0.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_2">
									<bodespeicher:Höhe>328.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>0.049</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>28.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung>ZT=tiefstes Absenkziel</bodespeicher:Bemerkung>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_3">
									<bodespeicher:Höhe>330.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>0.155</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>34.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_4">
									<bodespeicher:Höhe>332.8</bodespeicher:Höhe>
									<bodespeicher:Inhalt>0.453</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>40.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung>Schieberachse Umlaufstollen TS Rappbode</bodespeicher:Bemerkung>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_5">
									<bodespeicher:Höhe>336.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>1.025</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>46.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_6">
									<bodespeicher:Höhe>338.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>1.554</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>50.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_7">
									<bodespeicher:Höhe>340.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>2.202</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>53.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_8">
									<bodespeicher:Höhe>342.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>2.956</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>56.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_9">
									<bodespeicher:Höhe>344.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>3.848</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>58.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_10">
									<bodespeicher:Höhe>346.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>4.884</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>60.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung>GA begrenzt auf 30 m3/s</bodespeicher:Bemerkung>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_11">
									<bodespeicher:Höhe>348.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>6.038</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>60.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_12">
									<bodespeicher:Höhe>350.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>7.288</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>60.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_13">
									<bodespeicher:Höhe>351.9</bodespeicher:Höhe>
									<bodespeicher:Inhalt>8.535</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>60.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>0.0</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung>Vollstau</bodespeicher:Bemerkung>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_14">
									<bodespeicher:Höhe>352.0</bodespeicher:Höhe>
									<bodespeicher:Inhalt>8.602</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>60.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>3.7</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_15">
									<bodespeicher:Höhe>352.2</bodespeicher:Höhe>
									<bodespeicher:Inhalt>8.737</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>60.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>18.7</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_16">
									<bodespeicher:Höhe>352.4</bodespeicher:Höhe>
									<bodespeicher:Inhalt>8.873</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>60.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>40.4</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung/>
								</bodespeicher:EA>
							</bodespeicher:EAMember>
							<bodespeicher:EAMember>
								<bodespeicher:EA fid="EA_wend_17">
									<bodespeicher:Höhe>352.9</bodespeicher:Höhe>
									<bodespeicher:Inhalt>9.215</bodespeicher:Inhalt>
									<bodespeicher:Grundablass>60.0</bodespeicher:Grundablass>
									<bodespeicher:Überlauf>114.1</bodespeicher:Überlauf>
									<bodespeicher:Bemerkung>ZH=höchstes Stauziel</bodespeicher:Bemerkung>
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
							<gml:coordinates cs="," decimal="." ts=" ">5210386.315687956,5741854.463837401</gml:coordinates>
						</gml:Point>
					</bodespeicher:Ort>
					<bodespeicher:Messstellennummer>579000</bodespeicher:Messstellennummer>
					<bodespeicher:Kurz_Name>urapp</bodespeicher:Kurz_Name>
					<bodespeicher:Abgabe>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicherabgabe/Überleitung_Rappbode.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Abgabe>
					<bodespeicher:Zufluss_gerechnet>
						<obslink:TimeseriesLink linktype="zml" ns1:actuate="onRequest" ns1:href="./Zeitreihen/Speicherzufluss/Überleitung_Rappbode.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org" xmlns:ns1="http://www.w3.org/1999/xlink"/>
					</bodespeicher:Zufluss_gerechnet>
				</bodespeicher:Überleitung>
			</bodespeicher:SpeicherMember>
		</bodespeicher:SpeicherCollection>
	</bodespeicher:SpeicherCollectionAssociation>
</BodeModell>
