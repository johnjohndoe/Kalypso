<?xml version="1.0" encoding="UTF-8"?>
<!-- edited with XMLSPY v5 rel. 4 U (http://www.xmlspy.com) by Andersen (no) -->
<SpreeModell
   xmlns="org.kalypso.spree.modell" 
   xmlns:gml="http://www.opengis.net/gml" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:obslink="obslink.zml.kalypso.org" 
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="org.kalypso.spree.modell ./.model/schema/modell.xsd">
	<EZCollectionAssociation>
		<EZCollection>
			<EZMember>
				<Einzugsgebiet>
					<Name>Schirgiswalde</Name>
					<Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5460570,5660600</gml:coordinates>
						</gml:Point>
					</Ort>
					<BodenfeuchteMin>0.01</BodenfeuchteMin>
					<Bodenfeuchte>0.0</Bodenfeuchte>
					<BodenfeuchteMax>70.0</BodenfeuchteMax>
					<Ausdehnung/>
					<Niederschlag_eingabeEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/PG_SCHIRG.zml"/>
					</Niederschlag_eingabeEingang>
					<Niederschlag_eingabe>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/PG_SCHIRG.zml"/>
					</Niederschlag_eingabe>
					<Wasserstand_gemessenEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/W_SCHIRG.zml"/>
					</Wasserstand_gemessenEingang>
					<Wasserstand_gemessen>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/W_SCHIRG.zml"/>
					</Wasserstand_gemessen>
					<Wasserstand_gerechnet>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./ergebnisse/zeitreihen/WV_SCHIRG.zml"/>
					</Wasserstand_gerechnet>
				</Einzugsgebiet>
			</EZMember>
			<EZMember>
				<Einzugsgebiet>
					<Name>Bautzen WB</Name>
					<Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5458610,5670080</gml:coordinates>
						</gml:Point>
					</Ort>
					<BodenfeuchteMin>0.01</BodenfeuchteMin>
					<Bodenfeuchte>0.0</Bodenfeuchte>
					<BodenfeuchteMax>130.0</BodenfeuchteMax>
					<Ausdehnung/>
					
					<Niederschlag_eingabeEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/PG_BAUTZWB.zml"/>
					</Niederschlag_eingabeEingang>
					<Niederschlag_eingabe>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/PG_BAUTZWB.zml"/>
					</Niederschlag_eingabe>
					<Wasserstand_gemessenEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/W_BAUTZWB.zml"/>
					</Wasserstand_gemessenEingang>
					<Wasserstand_gemessen>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/W_BAUTZWB.zml"/>
					</Wasserstand_gemessen>
					<Wasserstand_gerechnet/>
					<Wasserstand_gerechnet>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./ergebnisse/zeitreihen/WV_BAUTZWB.zml"/>
					</Wasserstand_gerechnet>
				</Einzugsgebiet>
			</EZMember>
			<EZMember>
				<Einzugsgebiet>
					<Name>Gröditz 1</Name>
					<Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5473800,5674170</gml:coordinates>
						</gml:Point>
					</Ort>
					<BodenfeuchteMin>0.01</BodenfeuchteMin>
					<Bodenfeuchte>0.0</Bodenfeuchte>
					<BodenfeuchteMax>70.0</BodenfeuchteMax>
					<Ausdehnung/>
					
					<Niederschlag_eingabeEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/PG_GROEDI.zml"/>
					</Niederschlag_eingabeEingang>
					<Niederschlag_eingabe>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/PG_GROEDI.zml"/>
					</Niederschlag_eingabe>
					<Wasserstand_gemessenEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/W_GROEDI.zml"/>
					</Wasserstand_gemessenEingang>
					<Wasserstand_gemessen>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/W_GROEDI.zml"/>
					</Wasserstand_gemessen>
					<Wasserstand_gerechnet>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./ergebnisse/zeitreihen//WV_GROEDI.zml"/>
					</Wasserstand_gerechnet>
				</Einzugsgebiet>
			</EZMember>
			<EZMember>
				<Einzugsgebiet>
					<Name>Jänkendorf</Name>
					<Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5487000,5679730</gml:coordinates>
						</gml:Point>
					</Ort>
					<BodenfeuchteMin>0.01</BodenfeuchteMin>
					<Bodenfeuchte>0.0</Bodenfeuchte>
					<BodenfeuchteMax>200.0</BodenfeuchteMax>
					<Ausdehnung/>

					<Niederschlag_eingabeEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/PG_JAENKD.zml"/>
					</Niederschlag_eingabeEingang>
					<Niederschlag_eingabe>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/PG_JAENKD.zml"/>
					</Niederschlag_eingabe>
					<Wasserstand_gemessenEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/W_JAENKD.zml"/>
					</Wasserstand_gemessenEingang>
					<Wasserstand_gemessen>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/W_JAENKD.zml"/>
					</Wasserstand_gemessen>
					<Wasserstand_gerechnet>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./ergebnisse/zeitreihen//WV_JAENKD.zml"/>
					</Wasserstand_gerechnet>
				</Einzugsgebiet>
			</EZMember>
			<EZMember>
				<Einzugsgebiet>
					<Name>Särichen</Name>
					<Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5492110,5681830</gml:coordinates>
						</gml:Point>
					</Ort>
					<BodenfeuchteMin>0.01</BodenfeuchteMin>
					<Bodenfeuchte>0.0</Bodenfeuchte>
					<BodenfeuchteMax>310.0</BodenfeuchteMax>
					<Ausdehnung/>
					
					<Niederschlag_eingabeEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/PG_SAERI.zml"/>
					</Niederschlag_eingabeEingang>
					<Niederschlag_eingabe>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/PG_SAERI.zml"/>
					</Niederschlag_eingabe>
					<Wasserstand_gemessenEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/W_SAERI.zml"/>
					</Wasserstand_gemessenEingang>
					<Wasserstand_gemessen>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/W_SAERI.zml"/>
					</Wasserstand_gemessen>
					<Wasserstand_gerechnet>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./ergebnisse/zeitreihen//WV_SAERI.zml"/>
					</Wasserstand_gerechnet>
				</Einzugsgebiet>
			</EZMember>
		</EZCollection>
	</EZCollectionAssociation>
	
	<FlusslaufModellCollectionAssociation>
		<FlusslaufModellCollection>
			<FlusslaufModellMember>
				<FlusslaufModell fid="new">
					<Name>Lieske</Name>
					<Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5467400,5688520</gml:coordinates>
						</gml:Point>
					</Ort>
					<Korrektur_Faktor>1.0</Korrektur_Faktor>
					<Korrektur_Niveau>0.0</Korrektur_Niveau>
					<Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
					<KorrekturEmpfehlungLaufzeit>0.0</KorrekturEmpfehlungLaufzeit>

					<Wasserstand_gemessenEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/W_LIESKE.zml"/>
					</Wasserstand_gemessenEingang>
					<Wasserstand_gemessen>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/W_LIESKE.zml"/>
					</Wasserstand_gemessen>
					<Wasserstand_gerechnet>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./ergebnisse/zeitreihen//WV_LIESKE.zml"/>
					</Wasserstand_gerechnet>
				</FlusslaufModell>
			</FlusslaufModellMember>
			
			<FlusslaufModellMember>
				<FlusslaufModell>
					<Name>Boxberg</Name>
					<Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5470540,5696840</gml:coordinates>
						</gml:Point>
					</Ort>
					<Korrektur_Faktor>1.0</Korrektur_Faktor>
					<Korrektur_Niveau>0.0</Korrektur_Niveau>
					<Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
					<KorrekturEmpfehlungLaufzeit>-1.0</KorrekturEmpfehlungLaufzeit>

					<Wasserstand_gemessenEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/W_BOXBRG.zml"/>
					</Wasserstand_gemessenEingang>
					<Wasserstand_gemessen>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/W_BOXBRG.zml"/>
					</Wasserstand_gemessen>
					<Wasserstand_gerechnet>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./ergebnisse/zeitreihen//WV_BOXBRG.zml"/>
					</Wasserstand_gerechnet>
				</FlusslaufModell>
			</FlusslaufModellMember>
			<FlusslaufModellMember>
				<FlusslaufModell>
					<Name>Sprey</Name>
					<Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5466760,5699710</gml:coordinates>
						</gml:Point>
					</Ort>
					<Korrektur_Faktor>1.0</Korrektur_Faktor>
					<Korrektur_Niveau>0.0</Korrektur_Niveau>
					<Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
					<KorrekturEmpfehlungLaufzeit>0.0</KorrekturEmpfehlungLaufzeit>

					<Wasserstand_gemessenEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/W_SPREY.zml"/>
					</Wasserstand_gemessenEingang>
					<Wasserstand_gemessen>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/W_SPREY.zml"/>
					</Wasserstand_gemessen>
					<Wasserstand_gerechnet>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./ergebnisse/zeitreihen//WV_SPREY.zml"/>
					</Wasserstand_gerechnet>
				</FlusslaufModell>
			</FlusslaufModellMember>
			<FlusslaufModellMember>
				<FlusslaufModell>
					<Name>Spreewitz</Name>
					<Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5458780,5708640</gml:coordinates>
						</gml:Point>
					</Ort>
					<Korrektur_Faktor>1.0</Korrektur_Faktor>
					<Korrektur_Niveau>0.0</Korrektur_Niveau>
					<Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
					<KorrekturEmpfehlungLaufzeit>0.0</KorrekturEmpfehlungLaufzeit>

					<Wasserstand_gemessenEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/W_SPWITZ.zml"/>
					</Wasserstand_gemessenEingang>
					<Wasserstand_gemessen>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/W_SPWITZ.zml"/>
					</Wasserstand_gemessen>
					<Wasserstand_gerechnet>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./ergebnisse/zeitreihen//WV_SPWITZ.zml"/>
					</Wasserstand_gerechnet>
				</FlusslaufModell>
			</FlusslaufModellMember>
			<FlusslaufModellMember>
				<FlusslaufModell fid="x">
					<Name>Spremberg</Name>
					<Ort/>
					<Korrektur_Faktor>1.0</Korrektur_Faktor>
					<Korrektur_Niveau>0.0</Korrektur_Niveau>
					<Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
					<KorrekturEmpfehlungLaufzeit>0.0</KorrekturEmpfehlungLaufzeit>

					<Wasserstand_gemessenEingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/W_SPREMB.zml"/>
					</Wasserstand_gemessenEingang>
					<Wasserstand_gemessen>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/W_SPREMB.zml"/>
					</Wasserstand_gemessen>
					<Wasserstand_gerechnet>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./ergebnisse/zeitreihen//WV_SPREMB.zml"/>
					</Wasserstand_gerechnet>
				</FlusslaufModell>
			</FlusslaufModellMember>
		</FlusslaufModellCollection>
	</FlusslaufModellCollectionAssociation>
	
	<TalsperreCollectionAssociation>
		<TalsperreCollection>
			<TalsperreMember>
				<Talsperre fid="ID2">
					<Name>Quitzdorf</Name>
					<Anfangsstauvolumen>0.0</Anfangsstauvolumen>
					<Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<!-- Koordinaten Pegel Sproitz -->
							<gml:coordinates cs="," decimal="." ts=" ">5482460,5683900</gml:coordinates>
						</gml:Point>
					</Ort>
					<Abgabe_eingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/QV_TSQUITZ.zml"/>
					</Abgabe_eingang>
					<Abgabe>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/QV_TSQUITZ.zml"/>
					</Abgabe>
					<Stauinhalt>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/V_TSQUITZ.zml"/>
					</Stauinhalt>
				</Talsperre>
			</TalsperreMember>
			<TalsperreMember>
				<Talsperre fid="ID1">
					<Name>Bautzen</Name>
					<Anfangsstauvolumen>0.0</Anfangsstauvolumen>
					<Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5463280,5675800</gml:coordinates>
						</gml:Point>
					</Ort>
					<Abgabe_eingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/QV_TSBAUTZ.zml"/>
					</Abgabe_eingang>
					<Abgabe>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/QV_TSBAUTZ.zml"/>
					</Abgabe>
					<Stauinhalt>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/V_TSBAUTZ.zml"/>
					</Stauinhalt>
				</Talsperre>
			</TalsperreMember>
		</TalsperreCollection>
	</TalsperreCollectionAssociation>
	<FlutungCollectionAssociation>
		<FlutungCollection>
			<FlutungMember>
				<Flutung>
					<Name>Überleitung Restlochkette</Name>
					<Ort/>
					<Abgabe_eingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/QV_RLKETTE.zml"/>
					</Abgabe_eingang>
					<Abgabe>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/QV_RLKETTE.zml"/>
					</Abgabe>
				</Flutung>
			</FlutungMember>
			<FlutungMember>
				<Flutung>
					<Name>Zuleiter Restloch Lohsa II</Name>
					<Ort/>
					<Abgabe_eingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/QV_LOHSA.zml"/>
					</Abgabe_eingang>
					<Abgabe>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/QV_LOHSA.zml"/>
					</Abgabe>
				</Flutung>
			</FlutungMember>
			<FlutungMember>
				<Flutung>
					<Name>Zuleiter Restloch Bärwalde</Name>
					<Ort/>
					<Abgabe_eingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/QV_BWALDE.zml"/>
					</Abgabe_eingang>
					<Abgabe>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/QV_BWALDE.zml"/>
					</Abgabe>
				</Flutung>
			</FlutungMember>
			<FlutungMember>
				<Flutung fid="neu">
					<Name>Abzweig Kleine Spree</Name>
					<Ort/>
					<Abgabe_eingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/QV_SPWIESE.zml"/>
					</Abgabe_eingang>
					<Abgabe>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/QV_SPWIESE.zml"/>
					</Abgabe>
				</Flutung>
			</FlutungMember>
		</FlutungCollection>
	</FlutungCollectionAssociation>
	<ZuflussCollectionAssociation>
		<ZuflussCollection>
			<ZuflussMember>
				<Zufluss>
					<Name>Burgneudorf</Name>
					<Ort>
						<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
							<gml:coordinates cs="," decimal="." ts=" ">5457600,5707320</gml:coordinates>
						</gml:Point>
					</Ort>
					<Zugabe_eingang>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="kalypso-ocs:Spree://2004/QP_BURGNEU.zml"/>
					</Zugabe_eingang>
					<Zugabe>
						<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Wert" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="./zeitreihen/QP_BURGNEU.zml"/>
					</Zugabe>
				</Zufluss>
			</ZuflussMember>
		</ZuflussCollection>
	</ZuflussCollectionAssociation>
</SpreeModell>
