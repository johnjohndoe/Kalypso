<?xml version="1.0" encoding="UTF-8"?>
<collection xmlns:na="http://www.tuhh.de/kalypsoSpree" xmlns:gml="http://www.opengis.net/gml" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:obslink="obslink.zml.kalypso.org">
	<gml:featureMember>
		<Zufluss fid="new">
			<Name>Burgneudorf</Name>
			<Ort>
				<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
					<gml:coordinates cs="," decimal="." ts=" ">5457600,5707320</gml:coordinates>
				</gml:Point>
			</Ort>
			<Zugabe_eingang>
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Zufluss" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/zuflüsse/QP_BURGNEU.zml"/>
			</Zugabe_eingang>
			<Zugabe/>
		</Zufluss>
	</gml:featureMember>
	<gml:featureMember>
		<Flutung fid="neu">
			<Name>Abzweig Kleine Spree</Name>
			<Ort/>
			<Abgabe_eingang>
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Flutung" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/flutungen/QV_SPWIESE.zml"/>
			</Abgabe_eingang>
			<Abgabe/>
		</Flutung>
	</gml:featureMember>
	<gml:featureMember>
		<Flutung fid="new">
			<Name>Zuleiter Restloch Bärwalde</Name>
			<Ort/>
			<Abgabe_eingang>
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Flutung" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/flutungen/QV_BWALDE.zml"/>
			</Abgabe_eingang>
			<Abgabe/>
		</Flutung>
	</gml:featureMember>
	<gml:featureMember>
		<Flutung fid="new">
			<Name>Zuleiter Restloch Lohsa II</Name>
			<Ort/>
			<Abgabe_eingang>
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Flutung" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/flutungen/QV_LOHSA.zml"/>
			</Abgabe_eingang>
			<Abgabe/>
		</Flutung>
	</gml:featureMember>
	<gml:featureMember>
		<Flutung fid="new">
			<Name>Überleitung Restlochkette</Name>
			<Ort/>
			<Abgabe_eingang>
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Flutung" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/flutungen/QV_RLKETTE.zml"/>
			</Abgabe_eingang>
			<Abgabe/>
		</Flutung>
	</gml:featureMember>
	<gml:featureMember>
		<Flusslaufmodell fid="new">
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
			<Wasserstand_gemessenEingang/>
			<Wasserstand_gemessen/>
			<Wasserstand_gerechnet/>
		</Flusslaufmodell>
	</gml:featureMember>
	<gml:featureMember>
		<Flusslaufmodell fid="new">
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
			<Wasserstand_gemessenEingang/>
			<Wasserstand_gemessen/>
			<Wasserstand_gerechnet/>
		</Flusslaufmodell>
	</gml:featureMember>
	<gml:featureMember>
		<Flusslaufmodell fid="new">
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
			<Wasserstand_gemessenEingang/>
			<Wasserstand_gemessen/>
			<Wasserstand_gerechnet/>
		</Flusslaufmodell>
	</gml:featureMember>
	<gml:featureMember>
		<Flusslaufmodell fid="new">
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
			<Wasserstand_gemessenEingang/>
			<Wasserstand_gemessen/>
			<Wasserstand_gerechnet/>
		</Flusslaufmodell>
	</gml:featureMember>
	<gml:featureMember>
		<Flusslaufmodell fid="x">
			<Name>Spremberg</Name>
			<Ort/>
			<Korrektur_Faktor>1.0</Korrektur_Faktor>
			<Korrektur_Niveau>0.0</Korrektur_Niveau>
			<Korrektur_Laufzeit>0.0</Korrektur_Laufzeit>
			<KorrekturEmpfehlungLaufzeit>0.0</KorrekturEmpfehlungLaufzeit>
			<Wasserstand_gemessenEingang/>
			<Wasserstand_gemessen/>
			<Wasserstand_gerechnet/>
		</Flusslaufmodell>
	</gml:featureMember>
	<gml:featureMember>
		<Einzugsgebiet fid="new">
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
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Niederschlag" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/niederschlag/PG_SCHIRG.zml"/>
			</Niederschlag_eingabeEingang>
			<Niederschlag_eingabe/>
			<Wasserstand_gemessenEingang>
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Pegel" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/pegel/W_SCHIRG.zml"/>
			</Wasserstand_gemessenEingang>
			<Wasserstand_gemessen/>
			<Wasserstand_gerechnet/>
		</Einzugsgebiet>
	</gml:featureMember>
	<gml:featureMember>
		<Einzugsgebiet fid="new">
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
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Niederschlag" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/niederschlag/PG_BAUTZWB.zml"/>
			</Niederschlag_eingabeEingang>
			<Niederschlag_eingabe/>
			<Wasserstand_gemessenEingang>
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Pegel" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/pegel/W_BAUTZWB.zml"/>
			</Wasserstand_gemessenEingang>
			<Wasserstand_gemessen/>
			<Wasserstand_gerechnet/>
		</Einzugsgebiet>
	</gml:featureMember>
	<gml:featureMember>
		<Einzugsgebiet fid="new">
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
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Niederschlag" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/niederschlag/PG_GROEDI.zml"/>
			</Niederschlag_eingabeEingang>
			<Niederschlag_eingabe/>
			<Wasserstand_gemessenEingang>
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Pegel" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/pegel/W_GROEDI.zml"/>
			</Wasserstand_gemessenEingang>
			<Wasserstand_gemessen/>
			<Wasserstand_gerechnet/>
		</Einzugsgebiet>
	</gml:featureMember>
	<gml:featureMember>
		<Einzugsgebiet fid="new">
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
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Niederschlag" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/niederschlag/PG_JAENKD.zml"/>
			</Niederschlag_eingabeEingang>
			<Niederschlag_eingabe/>
			<Wasserstand_gemessenEingang>
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Pegel" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/pegel/W_JAENKD.zml"/>
			</Wasserstand_gemessenEingang>
			<Wasserstand_gemessen/>
			<Wasserstand_gerechnet/>
		</Einzugsgebiet>
	</gml:featureMember>
	<gml:featureMember>
		<Einzugsgebiet fid="new">
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
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Niederschlag" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/niederschlag/PG_SAERI.zml"/>
			</Niederschlag_eingabeEingang>
			<Niederschlag_eingabe/>
			<Wasserstand_gemessenEingang>
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Pegel" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/pegel/W_SAERI.zml"/>
			</Wasserstand_gemessenEingang>
			<Wasserstand_gemessen/>
			<Wasserstand_gerechnet/>
		</Einzugsgebiet>
	</gml:featureMember>
	<gml:featureMember>
		<Talsperre fid="ID1">
			<Name>Bautzen</Name>
			<Anfangsstauvolumen>0.0</Anfangsstauvolumen>
			<Ort>
				<gml:Point srsName="http://www.opengis.net/gml/srs/EPSG.xml#31469">
					<gml:coordinates cs="," decimal="." ts=" ">5463280,5675800</gml:coordinates>
				</gml:Point>
			</Ort>
			<Abgabe_eingang>
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Abgabe" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/talsperren_abgaben/QV_TSBAUTZ.zml"/>
			</Abgabe_eingang>
			<Abgabe/>
		</Talsperre>
	</gml:featureMember>
	<gml:featureMember>
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
				<obslink:TimeseriesLink linktype="zml" timeaxis="Datum" valueaxis="Abgabe" xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple" xlink:actuate="onRequest" xlink:href="TYPE=relative#LOCATION=zeitreihen/talsperren_abgaben/QV_TSQUITZ.zml"/>
			</Abgabe_eingang>
			<Abgabe/>
		</Talsperre>
	</gml:featureMember>
</collection>
