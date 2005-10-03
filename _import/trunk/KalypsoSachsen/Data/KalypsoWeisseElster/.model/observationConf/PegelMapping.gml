<?xml version="1.0" encoding="UTF-8"?>
<MappingCollection fid="ID1" xmlns="http://org.kalypso.updateObservationMapping" xmlns:gml="http://www.opengis.net/gml" xmlns:obslink="obslink.zml.kalypso.org" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://org.kalypso.updateObservationMapping ">
<mappingMember>
<!--
	name	PegelName
	point	ORT

	local1: 	Rechenfall/Pegel_gemessen.zml
	local2:		Rechenfall/Pegel_berechnet.zml
	local3:		Rechenfall/Pegel_berechnet_spurM.zml 
	local4:		Rechenfall/Pegel_berechnet_spurU.zml 
	local5:		Rechenfall/Pegel_berechnet_spurO.zml 

	remote1:  	PSI-Pegel-Messung (ECHT)
	remote2:	PSI-Pegel-SpurM (ECHT)
	remote3:	PSI-Pegel-SpurU (ECHT)
	remote4:	PSI-Pegel-SpurO (ECHT)

	remote6:  	PSI-Pegel-Messung (TEST)
	remote7:	PSI-Pegel-SpurM (TEST)
	remote8:	PSI-Pegel-SpurU (TEST)
	remote9:	PSI-Pegel-SpurO (TEST)

	 -->
	<MappingObservation fid="MappingObservation16">
		<gml:name>Goessnitz</gml:name>
		<point>
			<gml:Point srsName="EPSG:31469">
				<gml:coordinates cs="," decimal="." ts=" ">
					5318996.736443012,5642042.125655939</gml:coordinates>
			</gml:Point>
		</point>
		<inObservationLink></inObservationLink>
		<outObservationLink></outObservationLink>
		<local1>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="Pegel/Pegel_Goessnitz.zml" ns1:type="simple" 
				xmlns="obslink.zml.kalypso.org"/>
		</local1>
		<local2>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="Ergebnisse/Berechnet/Pegel_Goessnitz.zml" 
				ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
		</local2>
		<local3>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Goessnitz.zml" 
				ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
		</local3>
		<local4>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Goessnitz.zml" 
				ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
		</local4>
		<local5>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Goessnitz.zml" 
				ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
		</local5>
		<remote1>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577510" 
				ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
		</remote1>
		<remote2>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577510.P3_MW" 
				ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
		</remote2>
		<remote3>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577510.P1_MW" 
				ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
		</remote3>
		<remote4>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577510.P2_MW" 
				ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
		</remote4>
		<remote6>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577510" 
				ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
		</remote6>
		<remote7>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577510.P3_MW" 
				ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
		</remote7>
		<remote8>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577510.P1_MW" 
				ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
		</remote8>
		<remote9>
			<TimeseriesLink linktype="zml" 
				xmlns:ns1="http://www.w3.org/1999/xlink" 
				ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577510.P2_MW" 
				ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
		</remote9>
	</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation15">
<gml:name>Neukirchen</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5314660.711613872,5630604.303229496</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Neukirchen.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Neukirchen.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Neukirchen.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Neukirchen.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Neukirchen.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577501" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577501.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577501.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577501.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577501" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577501.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577501.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577501.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation14">
<gml:name>Oberthau</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5301585.33122749,5698361.239857811</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Oberthau.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Oberthau.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Oberthau.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Oberthau.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Oberthau.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576900" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576900.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576900.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576900.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576900" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576900.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576900.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576900.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation13">
<gml:name>Leipzig-Thekla</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5321657.277031464,5697187.435624962</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Leipzig-Thekla.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Leipzig-Thekla.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Leipzig-Thekla.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Leipzig-Thekla.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Leipzig-Thekla.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...578110" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...578110.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...578110.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...578110.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9578110" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9578110.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9578110.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9578110.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation12">
<gml:name>Albrechtshain</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5330327.536326693,5688327.74399764</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Albrechtshain.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Albrechtshain.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Albrechtshain.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Albrechtshain.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Albrechtshain.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...578090" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...578090.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...578090.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...578090.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9578090" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9578090.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9578090.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9578090.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation11">
<gml:name>Kleindalzig</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5310762.808996597,5678330.279297302</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Kleindalzig.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Kleindalzig.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Kleindalzig.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Kleindalzig.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Kleindalzig.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576631" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576631.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576631.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576631.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576631" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576631.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576631.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576631.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation10">
<gml:name>Zeitz</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5300116.694401994,5661645.295800221</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Zeitz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Zeitz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Zeitz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Zeitz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Zeitz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576610" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576610.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576610.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576610.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576610" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576610.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576610.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576610.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation9">
<gml:name>Gera-Langenberg</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5292345.678021837,5645711.651693126</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Gera-Langenberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Gera-Langenberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Gera-Langenberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Gera-Langenberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Gera-Langenberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576520" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576520.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576520.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576520.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576520" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576520.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576520.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576520.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation8">
<gml:name>Weida</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5292922.275878815,5628931.127386448</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Weida.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Weida.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Weida.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Weida.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Weida.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577320" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577320.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577320.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577320.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577320" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577320.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577320.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577320.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation7">
<gml:name>Greiz</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5302157.834856477,5617143.971582546</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Greiz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Greiz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Greiz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Greiz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Greiz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576470" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576470.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576470.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576470.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576470" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576470.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576470.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576470.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation6">
<gml:name>Mylau</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5306915.350094838,5611737.161947025</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Mylau.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Mylau.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Mylau.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Mylau.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Mylau.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577220" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577220.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577220.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577220.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577220" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577220.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577220.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577220.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation5">
<gml:name>Rodewisch</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5315890.077906612,5603044.48975366</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Rodewisch.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Rodewisch.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Rodewisch.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Rodewisch.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Rodewisch.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577211" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577211.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577211.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...577211.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577211" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577211.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577211.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9577211.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation4">
<gml:name>Elsterberg</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5299414.000177055,5612170.470332178</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Elsterberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Elsterberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Elsterberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Elsterberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Elsterberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576440" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576440.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576440.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576440.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576440" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576440.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576440.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576440.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation3">
<gml:name>Strassberg</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5294168.223977881,5597758.914728787</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Strassberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Strassberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Strassberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Strassberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Strassberg.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576421" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576421.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576421.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576421.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576421" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576421.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576421.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576421.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation2">
<gml:name>Oelsnitz</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5299489.913083247,5588907.157861803</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Oelsnitz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Oelsnitz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Oelsnitz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Oelsnitz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Oelsnitz.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576410" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576410.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576410.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576410.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576410" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576410.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576410.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576410.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation1">
<gml:name>Adorf</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5304896.319194002,5580135.64888929</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_Adorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_Adorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_Adorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_Adorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_Adorf.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576400" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576400.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576400.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576400.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576400" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576400.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576400.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576400.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
<mappingMember>
<MappingObservation fid="MappingObservation0">
<gml:name>Bad Elster</gml:name>
<point>
<gml:Point srsName="EPSG:31469">
<gml:coordinates cs="," decimal="." ts=" ">5303423.467381298,5573911.033847384</gml:coordinates>
</gml:Point>
</point>
<inObservationLink></inObservationLink>
<outObservationLink></outObservationLink>
<local1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Pegel/Pegel_BadElster.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local1>
<local2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Pegel_BadElster.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local2>
<local3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurM_Pegel_BadElster.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local3>
<local4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurU_Pegel_BadElster.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local4>
<local5>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="Ergebnisse/Berechnet/Ablage/SpurO_Pegel_BadElster.zml" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</local5>
<remote1>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576391" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote1>
<remote2>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576391.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote2>
<remote3>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576391.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote3>
<remote4>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://HN.5_WE.02PG...576391.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote4>
<remote6>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576391" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote6>
<remote7>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576391.P3_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote7>
<remote8>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576391.P1_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote8>
<remote9>
<TimeseriesLink linktype="zml" xmlns:ns1="http://www.w3.org/1999/xlink" ns1:href="kalypso-ocs:psicompact://TN.5_WE.02PG...9576391.P2_MW" ns1:type="simple" xmlns="obslink.zml.kalypso.org"/>
</remote9>
</MappingObservation>
</mappingMember>
</MappingCollection>
