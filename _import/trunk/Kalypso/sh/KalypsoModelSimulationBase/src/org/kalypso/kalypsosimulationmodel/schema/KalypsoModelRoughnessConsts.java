package org.kalypso.kalypsosimulationmodel.schema;

import javax.xml.namespace.QName;


public class KalypsoModelRoughnessConsts
{
	public static final QName WBR_F_ROUGHNESS=
		new QName(
				UrlCatalogRoughness.NS_ROUGHNESS_MODEL,
				"Roughness");
	
	public static final QName WBR_PROP_KS=
		new QName(
				UrlCatalogRoughness.NS_ROUGHNESS_MODEL,
				"ks");
	
	public static final QName WBR_PROP_ROUGHNESS=
		new QName(
				UrlCatalogRoughness.NS_ROUGHNESS_MODEL,
				"ratioAxAy");
	public static final QName WBR_PROP_DP=
		new QName(
				UrlCatalogRoughness.NS_ROUGHNESS_MODEL,
				"dp");
	
	public static final QName WBR_PROP_EDDY=
		new QName(
				UrlCatalogRoughness.NS_ROUGHNESS_MODEL,
				"eddy");
	
	public static final QName WBR_PROP_MARSH=
		new QName(
				UrlCatalogRoughness.NS_ROUGHNESS_MODEL,
				"marsh");
	
}
