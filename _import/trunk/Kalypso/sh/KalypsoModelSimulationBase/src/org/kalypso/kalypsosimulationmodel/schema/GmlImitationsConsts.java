package org.kalypso.kalypsosimulationmodel.schema;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;


public class GmlImitationsConsts
{
	public static final QName WBGML_F_MULTIPOINT_COVERAGE=
					new QName(
							UrlCatalogGmlImitations.NS_GML_IMITATIONS,
							"PointCoverage");
	
	public static final QName WBGML_PROP_MULTIPOINT_DOMAIN=
		new QName(
				UrlCatalogGmlImitations.NS_GML_IMITATIONS,
				"multiPointDomain");
	
	public static final QName WBGML_F_MULTIPOINT=
		new QName(
				UrlCatalogGmlImitations.NS_GML_IMITATIONS,
				"MultiPoint");
	
	public static final QName WBGML_PROP_RANGESET=
			new QName(
					UrlCatalogGmlImitations.NS_GML_IMITATIONS,
					"featureRangeSet");
	
	public static final QName WBGML_F_FEATURERANGESET=
		new QName(
				UrlCatalogGmlImitations.NS_GML_IMITATIONS,
				"FeatureRangeSet");
	
	public static final QName GML_PROP_RANGESET=
			new QName(
					NS.GML3,
					"rangeSet");

	public static final QName GML_PROP_FEATURE_MEMBER=
		new QName(
				NS.GML3,
				"featureMember");
	public static final QName GML_PROP_POINT_MEMBER=
		new QName(
				NS.GML3,
				"pointMember");
	
}
