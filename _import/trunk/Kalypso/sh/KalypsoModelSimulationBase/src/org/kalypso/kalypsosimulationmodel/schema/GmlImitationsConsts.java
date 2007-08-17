package org.kalypso.kalypsosimulationmodel.schema;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;


public class GmlImitationsConsts
{
	public static final QName WBGML_F_MULTIPOINT_COVERAGE=
					new QName(
							UrlCatalogGmlImitations.NS_GML_IMITATIONS,
							"PointCoverage"); //$NON-NLS-1$
	
	public static final QName WBGML_PROP_MULTIPOINT_DOMAIN=
		new QName(
				UrlCatalogGmlImitations.NS_GML_IMITATIONS,
				"multiPointDomain"); //$NON-NLS-1$
	
	public static final QName WBGML_F_MULTIPOINT=
		new QName(
				UrlCatalogGmlImitations.NS_GML_IMITATIONS,
				"MultiPoint"); //$NON-NLS-1$
	
	public static final QName WBGML_PROP_RANGESET=
			new QName(
					UrlCatalogGmlImitations.NS_GML_IMITATIONS,
					"featureRangeSet"); //$NON-NLS-1$
	
	public static final QName WBGML_F_FEATURERANGESET=
		new QName(
				UrlCatalogGmlImitations.NS_GML_IMITATIONS,
				"FeatureRangeSet"); //$NON-NLS-1$
	
	public static final QName GML_PROP_RANGESET=
			new QName(
					NS.GML3,
					"rangeSet"); //$NON-NLS-1$

	public static final QName GML_PROP_FEATURE_MEMBER=
		new QName(
				NS.GML3,
				"featureMember"); //$NON-NLS-1$
	public static final QName GML_PROP_POINT_MEMBER=
		new QName(
				NS.GML3,
				"pointMember"); //$NON-NLS-1$
	
}
