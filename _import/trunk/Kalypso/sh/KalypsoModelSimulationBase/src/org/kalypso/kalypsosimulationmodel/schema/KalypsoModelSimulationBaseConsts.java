package org.kalypso.kalypsosimulationmodel.schema;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;




public class KalypsoModelSimulationBaseConsts
{
	public static final QName SIM_BASE_PLYNOMIAL1D=
		new QName(
				UrlCatalogModelSimulationBase.SIM_MODEL_NS,
				"Polynomial1D");
	
	public static final QName SIM_BASE_PROP_ORDER=
			new QName(
					UrlCatalogModelSimulationBase.SIM_MODEL_NS,
					"order");
		
	
	public static final QName SIM_BASE_PROP_COEFFICIENTS=
				new QName(
						UrlCatalogModelSimulationBase.SIM_MODEL_NS,
						"coefficients");
	
	public static final QName SIM_BASE_F_ROUGHNESS_POLYGON=
		new QName(
				UrlCatalogModelSimulationBase.SIM_MODEL_NS,
				"RoughnessPolygon");
	
	public static final QName SIM_BASE_PROP_ROUGHNESS_ID=
		new QName(
				UrlCatalogModelSimulationBase.SIM_MODEL_NS,
				"roughnessID");
	
	
//	public static final QName 
//		SIM_BASE_PROP_ROUGHNESS_POLYGON = 
//				new QName(NS.GML3, "polygonProperty");
	

//	public static final QName 
//		SIM_BASE_PROP_ROUGHNESS_POLYGON = 
//				new QName(NS.GML3, "surfaceProperty");
	
	public static final QName 
		SIM_BASE_PROP_ROUGHNESS_POLYGON = 
			new QName(
					UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
					"polygonProperty");
	
	public static final QName SIM_BASE_PROP_DEGREEX = 
			new QName(
					UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
					"degreeX");
	
	public static final QName SIM_BASE_PROP_DEGREEY = 
		new QName(
				UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
				"degreeY");
	
	public static final QName SIM_BASE_F_POLYNOMIAL2D = 
		new QName(
				UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
				"Polynomial2D");
	
	static
	{
	
	}
}
