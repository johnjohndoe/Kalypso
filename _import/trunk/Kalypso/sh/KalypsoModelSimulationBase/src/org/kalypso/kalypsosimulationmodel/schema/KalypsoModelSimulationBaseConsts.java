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
	
	public static final QName SIM_BASE_ROUGHNESS_POLYGON=
		new QName(
				UrlCatalogModelSimulationBase.SIM_MODEL_NS,
				"RoughnessPolygon");
	
	public static final QName SIM_BASE_PROP_ROUGHNESS_ID=
		new QName(
				UrlCatalogModelSimulationBase.SIM_MODEL_NS,
				"roughnessID");
	
	public static final QName 
		SIM_BASE_PROP_ROUGTHNESS_POLYGON = 
				new QName(NS.GML3, "polygonProperty");
	
	
	static
	{
	
	}
}
