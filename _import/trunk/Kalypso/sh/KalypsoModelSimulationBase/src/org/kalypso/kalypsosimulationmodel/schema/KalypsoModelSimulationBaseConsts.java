package org.kalypso.kalypsosimulationmodel.schema;

import javax.xml.namespace.QName;


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
}
