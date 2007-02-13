package org.kalypso.kalypsosimulationmodel.schema;

import javax.xml.namespace.QName;

import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

public class KalypsoModelSimulationBaseConsts
{
	public static final String STRING_GAUSS_KRUEGER = "EPSG:31467";
	
	public static final CS_CoordinateSystem CS_GAUSS_KRUEGER = ConvenienceCSFactory.getInstance().getOGCCSByName(STRING_GAUSS_KRUEGER);
	
	public static final QName SIM_BASE_PLYNOMIAL1D=
		new QName(
				UrlCatalogModelSimulationBase.SIM_MODEL_NS,
				"Polynomial1D");
	
	public static final QName SIM_BASE_PROP_POLYNOMIAL1D = 
		new QName(
				UrlCatalogModelSimulationBase.SIM_MODEL_NS,
		"polynomial1D");

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
	
	public static final QName SIM_BASE_PROP_ROUGHNESS_STYLE=
		new QName(
				UrlCatalogModelSimulationBase.SIM_MODEL_NS,
				"roughnessStyle");
	
    public static final QName SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER =
        new QName(
                UrlCatalogModelSimulationBase.SIM_MODEL_NS,
                "roughnessClassMember");
    
	public static final QName SIM_BASE_F_ROUGHNESS_POLYGON_COLLECTION=
		new QName(
			UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
			"RoughnessLayerPolygonCollection");
	
	public static final QName SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON=
		new QName(
				UrlCatalogModelSimulationBase.SIM_MODEL_NS,
				"roughnessLayerMember");
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
    
    public static final QName SIM_BASE_PROP_TERRAIN_ELE_SYS = 
        new QName(
                UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
                "terrainElevationModelSystem");
    
    public static final QName SIM_BASE_PROP_TERRAIN_ELE_MODEL = 
      new QName(
              UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
              "terrainElevationModel");
    
    public static final QName SIM_BASE_F_TERRAIN_ELE_SYS = 
      new QName(
              UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
              "TerrainElevationModelSystem");
    
    public static final QName SIM_BASE_F_TERRAIN_ELE_MODEL = 
      new QName(
              UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
              "TerrainElevationModel");
}
