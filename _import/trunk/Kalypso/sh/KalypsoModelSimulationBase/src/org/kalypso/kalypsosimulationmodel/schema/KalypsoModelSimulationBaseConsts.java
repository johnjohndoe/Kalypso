package org.kalypso.kalypsosimulationmodel.schema;

import javax.xml.namespace.QName;

import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

public class KalypsoModelSimulationBaseConsts
{
	public static final String STRING_GAUSS_KRUEGER = "EPSG:31467";
	
	public static final CS_CoordinateSystem CS_GAUSS_KRUEGER = ConvenienceCSFactory.getInstance().getOGCCSByName(STRING_GAUSS_KRUEGER);
	
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
    public static final QName SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER = 
      new QName(
              UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
              "NativeTerrainElevationModelWrapper");
    
    public static final QName SIM_BASE_PROP_FILE_NAME = 
      new QName(
              UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
              "fileName");
    
    public static final QName SIM_BASE_F_TERRAIN_ELE_MODEL = 
      new QName(
              UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
              "TerrainElevationModel");
    public static final QName SIM_BASE_PROP_GRID_COVERAGE = 
      new QName(
              UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
              "gridCoverage");    
    public static final QName SIM_BASE_F_GRID_COVERAGE_ELE_MODEL_WRAPPER = 
      new QName(
              UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
              "GridCoverageElevationModelWrapper");

    public static final QName SIM_BASE_F_SIMPLE_OPERATIONAL_MODEL =
        new QName(
                UrlCatalogModelSimulationBase.SIM_MODEL_NS,
                "SimpleOperationalModel");
    
    public static final QName SIM_BASE_P_OPERATIONAL_CONCEPT =
      new QName(
              UrlCatalogModelSimulationBase.SIM_MODEL_NS,
              "operationalConcept");
  
    public static final QName SIM_BASE_P_POINT_COVERAGE =
      new QName(
              UrlCatalogModelSimulationBase.GML_IMITATIONS_NS_PREFIX,
              "pointCoverage");
  

}
