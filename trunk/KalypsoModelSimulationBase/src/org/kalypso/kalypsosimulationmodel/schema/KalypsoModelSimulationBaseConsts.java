package org.kalypso.kalypsosimulationmodel.schema;

import javax.xml.namespace.QName;

public class KalypsoModelSimulationBaseConsts
{
  public static final QName SIM_BASE_PROP_VERSION = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "version" ); //$NON-NLS-1$
  
  public static final QName SIM_BASE_F_ROUGHNESS_POLYGON = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessPolygon" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_ROUGHNESS_STYLE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessStyle" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessClassMember" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_ROUGHNESS_POLYGON_COLLECTION = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessLayer" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessLayerMember" ); //$NON-NLS-1$

  //wind
//  public static final QName SIM_BASE_PROP_WIND_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "WindModel" ); //$NON-NLS-1$
  
  public static final QName SIM_BASE_P_WIND_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "windModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_WIND_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "windDataModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_WIND_ELE_SYS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "windDataModelSystem" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_BASE_WIND_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "_WindDataModel" ); //$NON-NLS-1$

  //terrain
  public static final QName SIM_BASE_PROP_TERRAIN_ELE_SYS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "terrainElevationModelSystem" ); //$NON-NLS-1$
  
  public static final QName SIM_BASE_PROP_TERRAIN_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "terrainElevationModel" ); //$NON-NLS-1$
  
  public static final QName SIM_BASE_F_TERRAIN_ELE_SYS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "TerrainElevationModelSystem" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "NativeTerrainElevationModelWrapper" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_TERRAIN_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "TerrainElevationModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_TERRAIN_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "terrainModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_BASE_TERRAIN_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "_TerrainElevationModel" ); //$NON-NLS-1$

  //coverage
  public static final QName SIM_BASE_PROP_GRID_COVERAGE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "gridCoverage" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_GRID_COVERAGE_ELE_MODEL_WRAPPER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "GridCoverageElevationModelWrapper" ); //$NON-NLS-1$

  //others
  public static final QName SIM_BASE_P_FLOW_RELATIONSHIP_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "flowRelationshipModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_DICRRETISATION_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "feDiscretisationModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_FILE_NAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "fileName" ); //$NON-NLS-1$
}
