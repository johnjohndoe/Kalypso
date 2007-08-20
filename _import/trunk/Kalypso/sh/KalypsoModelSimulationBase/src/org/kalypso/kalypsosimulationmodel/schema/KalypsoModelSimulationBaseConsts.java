package org.kalypso.kalypsosimulationmodel.schema;

import javax.xml.namespace.QName;

import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

public class KalypsoModelSimulationBaseConsts
{
  public static final String STRING_GAUSS_KRUEGER = "EPSG:31467"; //$NON-NLS-1$

  public static final CS_CoordinateSystem CS_GAUSS_KRUEGER = ConvenienceCSFactory.getInstance().getOGCCSByName( STRING_GAUSS_KRUEGER );

  public static final QName SIM_BASE_F_ROUGHNESS_POLYGON = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessPolygon" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_ROUGHNESS_STYLE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessStyle" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessClassMember" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_ROUGHNESS_POLYGON_COLLECTION = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessLayer" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessLayerMember" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_TERRAIN_ELE_SYS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "terrainElevationModelSystem" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_TERRAIN_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "terrainElevationModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_TERRAIN_ELE_SYS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "TerrainElevationModelSystem" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "NativeTerrainElevationModelWrapper" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_SRS_NAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "srsName" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_FILE_NAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "fileName" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_TERRAIN_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "TerrainElevationModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_TERRAIN_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "terrainModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_BASE_TERRAIN_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "_TerrainElevationModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_GRID_COVERAGE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "gridCoverage" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_GRID_COVERAGE_ELE_MODEL_WRAPPER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "GridCoverageElevationModelWrapper" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_SIMPLE_OPERATIONAL_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "SimpleOperationalModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_SIMULATION_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "SimulationModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_OPERATIONAL_CONCEPT = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "operationalConcept" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_POINT_COVERAGE = new QName( UrlCatalogModelSimulationBase.GML_IMITATIONS_NS_PREFIX, "pointCoverage" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_FLOW_RESISTANCE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "FlowResistanceModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_FLOW_RESISTANCE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "flowResistanceModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_FLOW_RELATIONSHIP_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "flowRelationshipModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_FLOW_RESISTANCE_CONCEPT = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "flowResistanceConcept" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_APPLICATION_ZONE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "applicationZone" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_STATIC_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "staticModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_OPERATIONAL_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "operationalModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_CONTROL_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "controlModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_RESULT_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "resultModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_EVALUATION_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "evaluationModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_OVERRIDING_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "overridingModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_ADDITIONAL_MODEL_PROPS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "additionalModelProperties" ); //$NON-NLS-1$

  public static final QName SIM_BASE_P_DICRRETISATION_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "feDiscretisationModel" ); //$NON-NLS-1$
}
