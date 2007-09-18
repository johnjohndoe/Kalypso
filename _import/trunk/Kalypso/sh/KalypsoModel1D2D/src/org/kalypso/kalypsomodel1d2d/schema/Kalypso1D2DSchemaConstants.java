package org.kalypso.kalypsomodel1d2d.schema;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;

/**
 * @author Patrice Congo
 */
public class Kalypso1D2DSchemaConstants
{

  public final static QName GML_PROP_BOUNDED_BY = new QName( NS.GML3, "boundedBy" );

  public static final QName SIMULATION_MODEL1D2D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "SimulationModelType1D2D" );

  public final static QName WB1D2D_F_NODE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Node" );

  public final static QName WB1D2D_F_MIDDLE_NODE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "MiddleNode" );

  public final static QName WB1D2D_PROP_POINT = new QName( NS.GML3, "pointProperty" );

  public final static QName WB1D2D_F_EDGE_INV = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "EdgeInv" );

  public final static QName WB1D2D_PROP_DIRECTEDNODE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "directedNode" );

  public final static QName WB1D2D_PROP_MIDDLE_NODE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "middleNode" );

  public final static QName WB1D2D_PROP_EDGE_CONTAINERS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edgeContainer" );

  public final static QName WB1D2D_PROP_ELEMENT_CONTAINERS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "elementContainer" );

  public final static QName WB1D2D_PROP_NODE_CONTAINERS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "nodeContainer" );

  public final static QName WB1D2D_F_ELEMENT = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Element" );

  // public final static QName WB1D2D_F_ELEMENT1D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Element1D" );

  // public final static QName WB1D2D_F_BOUNDARY_LINE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "BoundaryLine" );

  public final static QName WB1D2D_F_FE1D2D_2DElement = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Element2D" );

  public final static QName WB1D2D_F_FE1D2DContinuityLine = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "ContinuityLine" );

  public final static QName WB1D2D_F_JUNCTION1D2D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Junction1D2D" );

  public final static QName WB1D2D_F_ABSTRACT_JUNCTION1D2D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "_Junction1D2D" );

  public final static QName WB1D2D_F_JUNTCION_CONTEXT = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "_JunctionContext" );

  public final static QName WB1D2D_F_JUNTCION_CONTEXT_1D_CLINE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "JunctionContext1DToCLine" );

  public final static QName WB1D2D_F_JUNTCION_CONTEXT_1D_2D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "JunctionContext1DTo2D" );

  public final static QName WB1D2D_F_ELEMENT1D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Element1D" );

  public final static QName WB1D2D_F_BOUNDARY_LINE1D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "BoundaryLine1D" );

  public final static QName WB1D2D_PROP_IS_AT_EDGE_END = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "isAtEdgeEnd" );

  public final static QName WB1D2D_F_LINE_ELEMENT = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "LineElement" );

  public final static QName WB1D2D_F_JUNCTION1D2D_EDGE_EDGE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "EdgeToEdgeJunction1D2D" );

  public final static QName WB1D2D_F_JUNCTION1D2D_EDGE_CLINE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "EdgeToClineJunction1D2D" );

  public static final QName WB1D2D_F_POLY_ELEMENT = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "PolyElement" );

  public final static QName WB1D2D_PROP_DIRECTEDEDGE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "directedEdge" );

  public final static QName WB1D2D_PROP_JUNCTION_1DEDGE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edge1D" );

  public final static QName WB1D2D_PROP_JUNCTION_2DEDGE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edge2D" );

  public final static QName WB1D2D_PROP_FE1D2D_MIDDLE_NODE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "middleNode" );

  // public final static QName WB1D2D_F_JUNCTION1D2D_EDGE_EDGE =
  // new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "EdgeToEdgeJunction1D2D" );

  // public final static QName WB1D2D_F_JUNCTION1D2D_EDGE_CLINE =
  // new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "EdgeToClineJunction1D2D" );

  public static final QName WB1D2D_F_COMPLEX_ELE_2D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "ComplexElement2D" );

  public static final QName WB1D2D_F_CALCULATION_UNIT = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "CalculationUnit" );

  public static final QName WB1D2D_F_CALC_UNIT_1D2D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "CalculationUnit1D2D" );

  public static final QName WB1D2D_PROP_CALC_UNIT = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "calculationUnit" );

  public static final QName WB1D2D_F_CALC_UNIT_1D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "CalculationUnit1D" );

  public static final QName WB1D2D_F_CALC_UNIT_2D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "CalculationUnit2D" );

  public static final QName WB1D2D_F_RIVER_CHANNEL1D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "RiverChannel1D" );

  public static final QName WB1D2D_F_RIVER_CHANNEL_2D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "RiverChannel2D" );

  public final static QName WB1D2D_PROP_COMPLEX_ELE_CONTAINER = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "complexElementContainer" );

  public final static QName WB1D2D_PROP_ELE_2D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "element2D" );

  public static final QName WB1D2D_PROP_ELEMENT1D = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "element1D" );

  public static final QName WB1D2D_PROP_CONTINUITY_LINE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "continuityLine" );

  public static final QName WB1D2D_PROP_BOUNDARY_LINE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "boundaryLine" );

  // fe1d2d_2DElement
  // ///////////////////////////////////////////////////////////////
  public final static QName WB1D2D_F_DiscretisationModel = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "DiscretisationModel" );

  public final static QName WB1D2D_F_STATIC_MODEL = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "StaticModel1D2D" );

  /**
   * QName for a property linking a feature to an edge. Use for example in DiscretisationModel feature
   */
  public final static QName WB1D2D_PROP_EDGES = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edge" );

  public final static QName WB1D2D_PROP_EDGE_IN_INV = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edge" );

  public final static QName WB1D2D_PROP_EDGEINV = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edgeInv" );

  /**
   * QName for a property linking a feature to an element. Use for example in DiscretisationModel feature
   */
  public final static QName WB1D2D_PROP_ELEMENTS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "element" );

  public final static QName WB1D2D_PROP_CONTINUITY_LINES = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "continuityLine" );

  /**
   * QName for a property linking a feature to a node. Use for example in DiscretisationModel feature
   */
  public final static QName WB1D2D_PROP_NODES = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "node" );

  /**
   * QName for a property linking a feature to a node. Use for example in DiscretisationModel feature
   */
  public final static QName WB1D2D_PROP_COMPLEX_ELEMENTS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "complexElement" );

  public static final QName WB1D2D_PROP_EDGE_GEOM =
  // new QName( NS.GML3, "curveProperty" );
  new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "geometry" );

  public static final QName WB1D2D_PROP_ELEMENT_GEOM = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "geometry" );

  // Control model
  public final static QName WB1D2DCONTROL_F_MODEL_GROUP = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "ControlModelGroup" );

  public final static QName WB1D2DCONTROL_F_MODEL = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "ControlModel" );

  public final static QName WB1D2DCONTROL_F_MODEL_COLLECTION = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "ControlModelCollection" );

  public final static QName WB1D2DCONTROL_FP_MODEL_COLLECTION = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "controlModelCollection" );

  public final static QName WB1D2DCONTROL_XP_ACTIVE_MODEL = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "activeModelID" );

  public final static QName WB1D2DCONTROL_PROP_VERSION = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "Version" );

  public final static QName WB1D2DCONTROL_PROP_CONTROL_MODEL_MEMBER = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "controlModelMember" );

  public final static QName WB1D2DCONTROL_PROP_IDNOPT = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "IDNOPT" );

  public final static QName WB1D2DCONTROL_PROP_STARTSIM = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "startsim" );

  public final static QName WB1D2DCONTROL_PROP_IEDSW = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "IEDSW" );

  public final static QName WB1D2DCONTROL_PROP_TBFACT = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "TBFACT" );

  public final static QName WB1D2DCONTROL_PROP_TBMIN = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "TBMIN" );

  public final static QName WB1D2DCONTROL_PROP_OMEGA = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "OMEGA" );

  public final static QName WB1D2DCONTROL_PROP_ELEV = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "ELEV" );

  public final static QName WB1D2DCONTROL_PROP_UDIR = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "UDIR" );

  public final static QName WB1D2DCONTROL_PROP_UNOM = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "UNOM" );

  public final static QName WB1D2DCONTROL_PROP_HMIN = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "HMIN" );

  public final static QName WB1D2DCONTROL_PROP_DSET = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "DSET" );

  public final static QName WB1D2DCONTROL_PROP_DSETD = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "DSETD" );

  public final static QName WB1D2DCONTROL_PROP_NITI = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "NITI" );

  public final static QName WB1D2DCONTROL_PROP_NITN = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "NITN" );

  public final static QName WB1D2DCONTROL_PROP_CONV_1 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "CONV_1" );

  public final static QName WB1D2DCONTROL_PROP_CONV_2 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "CONV_2" );

  public final static QName WB1D2DCONTROL_PROP_CONV_3 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "CONV_3" );

  public final static QName WB1D2DCONTROL_PROP_IDRPT = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "IDRPT" );

  public final static QName WB1D2DCONTROL_PROP_DRFACT = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "DRFACT" );

  public final static QName WB1D2DCONTROL_PROP_IACCYC = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "IACCYC" );

  public final static QName WB1D2DCONTROL_PROP_RESTART = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_restart" );

  public final static QName WB1D2DCONTROL_PROP_VEGETA = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "VEGETA" );

  public final static QName WB1D2DCONTROL_PROP_AC1 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "AC1" );

  public final static QName WB1D2DCONTROL_PROP_AC2 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "AC2" );

  public final static QName WB1D2DCONTROL_PROP_AC3 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "AC3" );

  public final static QName WB1D2DCONTROL_PROP_P_BOTTOM = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_p_bottom" );

  public final static QName WB1D2DCONTROL_PROP_RELAXATION_FACTOR = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_steadyBC" );

  public final static QName WB1D2DCONTROL_PROP_STEADY_CHECKBOX = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_steady" );

  public final static QName WB1D2DCONTROL_PROP_UNSTEADY_CHECKBOX = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_unsteady" );

  public final static QName WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "timestepsMember" );

  public final static QName WB1D2DCONTROL_F_TIMESTEPS_OBSERVATION = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "TimestepsObservation" );

  public final static QName SIMMETA_PROP_SIMDESCRIPTOR = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "simulationDescriptor" );

  public final static QName SIMMETA_PROP_MODELDESCRIPTOR = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "modelDescriptor" );

  public final static QName SIMMETA_F_SIMDESCRIPTOR_COLLECTION = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "SimulationDescriptionCollection" );

  public final static QName SIMMETA_F_SIMDESCRIPTOR = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "SimulationDescriptor" );

  public final static QName SIMMETA_F_MODELDESCRIPTOR = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "ModelDescriptor" );

  public final static QName SIMMETA_F_RESULT = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "ResultModelDescriptor" );

  public final static QName SIMMETA_PROP_MODEL_ID = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "modelID" );

  public final static QName SIMMETA_PROP_MODEL_NAME = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "modelName" );

  public final static QName SIMMETA_PROP_MODEL_TYPE = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "modelType" );

  public final static QName SIMMETA_PROP_WORKSPACE_PATH = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "workspacePath" );

  public final static QName SIMMETA_PROP_TIME = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "time" );

  public final static QName SIMMETA_PROP_CONTROL_MODEL = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "controlModel" );

  public final static QName SIMMETA_PROP_SCENARIO_NAME = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "scenarioName" );

  public final static QName SIMMETA_PROP_SIMULATION_TYPE = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "simulationType" );

  public final static QName SIMMETA_PROP_TIN_DEPTH = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "tinDepth" );

  public final static QName SIMMETA_PROP_TIN_VELOCITY = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "tinVelocity" );

  public final static QName SIMMETA_PROP_TIN_WATERLEVEL = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "tinWaterLevel" );

  public final static QName SIMMETA_PROP_GMT = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "gmt" );

  public final static QName SIMMETA_PROP_TIME_STEP_NUM = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "timeStepNum" );

  public final static QName SIMMETA_PROP_START_TIME = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "startTime" );

  public final static QName SIMMETA_PROP_END_TIME = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "endTime" );

  public final static QName SIMMETA_PROP_RESTARTED = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "restarted" );

  public final static QName SIMMETA_PROP_AUTOCONVERGED = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "autoconverged" );

  public final static QName SIMMETA_PROP_CALCULATION_UNIT = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "calculationUnit" );

  public final static QName WB1D2DSIMMETA_PROP_CONTROL_MODEL = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "controlModel" );

  public final static QName SIMMETA_PROP_RESULT = new QName( UrlCatalog1D2D.MODEL_1D2DSIMMETA_NS, "result" );

  public static final QName OP1D2D_F_FLOWRELATIONSHIPS_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "FlowRelationshipModel" );

  public static final QName OP1D2D_F_BOUNDARY_CONDITION = new QName( UrlCatalog1D2D.MODEL_1D2DOperational_NS, "BoundaryCondition" );

  public static final QName OP1D2D_PROP_BOUNDARY_CONDITION = new QName( UrlCatalog1D2D.MODEL_1D2DOperational_NS, "boundaryCondition" );

  public static final QName OP1D2D_PROP_SCOPE_MARK = new QName( UrlCatalog1D2D.MODEL_1D2DOperational_NS, "parentCalculationUnit" );

  public static final QName OP1D2D_PROP_PARENT_CALCUNIT = new QName( UrlCatalog1D2D.MODEL_1D2DOperational_NS, "parentCalculationUnit" );

  public static final QName OP1D2D_PROP_STATIONARY_COND = new QName( UrlCatalog1D2D.MODEL_1D2DOperational_NS, "stationaryCondition" );

  public static final QName RES_1D2D_F_NODE_RES_COLLECTION = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "NodeResultCollection" );

  public static final QName RES_1D2D_F_NODE_RES_MEMBER = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "nodeResultMember" );

}
