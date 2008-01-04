/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.sobek.core.interfaces;

import javax.xml.namespace.QName;

/**
 * @author kuch
 */
public class ISobekConstants
{
  public static final String F_HYDRAULIC_BRANCH_LENGTH = "length";

  public static final String F_HYDRAULIC_BRANCH_LOWER_CONNECTION_NODE = "lowerConnectionNode";

  public static final String F_HYDRAULIC_BRANCH_MEMBER = "branchMember";

  public static final String F_HYDRAULIC_BRANCH_RIVER_LINE = "riverLine";

  public static final String F_HYDRAULIC_BRANCH_UPPER_CONNECTION_NODE = "upperConnectionNode";

  public static final String F_HYDRAULIC_CONNECTION_NODE = "ConnectionNode";

  public static final String F_HYDRAULIC_CROSS_SECTION_NODE = "CrossSectionNode";

  public static final String F_HYDRAULIC_CROSS_SECTION_NODE_LINKED_PROFILE = "linkedProfile";

  public static final String F_HYDRAULIC_DESCRIPTION = "description";

  public static final String F_HYDRAULIC_LINKAGE_NODE = "LinkageNode";

  public static final String F_HYDRAULIC_NAME = "name";

  public static final String F_HYDRAULIC_NODE_LINKED_BRANCH = "linkedBranch";

  public static final String F_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES = "linkedInflowBranches";

  public static final String F_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES = "linkedOutflowBranches";

  public static final String F_HYDRAULIC_NODE_LOCATION = "location";

  public static final String F_HYDRAULIC_NODE_MEMBER = "nodeMember";

  public static final String F_HYDRAULIC_SOBEK_BRANCH = "Branch";

  public static final String F_HYDRAULIC_UNIQUE_ID = "uniqueID";

  public static final String NS_SOBEK = "org.kalypso.model.wspm.sobek";

  public static final String NS_SOBEK_PROJECT = "org.kalypso.model.wspm.sobek.project";

  public static final QName QN_HYDRAULIC_BRANCH_LENGTH = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BRANCH_LENGTH );

  public static final QName QN_HYDRAULIC_BRANCH_LOWER_CONNECTION_NODE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BRANCH_LOWER_CONNECTION_NODE );

  public static final QName QN_HYDRAULIC_BRANCH_MEMBER = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BRANCH_MEMBER );

  public static final QName QN_HYDRAULIC_BRANCH_RIVER_LINE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BRANCH_RIVER_LINE );

  public static final QName QN_HYDRAULIC_BRANCH_UPPER_CONNECTION_NODE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BRANCH_UPPER_CONNECTION_NODE );

  public static final QName QN_HYDRAULIC_CONNECTION_NODE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_CONNECTION_NODE );

  public static final QName QN_HYDRAULIC_CROSS_SECTION_NODE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_CROSS_SECTION_NODE );

  public static final QName QN_HYDRAULIC_CROSS_SECTION_NODE_LINKED_PROFILE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_CROSS_SECTION_NODE_LINKED_PROFILE );

  public static final QName QN_HYDRAULIC_DESCRIPTION = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_DESCRIPTION );

  public static final QName QN_HYDRAULIC_LINKAGE_NODE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_LINKAGE_NODE );

  public static final QName QN_HYDRAULIC_NAME = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NAME );

  public static final QName QN_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES );

  public static final QName QN_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES );

  public static final QName QN_HYDRAULIC_NODE_LOCATION = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NODE_LOCATION );

  public static final QName QN_HYDRAULIC_NODE_MEMBER = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NODE_MEMBER );

  public static final QName QN_HYDRAULIC_SOBEK_BRANCH = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_SOBEK_BRANCH );

  public static final QName QN_HYDRAULIC_UNIQUE_ID = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_UNIQUE_ID );

  public static final QName QN_HYDRAULICNODE_LINKED_BRANCH = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NODE_LINKED_BRANCH );

  public static final String F_SOBEK_MODEL = "SobekModel";

  public static final QName QN_SOBEK_MODEL = new QName( ISobekConstants.NS_SOBEK_PROJECT, ISobekConstants.F_SOBEK_MODEL );

  public static final String F_SOBEK_MODEL_MEMBER = "sobekModelMember";

  public static final QName QN_SOBEK_MODEL_MEMBER = new QName( ISobekConstants.NS_SOBEK_PROJECT, ISobekConstants.F_SOBEK_MODEL_MEMBER );

  public static final String F_LN_LINKS_TO_BRANCH = "linksToBranch";

  public static final QName QN_LN_LINKS_TO_BRANCH = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_LN_LINKS_TO_BRANCH );

  public static final String NS_NOFPD_1D_MODEL = "org.kalypso.nofdpidss.1dmodel";

  public static final String F_NOFDP_POLDER_NODE = "PolderNode";

  public static final QName QN_NOFDP_POLDER_NODE = new QName( ISobekConstants.NS_NOFPD_1D_MODEL, ISobekConstants.F_NOFDP_POLDER_NODE );

  public static final String F_NOFDP_RETARDIN_BASIN_NODE = "RetardingBasinNode";

  public static final QName QN_NOFDP_RETARDIN_BASIN_NODE = new QName( ISobekConstants.NS_NOFPD_1D_MODEL, ISobekConstants.F_NOFDP_RETARDIN_BASIN_NODE );

  public static final String F_NOFDP_WEIR_NODE = "WeirNode";

  public static final QName QN_NOFDP_WEIR_NODE = new QName( ISobekConstants.NS_NOFPD_1D_MODEL, ISobekConstants.F_NOFDP_WEIR_NODE );

  public static final String NS_WSPM_PROFILE = "org.kalypso.model.wspmprofile";

  public static final String F_NOFDP_HYDRAULIC_PROFILE_MEMBER = "profileMember";

  public static final QName QN_NOFDP_HYDRAULIC_PROFILE_MEMBER = new QName( ISobekConstants.NS_WSPM_PROFILE, ISobekConstants.F_NOFDP_HYDRAULIC_PROFILE_MEMBER );

  public static final String F_NOFDP_HYDRAULIC_PROFILE = "Profile";

  public static final QName QN_NOFDP_HYDRAULIC_PROFILE = new QName( ISobekConstants.NS_WSPM_PROFILE, ISobekConstants.F_NOFDP_HYDRAULIC_PROFILE );

  public static final String F_HYDRAULIC_BOUNDARY_NODE = "BoundaryConditionNode";

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BOUNDARY_NODE );

  public static final String F_HYDRAULIC_LASTFALL_MEMBER = "lastfallMember";

  public static final QName QN_HYDRAULIC_LASTFALL_MEMBER = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_LASTFALL_MEMBER );

  public static final String F_HYDRAULIC_LASTFALL = "Lastfall";

  public static final QName QN_HYDRAULIC_LASTFALL = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_LASTFALL );

  public static final String F_HYDRAULIC_BOUNDARY_NODE_TYPE = "bcType";

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_TYPE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BOUNDARY_NODE_TYPE );

  public static final String F_LASTFALL_SIMULATION_BEGIN = "simulationBegin";

  public static final QName QN_LASTFALL_SIMULATION_BEGIN = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_LASTFALL_SIMULATION_BEGIN );

  public static final String F_LASTFALL_SIMULATION_END = "simulationEnd";

  public static final QName QN_LASTFALL_SIMULATION_END = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_LASTFALL_SIMULATION_END );

  public static final String F_LASTFALL_SIMULATION_PRE_TIME = "preSimulationTime";

  public static final QName QN_LASTFALL_SIMULATION_PRE_TIME = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_LASTFALL_SIMULATION_PRE_TIME );

  public static final String F_LASTFALL_SIMULATION_TIMESTEP = "simulationTimestep";

  public static final QName QN_LASTFALL_SIMULATION_TIMESTEP = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_LASTFALL_SIMULATION_TIMESTEP );

  public static final String F_LASTFALL_SIMULATION_TIMESTEP_MULTIPLIER = "resultTimeStepAsMultiple";

  public static final QName QN_LASTFALL_SIMULATION_TIMESTEP_MULTIPLIER = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_LASTFALL_SIMULATION_TIMESTEP_MULTIPLIER );

  public static final String F_HYDRAULIC_BOUNDARY_NODE_LASTFALL_DEFINITION_MEMBER = "lastfallDefinitionMember";

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_LASTFALL_DEFINITION_MEMBER = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BOUNDARY_NODE_LASTFALL_DEFINITION_MEMBER );

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_LINKED_LASTFALL = "linkedLastfall";

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LINKED_LASTFALL = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_LINKED_LASTFALL );

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_BEGINS = "observationBegin";

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_BEGINS = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_BEGINS );

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_ENDS = "observationEnd";

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_ENDS = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_ENDS );

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_TYPE = "typeOfLastBoundaryCondition";

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_TYPE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_TYPE );

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_LNK_TIME_SERIES = "sourceRepositoryObservation";

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LNK_TIME_SERIES = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_LNK_TIME_SERIES );

  public static final String P_HYDRAULIC_NODE_STATION_NAME = "stationName";

  public static final QName QN_HYDRAULIC_NODE_STATION_NAME = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_NODE_STATION_NAME );

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_OBSERVATION = "data";

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_OBSERVATION = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_OBSERVATION );

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE = "constValue";

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE );

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE_INTERVALL = "constValueIntervalMinutes";

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE_INTERVALL = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE_INTERVALL );

  public static final String NS_SOBEK_STRUCTURES = "org.kalypso.model.wspm.sobek.structures";

  public static final String P_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE = "SbkCompoundStructure";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE );

  public static final String F_HYDRAULIC_NODE_CONNECTION_TYPE = "typeOfConnectionNode";

  public static final QName QN_HYDRAULIC_NODE_CONNECTION_TYPE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NODE_CONNECTION_TYPE );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_WEIR = "SbkWeir";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_WEIR = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_WEIR );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_HEIGHT = "crestHeight";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_HEIGHT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_HEIGHT );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_WIDTH = "crestWidth";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_WIDTH = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_WIDTH );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_WEIR_DISCHARGE_COEFF = "dischargeCoeffCE";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_WEIR_DISCHARGE_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_WEIR_DISCHARGE_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_WEIR_FLOW_DIRECTION = "flowDirection";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_WEIR_FLOW_DIRECTION = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_WEIR_FLOW_DIRECTION );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_WEIR_LATERAL_CONTRACTION_COEFF = "lateralContractionCoeffCW";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_WEIR_LATERAL_CONTRACTION_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_WEIR_LATERAL_CONTRACTION_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR = "SbkRiverWeir";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE = "SbkGeneralStructure";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE = "SbkDatabaseStructure";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP = "SbkPump";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_FLOW_DIRECTION = "flowDirection";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_FLOW_DIRECTION = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_FLOW_DIRECTION );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_PUMP_CONTROL = "pumpControl";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_PUMP_CONTROL = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_PUMP_CONTROL );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_TYPE = "reductionType";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_TYPE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_TYPE );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_CONSTANT = "reductionConstant";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_CONSTANT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_CONSTANT );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_CAPACITY = "capacity";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_CAPACITY = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_CAPACITY );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_SUCTION = "switchOnLevelSuctionSide";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_SUCTION = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_SUCTION );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_SUCTION = "switchOffLevelSuctionSide";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_SUCTION = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_SUCTION );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_PRESSURE = "switchOnLevelPressureSide";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_PRESSURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_PRESSURE );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_PRESSURE = "switchOffLevelPressureSide";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_PRESSURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_PRESSURE );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_HEIGHT = "crestHeight";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_HEIGHT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_HEIGHT );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_WIDTH = "crestWidth";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_WIDTH = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_WIDTH );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_SHAPE = "crestShape";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_SHAPE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_SHAPE );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_CORRECTION_COEFF = "posCorrectionCoeff";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_CORRECTION_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_CORRECTION_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_SUBMERGE_LIMIT = "posSubmergeLimit";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_SUBMERGE_LIMIT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_SUBMERGE_LIMIT );

  public static final String F_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_REDUCTION_FACTORS_MEMBER = "posReductionFactorsMember";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_REDUCTION_FACTORS = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.F_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_REDUCTION_FACTORS_MEMBER );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_CORRECTION_COEFF = "negCorrectionCoeff";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_CORRECTION_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_CORRECTION_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_SUBMERGE_LIMIT = "negSubmergeLimit";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_SUBMERGE_LIMIT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_SUBMERGE_LIMIT );

  public static final String F_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_REDUCTION_FACTORS_MEMBER = "negReductionFactorsMember";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_REDUCTION_FACTORS = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.F_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_REDUCTION_FACTORS_MEMBER );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_CREST_HEIGHT = "crestHeight";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_CREST_HEIGHT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_CREST_HEIGHT );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_NUMBER_GATE_VALUES = "numOfGateValues";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_NUMBER_GATE_VALUES = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_NUMBER_GATE_VALUES );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_SECOND_AXIS_VALUE_TYPE = "secondAxisValueType";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_SECOND_AXIS_VALUE_TYPE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_SECOND_AXIS_VALUE_TYPE );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_INTERPOLATION_TYPE = "interpolationType";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_INTERPOLATION_TYPE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_INTERPOLATION_TYPE );

  public static final String F_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_MEMBER = "databaseMember";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_MEMBER = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.F_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_MEMBER );

  public static final String F_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_USAGE_MEMBER = "databaseUsageMember";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_USAGE_MEMBER = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.F_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_USAGE_MEMBER );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_UPSTREAM = "widthUpstream";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_UPSTREAM = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_UPSTREAM );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_LEFT = "widthStructureLeft";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_LEFT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_LEFT );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE = "widthStructure";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_RIGHT = "widthStructureRight";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_RIGHT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_RIGHT );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_DOWNSTREAM = "widthDownstream";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_DOWNSTREAM = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_DOWNSTREAM );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_UPSTREAM = "bedLevelUpstream";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_UPSTREAM = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_UPSTREAM );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_LEFT = "bedLevelStructureLeft";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_LEFT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_LEFT );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE = "bedLevelStructure";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_RIGHT = "bedLevelStructureRight";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_RIGHT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_RIGHT );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_DOWNSTREAM = "bedLevelDownstream";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_DOWNSTREAM = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_DOWNSTREAM );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_GATE_HEIGHT = "gateHeight";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_GATE_HEIGHT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_GATE_HEIGHT );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_GATE_FLOW_COEFF = "posFreeGateFlowCoeff";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_GATE_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_GATE_FLOW_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_GATE_FLOW_COEFF = "posDrownedGateFlowCoeff";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_GATE_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_GATE_FLOW_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_WEIR_FLOW_COEFF = "posFreeWeirFlowCoeff";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_WEIR_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_WEIR_FLOW_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_WEIR_FLOW_COEFF = "posDrownedWeirFlowCoeff";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_WEIR_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_WEIR_FLOW_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_CONTRACTION_COEFF = "posContractionCoefficient";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_CONTRACTION_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_CONTRACTION_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_GATE_FLOW_COEFF = "negFreeGateFlowCoeff";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_GATE_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_GATE_FLOW_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_GATE_FLOW_COEFF = "negDrownedGateFlowCoeff";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_GATE_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_GATE_FLOW_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_WEIR_FLOW_COEFF = "negFreeWeirFlowCoeff";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_WEIR_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_WEIR_FLOW_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_WEIR_FLOW_COEFF = "negDrownedWeirFlowCoeff";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_WEIR_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_WEIR_FLOW_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_CONTRACTION_COEFF = "negContractionCoefficient";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_CONTRACTION_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_CONTRACTION_COEFF );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_EXTRA_RESISTENCE = "extraResistence";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_EXTRA_RESISTENCE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_EXTRA_RESISTENCE );

  public static final String F_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE_STRUCTURE_NODE_MEMBER = "abstractSbkCompoundStructureNodeMember";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE_STRUCTURE_NODE_MEMBER = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.F_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE_STRUCTURE_NODE_MEMBER );

  public static final String NS_SOBEK_COMMON = "org.kalypso.model.wspm.sobek.common";

  public static final String P_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_PERIOD = "interpolationPeriod";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_PERIOD = new QName( ISobekConstants.NS_SOBEK_COMMON, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_PERIOD );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_VALUE = "interpolationValue";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_VALUE = new QName( ISobekConstants.NS_SOBEK_COMMON, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_VALUE );

  public static final String P_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_TYPE = "interpolationType";

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_TYPE = new QName( ISobekConstants.NS_SOBEK_COMMON, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_TYPE );

  public static final String F_SBK_STRUCT_LINKS_TO_BRANCH = "linksToBranch";

  public static final QName QN_SBK_STRUCT_LINKS_TO_BRANCH = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.F_SBK_STRUCT_LINKS_TO_BRANCH );

}
