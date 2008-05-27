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
  public static final String F_HYDRAULIC_BOUNDARY_NODE = "BoundaryConditionNode"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_BOUNDARY_NODE_LASTFALL_DEFINITION_MEMBER = "lastfallDefinitionMember"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_BOUNDARY_NODE_TYPE = "bcType"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_BRANCH_LENGTH = "length"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_BRANCH_LOWER_CONNECTION_NODE = "lowerConnectionNode"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_BRANCH_MEMBER = "branchMember"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_BRANCH_RIVER_LINE = "riverLine"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_BRANCH_UPPER_CONNECTION_NODE = "upperConnectionNode"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_CONNECTION_NODE = "ConnectionNode"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_CROSS_SECTION_NODE = "CrossSectionNode"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_CROSS_SECTION_NODE_LINKED_PROFILE = "linkedProfile"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_DESCRIPTION = "description"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_LASTFALL = "Lastfall"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_LASTFALL_MEMBER = "lastfallMember"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_LINKAGE_NODE = "LinkageNode"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_NAME = "name"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_NODE_CONNECTION_TYPE = "typeOfConnectionNode"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_NODE_LINKED_BRANCH = "linkedBranch"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES = "linkedInflowBranches"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES = "linkedOutflowBranches"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_NODE_LOCATION = "location"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_NODE_MEMBER = "nodeMember"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE_STRUCTURE_NODE_MEMBER = "abstractSbkCompoundStructureNodeMember"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_MEMBER = "databaseMember"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_USAGE_MEMBER = "databaseUsageMember"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_REDUCTION_FACTORS_MEMBER = "negReductionFactorsMember"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_REDUCTION_FACTORS_MEMBER = "posReductionFactorsMember"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_SOBEK_BRANCH = "Branch"; //$NON-NLS-1$

  public static final String F_HYDRAULIC_UNIQUE_ID = "uniqueID"; //$NON-NLS-1$

  public static final String F_LASTFALL_SIMULATION_BEGIN = "simulationBegin"; //$NON-NLS-1$

  public static final String F_LASTFALL_SIMULATION_END = "simulationEnd"; //$NON-NLS-1$

  public static final String F_LASTFALL_SIMULATION_PRE_TIME = "preSimulationTime"; //$NON-NLS-1$

  public static final String F_LASTFALL_SIMULATION_TIMESTEP = "simulationTimestep"; //$NON-NLS-1$

  public static final String F_LASTFALL_SIMULATION_TIMESTEP_MULTIPLIER = "resultTimeStepAsMultiple"; //$NON-NLS-1$

  public static final String F_LN_LINKS_TO_BRANCH = "linksToBranch"; //$NON-NLS-1$

  public static final String F_NOFDP_HYDRAULIC_PROFILE = "Profile"; //$NON-NLS-1$

  public static final String F_NOFDP_HYDRAULIC_PROFILE_MEMBER = "profileMember"; //$NON-NLS-1$

  public static final String F_NOFDP_POLDER_NODE = "PolderNode"; //$NON-NLS-1$

  public static final String F_NOFDP_RETARDIN_BASIN_NODE = "RetardingBasinNode"; //$NON-NLS-1$

  public static final String F_NOFDP_WEIR_NODE = "WeirNode"; //$NON-NLS-1$

  public static final String F_SBK_STRUCT_LINKS_TO_BRANCH = "linksToBranch"; //$NON-NLS-1$

  public static final String F_SOBEK_MODEL = "SobekModel"; //$NON-NLS-1$

  public static final String F_SOBEK_MODEL_MEMBER = "sobekModelMember"; //$NON-NLS-1$

  public static final String NS_NOFPD_1D_MODEL = "org.kalypso.nofdpidss.1dmodel"; //$NON-NLS-1$

  public static final String NS_SOBEK = "org.kalypso.model.wspm.sobek"; //$NON-NLS-1$

  public static final String NS_SOBEK_COMMON = "org.kalypso.model.wspm.sobek.common"; //$NON-NLS-1$

  public static final String NS_SOBEK_PROJECT = "org.kalypso.model.wspm.sobek.project"; //$NON-NLS-1$

  public static final String NS_SOBEK_STRUCTURES = "org.kalypso.model.wspm.sobek.structures"; //$NON-NLS-1$

  public static final String NS_SOBEK_RESULT_TIME_SERIES = "org.kalypso.model.wspm.sobek.result.ts"; //$NON-NLS-1$

  public static final String NS_WSPM_PROFILE = "org.kalypso.model.wspmprofile"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_BEGINS = "observationBegin"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE = "constValue"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE_INTERVALL = "constValueIntervalMinutes"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_ENDS = "observationEnd"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_LINKED_LASTFALL = "linkedLastfall"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_LNK_TIME_SERIES = "sourceRepositoryObservation"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_OBSERVATION = "data"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_BOUNDARY_NODE_CONDITION_TYPE = "typeOfLastBoundaryCondition"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_NODE_STATION_NAME = "stationName"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE = "SbkCompoundStructure"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE = "SbkDatabaseStructure"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_CREST_HEIGHT = "crestHeight"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_INTERPOLATION_TYPE = "interpolationType"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_NUMBER_GATE_VALUES = "numOfGateValues"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_SECOND_AXIS_VALUE_TYPE = "secondAxisValueType"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE = "SbkGeneralStructure"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_DOWNSTREAM = "bedLevelDownstream"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE = "bedLevelStructure"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_LEFT = "bedLevelStructureLeft"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_RIGHT = "bedLevelStructureRight"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_UPSTREAM = "bedLevelUpstream"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_EXTRA_RESISTENCE = "extraResistence"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_GATE_HEIGHT = "gateHeight"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_CONTRACTION_COEFF = "negContractionCoefficient"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_GATE_FLOW_COEFF = "negDrownedGateFlowCoeff"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_WEIR_FLOW_COEFF = "negDrownedWeirFlowCoeff"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_GATE_FLOW_COEFF = "negFreeGateFlowCoeff"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_WEIR_FLOW_COEFF = "negFreeWeirFlowCoeff"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_CONTRACTION_COEFF = "posContractionCoefficient"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_GATE_FLOW_COEFF = "posDrownedGateFlowCoeff"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_WEIR_FLOW_COEFF = "posDrownedWeirFlowCoeff"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_GATE_FLOW_COEFF = "posFreeGateFlowCoeff"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_WEIR_FLOW_COEFF = "posFreeWeirFlowCoeff"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_DOWNSTREAM = "widthDownstream"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE = "widthStructure"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_LEFT = "widthStructureLeft"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_RIGHT = "widthStructureRight"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_UPSTREAM = "widthUpstream"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP = "SbkPump"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_CAPACITY = "capacity"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_FLOW_DIRECTION = "flowDirection"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_PUMP_CONTROL = "pumpControl"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_CONSTANT = "reductionConstant"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_TYPE = "reductionType"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_PRESSURE = "switchOffLevelPressureSide"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_SUCTION = "switchOffLevelSuctionSide"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_PRESSURE = "switchOnLevelPressureSide"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_SUCTION = "switchOnLevelSuctionSide"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR = "SbkRiverWeir"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_HEIGHT = "crestHeight"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_SHAPE = "crestShape"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_WIDTH = "crestWidth"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_CORRECTION_COEFF = "negCorrectionCoeff"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_SUBMERGE_LIMIT = "negSubmergeLimit"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_CORRECTION_COEFF = "posCorrectionCoeff"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_SUBMERGE_LIMIT = "posSubmergeLimit"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_PERIOD = "interpolationPeriod"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_TYPE = "interpolationType"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_VALUE = "interpolationValue"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_WEIR = "SbkWeir"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_HEIGHT = "crestHeight"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_WIDTH = "crestWidth"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_WEIR_DISCHARGE_COEFF = "dischargeCoeffCE"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_WEIR_FLOW_DIRECTION = "flowDirection"; //$NON-NLS-1$

  public static final String P_HYDRAULIC_SBK_STRUCTURE_WEIR_LATERAL_CONTRACTION_COEFF = "lateralContractionCoeffCW"; //$NON-NLS-1$

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BOUNDARY_NODE );

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_BEGINS = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_BEGINS );

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE );

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE_INTERVALL = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE_INTERVALL );

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_ENDS = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_ENDS );

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LINKED_LASTFALL = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_LINKED_LASTFALL );

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LNK_TIME_SERIES = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_LNK_TIME_SERIES );

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_OBSERVATION = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_OBSERVATION );

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_TYPE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_BOUNDARY_NODE_CONDITION_TYPE );

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_LASTFALL_DEFINITION_MEMBER = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BOUNDARY_NODE_LASTFALL_DEFINITION_MEMBER );

  public static final QName QN_HYDRAULIC_BOUNDARY_NODE_TYPE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BOUNDARY_NODE_TYPE );

  public static final QName QN_HYDRAULIC_BRANCH_LENGTH = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BRANCH_LENGTH );

  public static final QName QN_HYDRAULIC_BRANCH_LOWER_CONNECTION_NODE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BRANCH_LOWER_CONNECTION_NODE );

  public static final QName QN_HYDRAULIC_BRANCH_MEMBER = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BRANCH_MEMBER );

  public static final QName QN_HYDRAULIC_BRANCH_RIVER_LINE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BRANCH_RIVER_LINE );

  public static final QName QN_HYDRAULIC_BRANCH_UPPER_CONNECTION_NODE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_BRANCH_UPPER_CONNECTION_NODE );

  public static final QName QN_HYDRAULIC_CONNECTION_NODE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_CONNECTION_NODE );

  public static final QName QN_HYDRAULIC_CROSS_SECTION_NODE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_CROSS_SECTION_NODE );

  public static final QName QN_HYDRAULIC_CROSS_SECTION_NODE_LINKED_PROFILE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_CROSS_SECTION_NODE_LINKED_PROFILE );

  public static final QName QN_HYDRAULIC_DESCRIPTION = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_DESCRIPTION );

  public static final QName QN_HYDRAULIC_LASTFALL = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_LASTFALL );

  public static final QName QN_HYDRAULIC_LASTFALL_MEMBER = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_LASTFALL_MEMBER );

  public static final QName QN_HYDRAULIC_LINKAGE_NODE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_LINKAGE_NODE );

  public static final QName QN_HYDRAULIC_NAME = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NAME );

  public static final QName QN_HYDRAULIC_NODE_CONNECTION_TYPE = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NODE_CONNECTION_TYPE );

  public static final QName QN_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NODE_LINKED_INFLOWING_BRANCHES );

  public static final QName QN_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NODE_LINKED_OUTFLOWING_BRANCHES );

  public static final QName QN_HYDRAULIC_NODE_LOCATION = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NODE_LOCATION );

  public static final QName QN_HYDRAULIC_NODE_MEMBER = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NODE_MEMBER );

  public static final QName QN_HYDRAULIC_NODE_STATION_NAME = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.P_HYDRAULIC_NODE_STATION_NAME );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE_STRUCTURE_NODE_MEMBER = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.F_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE_STRUCTURE_NODE_MEMBER );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_CREST_HEIGHT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_CREST_HEIGHT );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_MEMBER = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.F_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_MEMBER );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_USAGE_MEMBER = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.F_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_USAGE_MEMBER );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_INTERPOLATION_TYPE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_INTERPOLATION_TYPE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_NUMBER_GATE_VALUES = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_NUMBER_GATE_VALUES );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_SECOND_AXIS_VALUE_TYPE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_SECOND_AXIS_VALUE_TYPE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_DOWNSTREAM = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_DOWNSTREAM );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_LEFT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_LEFT );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_RIGHT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_RIGHT );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_UPSTREAM = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_UPSTREAM );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_EXTRA_RESISTENCE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_EXTRA_RESISTENCE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_GATE_HEIGHT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_GATE_HEIGHT );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_CONTRACTION_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_CONTRACTION_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_GATE_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_GATE_FLOW_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_WEIR_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_WEIR_FLOW_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_GATE_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_GATE_FLOW_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_WEIR_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_WEIR_FLOW_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_CONTRACTION_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_CONTRACTION_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_GATE_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_GATE_FLOW_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_WEIR_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_WEIR_FLOW_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_GATE_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_GATE_FLOW_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_WEIR_FLOW_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_WEIR_FLOW_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_DOWNSTREAM = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_DOWNSTREAM );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_LEFT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_LEFT );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_RIGHT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_RIGHT );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_UPSTREAM = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_UPSTREAM );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_CAPACITY = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_CAPACITY );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_FLOW_DIRECTION = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_FLOW_DIRECTION );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_PUMP_CONTROL = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_PUMP_CONTROL );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_CONSTANT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_CONSTANT );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_TYPE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_TYPE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_PRESSURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_PRESSURE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_SUCTION = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_SUCTION );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_PRESSURE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_PRESSURE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_SUCTION = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_SUCTION );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_HEIGHT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_HEIGHT );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_SHAPE = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_SHAPE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_WIDTH = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_WIDTH );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_CORRECTION_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_CORRECTION_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_REDUCTION_FACTORS = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.F_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_REDUCTION_FACTORS_MEMBER );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_SUBMERGE_LIMIT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_SUBMERGE_LIMIT );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_CORRECTION_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_CORRECTION_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_REDUCTION_FACTORS = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.F_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_REDUCTION_FACTORS_MEMBER );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_SUBMERGE_LIMIT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_SUBMERGE_LIMIT );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_PERIOD = new QName( ISobekConstants.NS_SOBEK_COMMON, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_PERIOD );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_TYPE = new QName( ISobekConstants.NS_SOBEK_COMMON, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_TYPE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_VALUE = new QName( ISobekConstants.NS_SOBEK_COMMON, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_TABLE_INTERPOLATION_VALUE );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_WEIR = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_WEIR );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_HEIGHT = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_HEIGHT );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_WIDTH = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_WIDTH );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_WEIR_DISCHARGE_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_WEIR_DISCHARGE_COEFF );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_WEIR_FLOW_DIRECTION = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_WEIR_FLOW_DIRECTION );

  public static final QName QN_HYDRAULIC_SBK_STRUCTURE_WEIR_LATERAL_CONTRACTION_COEFF = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.P_HYDRAULIC_SBK_STRUCTURE_WEIR_LATERAL_CONTRACTION_COEFF );

  public static final QName QN_HYDRAULIC_SOBEK_BRANCH = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_SOBEK_BRANCH );

  public static final QName QN_HYDRAULIC_UNIQUE_ID = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_UNIQUE_ID );

  public static final QName QN_HYDRAULICNODE_LINKED_BRANCH = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_NODE_LINKED_BRANCH );

  public static final QName QN_LASTFALL_SIMULATION_BEGIN = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_LASTFALL_SIMULATION_BEGIN );

  public static final QName QN_LASTFALL_SIMULATION_END = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_LASTFALL_SIMULATION_END );

  public static final QName QN_LASTFALL_SIMULATION_PRE_TIME = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_LASTFALL_SIMULATION_PRE_TIME );

  public static final QName QN_LASTFALL_SIMULATION_TIMESTEP = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_LASTFALL_SIMULATION_TIMESTEP );

  public static final QName QN_LASTFALL_SIMULATION_TIMESTEP_MULTIPLIER = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_LASTFALL_SIMULATION_TIMESTEP_MULTIPLIER );

  public static final QName QN_LN_LINKS_TO_BRANCH = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_LN_LINKS_TO_BRANCH );

  public static final QName QN_NOFDP_HYDRAULIC_PROFILE = new QName( ISobekConstants.NS_WSPM_PROFILE, ISobekConstants.F_NOFDP_HYDRAULIC_PROFILE );

  public static final QName QN_NOFDP_HYDRAULIC_PROFILE_MEMBER = new QName( ISobekConstants.NS_WSPM_PROFILE, ISobekConstants.F_NOFDP_HYDRAULIC_PROFILE_MEMBER );

  public static final QName QN_NOFDP_POLDER_NODE = new QName( ISobekConstants.NS_NOFPD_1D_MODEL, ISobekConstants.F_NOFDP_POLDER_NODE );

  public static final QName QN_NOFDP_RETARDIN_BASIN_NODE = new QName( ISobekConstants.NS_NOFPD_1D_MODEL, ISobekConstants.F_NOFDP_RETARDIN_BASIN_NODE );

  public static final QName QN_NOFDP_WEIR_NODE = new QName( ISobekConstants.NS_NOFPD_1D_MODEL, ISobekConstants.F_NOFDP_WEIR_NODE );

  public static final QName QN_SBK_STRUCT_LINKS_TO_BRANCH = new QName( ISobekConstants.NS_SOBEK_STRUCTURES, ISobekConstants.F_SBK_STRUCT_LINKS_TO_BRANCH );

  public static final QName QN_SOBEK_MODEL = new QName( ISobekConstants.NS_SOBEK_PROJECT, ISobekConstants.F_SOBEK_MODEL );

  public static final QName QN_SOBEK_MODEL_MEMBER = new QName( ISobekConstants.NS_SOBEK_PROJECT, ISobekConstants.F_SOBEK_MODEL_MEMBER );

}
