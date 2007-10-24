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

  public static final String F_HYDRAULIC_CROSS_SECTION_NODE_LINKED_BRANCH = "linksToBranch";

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

  public static final QName QN_HYDRAULIC_CROSS_SECTION_NODE_LINKED_BRANCH = new QName( ISobekConstants.NS_SOBEK, ISobekConstants.F_HYDRAULIC_CROSS_SECTION_NODE_LINKED_BRANCH );

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

}
