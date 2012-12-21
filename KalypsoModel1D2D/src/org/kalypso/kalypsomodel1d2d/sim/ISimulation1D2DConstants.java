/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.sim;

import java.util.Date;

/**
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 */
public interface ISimulation1D2DConstants
{
  String OUTPUT_ITR_RMA = "Output.itr"; //$NON-NLS-1$

  String OUTPUT_ITR_SWAN = "PRINT"; //$NON-NLS-1$

  String OUTPUT_ITR_TELEMAC = "sortie*"; //$NON-NLS-1$

  String MODEL_2D = "model.2d"; //$NON-NLS-1$

  String OUTPUT_DIR_NAME = "./"; //$NON-NLS-1$

  String RESOURCEBASE = "resource/"; //$NON-NLS-1$

  String R10_File = "control.r10"; //$NON-NLS-1$

  String BUILDING_File = "bauwerke.txt"; //$NON-NLS-1$

  String WIND_RMA10_File = "wind.txt"; //$NON-NLS-1$

  String WIND_RMA10_COORDS_File = "windCoord.txt"; //$NON-NLS-1$

  String SURFACE_TRACTTION_RMA10_File = "surfaceTr.txt"; //$NON-NLS-1$

  String BC_WQ_File = "wqusw.txt"; //$NON-NLS-1$

  String SIM_RMA10_EXE_FILE_PREFIX = "rma-kalypso_"; //$NON-NLS-1$

  String SIM_SWAN_EXE_FILE_PREFIX = "swan"; //$NON-NLS-1$

  String SIM_SWAN_CONTROL_FILE = "INPUT"; //$NON-NLS-1$

  String SIM_SWAN_COORD_SHIFT_FILE = "kalypso_swan_shift"; //$NON-NLS-1$

  String SIM_SWAN_COORD_SHIFT_X = "X"; //$NON-NLS-1$

  String SIM_SWAN_COORD_SHIFT_Y = "Y"; //$NON-NLS-1$

  String SIM_SWAN_WIND_FILE = "wind"; //$NON-NLS-1$

  String SWAN_BOUNDARY_FILE_PREFIX = "bnd_TPAR_"; //$NON-NLS-1$

  String SIM_SWAN_DIRECTION_OUT_PARAM = "Dir"; //$NON-NLS-1$

  String SIM_SWAN_HSIG_OUT_PARAM = "Hsig"; //$NON-NLS-1$

  String SIM_SWAN_PERIOD_OUT_PARAM = "TM01"; //$NON-NLS-1$

  String SIM_SWAN_WATER_LEVEL_SERIES_FILE = "wl"; //$NON-NLS-1$

  String SIM_SWAN_WATER_LEVEL_DATA_FILE = "wl"; //$NON-NLS-1$

  String SIM_SWAN_CURRENT_SERIES_FILE = "currnt"; //$NON-NLS-1$

  String SIM_SWAN_CURRENT_DATA_FILE = "currnt"; //$NON-NLS-1$

  String SIM_SWAN_TRIANGLE_FILE = "mshtri"; //$NON-NLS-1$

  String SIM_SWAN_MAT_RESULT_EXT = "mat"; //$NON-NLS-1$

  String SIM_SWAN_EXCLUSION_NUMBER = "0.0";// "-999.0000"; //$NON-NLS-1$

  String SIM_SWAN_TIME_SUFFIX = "_time_"; //$NON-NLS-1$

  String SIM_SWAN_DATA_FILE_EXT = ".txt"; //$NON-NLS-1$

  String SIM_SWAN_HOT_FILE = "restart_file.hot"; //$NON-NLS-1$

  /** The date representing the steady calculation step */
  Date STEADY_DATE = new Date( 0 );

  /** The date representing the maximum calculation step */
  Date MAXI_DATE = new Date( 1 );

  String STEADY_PREFIX = "steady"; //$NON-NLS-1$

  String MAXI_PREFIX = "maxi"; //$NON-NLS-1$

  String MINI_PREFIX = "mini"; //$NON-NLS-1$

  String MODEL_PREFIX = "model"; //$NON-NLS-1$

  // input
  String DISCRETISATIOMODEL_ID = "DiscretisationModel"; //$NON-NLS-1$

  String TERRAINMODEL_ID = "TerrainModel"; //$NON-NLS-1$

  String FLOWRELATIONSHIPMODEL_ID = "FlowRelationshipModel"; //$NON-NLS-1$

  String CONTROL_ID = "SimulationControlModel"; //$NON-NLS-1$

  String ROUGHNESS_ID = "Roughness"; //$NON-NLS-1$

  String SWAN_INPUT_CALC_PATH = "SWAN_CALC_PATH"; //$NON-NLS-1$

  String RMA_INPUT_CALC_PATH = "resultsRMA"; //$NON-NLS-1$

  // output
  String SIMULATIONRESULTMODEL_ID = "SimulationResultModel"; //$NON-NLS-1$

  String RESULT_DIR_NAME_ID = "RESULT_DIR_NAME"; //$NON-NLS-1$

  String RESULT_2d_ZIP_ID = "RESULT_2d_ZIP"; //$NON-NLS-1$

  String SIMULATION_LOG_GML = "simulation_log.gml"; //$NON-NLS-1$

  // ////////////////
  // Status Codes //
  // ////////////////
  int CODE_NONE = -1;

  /** General info's of simulation process */
  int CODE_RUNNING = 0;

  /** Fine grained info's about the simulation process */
  int CODE_RUNNING_FINE = 1;

  // Pre-processing
  /** General status from pre-processing the calculation */
  int CODE_PRE = 1000;

  // RMAKalypso codes
  /** Any message from the RMAKalypso */
  int CODE_RMA10S = 2000;

  // Result Post-processing
  /** General status from processing the results. */
  int CODE_POST = 3000;

  /** SimplDateFormat string for displaying timesteps to the user */
  String TIMESTEP_DISPLAY_FORMAT = "dd.MM.yyyy HH:mm:ss.SSS"; //$NON-NLS-1$";
}