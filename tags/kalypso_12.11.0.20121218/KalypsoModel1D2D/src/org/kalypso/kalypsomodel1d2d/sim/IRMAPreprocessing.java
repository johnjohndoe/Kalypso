/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.sim;

/**
 * @author Gernot Belger
 */
public interface IRMAPreprocessing
{
  String INPUT_RESTART_FILE_PREFIX = "restartFile"; //$NON-NLS-1$

  String INPUT_ROUGHNESS = "roughness"; //$NON-NLS-1$

  String INPUT_FLOW_RELATIONSHIPS = "flowRelationships"; //$NON-NLS-1$

  String INPUT_WIND_RELATIONSHIPS = "wind"; //$NON-NLS-1$

  String INPUT_CALCULATION_UNIT_ID = "calculationUnitID"; //$NON-NLS-1$

  String INPUT_MESH = "mesh"; //$NON-NLS-1$

  String INPUT_CONTROL = "control"; //$NON-NLS-1$

  /** the log of geo status elements i.e. the log file of the preprocessing */
  String OUTPUT_LOG = "log"; //$NON-NLS-1$

  String OUTPUT_MESH = ISimulation1D2DConstants.MODEL_2D;

  String OUTPUT_BC_WQ = ISimulation1D2DConstants.BC_WQ_File;

  String OUTPUT_BUILDINGS = ISimulation1D2DConstants.BUILDING_File;

  String OUTPUT_WIND = ISimulation1D2DConstants.WIND_RMA10_File;

  String OUTPUT_WIND_COORD = ISimulation1D2DConstants.WIND_RMA10_COORDS_File;

  String OUTPUT_CONTROL = ISimulation1D2DConstants.R10_File;

  String INPUT_RESTART_FILE = "restartFile0"; //$NON-NLS-1$

  String OUTPUT_RMA_VERSION = "rmaVersion"; //$NON-NLS-1$
}