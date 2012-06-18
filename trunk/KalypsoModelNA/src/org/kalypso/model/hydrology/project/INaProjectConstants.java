/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.hydrology.project;

/**
 * Constants relevant for the project structure of an na model.<br/>
 * Most pathes are relative to the project root.
 * 
 * @author Gernot Belger
 * @deprecated Use {@link ScenarioAccessor} instead. Move all constant into that class.
 */
@Deprecated
public interface INaProjectConstants
{
  String FOLDER_BASIS = "Basis"; //$NON-NLS-1$

  String FOLDER_ZEITREIHEN_MANAGEMENT = "ZeitreihenManagement";//$NON-NLS-1$

  String FOLDER_ZEITREIHEN = "Zeitreihen";//$NON-NLS-1$

  String FOLDER_MODEL = ".model";//$NON-NLS-1$

  String FOLDER_MODELS = ".models"; //$NON-NLS-1$

  /**
   * Do not use any more. A calc case is now called simulation. Hence "Rechenvarianten" has changed to "Simulationen".
   * 
   * @deprecated Use {@link RrmScenario#FOLDER_SIMULATIONEN} instead.
   */
  @Deprecated
  String FOLDER_RECHENVARIANTEN = "Rechenvarianten";//$NON-NLS-1$

  /**
   * @deprecated see {@link #FOLDER_RECHENVARIANTEN}
   */
  @Deprecated
  String PATH_RECHENVARIANTEN = RrmProject.FOLDER_BASIS + "/" + FOLDER_RECHENVARIANTEN; //$NON-NLS-1$

  String FOLDER_TIMESERIES = "timeseries"; //$NON-NLS-1$

  String PATH_TIMESERIES = FOLDER_MODEL + "/" + FOLDER_TIMESERIES; //$NON-NLS-1$

  String GML_MODELL_FILE = "modell.gml"; //$NON-NLS-1$

  String GML_MODELL_PATH = FOLDER_MODELS + "/" + GML_MODELL_FILE; //$NON-NLS-1$

  String GML_HYDROTOP_FILE = "hydrotop.gml";//$NON-NLS-1$

  String GML_HYDROTOP_PATH = FOLDER_MODELS + "/" + GML_HYDROTOP_FILE; //$NON-NLS-1$

  String GML_SYNTH_N_FILE = "synthN.gml";//$NON-NLS-1$

  String GML_SYNTH_N_PATH = FOLDER_MODELS + "/" + GML_SYNTH_N_FILE; //$NON-NLS-1$

  String GML_GEOLOGIE_FILE = "geologie.gml";//$NON-NLS-1$

  String GML_GEOLOGIE_PATH = FOLDER_MODELS + "/" + GML_GEOLOGIE_FILE; //$NON-NLS-1$

  String GML_LANDUSE_FILE = "landuse.gml";//$NON-NLS-1$

  String GML_LANDUSE_PATH = FOLDER_MODELS + "/" + GML_LANDUSE_FILE; //$NON-NLS-1$

  String GML_PARAMETER_FILE = "parameter.gml";//$NON-NLS-1$

  String GML_PARAMETER_PATH = FOLDER_MODELS + "/" + GML_PARAMETER_FILE; //$NON-NLS-1$

  String GML_PEDOLOGIE_FILE = "pedologie.gml";//$NON-NLS-1$

  String GML_PEDOLOGIE_PATH = FOLDER_MODELS + "/" + GML_PEDOLOGIE_FILE; //$NON-NLS-1$

  String CALC_CASE_TEMPLATE_DIR = ".model/calcCaseTemplate"; //$NON-NLS-1$

  String GML_STATIONS = "stations.gml"; //$NON-NLS-1$

  String GML_STATIONS_PATH = PATH_TIMESERIES + "/" + GML_STATIONS; //$NON-NLS-1$

  String GML_CATCHMENT_MODEL_FILE = "catchmentModels.gml"; //$NON-NLS-1$

  String GML_TIMESERIES_MAPPINGS_FILE = "timeseriesMappings.gml"; //$NON-NLS-1$

  String GML_CATCHMENT_MODEL_PATH = FOLDER_MODELS + "/" + GML_CATCHMENT_MODEL_FILE; //$NON-NLS-1$

  String GML_TIMESERIES_MAPPINGS_PATH = FOLDER_MODELS + "/" + GML_TIMESERIES_MAPPINGS_FILE; //$NON-NLS-1$

  String GML_SIMULATIONS_FILE = "simulations.gml"; //$NON-NLS-1$

  String GML_SIMULATIONS_PATH = FOLDER_MODELS + "/" + GML_SIMULATIONS_FILE; //$NON-NLS-1$

  /** Temporary thiessen stations; only used in thiessen wizard */
  String GML_THIESSEN_STATION_FILE = ".thiessenStations.gml"; //$NON-NLS-1$

  String GML_THIESSEN_STATION_PATH = FOLDER_MODELS + "/" + GML_THIESSEN_STATION_FILE; //$NON-NLS-1$
}