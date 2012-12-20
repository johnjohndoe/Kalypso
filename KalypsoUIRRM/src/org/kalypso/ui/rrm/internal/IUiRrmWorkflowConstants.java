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
package org.kalypso.ui.rrm.internal;

/**
 * Constants regarding workflow and/or scenario stuff.
 *
 * @author Gernot Belger
 */
public interface IUiRrmWorkflowConstants
{
  /** THE na model i.e. modell.gml */
  String SCENARIO_DATA_MODEL = "model"; //$NON-NLS-1$

  String SCENARIO_DATA_STATIONS = "stations"; //$NON-NLS-1$

  String SCENARIO_DATA_CATCHMENT_MODELS = "catchmentModels"; //$NON-NLS-1$

  String SCENARIO_DATA_SIMULATIONS = "simulations"; //$NON-NLS-1$

  String SCENARIO_DATA_TIMESERIES_MAPPINGS = "timeseriesMapppings"; //$NON-NLS-1$
}
