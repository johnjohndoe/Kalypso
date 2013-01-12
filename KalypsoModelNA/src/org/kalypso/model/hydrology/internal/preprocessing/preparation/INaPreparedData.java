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
package org.kalypso.model.hydrology.internal.preprocessing.preparation;

import java.util.logging.Logger;

import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.NaCatchmentData;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ParameterHash;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.osgi.framework.Version;

/**
 * Provides all data necessary to write the ascii files.<br/>
 * In particular, we do NOT use {@link org.kalypso.model.hydrology.INaSimulationData} in order to separate the ascii writing comletely from the rest of the simulation stuff.
 * 
 * @author Gernot Belger
 */
public interface INaPreparedData
{
  NAControl getMetaControl( );

  NAModellControl getControl( );

  Node getRootNode( );

  NaModell getModel( );

  IDManager getIdManager( );

  NaCatchmentData getCatchmentData( );

  NAOptimize getNaOptimize( );

  GMLWorkspace getSynthNWorkspace( );

  RelevantNetElements getRelevantElements( );

  /**
   * @deprecated Use new logging mechanism instead
   */
  @Deprecated
  Logger getLogger( );

  TimeseriesFileManager getTimeseriesManager( );

  Parameter getParameter( );

  ParameterHash getLanduseHash( );

  InitialValues getInitialValues( );

  Version getCalcCoreVersion( );
}