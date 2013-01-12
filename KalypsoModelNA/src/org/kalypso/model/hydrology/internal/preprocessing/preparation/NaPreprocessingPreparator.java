/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.osgi.framework.Version;

/**
 * @author Gernot Belger
 */
public final class NaPreprocessingPreparator
{
  public static INaPreparedData prepareData( final NAModellControl control, final NAControl metaControl, final NaModell model, final InitialValues initialValue, final GMLWorkspace synthWorkspace, final NAOptimize optimize, final Parameter parameter, final ParameterHash landuseHash, final Node rootNode, final NaCatchmentData catchmentData, final IDManager idManager, final Version calcCoreVersion, final Logger logger ) throws SimulationException
  {
    return new NaResolvedModelData( control, metaControl, model, initialValue, synthWorkspace, optimize, parameter, landuseHash, rootNode, catchmentData, idManager, calcCoreVersion, logger );
  }
}