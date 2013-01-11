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
package org.kalypso.model.wspm.tuhh.ui.imports.ctripple;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleHorizonMapper;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleProfile;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleProfileHorizon;

/**
 * @author Holger Albert
 */
public abstract class AbstractCodedTrippleWorker
{
  public AbstractCodedTrippleWorker( )
  {
  }

  public abstract void updateClassifications( final CodedTrippleImportData data ) throws Exception;

  public abstract IProfileFeature createNewProfile( final CodedTrippleImportData data, final CodedTrippleProfile profile ) throws CoreException;

  public abstract void createMarkers( final IProfileFeature profileFeature );

  public abstract void fireChangeEvents( );

  protected String getName( final CodedTrippleProfileHorizon baseHorizon )
  {
    return baseHorizon.getHorizonId();
  }

  protected String getDescription( final CodedTrippleHorizonMapper mapper, final CodedTrippleProfileHorizon baseHorizon )
  {
    final String horizonId = baseHorizon.getHorizonId();
    return mapper.getHorizonIdDescription( horizonId );
  }
}