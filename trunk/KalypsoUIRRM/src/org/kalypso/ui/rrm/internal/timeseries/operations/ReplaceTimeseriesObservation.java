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
package org.kalypso.ui.rrm.internal.timeseries.operations;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.IMergeTimeseriesOperation;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;

/**
 * @author Dirk Kuch
 */
public class ReplaceTimeseriesObservation implements IMergeTimeseriesOperation
{
  private final FeatureBean<ITimeseries> m_timeseries;

  private IObservation m_observation;

  public ReplaceTimeseriesObservation( final FeatureBean<ITimeseries> timeseries )
  {
    m_timeseries = timeseries;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final ITimeseries timeseries = m_timeseries.getFeature();
      final ZmlLink link = timeseries.getDataLink();
      final IFile targetFile = link.getFile();

      ZmlFactory.writeToFile( m_observation, targetFile );
    }
    catch( final SensorException e )
    {
      e.printStackTrace();

      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString("ReplaceTimeseriesObservation_0"), e ); //$NON-NLS-1$
    }

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString("ReplaceTimeseriesObservation_1") ); //$NON-NLS-1$
  }

  @Override
  public void setObservation( final IObservation observation )
  {
    m_observation = observation;
  }

}
