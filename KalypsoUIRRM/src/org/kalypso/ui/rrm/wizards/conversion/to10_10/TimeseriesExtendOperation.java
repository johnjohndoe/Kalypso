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
package org.kalypso.ui.rrm.wizards.conversion.to10_10;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.Interval;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class TimeseriesExtendOperation implements ICoreRunnableWithProgress
{
  private final ZmlLink m_link;

  private final Interval m_simulationRange;

  private IObservation m_observation;

  public TimeseriesExtendOperation( final ZmlLink link, final Interval simulationRange )
  {
    m_link = link;
    m_simulationRange = simulationRange;
  }
  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    /* Check preliminary conditions */
    if( !m_link.isLinkSet() )
      return Status.OK_STATUS;

    final String href = m_link.getHref();
    final Feature feature = m_link.getFeature();
    if( !m_link.isLinkExisting() )
    {
      final String featureLabel = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
      final String msg = String.format( Messages.getString("TimeseriesExtendOperation_0"), href, featureLabel ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), msg );
    }

    try
    {
      /* Load timeseries and check if we need to do anything */
      m_observation = m_link.loadObservation();
      final ITupleModel values = m_observation.getValues( null );

      final TimeseriesExtender extender = new TimeseriesExtender( values, href );
      extender.checkSize();
      if( !extender.checkRange( m_simulationRange ) )
        return Status.OK_STATUS;

      final IStatus extendStatus = extender.extend( m_simulationRange );
      if( !extendStatus.matches( IStatus.ERROR ) )
      {
        final ITupleModel extendedValues = extender.getExtendedValues();
        writeTimeseries( extendedValues );
      }

      return extendStatus;
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
    catch( final SensorException e )
    {
      final String msg = String.format( Messages.getString("TimeseriesExtendOperation_1"), href ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), msg );
    }
  }

  private void writeTimeseries( final ITupleModel newModel ) throws CoreException, SensorException
  {
    final String href = m_link.getHref();
    final String name = m_observation.getName();
    final MetadataList metadataList = m_observation.getMetadataList();
    final MetadataList clonedMetadata = (MetadataList) metadataList.clone();

    final SimpleObservation newObservation = new SimpleObservation( href, name, clonedMetadata, newModel );
    m_link.saveObservation( newObservation );
  }

}
