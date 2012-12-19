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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.util.Date;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.TupleModelDataSet;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.timeseries.datasource.DataSourceHandler;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.repository.IDataSourceItem;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

import com.google.common.base.Objects;

/**
 * @author Dirk Kuch
 */
public class ChangeTimeseriesParameterTypeWorker implements ICoreRunnableWithProgress
{
  private final ITimeseries m_timeseries;

  private final String m_targetType;

  public ChangeTimeseriesParameterTypeWorker( final ITimeseries timeseries, final String targetType )
  {
    m_timeseries = timeseries;
    m_targetType = targetType;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final StatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );
    final ZmlLink link = m_timeseries.getDataLink();
    final File file = link.getJavaFile();

    try
    {
      final IObservation sourceObservation = ZmlFactory.parseXML( file.toURI().toURL() );
      final ITupleModel sourceModel = sourceObservation.getValues( null );
      final MetadataList metadata = sourceObservation.getMetadataList();
      final ChangeParameterTypeVisitor visitor = new ChangeParameterTypeVisitor( metadata, m_targetType );
      sourceModel.accept( visitor, 0 );

      final SimpleTupleModel targetModel = new SimpleTupleModel( visitor.getTargetAxis() );
      final DataSourceHandler dataSources = new DataSourceHandler( metadata );

      final Map<Date, TupleModelDataSet> converted = visitor.getConverted();
      final Set<Entry<Date, TupleModelDataSet>> entries = converted.entrySet();
      for( final Entry<Date, TupleModelDataSet> entry : entries )
      {
        final Date date = entry.getKey();
        final TupleModelDataSet dataset = entry.getValue();

        final Object value = dataset.getValue();
        final Object status = Objects.firstNonNull( dataset.getStatus(), KalypsoStati.BIT_OK );
        final String source = Objects.firstNonNull( dataset.getSource(), IDataSourceItem.SOURCE_UNKNOWN );
        final Object sourceIndex = dataSources.addDataSource( source, source );

        targetModel.addTuple( new Object[] { date, value, status, sourceIndex } );
      }

      final SimpleObservation targetObservation = new SimpleObservation( sourceObservation.getHref(), sourceObservation.getName(), metadata, targetModel );
      ZmlFactory.writeToFile( targetObservation, file );

      m_timeseries.setParameterType( m_targetType );

      final ZmlLink updatedLnk = m_timeseries.getDataLink();
      final File updatedFile = updatedLnk.getJavaFile();

      FileUtils.moveFile( file, updatedFile );
    }
    catch( final Exception e )
    {
      collector.add( IStatus.WARNING, Messages.getString("ChangeTimeseriesParameterTypeWorker_0"), e ); //$NON-NLS-1$
    }

    return collector.asMultiStatusOrOK( String.format( Messages.getString("ChangeTimeseriesParameterTypeWorker_1"), m_timeseries.getName(), m_targetType ) ); //$NON-NLS-1$
  }
}
