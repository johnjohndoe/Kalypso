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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.shape.ShapeDataException;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.data.SimpleShapeData;
import org.kalypso.shape.data.SimpleShapeDataFactory;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.deegree.GM_Object2Shape;
import org.kalypso.shape.deegree.IShapeDataFactory;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.util.GeometryExtracter;

/**
 * @author Gernot Belger
 */
public class BanklineExportShapeWorker implements ICoreRunnableWithProgress
{
  private static final short FIELD_LENGTH_STATUS = (short) 128;

  private static final short FIELD_LENGTH_TYPE = (short) 20;

  private static final short FIELD_LENGTH_NAME = (short) 64;

  private final GM_Object2Shape m_channelShaper;

  private final IStatusCollector m_log = new StatusCollector( KalypsoModelWspmTuhhUIPlugin.getID() );

  private final SimpleShapeData m_simpleShapeData;

  private final Feature[] m_exportableElements;

  private final IBanklineMarkerProvider m_markerProvider;

  private final double m_densifyDistance;

  public BanklineExportShapeWorker( final BanklineExportData data )
  {
    final Charset charset = data.getExportCharset();
    final String srs = data.getExportCrs();
    final IDBFField[] fields = createFields();

    m_channelShaper = new GM_Object2Shape( ShapeType.POLYGON, data.getExportCrs() );

    m_simpleShapeData = new SimpleShapeData( charset, srs, ShapeType.POLYGON, fields );
    m_exportableElements = data.getExportableElements();
    m_markerProvider = data.getMarkerChooser();

    if( data.getDensifyEnabled() )
      m_densifyDistance = data.getDensifyDistance();
    else
      m_densifyDistance = Double.NaN;
  }

  private static IDBFField[] createFields( )
  {
    try
    {
      final Collection<IDBFField> fields = new ArrayList<>();

      // TODO: get length from available data
      fields.add( new DBFField( Messages.getString( "BanklineExportShapeWorker_0" ), FieldType.C, FIELD_LENGTH_NAME, (short) 0 ) ); //$NON-NLS-1$
      fields.add( new DBFField( Messages.getString( "BanklineExportShapeWorker_1" ), FieldType.C, FIELD_LENGTH_NAME, (short) 0 ) ); //$NON-NLS-1$
      fields.add( new DBFField( Messages.getString( "BanklineExportShapeWorker_2" ), FieldType.C, FIELD_LENGTH_NAME, (short) 0 ) ); //$NON-NLS-1$
      fields.add( new DBFField( Messages.getString( "BanklineExportShapeWorker_3" ), FieldType.C, FIELD_LENGTH_TYPE, (short) 0 ) ); //$NON-NLS-1$
      fields.add( new DBFField( Messages.getString( "BanklineExportShapeWorker_4" ), FieldType.C, FIELD_LENGTH_STATUS, (short) 0 ) ); //$NON-NLS-1$

      return fields.toArray( new IDBFField[fields.size()] );
    }
    catch( final DBaseException e )
    {
      // shall never happen, else it's a bug!
      throw new IllegalStateException( e );
    }
  }

  public IShapeDataFactory getShapeData( )
  {
    return new SimpleShapeDataFactory( m_simpleShapeData );
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final Feature[] flatElements = flattenExportableElements();

    monitor.beginTask( Messages.getString( "BanklineExportShapeWorker_5" ), flatElements.length ); //$NON-NLS-1$

    for( int i = 0; i < flatElements.length; i++ )
    {
      final Feature feature = flatElements[i];
      monitor.subTask( String.format( "%s (%d/%d)", feature.getName(), i + 1, flatElements.length ) ); //$NON-NLS-1$

      try
      {
        addData( feature, new SubProgressMonitor( monitor, 1 ) );
        m_log.add( IStatus.OK, "%s", null, feature.getName() ); //$NON-NLS-1$
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        m_log.add( IStatus.ERROR, "%s", e, feature.getName() ); //$NON-NLS-1$
      }

      if( monitor.isCanceled() )
        throw new OperationCanceledException();
    }

    final String message = Messages.getString( "BanklineExportShapeWorker_9" ); //$NON-NLS-1$
    return m_log.asMultiStatus( message );
  }

  private Feature[] flattenExportableElements( )
  {
    final Collection<Feature> flatElements = new ArrayList<>();

    for( final Feature feature : m_exportableElements )
    {
      if( feature instanceof WspmWaterBody )
      {
        flatElements.add( feature );

        /* Add all reaches of each selected water body */
        final WspmWaterBody water = (WspmWaterBody) feature;
        final IFeatureBindingCollection<WspmReach> reaches = water.getReaches();
        flatElements.addAll( reaches );
      }
      else if( feature instanceof WspmReach )
        flatElements.add( feature );
      else
      {
        final String message = String.format( Messages.getString( "BanklineExportShapeWorker_10" ), feature.getFeatureType().getQName() ); //$NON-NLS-1$
        m_log.add( IStatus.ERROR, message );
      }
    }

    return flatElements.toArray( new Feature[flatElements.size()] );
  }

  private void addData( final Feature element, final IProgressMonitor monitor ) throws ShapeDataException, GM_Exception
  {
    monitor.beginTask( Messages.getString( "BanklineExportShapeWorker_11" ), 1 ); //$NON-NLS-1$

    // The built geometries are in Kalypso-SRS, because the geometries are derived from the wspm-workspace
    final String kalypsoSrs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final BanklineBuilder exporter = new BanklineBuilder( element, m_markerProvider, m_densifyDistance );

    final IStatus status = exporter.execute( monitor );
    if( !status.isOK() )
      m_log.add( status );

    final Geometry banklineGeometry = exporter.getBanklineGeometry();
    if( banklineGeometry != null )
    {
      final Object[] data = new Object[5];

      final WspmWaterBody water = exporter.getWaterBody();
      final TuhhReach reach = exporter.getReach();
      final String waterName = water == null ? StringUtils.EMPTY : water.getName();
      final String refId = water == null ? StringUtils.EMPTY : water.getRefNr();
      final String reachName = reach == null ? StringUtils.EMPTY : reach.getName();

      data[0] = StringUtils.abbreviate( waterName, FIELD_LENGTH_NAME );
      data[1] = StringUtils.abbreviate( refId, FIELD_LENGTH_NAME );
      data[2] = StringUtils.abbreviate( reachName, FIELD_LENGTH_NAME );
      data[3] = StringUtils.abbreviate( element.getFeatureType().getQName().getLocalPart(), FIELD_LENGTH_TYPE );
      data[4] = StringUtils.abbreviate( status.getMessage(), FIELD_LENGTH_STATUS );

      final List< ? > geometries = GeometryExtracter.extract( banklineGeometry, Polygon.class );
      for( final Object geom : geometries )
        addShapeElement( (Geometry) geom, kalypsoSrs, data );
    }

    monitor.done();
  }

  private void addShapeElement( final Geometry banklineGeometry, final String kalypsoSrs, final Object[] data ) throws ShapeDataException, GM_Exception
  {
    final ISHPGeometry geometry = m_channelShaper.convert( JTSAdapter.wrap( banklineGeometry, kalypsoSrs ) );
    m_simpleShapeData.addRow( geometry, data );
  }
}