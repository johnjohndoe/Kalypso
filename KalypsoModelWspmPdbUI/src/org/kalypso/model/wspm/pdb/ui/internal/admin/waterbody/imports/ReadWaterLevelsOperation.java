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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Map;

import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.db.utils.PdbMappingUtils;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypso.shape.tools.SHP2JTS;
import org.kalypso.transformation.transformer.JTSTransformer;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * Reads water levels from a shape file.
 * 
 * @author Gernot Belger
 */
public class ReadWaterLevelsOperation implements ICoreRunnableWithProgress
{
  private static final String STR_FAILED_TO_READ_WATER_LEVELS_FROM_SHAPE = Messages.getString( "ReadWaterLevelsOperation.0" ); //$NON-NLS-1$

  private WaterlevelFixation[] m_waterLevels;

  private final ImportWaterLevelsData m_data;

  private final Map<WaterlevelFixation, IStatus> m_waterLevelStatus;

  private SHP2JTS m_shp2jts;

  private GeometryFactory m_factory;

  private JTSTransformer m_transformer;

  public ReadWaterLevelsOperation( final ImportWaterLevelsData data, final Map<WaterlevelFixation, IStatus> waterLevelStatus )
  {
    m_data = data;
    m_waterLevelStatus = waterLevelStatus;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    m_waterLevelStatus.clear();

    final Collection<WaterlevelFixation> wbs = new ArrayList<>();

    try( ShapeFile shapeFile = m_data.openShape() )
    {
      final IDBFField[] fields = shapeFile.getFields();

      final int numRecords = shapeFile.getNumRecords();

      final String taskName = String.format( "Reading shape file '%s'", m_data.getShapeName() );
      monitor.beginTask( taskName, numRecords );

      final int progressStep = Math.max( 1, numRecords / 1000 );

      for( int row = 0; row < numRecords; row++ )
      {
        final Object[] data = shapeFile.getRow( row );
        final ISHPGeometry shape = shapeFile.getShape( row );
        final WaterlevelFixation wb = toWaterlevelFixation( shape, data, fields );

        final IStatus valid = checkWaterlevel( wb );
        wbs.add( wb );
        m_waterLevelStatus.put( wb, valid );

        if( row % progressStep == 0 )
        {
          ProgressUtilities.worked( monitor, progressStep );
          monitor.subTask( String.format( "row %d", row ) );
        }
      }
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final IOException | DBaseException | MismatchedDimensionException | FactoryException | TransformException e )
    {
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, STR_FAILED_TO_READ_WATER_LEVELS_FROM_SHAPE, e );
      throw new CoreException( status );
    }
//    catch( final Exception e )
//    {
//      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, STR_FAILED_TO_READ_WATER_LEVELS_FROM_SHAPE, e );
//      throw new CoreException( status );
//    }

    m_waterLevels = wbs.toArray( new WaterlevelFixation[wbs.size()] );
    return Status.OK_STATUS;
  }

  private IStatus checkWaterlevel( final WaterlevelFixation wb )
  {
    final IStatusCollector stati = new StatusCollector( WspmPdbUiPlugin.PLUGIN_ID );

    final BigDecimal station = wb.getStation();
    if( station == null )
      stati.add( IStatus.ERROR, Messages.getString( "ReadWaterLevelsOperation.1" ) ); //$NON-NLS-1$

    final Point location = wb.getLocation();
    if( location == null )
      stati.add( IStatus.WARNING, Messages.getString( "ReadWaterLevelsOperation.2" ) ); //$NON-NLS-1$

    if( stati.size() == 1 )
      return stati.getAllStati()[0];

    return stati.asMultiStatusOrOK( Messages.getString( "ReadWaterLevelsOperation.3" ) ); //$NON-NLS-1$
  }

  public WaterlevelFixation[] getWaterBodies( )
  {
    return m_waterLevels;
  }

  private WaterlevelFixation toWaterlevelFixation( final ISHPGeometry shape, final Object[] data, final IDBFField[] fields ) throws CoreException, MismatchedDimensionException, FactoryException, TransformException
  {
    final WaterlevelFixation waterLevel = new WaterlevelFixation();

    final Point point = getLocation( shape );

    final ImportAttributeInfo< ? >[] attributeInfos = m_data.getAttributeInfos();
    for( final ImportAttributeInfo< ? > info : attributeInfos )
    {
      final String property = info.getProperty();
      final Object value = findValue( info, fields, data );
      BeanProperties.value( property ).setValue( waterLevel, value );

      /* Set height value as z to geometry */
      // REMARK: Oracle needs the third ordinate!
      if( WaterlevelFixation.PROPERTY_WATERLEVEL.equals( property ) )
      {
        final BigDecimal height = (BigDecimal)value;
        if( height != null )
          point.getCoordinate().z = height.doubleValue();
      }
    }

    waterLevel.setLocation( point );

    return waterLevel;
  }

  /**
   * Extract location from shape geometry and transform to srs of database.
   */
  private Point getLocation( final ISHPGeometry shape ) throws FactoryException, MismatchedDimensionException, TransformException
  {
    final SHP2JTS shp2jts = getShape2JTS();

    final Geometry geometry = shp2jts.transform( shape );
    if( !(geometry instanceof Point) )
      return null;

    final JTSTransformer transformer = getTransformer();
    return transformer.transform( (Point)geometry );
  }

  private JTSTransformer getTransformer( ) throws FactoryException
  {
    if( m_transformer == null )
    {
      /* init srs stuff */
      final String shapeSRS = m_data.getSrs();
      final int shapeSRID = JTSAdapter.toSrid( shapeSRS );
      final int dbSRID = m_data.getDbSRID();

      m_transformer = new JTSTransformer( shapeSRID, dbSRID );
    }

    return m_transformer;
  }

  private SHP2JTS getShape2JTS( )
  {
    if( m_shp2jts == null )
    {
      final GeometryFactory factory = getFactory();
      m_shp2jts = new SHP2JTS( factory );
    }

    return m_shp2jts;
  }

  private GeometryFactory getFactory( )
  {
    if( m_factory == null )
    {
      final String shapeSRS = m_data.getSrs();
      final int shapeSRID = JTSAdapter.toSrid( shapeSRS );

      m_factory = new GeometryFactory( new PrecisionModel(), shapeSRID );
    }

    return m_factory;
  }

  private Object findValue( final ImportAttributeInfo< ? > info, final IDBFField[] fields, final Object[] data ) throws CoreException
  {
    final IDBFField field = info.getField();
    if( field == ImportAttributeInfo.FIELD_USE_DEFAULT )
      return info.getDefaultValue();

    final int fieldIndex = findFieldIndex( field.getName(), fields );
    final Object value = data[fieldIndex];

    switch( info.getProperty() )
    {
      case WaterlevelFixation.PROPERTY_STATION:
        return parseDecimal( value, field.getName(), WaterlevelFixation.PROPERTY_STATION );

      case WaterlevelFixation.PROPERTY_WATERLEVEL:
        return parseDecimal( value, field.getName(), WaterlevelFixation.PROPERTY_WATERLEVEL );

      case WaterlevelFixation.PROPERTY_DISCHARGE:
        return parseDecimal( value, field.getName(), WaterlevelFixation.PROPERTY_DISCHARGE );

      case WaterlevelFixation.PROPERTY_MEASURMENT_DATE:
        return parseDate( value, field.getName() );

      case WaterlevelFixation.PROPERTY_DESCRIPTION:
        return value == null ? null : value.toString();

      default:
        return value;
    }
  }

  private Date parseDate( final Object value, final String label ) throws CoreException
  {
    if( value == null )
      return null;

    if( value instanceof Date )
      return (Date)value;

    final String msg = String.format( Messages.getString( "ReadWaterLevelsOperation.4" ), label, value ); //$NON-NLS-1$
    final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, msg );
    throw new CoreException( status );
  }

  private BigDecimal parseDecimal( final Object value, final String label, final String precisionProperty ) throws CoreException
  {
    final int scale = PdbMappingUtils.findScale( WaterlevelFixation.class, precisionProperty );

    if( value == null )
      return null;

    if( value instanceof Number )
      return new BigDecimal( ((Number)value).doubleValue() ).setScale( scale, BigDecimal.ROUND_HALF_UP );

    final String text = value.toString();
    try
    {
      return new BigDecimal( text ).setScale( scale, BigDecimal.ROUND_HALF_UP );
    }
    catch( final NumberFormatException e )
    {
      final String msg = String.format( Messages.getString( "ReadWaterLevelsOperation.5" ), label, text ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, msg );
      throw new CoreException( status );
    }
  }

  private int findFieldIndex( final String name, final IDBFField[] fields )
  {
    for( int i = 0; i < fields.length; i++ )
    {
      if( name.equals( fields[i].getName() ) )
        return i;
    }

    throw new IllegalArgumentException();
  }
}