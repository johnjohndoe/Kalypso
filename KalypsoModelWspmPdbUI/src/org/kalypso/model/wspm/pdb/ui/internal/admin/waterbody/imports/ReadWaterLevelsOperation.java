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
import java.util.Date;
import java.util.SortedMap;
import java.util.TreeMap;

import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.hibernate.Session;
import org.hibernatespatial.mgeom.MGeometryFactory;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbExecutorOperation;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.CrossSectionProvider;
import org.kalypso.model.wspm.pdb.wspm.WaterlevelsForStation;
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

  private final SortedMap<BigDecimal, WaterlevelsForStation> m_waterlevels = new TreeMap<>();

  private final ImportWaterLevelsData m_data;

  private SHP2JTS m_shp2jts;

  private GeometryFactory m_factory;

  private JTSTransformer m_transformer;

  private final MGeometryFactory m_sectionFactory;

  public ReadWaterLevelsOperation( final ImportWaterLevelsData data )
  {
    m_data = data;
    m_sectionFactory = new MGeometryFactory( new PrecisionModel(), data.getDbSRID() );
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor );
    progress.beginTask( "Loading waterlevels", 100 );

    reloadSections( m_data, progress.newChild( 5, SubMonitor.SUPPRESS_NONE ) );

    readShapeFile( progress.newChild( 75, SubMonitor.SUPPRESS_NONE ) );

    build2dWaterlevels( progress.newChild( 20, SubMonitor.SUPPRESS_NONE ) );

    return Status.OK_STATUS;
  }

  private void reloadSections( final ImportWaterLevelsData data, final IProgressMonitor monitor )
  {
    final IPdbConnection connection = data.getConnection();

    final IPdbOperation operation = new IPdbOperation()
    {
      @Override
      public String getLabel( )
      {
        return null;
      }

      @Override
      public void execute( final Session session )
      {
        data.reloadCrossSections( session );
      }
    };

    final PdbExecutorOperation runnable = new PdbExecutorOperation( connection, operation, "Failed to load cross section from database" );
    runnable.execute( monitor );
  }

  private void readShapeFile( final IProgressMonitor monitor ) throws CoreException
  {
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

        // TODO: log warning?
        if( wb.getWaterlevel() != null )
          addWaterlevel( wb );

        if( row % progressStep == 0 )
        {
          ProgressUtilities.worked( monitor, progressStep );
          monitor.subTask( String.format( "row %,d", row ) );
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
  }

  private void addWaterlevel( final WaterlevelFixation wb )
  {
    final IStatus valid = checkWaterlevel( wb );

    final BigDecimal station = wb.getStation();
    final WaterlevelsForStation container = getWaterlevelContainer( station );

    container.addWaterlevel( wb, valid );
  }

  private WaterlevelsForStation getWaterlevelContainer( final BigDecimal station )
  {
    final BigDecimal fixedStation;
    if( station == null )
      fixedStation = new BigDecimal( Double.MAX_VALUE );
    else
      fixedStation = station;

    if( !m_waterlevels.containsKey( fixedStation ) )
      m_waterlevels.put( station, new WaterlevelsForStation( fixedStation ) );

    return m_waterlevels.get( fixedStation );
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

  public WaterlevelsForStation[] getWaterlevels( )
  {
    return m_waterlevels.values().toArray( new WaterlevelsForStation[m_waterlevels.values().size()] );
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
        // FIXME: variable unit?
        return parseDecimal( value, field.getName(), WaterlevelFixation.PROPERTY_STATION ).movePointRight( 3 );

      case WaterlevelFixation.PROPERTY_WATERLEVEL:
        final BigDecimal waterlevel = parseDecimal( value, field.getName(), WaterlevelFixation.PROPERTY_WATERLEVEL );
        // FIXME: UGLY!
        if( waterlevel != null && waterlevel.doubleValue() < 0.01 )
          return null;

        return waterlevel;

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
//    final int scale = PdbMappingUtils.findScale( WaterlevelFixation.class, precisionProperty );
    final int scale = 4;

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

  private void build2dWaterlevels( final IProgressMonitor monitor )
  {
    final Event event = m_data.getEvent();
    final String eventName = event.getName();

    monitor.beginTask( "Building 2D Waterlevels", m_waterlevels.size() );

    for( final WaterlevelsForStation waterlevel : m_waterlevels.values() )
    {
      monitor.subTask( waterlevel.getStation().toString() );

      /* find cross section for station */
      final BigDecimal station = waterlevel.getStation();
      final CrossSection section = m_data.getCrossSection( station );

      final CrossSectionProvider sectionProvider = new CrossSectionProvider( section, m_sectionFactory );
      waterlevel.create2DWaterlevels( eventName, sectionProvider );

      ProgressUtilities.worked( monitor, 1 );
    }
  }
}