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
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.deegree.SHP2GM_Object;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Point;

/**
 * Reads water levels from a shape file.
 * 
 * @author Gernot Belger
 */
public class ReadWaterLevelsOperation implements ICoreRunnableWithProgress
{
  private WaterlevelFixation[] m_waterLevels;

  private final ImportWaterLevelsData m_data;

  private final Map<WaterlevelFixation, IStatus> m_waterLevelStatus;

  public ReadWaterLevelsOperation( final ImportWaterLevelsData data, final Map<WaterlevelFixation, IStatus> waterLevelStatus )
  {
    m_data = data;
    m_waterLevelStatus = waterLevelStatus;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    m_waterLevelStatus.clear();

    final Collection<WaterlevelFixation> wbs = new ArrayList<WaterlevelFixation>();

    ShapeFile shapeFile = null;
    try
    {
      shapeFile = m_data.openShape();

      final IDBFField[] fields = shapeFile.getFields();

      for( int row = 0; row < shapeFile.getNumRecords(); row++ )
      {
        final Object[] data = shapeFile.getRow( row );
        final ISHPGeometry shape = shapeFile.getShape( row );
        final WaterlevelFixation wb = toWaterlevelFixation( shape, data, fields );

        final IStatus valid = checkWaterlevel(wb);
        wbs.add( wb );
        m_waterLevelStatus.put( wb, valid );
      }
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Failed to read water levels from shape", e );
      throw new CoreException( status );
    }

    try
    {
      shapeFile.close();
    }
    catch( final IOException e )
    {
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Failed to read water levels from shape", e );
      throw new CoreException( status );
    }

    m_waterLevels = wbs.toArray( new WaterlevelFixation[wbs.size()] );
    return Status.OK_STATUS;
  }

  private IStatus checkWaterlevel( final WaterlevelFixation wb )
  {
    final IStatusCollector stati = new StatusCollector( WspmPdbUiPlugin.PLUGIN_ID );

    final BigDecimal station = wb.getStation();
    if( station == null )
      stati.add( IStatus.ERROR, "Missing station value" );

    final Point location = wb.getLocation();
    if( location == null )
      stati.add( IStatus.WARNING, "Missing geometry value" );

    if( stati.size() == 1 )
      return stati.getAllStati()[0];

    return stati.asMultiStatusOrOK( "Mehrere Warnungen" );
  }

  public WaterlevelFixation[] getWaterBodies( )
  {
    return m_waterLevels;
  }

  private WaterlevelFixation toWaterlevelFixation( final ISHPGeometry shape, final Object[] data, final IDBFField[] fields ) throws GM_Exception, CoreException
  {
    final WaterlevelFixation waterLevel = new WaterlevelFixation();

    final GM_Point location = (GM_Point) SHP2GM_Object.transform( m_data.getSrs(), shape );
    final Point point = location == null ? null : (Point) JTSAdapter.export( location );
    waterLevel.setLocation( point );

    final ImportAttributeInfo< ? >[] attributeInfos = m_data.getAttributeInfos();
    for( final ImportAttributeInfo< ? > info : attributeInfos )
    {
      final String property = info.getProperty();
      final Object value = findValue( info, fields, data );
      BeanProperties.value( property ).setValue( waterLevel, value );
    }

    return waterLevel;
  }

  private Object findValue( final ImportAttributeInfo< ? > info, final IDBFField[] fields, final Object[] data ) throws CoreException
  {
    final IDBFField field = info.getField();
    if( field == ImportAttributeInfo.FIELD_USE_DEFAULT )
      return info.getDefaultValue();

    final int fieldIndex = findFieldIndex( field.getName(), fields );
    final Object value = data[fieldIndex];

    if( WaterlevelFixation.PROPERTY_STATION.equals( info.getProperty() ) )
      return parseDecimal( value, field.getName(), 4 );
    if( WaterlevelFixation.PROPERTY_WATERLEVEL.equals( info.getProperty() ) )
      return parseDecimal( value, field.getName(), 2 );
    if( WaterlevelFixation.PROPERTY_DISCHARGE.equals( info.getProperty() ) )
      return parseDecimal( value, field.getName(), 3 );

    if( WaterlevelFixation.PROPERTY_MEASURMENT_DATE.equals( info.getProperty() ) )
      return parseDate( value, field.getName() );

    if( WaterlevelFixation.PROPERTY_DESCRIPTION.equals( info.getProperty() ) )
      return value == null ? null : value.toString();

    return value;
  }

  private Date parseDate( final Object value, final String label ) throws CoreException
  {
    if( value == null )
      return null;

    if( value instanceof Date )
      return (Date) value;

    final String msg = String.format( "Failed to read value of column '%s': '%s'. Value should be a date.", label, value );
    final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, msg );
    throw new CoreException( status );
  }

  private BigDecimal parseDecimal( final Object value, final String label, final int scale ) throws CoreException
  {
    if( value == null )
      return null;

    if( value instanceof Number )
      return new BigDecimal( ((Number) value).doubleValue() ).setScale( scale, BigDecimal.ROUND_HALF_UP );

    final String text = value.toString();
    try
    {
      return new BigDecimal( text ).setScale( scale, BigDecimal.ROUND_HALF_UP );
    }
    catch( final NumberFormatException e )
    {
      final String msg = String.format( "Failed to parse value of column '%s': '%s'. Value should be a number.", label, text );
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