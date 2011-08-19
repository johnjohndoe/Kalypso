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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.pdb.db.constants.WaterBodyConstants.STATIONING_DIRECTION;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.deegree.SHP2GM_Object;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

/**
 * Reads water bodies from a shape file.
 * 
 * @author Gernot Belger
 */
public class ReadWaterBodiesOperation implements ICoreRunnableWithProgress
{
  private WaterBody[] m_waterBodies;

  private final ImportWaterBodiesData m_data;

  private final Map<WaterBody, IStatus> m_waterBodyStatus;

  public ReadWaterBodiesOperation( final ImportWaterBodiesData data, final Map<WaterBody, IStatus> waterBodyStatus )
  {
    m_data = data;
    m_waterBodyStatus = waterBodyStatus;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    m_waterBodyStatus.clear();

    final Collection<WaterBody> wbs = new ArrayList<WaterBody>();

    ShapeFile shapeFile = null;
    try
    {
      shapeFile = m_data.openShape();

      final IDBFField[] fields = shapeFile.getFields();

      for( int row = 0; row < shapeFile.getNumRecords(); row++ )
      {
        final Object[] data = shapeFile.getRow( row );
        final ISHPGeometry shape = shapeFile.getShape( row );
        final WaterBody wb = toWaterBody( shape, data, fields );
        wbs.add( wb );
      }
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Failed to read water bodies from shape", e );
      throw new CoreException( status );
    }

    try
    {
      shapeFile.close();
    }
    catch( final IOException e )
    {
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Failed to read water bodies from shape", e );
      throw new CoreException( status );
    }

    m_waterBodies = wbs.toArray( new WaterBody[wbs.size()] );
    return Status.OK_STATUS;
  }

  public WaterBody[] getWaterBodies( )
  {
    return m_waterBodies;
  }

  private WaterBody toWaterBody( final ISHPGeometry shape, final Object[] data, final IDBFField[] fields ) throws GM_Exception, CoreException
  {
    final WaterBody waterBody = new WaterBody();

    final GM_Object riverlineObject = SHP2GM_Object.transform( m_data.getSrs(), shape );
    final GM_Curve riverline = toCurve( waterBody, riverlineObject );
    waterBody.setRiverline( JTSAdapter.export( riverline ) );

    final ImportAttributeInfo< ? >[] attributeInfos = m_data.getAttributeInfos();
    for( final ImportAttributeInfo< ? > info : attributeInfos )
    {
      final String property = info.getProperty();
      final Object value = findValue( info, fields, data );
      BeanProperties.value( property ).setValue( waterBody, value );
    }

    return waterBody;
  }

  private GM_Curve toCurve( final WaterBody waterBody, final GM_Object object )
  {
    /* We know its a multi curve, because we get it from a polyline shape */
    final GM_MultiCurve multi = (GM_MultiCurve) object;
    final GM_Curve[] allCurves = multi.getAllCurves();
    if( allCurves.length == 0 )
    {
      m_waterBodyStatus.put( waterBody, new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, "Geometry is Null" ) );
      return null;
    }

    if( allCurves.length > 1 )
      m_waterBodyStatus.put( waterBody, new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, "Multi Polyline (using first)" ) );

    return allCurves[0];
  }

  private Object findValue( final ImportAttributeInfo< ? > info, final IDBFField[] fields, final Object[] data ) throws CoreException
  {
    final IDBFField field = info.getField();
    if( field == ImportAttributeInfo.FIELD_USE_DEFAULT )
      return info.getDefaultValue();

    final int fieldIndex = findFieldIndex( field.getName(), fields );
    final Object value = data[fieldIndex];

    if( WaterBody.PROPERTY_DIRECTION_OF_STATIONING.equals( info.getProperty() ) )
      return parseDirection(value);
    if( WaterBody.PROPERTY_RANK.equals( info.getProperty() ) )
      return parseRank( value );

    return value;
  }

  private Integer parseRank( final Object value ) throws CoreException
  {
    if( value == null )
      return null;

    if( value instanceof Number )
      return ((Number) value).intValue();

    final String text = value.toString();
    try
    {
      return Integer.parseInt( text );
    }
    catch( final NumberFormatException e )
    {
      final String msg = String.format( "Failed to parse rank from value '%s'. Value should be a number.", text );
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, msg );
      throw new CoreException( status );
    }
  }

  private STATIONING_DIRECTION parseDirection( final Object value ) throws CoreException
  {
    if( value == null )
      return STATIONING_DIRECTION.upstream;

    try
    {
      return STATIONING_DIRECTION.valueOf( value.toString() );
    }
    catch( final IllegalArgumentException e )
    {
      final String possibleValues = StringUtils.join( STATIONING_DIRECTION.values() );
      final String msg = String.format( "Failed to parse stationing direction from value '%s'. Possible values are: %s", value, possibleValues );
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