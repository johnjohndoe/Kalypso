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

import org.apache.commons.lang3.ObjectUtils;
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
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.deegree.SHP2GM_Object;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypsodeegree.model.geometry.GM_Curve;
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
  private static final String STR_FAILED_TO_READ_WATER_BODIES_FROM_SHAPE = Messages.getString( "ReadWaterBodiesOperation.0" ); //$NON-NLS-1$

  // private final SHP2JTS m_shape2Jts = new SHP2JTS( new GeometryFactory() );

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

    final Collection<WaterBody> wbs = new ArrayList<>();

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
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, STR_FAILED_TO_READ_WATER_BODIES_FROM_SHAPE, e );
      throw new CoreException( status );
    }

    try
    {
      shapeFile.close();
    }
    catch( final IOException e )
    {
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, STR_FAILED_TO_READ_WATER_BODIES_FROM_SHAPE, e );
      throw new CoreException( status );
    }

    m_waterBodies = wbs.toArray( new WaterBody[wbs.size()] );
    return Status.OK_STATUS;
  }

  public WaterBody[] getWaterBodies( )
  {
    return m_waterBodies;
  }

  private WaterBody toWaterBody( final ISHPGeometry shape, final Object[] data, final IDBFField[] fields ) throws Exception
  {
    final WaterBody waterBody = new WaterBody();

    // TODO: replace with Shape2JTS
// final String shapeSrs = m_data.getShapeSrs();
// final int shapeSRID = JTSAdapter.toSrid( shapeSrs );
// final Geometry riverline = m_shape2Jts.transform( shapeSRID, shape );

    final GM_Object riverlineObject = SHP2GM_Object.transform( m_data.getShapeSrs(), shape );
    final GM_Curve riverline = toCurve( waterBody, riverlineObject );

    /* Project to SRS of the database */
    final int databaseSRID = m_data.getTargetSRID();
    final String databaseSRS = JTSAdapter.toSrs( databaseSRID );
    final GM_Curve databaseRiverline = (GM_Curve) riverline.transform( databaseSRS );

    waterBody.setRiverline( JTSAdapter.export( databaseRiverline ) );

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
      m_waterBodyStatus.put( waterBody, new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "ReadWaterBodiesOperation.1" ) ) ); //$NON-NLS-1$
      return null;
    }

    if( allCurves.length > 1 )
      m_waterBodyStatus.put( waterBody, new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "ReadWaterBodiesOperation.2" ) ) ); //$NON-NLS-1$

    return allCurves[0];
  }

  private Object findValue( final ImportAttributeInfo< ? > info, final IDBFField[] fields, final Object[] data ) throws CoreException
  {
    final String property = info.getProperty();
    final IDBFField field = info.getField();
    if( field == ImportAttributeInfo.FIELD_USE_DEFAULT )
      return info.getDefaultValue();

    final int fieldIndex = findFieldIndex( field.getName(), fields );
    final Object value = data[fieldIndex];

    switch( property )
    {
      case WaterBody.PROPERTY_DIRECTION_OF_STATIONING:
        return parseDirection( value );

      case WaterBody.PROPERTY_RANK:
        return parseRank( value );

      case WaterBody.PROPERTY_DESCRIPTION:
      case WaterBody.PROPERTY_LABEL:
      case WaterBody.PROPERTY_NAME:
        return parseAsString( value );
    }

    return value;
  }

  private String parseAsString( final Object value )
  {
    if( value instanceof String )
      return (String) value;

    return ObjectUtils.toString( value );
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
      final String msg = String.format( Messages.getString( "ReadWaterBodiesOperation.3" ), text ); //$NON-NLS-1$
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
      final String msg = String.format( Messages.getString( "ReadWaterBodiesOperation.4" ), value, possibleValues ); //$NON-NLS-1$
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