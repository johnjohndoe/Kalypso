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
package org.kalypso.convert.namodel.hydrotope;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.shape.FileMode;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.deegree.SHP2GM_Object;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * @author Gernot Belger
 */
public class ShapeWithHash
{
  private final Map<String, Integer> m_fieldHash = new HashMap<String, Integer>();

  private final File m_shapeFile;

  private ShapeFile m_shape;

  private final String m_crs;

  public ShapeWithHash( final File shapeFile, final String crs )
  {
    m_shapeFile = shapeFile;
    m_crs = crs;
  }

  private ShapeFile getShapeFile( ) throws CoreException
  {
    // lazy load shape
    if( m_shape == null )
    {
      final String shapePath = m_shapeFile.getAbsolutePath();
      try
      {
        final String shapeBase = FilenameUtils.removeExtension( shapePath );
        m_shape = new ShapeFile( shapeBase, ShapeSerializer.getShapeDefaultCharset(), FileMode.READ );
        final DBFField[] fields = m_shape.getFields();
        for( int i = 0; i < fields.length; i++ )
          m_fieldHash.put( fields[i].getName(), i );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        final String msg = String.format( "Failed to load shape '%s'", shapePath );
        throw new CoreException( new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, msg, e ) );
      }
    }

    return m_shape;
  }

  public GM_MultiSurface getGeometry( final int index ) throws CoreException
  {
    try
    {
      final ISHPGeometry shape = getShapeFile().getShape( index );
      final GM_Object geom = SHP2GM_Object.transform( m_crs, shape );

      /* allow for null geometries */
      if( geom == null )
        return null;

      if( geom instanceof GM_MultiSurface )
        return (GM_MultiSurface) geom;

      // FIXME This error message is nonsense!
      throw new NotImplementedException( Messages.getString( "org.kalypso.convert.namodel.hydrotope.LanduseShapeInputDescriptor.2" ) ); //$NON-NLS-1$
    }
    catch( final IOException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.convert.namodel.hydrotope.LanduseShapeInputDescriptor.1", m_shapeFile.getName() ); //$NON-NLS-1$
  }

  public int size( ) throws CoreException
  {
    final ShapeFile shapeFile = getShapeFile();
    return shapeFile.getNumRecords();
  }

  public Object getProperty( final int index, final String property ) throws CoreException
  {
    final Integer column = m_fieldHash.get( property );
    if( column == null )
    {
      final String message = Messages.getString( "org.kalypso.convert.namodel.hydrotope.LanduseShapeInputDescriptor.3", property ); //$NON-NLS-1$
      throw new CoreException( new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, message ) );
    }

    final ShapeFile shapeFile = getShapeFile();
    try
    {
      final Object[] row = shapeFile.getRow( index );
      return row[column.intValue()];
    }
    catch( final Exception e )
    {
      final String message = String.format( "Failed to access shape row '%d', property '%s'", index, property );
      throw new CoreException( new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, message, e ) );
    }
  }

}
