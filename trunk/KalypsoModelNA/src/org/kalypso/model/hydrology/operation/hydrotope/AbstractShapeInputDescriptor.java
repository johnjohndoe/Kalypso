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
package org.kalypso.model.hydrology.operation.hydrotope;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.operation.hydrotope.AbstractImportOperation.InputDescriptor;
import org.kalypso.shape.FileMode;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.deegree.SHP2GM_Object;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypsodeegree.model.geometry.GM_Object;

public abstract class AbstractShapeInputDescriptor<T extends GM_Object> implements InputDescriptor<T>
{
  private final File m_shapeFile;

  private ShapeFile m_shape;

  private final String m_crs;

  private final Charset m_charset;

  public AbstractShapeInputDescriptor( final File shapeFile, final String crs, final Charset charset )
  {
    m_shapeFile = shapeFile;
    m_crs = crs;
    m_charset = charset;
  }

  @Override
  public final void dispose( )
  {
    if( m_shape == null )
      return;

    try
    {
      m_shape.close();
      m_shape = null;
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
  }

  @Override
  public final String getName( final int index )
  {
    return Integer.toString( index );
  }

  @Override
  public final String getDescription( final int index )
  {
    return Messages.getString( "org.kalypso.model.hydrology.operation.hydrotope.AbstractShapeInputDescriptor.1", m_shapeFile.getName() ); //$NON-NLS-1$
  }

  @SuppressWarnings( "unchecked" )
  @Override
  public final T getGeometry( final int index ) throws CoreException
  {
    try
    {
      final ISHPGeometry shape = getShapeFile().getShape( index );

      final GM_Object geometry = SHP2GM_Object.transform( m_crs, shape );

      /* allow for null geometries */
      return (T)geometry;
    }
    catch( final IOException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  @Override
  public final int size( ) throws CoreException
  {
    try
    {
      return getShapeFile().getNumRecords();
    }
    catch( final IOException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  private ShapeFile getShapeFile( ) throws CoreException
  {
    // lazy load shape
    if( m_shape != null )
      return m_shape;

    try
    {
      final String shapeBase = FileUtilities.nameWithoutExtension( m_shapeFile.getAbsolutePath() );
      m_shape = new ShapeFile( shapeBase, m_charset, FileMode.READ );
    }
    catch( final DBaseException | IOException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }

    return m_shape;
  }

  protected final Object getProperty( final int row, final String property ) throws CoreException
  {
    final ShapeFile shape = getShapeFile();

    try
    {
      return shape.getRowValue( row, property );
    }
    catch( final DBaseException | IOException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
  }
}