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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.shape.ShapeDataException;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.deegree.GM_Object2Shape;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypso.shape.shp.SHPException;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
public class SobekShapePoint
{
  private final GM_Object2Shape m_gm2shp;

  private ShapeFile m_shapeFile;

  private final String m_baseName;

  public SobekShapePoint( final IProfileFeature[] profilesToExport, final String baseName )
  {
    m_baseName = baseName;
    final String crs = profilesToExport.length == 0 ? null : profilesToExport[0].getSrsName();
    m_gm2shp = new GM_Object2Shape( ShapeType.POINTZ, crs );
  }

  public void create( final File targetDir ) throws DBaseException, IOException
  {
    final File shapeFileBase = new File( targetDir, m_baseName );
    final String basePath = shapeFileBase.getAbsolutePath();

    final IDBFField[] fields = new IDBFField[2];
    fields[0] = new DBFField( "ID", FieldType.C, (short) 128, (short) 0 ); //$NON-NLS-1$
    fields[1] = new DBFField( "NAME", FieldType.C, (short) 128, (short) 0 ); //$NON-NLS-1$

    m_shapeFile = ShapeFile.create( basePath, ShapeType.POINTZ, Charset.defaultCharset(), fields );
  }

  public void close( ) throws IOException
  {
    if( m_shapeFile != null )
    {
      m_shapeFile.close();
    }
  }

  public void closeQuiet( )
  {
    try
    {
      close();
    }
    catch( final IOException e )
    {
      // ignored
    }
  }

  public void addEntry( final GM_Point lowPoint, final String id, final String name ) throws ShapeDataException, IOException, DBaseException, SHPException
  {
    final ISHPGeometry point = m_gm2shp.convert( lowPoint );
    final Object[] dbfData = new Object[] { id, name };
    m_shapeFile.addFeature( point, dbfData );
  }

}
