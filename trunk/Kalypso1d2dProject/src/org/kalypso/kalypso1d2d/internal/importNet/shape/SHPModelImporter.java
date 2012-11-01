/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypso1d2d.internal.importNet.shape;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;

import org.kalypso.kalypso1d2d.internal.importNet.twodm.ISmsConversionTarget;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypso.shape.shp.SHPException;
import org.kalypso.shape.tools.JTS2SHP;

import com.bce.gis.io.zweidm.IPolygonWithName;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Thomas Jung
 */
class SHPModelImporter implements ISmsConversionTarget
{
  private final File m_file;

  private final ShapeFile m_shapeFile;

  public SHPModelImporter( final File outputFile ) throws IOException, DBaseException
  {
    m_file = outputFile;

    final IDBFField[] fields = new IDBFField[] {};
    m_shapeFile = ShapeFile.create( m_file.getAbsolutePath(), ShapeType.POLYGONZ, Charset.defaultCharset(), fields );
  }

  @Override
  public void addElement( final IPolygonWithName item )
  {
    try
    {
      final Polygon polygon = item.getPolygon();

      final Object[] data = new Object[] {};

      final ISHPGeometry geometry = JTS2SHP.toShape( polygon );
      m_shapeFile.addFeature( geometry, data );
    }
    catch( IOException | DBaseException | SHPException e )
    {
      // FIXME exception handling
      e.printStackTrace();
    }
  }

  @Override
  public void finish( )
  {
    try
    {
      m_shapeFile.close();
      System.out.println( "Wrote shapeFile to:" + m_file.getAbsolutePath() ); //$NON-NLS-1$
    }
    catch( final IOException e )
    {
      // FIXME exception handling
      e.printStackTrace();
    }
  }
}