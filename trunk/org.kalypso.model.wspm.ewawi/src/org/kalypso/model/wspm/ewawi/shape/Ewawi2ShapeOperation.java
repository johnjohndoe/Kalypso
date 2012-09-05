/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.ewawi.shape;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.reader.EwawiDirReader;
import org.kalypso.model.wspm.ewawi.shape.writer.EwawiShape244Writer;
import org.kalypso.model.wspm.ewawi.shape.writer.EwawiShape32Writer;
import org.kalypso.model.wspm.ewawi.shape.writer.EwawiShape347Writer;
import org.kalypso.model.wspm.ewawi.shape.writer.EwawiShape348Writer;
import org.kalypso.model.wspm.ewawi.shape.writer.EwawiShape38Writer;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.shp.SHPException;

/**
 * @author Gernot Belger
 */
public class Ewawi2ShapeOperation implements IRunnableWithProgress
{
  private final Ewawi2ShapeData m_data;

  public Ewawi2ShapeOperation( final Ewawi2ShapeData data )
  {
    m_data = data;
  }

  @Override
  public void run( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      /* Get the ewawi plus objects. */
      final EwawiPlus[] ewawiData = readEwawiData();

      /* Read the gew shape. */
      final GewShape gewShape = readGewShape();

      /* Write shape with ID 32. */
      final EwawiShape32Writer writer32 = new EwawiShape32Writer( ewawiData, gewShape );
      writer32.writeShape();

      /* Write shape with ID 38. */
      final EwawiShape38Writer writer38 = new EwawiShape38Writer( ewawiData, gewShape );
      writer38.writeShape();

      /* Write shape with ID 244. */
      final EwawiShape244Writer writer244 = new EwawiShape244Writer( ewawiData, gewShape );
      writer244.writeShape();

      /* Write shape with ID 347. */
      final EwawiShape347Writer writer347 = new EwawiShape347Writer( ewawiData, gewShape );
      writer347.writeShape();

      /* Write shape with ID 348. */
      final EwawiShape348Writer writer348 = new EwawiShape348Writer( ewawiData, gewShape );
      writer348.writeShape();
    }
    catch( DBaseException | IOException | SHPException | EwawiException e )
    {
      throw new InvocationTargetException( e );
    }
  }

  /**
   * Reads all ewawi files from the input dir. Each set of files with the same generated key are put together.
   */
  private EwawiPlus[] readEwawiData( )
  {
    final EwawiDirReader reader = new EwawiDirReader();
    reader.read( m_data.getInputDir() );
    return reader.getData();
  }

  private GewShape readGewShape( ) throws DBaseException, IOException
  {
    final GewShape gewShape = new GewShape( m_data.getGewShape() );
    gewShape.init();

    return gewShape;
  }
}