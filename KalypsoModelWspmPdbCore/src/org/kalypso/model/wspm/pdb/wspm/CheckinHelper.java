/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
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
package org.kalypso.model.wspm.pdb.wspm;

import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Point;

/**
 * @author Holger Albert
 */
public class CheckinHelper
{
  private CheckinHelper( )
  {
  }

  public static Point toPoint( final GM_Point point, final IGeoTransformer transformer ) throws PdbConnectException
  {
    try
    {
      final GM_Object transformedPoint = transformer.transform( point );
      return (Point)JTSAdapter.export( transformedPoint );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new PdbConnectException( CheckinStatePdbOperation.STR_FAILED_TO_CONVERT_GEOMETRY, e );
    }
  }
}