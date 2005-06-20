/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.model.ct;

// OpenGIS dependences (SEAGIS)
import org.kalypsodeegree_impl.model.cs.CoordinateSystem;
import org.kalypsodeegree_impl.model.resources.Utilities;
import org.kalypsodeegree_impl.model.resources.css.ResourceKeys;
import org.kalypsodeegree_impl.model.resources.css.Resources;

/**
 * Thrown when a coordinate transformation can't be created. It may be because there is no known path between source and
 * coordinate systems, or because the requested transformation is not available in the environment.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
public class CannotCreateTransformException extends TransformException
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 5368463308772454145L;

  /**
   * Construct an exception with no detail message.
   */
  public CannotCreateTransformException()
  {}

  /**
   * Construct an exception with the specified detail message.
   */
  public CannotCreateTransformException( final String message )
  {
    super( message );
  }

  /**
   * Construct an exception with a message stating that no transformation path has been found between the specified
   * coordinate system.
   */
  public CannotCreateTransformException( final CoordinateSystem sourceCS, final CoordinateSystem targetCS )
  {
    this( Resources.format( ResourceKeys.ERROR_NO_TRANSFORMATION_PATH_$2, getName( sourceCS ), getName( targetCS ) ) );
  }

  /**
   * Gets a display name for the specified coordinate system.
   */
  private static String getName( final CoordinateSystem cs )
  {
    return Utilities.getShortClassName( cs ) + '(' + cs.getName( null ) + ')';
  }
}