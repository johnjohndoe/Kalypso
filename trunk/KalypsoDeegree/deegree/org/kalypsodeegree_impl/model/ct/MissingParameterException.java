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
package org.deegree_impl.model.ct;

// Resources
import org.deegree_impl.model.resources.css.ResourceKeys;
import org.deegree_impl.model.resources.css.Resources;

/**
 * Thrown when a parameter was missing. For example, this exception may be
 * thrown when a map projection was requested but the "semi_major" parameter was
 * not specified.
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
public class MissingParameterException extends RuntimeException
{
  /**
   * Serial number for interoperability with different versions.
   */
  private static final long serialVersionUID = 3365753083955970327L;

  /**
   * The missing parameter name.
   */
  private final String parameter;

  /**
   * Constructs an exception with the specified detail message.
   * 
   * @param msg
   *          the detail message, or <code>null</code> to construct a default
   *          message from the missing parameter name.
   * @param parameter
   *          The missing parameter name.
   */
  public MissingParameterException( final String msg, final String parameter )
  {
    super( ( msg != null || parameter == null ) ? msg : Resources.format(
        ResourceKeys.ERROR_MISSING_PARAMETER_$1, parameter ) );
    this.parameter = parameter;
  }

  /**
   * Returns the missing parameter name.
   */
  public String getMissingParameterName()
  {
    return parameter;
  }
}