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

// OpenGIS (SEAS) dependencies
import org.deegree_impl.model.cs.Projection;

/**
 * Classe de base des projections cartographiques cylindriques. Les projections
 * cylindriques consistent à projeter la surface de la Terre sur un cylindre
 * tangeant ou sécant à la Terre. Les parallèles et mes méridiens apparaissent
 * habituellement comme des lignes droites.
 * 
 * On peut trouver plus de détails sur les projections cylindriques à l'adresse
 * <a
 * href="http://everest.hunter.cuny.edu/mp/cylind.html">http://everest.hunter.cuny.edu/mp/cylind.html
 * </a>.
 * 
 * <p>
 * &nbsp;
 * </p>
 * <p align="center">
 * <img src="doc-files/CylindricalProjection.png">
 * </p>
 * <p align="center">
 * Représentation d'une projection cylindrique <br>
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
abstract class CylindricalProjection extends MapProjection
{
  /**
   * Construct a new map projection from the suplied parameters.
   * 
   * @param parameters
   *          The parameter values in standard units.
   * @throws MissingParameterException
   *           if a mandatory parameter is missing.
   */
  protected CylindricalProjection( final Projection parameters ) throws MissingParameterException
  {
    super( parameters );
  }
}