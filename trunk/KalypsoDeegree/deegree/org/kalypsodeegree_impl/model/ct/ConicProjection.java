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

// OpenGIS (SEAS) dependencies
import org.kalypsodeegree_impl.model.cs.Projection;

/**
 * Classe de base des projections cartographiques coniques. Les projections coniques consistent � projeter la surface de
 * la Terre sur un c�ne tangeant ou s�cant � la Terre. Les parall�les apparaissent habituellement comme des arcs de
 * cercles et les m�ridiens comme des lignes droites. Les projections coniques ne sont pas tr�s utilis�s du fait que
 * leurs distorsions augmentent rapidement � mesure que l'on s'�loigne des parall�les standards. Elles sont plut�t
 * utilis�es pour les r�gions aux latitudes moyennes qui s'�tendent sur une large r�gion d'est en ouest, comme les
 * Etats-Unis.
 * 
 * On peut trouver plus de d�tails sur les projections coniques � l'adresse <a
 * href="http://everest.hunter.cuny.edu/mp/conic.html">http://everest.hunter.cuny.edu/mp/conic.html </a>.
 * 
 * <p>
 * &nbsp;
 * </p>
 * <p align="center">
 * <img src="doc-files/ConicProjection.png">
 * </p>
 * <p align="center">
 * Repr�sentation d'une projection conique <br>
 * 
 * @version 1.0
 * @author Martin Desruisseaux
 */
abstract class ConicProjection extends MapProjection
{
  /**
   * Construct a new map projection from the suplied parameters.
   * 
   * @param parameters
   *          The parameter values in standard units.
   * @throws MissingParameterException
   *           if a mandatory parameter is missing.
   */
  protected ConicProjection( final Projection parameters ) throws MissingParameterException
  {
    super( parameters );
  }
}