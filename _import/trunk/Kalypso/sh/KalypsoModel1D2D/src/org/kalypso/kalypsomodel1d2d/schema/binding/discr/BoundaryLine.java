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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Default implementation of {@link IBoundaryLine}
 * 
 * @author Patrice Congo
 *
 */

@SuppressWarnings("unchecked")
public class BoundaryLine<    
                CT extends IFE1D2DComplexElement, 
                ET extends IFE1D2DEdge> 
                extends LineElement<CT, ET>
                implements IBoundaryLine<CT, ET>
{
  public BoundaryLine( Feature featureToBind )
  {
    this(
        featureToBind,
        Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE,
        (Class<CT>)IFE1D2DComplexElement.class,
        (Class<ET>)IFE1D2DEdge.class );
  }
  
  /**
   * Creates a new boundary line element binding the specified feature
   * @param featureToBind the feature to bind
   * @param featureQName the required feature q-name
   * @param complexElementClass the target binding class for
   *            containing complex element
   * @param edgeClass the target binding class for edges
   */
  public BoundaryLine( 
              Feature featureToBind, 
              QName featureQName, 
              Class<CT> complexElementClass, 
              Class<ET> edgeClass )
  {
    super( featureToBind, featureQName, complexElementClass, edgeClass );
  }

  /**
   * Creates a boundary line element with a specified GML ID.
   * The parent feature respectively its link to the newly 
   * created continuity line are specified as parameters.
   * @param parentFeature the parent feature
   * @param propQName the q-name of the property linking the
   *    parent feature to the continuity line 
   * @param gmlID the gmlID the newly created id will have
   * @param featureQName the q-name denoting the type of the feature
   * @param complexElementClass the target binding class for containing
   *           complex elements
   * @param edgeClass the target binding class for 
   */
  public BoundaryLine( 
              Feature parentFeature, 
              QName propQName, 
              String gmlID, 
              QName featureQName, 
              Class<CT> complexElementClass, 
              Class<ET> edgeClass )
  {
    super( 
        parentFeature, 
        propQName, 
        gmlID, 
        featureQName, 
        complexElementClass, 
        edgeClass );
  }
  
}
