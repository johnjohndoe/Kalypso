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
import org.kalypso.kalypsomodel1d2d.schema.binding.Util;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

@SuppressWarnings("unchecked")
/**
 * Default implementation for {@link IFE1D2DContinuityLine} for binding a feature of the type wb1d2d:ContinuityLine
 * 
 * @author Patrice Congo
 */
public class FE1D2DContinuityLine<CT extends IFE1D2DComplexElement, ET extends IFE1D2DEdge> extends LineElement<CT, ET>// Element2D<CT,ET>
    implements IFE1D2DContinuityLine<CT, ET>
{

  public FE1D2DContinuityLine( final Feature featureToBind, QName featureQName, Class<CT> complexElementClass, Class<ET> edgeClass )
  {
    super( featureToBind, featureQName, complexElementClass, edgeClass );
  }

  /**
   * Create a new continuity line binding the provided feature.
   * 
   * @param featureToBind
   *            the feature to bind. null values are illegal
   * @throws IllegalArgumentException
   *             if the passed featureToBind parameter is null
   */
  public FE1D2DContinuityLine( final Feature featureToBind ) throws IllegalArgumentException
  {
    this( featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine, (Class<CT>) IFE1D2DComplexElement.class, (Class<ET>) IFE1D2DEdge.class );
  }

  /**
   * Creates a new continuity line that bind a feature created and linked as property of the provided parent feature.
   * 
   * @param parentFeature
   *            the parent feature of the new to created continnuity line feature
   * 
   * @throws IllegalArgumentException
   *             if the passed parentfeature or the the property QName is null
   */
  public FE1D2DContinuityLine( Feature parentFeature, QName propQName ) throws IllegalArgumentException
  {
    this( parentFeature, propQName, Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine );
  }

  /**
   * Creates a new continuity line that bind a feature created and linked as property of the provided parent feature.
   * The type of the feature to create is specified by the newFeatureQName Q-name
   * 
   * @param parentFeature
   *            the parent feature of the new to created continnuity line feature
   * 
   * @throws IllegalArgumentException
   *             if the passed parentfeature or the the property QName is null
   */
  public FE1D2DContinuityLine( Feature parentFeature, QName propQName, QName newFeatureQName ) throws IllegalArgumentException
  {
// super( parentFeature, propQName, newFeatureQName );
    this( Util.createFeatureAsProperty( parentFeature, propQName, newFeatureQName ) );
  }

  /**
   * Creates a continuity line with a specified GML ID. The parent feature respectively its link to the newly created
   * continuity line are specified as parameters.
   * 
   * @param parentFeature
   *            the parent feature
   * @param propQName
   *            the qname of the property linking the parent feature to the continuity line
   */
  public FE1D2DContinuityLine( Feature parentFeature, QName propQName, String gmlID )
  {
    this( FeatureHelper.createFeatureWithId( Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine, parentFeature, propQName, gmlID ) );
  }

}
