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
package org.kalypso.ogc.gml.actionfilters;

import org.kalypso.contribs.eclipse.ui.actionfilters.IActionFilterEx;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * A filter on {@link org.kalypsodeegree.model.feature.Feature} objects.
 * <p>
 * Tests the if the selected feature is contained in a list.
 * </p>
 * Argument: 'true' or 'false'. If true, the filter returns true if a feature is contained in a feature-list, if false,
 * it returns true if the feature is NOT inside a feature list (or is the root feature).
 * 
 * @author Schlienger
 */
public class FeatureListElementActionFilter implements IActionFilterEx
{
  public final static String ATTR_FEATURE_TYPE = "featureListElement"; //$NON-NLS-1$

  /**
   * @see org.eclipse.ui.IActionFilter#testAttribute(java.lang.Object, java.lang.String, java.lang.String)
   */
  public boolean testAttribute( final Object target, final String name, final String value )
  {
    if( !(target instanceof Feature) )
      return false;

    final Feature f = (Feature) target;

    if( ATTR_FEATURE_TYPE.equals( name ) )
    {
      final boolean isListElement = Boolean.parseBoolean( value );

      final IRelationType rt = FeatureHelper.findParentRelation( f );
      if( isListElement )
        return rt != null && rt.isList();
      else
        return rt == null || !rt.isList();
    }

    return false;
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.actionfilters.IActionFilterEx#getNames()
   */
  public String[] getNames( )
  {
    return new String[] { ATTR_FEATURE_TYPE };
  }
}
