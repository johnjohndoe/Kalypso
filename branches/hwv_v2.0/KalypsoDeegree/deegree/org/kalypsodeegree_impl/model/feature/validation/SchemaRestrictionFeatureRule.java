/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.feature.validation;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.validation.IFeatureMarkerCollector;
import org.kalypsodeegree.model.feature.validation.IFeatureRule;
import org.kalypsodeegree_impl.model.feature.validation.rules.IRule;
import org.kalypsodeegree_impl.model.feature.validation.rules.RuleFactory;

/**
 * A very generel rule which validates the restrictions defined in the gml application schema.
 * 
 * @author Gernot Belger
 */
public class SchemaRestrictionFeatureRule extends AbstractFeatureRule implements IFeatureRule
{
  /**
   * @see org.kalypsodeegree.model.feature.validation.IFeatureRule#validate(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypsodeegree.model.feature.validation.IFeatureMarkerCollector)
   */
  public void validate( final Feature feature, final IFeatureMarkerCollector collector ) 
  {
    final FeatureVisitor visitor = new FeatureVisitor()
    {
      public boolean visit( final Feature f )
      {
        final IFeatureType featureType = f.getFeatureType();
        final IPropertyType[] properties = featureType.getProperties();
        for( final IPropertyType pt : properties )
        {
          final Object property = f.getProperty( pt );

          final IRule[] rules = RuleFactory.getRules( pt );
          for( final IRule rule : rules )
          {
            final IStatus status = rule.isValid( property );
            if( !status.isOK() )
            {
              try
              {
                collector.createMarker( f, pt, status.matches( IStatus.ERROR ), status.getMessage(), "", "", null );
              }
              catch( final CoreException e )
              {
                // TODO Auto-generated catch block
                e.printStackTrace();
              }
            }
          }
        }

        return true;
      }
    };

    feature.getWorkspace().accept( visitor, feature, FeatureVisitor.DEPTH_INFINITE );
  }

}
