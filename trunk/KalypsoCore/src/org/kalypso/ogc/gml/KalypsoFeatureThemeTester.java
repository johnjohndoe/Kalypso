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
package org.kalypso.ogc.gml;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.PropertyTester;
import org.kalypso.gmlschema.feature.IFeatureType;

/**
 * @author Stefan Kurzbach
 */
public class KalypsoFeatureThemeTester extends PropertyTester
{

  private static final String FEATURE_TYPE = "featureType"; //$NON-NLS-1$

  /**
   * @see org.eclipse.core.expressions.IPropertyTester#test(java.lang.Object, java.lang.String, java.lang.Object[],
   *      java.lang.Object)
   */
  public boolean test( final Object receiver, final String property, final Object[] args, final Object expectedValue )
  {
    // ignore args and expectedValue, test if currently active scenario equals to this descriptors scenario
    if( !(receiver instanceof IKalypsoFeatureTheme) )
      return false;
    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) receiver;

    // tests if the feature type qname string representation is equal to the expected value
    if( property.equals( FEATURE_TYPE ) )
    {
      final IFeatureType featureType = theme.getFeatureType();
      // if feature type is null, return false
      // TODO: maybe wait for feature type to become available? blocking might not be feasible though
      if( featureType == null )
        return false;

      final QName qname = featureType.getQName();

      // ignore null qname?
      if( qname == null )
        return false;

      return qname.toString().equals( expectedValue );
    }
    // other properties here elseif
    return false;
  }
}
