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

import javax.xml.namespace.QName;

import org.kalypso.contribs.eclipse.ui.actionfilters.IActionFilterEx;
import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * A filter on {@link org.kalypsodeegree.model.feature.Feature} objects.
 * <p>
 * Tests the if the selected feature substitutes a certain feature type.
 * </p>
 * <p>
 * The value argument is one or more qnames (separated by semicolons ';' of the following for: namespace#localPart.
 * <p>
 * Example: org.kalypso.model.wspm#ProfileReachSegment;org.kalypso.model.wspmprof#Profile
 * </p>
 * 
 * @author Schlienger
 */
public class FeatureTypeActionFilter implements IActionFilterEx
{
  public final static String ATTR_FEATURE_TYPE = "featureType"; //$NON-NLS-1$

  private final boolean m_allowSubstitues;

  private final String m_attr_feature_type;

  public FeatureTypeActionFilter( )
  {
    this( true, ATTR_FEATURE_TYPE );
  }

  protected FeatureTypeActionFilter( final boolean allowSubstitues, final String attr_feature_type )
  {
    m_allowSubstitues = allowSubstitues;
    m_attr_feature_type = attr_feature_type;
  }

  /**
   * @see org.eclipse.ui.IActionFilter#testAttribute(java.lang.Object, java.lang.String, java.lang.String)
   */
  public boolean testAttribute( final Object target, final String name, final String value )
  {
    if( !(target instanceof Feature) )
      return false;

    final Feature f = (Feature) target;

    if( m_attr_feature_type.equals( name ) )
    {
      final String[] strings = value.split( ";" ); //$NON-NLS-1$
      for( final String string : strings )
      {
        final QName qName = QNameUtilities.createQName( string );

        final IFeatureType featureType = f.getFeatureType();
        if( m_allowSubstitues && GMLSchemaUtilities.substitutes( featureType, qName ) )
          return true;
        else if( !m_allowSubstitues && featureType.getQName().equals( qName ) )
          return true;
      }
    }

    return false;
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.actionfilters.IActionFilterEx#getNames()
   */
  public String[] getNames( )
  {
    return new String[] { m_attr_feature_type };
  }
}
