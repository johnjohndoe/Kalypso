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
package org.kalypsodeegree_impl.model.feature.gmlxpath.xelement;

import javax.xml.namespace.QName;

import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * xelement that represents a path-xpath element s *
 * 
 * @author doemming
 */
public class XElementFormPath extends AbstractXElement
{
  private final String m_propName;

  public XElementFormPath( String condition )
  {
    m_propName = condition.trim();
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.xpath.AbstractXElement#evaluateFeature(org.kalypsodeegree.model.feature.Feature,
   *      boolean)
   */
  @Override
  public Object evaluateFeature( final Feature contextFeature, final boolean featureTypeLevel )
  {
    final IFeatureType featureType = contextFeature.getFeatureType();
    
    if( featureTypeLevel )
    {
      // check featureType
      if( "*".equals( m_propName ) || m_propName.equals( featureType.getQName().getLocalPart() ) )
        return contextFeature;
      else
        return null;
    }

    final QName qname = QNameUtilities.fromString( m_propName );
    final IPropertyType pt;
    if( qname == null )
      pt = featureType.getProperty( m_propName );
    else
      pt = featureType.getProperty( qname );
    
    if( pt == null )
      return null;
    
    final Object value = contextFeature.getProperty( pt );
    if( pt instanceof IRelationType )
    {
      if( value instanceof String )
        return null; // xPath would not follow linked associations; why not??
      return value;
    }
    return value;
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.xpath.AbstractXElement#evaluateOther(java.lang.Object, boolean)
   */
  @Override
  public Object evaluateOther( Object context, boolean featureTypeLevel )
  {
    if( context instanceof GMLWorkspace )
    {
      final GMLWorkspace gmlWorkspace = (GMLWorkspace) context;
      return evaluateFeature( gmlWorkspace.getRootFeature(), featureTypeLevel );
    }
    return null;
  }
}
