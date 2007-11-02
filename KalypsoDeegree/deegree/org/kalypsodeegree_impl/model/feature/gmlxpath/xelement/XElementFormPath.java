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

import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;

import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualPropertyUtilities;

/**
 * xelement that represents a path-xpath element s *
 * 
 * @author doemming
 */
public class XElementFormPath extends AbstractXElement
{
  /* QName constant indicating that '*' was given as path. */
  private final static QName QNAME_ALL = new QName( "*", "*", "*" );

  private final QName m_propName;

  public XElementFormPath( final String condition, final NamespaceContext namespaceContext )
  {
    final QName regularQname = QNameUtilities.createQName( condition, namespaceContext );
    if( "*".equals( condition ) )
      m_propName = QNAME_ALL;
    else if( regularQname != null )
    {
      /* Try parse with namespaceContext */
      m_propName = regularQname;
    }
    else
    {
      /*
       * Fallback: maybe it is formatted like {namespaceUri}localPart (which is not regular, but still recognized for
       * backwards compability).
       */
      m_propName = QName.valueOf( condition );
    }

    if( m_propName == null )
      throw new IllegalArgumentException( "Could not parse qname: " + condition );
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
      // TODO: still tests for local part first, remove, if qname is allways correctly used.
      if( m_propName == QNAME_ALL || m_propName.getLocalPart().equals( featureType.getQName().getLocalPart() ) || m_propName.equals( featureType.getQName() ) )
        return contextFeature;
      else
        return null;
    }

    final IPropertyType pt = getPropertyType( featureType, m_propName );
    if( pt == null )
      return null;

    final Object value = contextFeature.getProperty( pt );
    if( pt instanceof IRelationType )
    {
      /* Resolve xlinks */
      if( value instanceof String )
        return contextFeature.getWorkspace().getFeature( (String) value );

      return value;
    }
    return value;
  }

  private IPropertyType getPropertyType( final IFeatureType featureType, final QName qname )
  {
    final String nsuri = qname.getNamespaceURI();
    final String localPart = qname.getLocalPart();

    final IPropertyType pt = nsuri == XMLConstants.NULL_NS_URI ? featureType.getProperty( localPart ) : featureType.getProperty( qname );

    if( pt != null )
      return pt;

    final VirtualFeatureTypeProperty vpt = VirtualPropertyUtilities.getPropertyType( featureType, localPart );
    if( vpt != null )
      return vpt;

    return null;
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.xpath.AbstractXElement#evaluateOther(java.lang.Object, boolean)
   */
  @Override
  public Object evaluateOther( final Object context, final boolean featureTypeLevel )
  {
    if( context instanceof GMLWorkspace )
    {
      final GMLWorkspace gmlWorkspace = (GMLWorkspace) context;
      return evaluateFeature( gmlWorkspace.getRootFeature(), featureTypeLevel );
    }
    return null;
  }

  public QName getQName( )
  {
    return m_propName;
  }
}
