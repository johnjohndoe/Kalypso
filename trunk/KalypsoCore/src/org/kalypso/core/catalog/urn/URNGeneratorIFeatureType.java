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
package org.kalypso.core.catalog.urn;

import javax.xml.namespace.QName;

import org.kalypso.contribs.java.net.URNUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;

/**
 * @author doemming
 */
public class URNGeneratorIFeatureType implements IURNGenerator
{
  /**
   * @see org.kalypso.core.catalog.IURNGenerator#getSupportingClass()
   */
  public Class getSupportingClass( )
  {
    return IFeatureType.class;
  }

  /**
   * @see org.kalypso.core.catalog.IURNGenerator#isURNGeneratorFor(java.lang.Object)
   */
  public boolean isURNGeneratorFor( final Object object )
  {
    if( object instanceof IFeatureType )
      return true;
    if( object instanceof QName )
      return true;
    return false;
  }

  /**
   * @see org.kalypso.core.catalog.IURNGenerator#generateURNFor(java.lang.Object)
   */
  public String generateURNFor( final Object object )
  {
    if( object instanceof IFeatureType )
    {
      final IFeatureType ft = (IFeatureType) object;
      return generateURNFor( ft.getQName() );
    }
    if( object instanceof QName )
    {
      final QName qName = (QName) object;
      final String namespaceURI = qName.getNamespaceURI();
      final String localPart = qName.getLocalPart();
      return "urn:ogc:gml:featuretype:" + URNUtilities.convertURN( namespaceURI ) + ":" + URNUtilities.convertURN( localPart );
    }
    return null;
  }

  /**
   * @see org.kalypso.core.catalog.IURNGenerator#generateURNPatternForRelated(java.lang.Object)
   */
  public String generateURNPatternForRelated( final Object object )
  {
    // nothing
    return null;
  }

  /**
   * @see org.kalypso.core.catalog.IURNGenerator#generateDefaultURNForRelated(java.lang.Object)
   */
  public String generateDefaultURNForRelated( final Object object )
  {
    // nothing
    return null;
  }
}
