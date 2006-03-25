/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.gml;

import java.util.Stack;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.xml.sax.Attributes;

/**
 * @author doemming
 */
public class FeatureParser
{
  private final FeatureTypeProvider m_provider;

  final Stack<Feature> m_stackFE = new Stack<Feature>();

  public FeatureParser( final FeatureTypeProvider provider )
  {
    m_provider = provider;
  }

  public void createFeature( String uri, String localName, Attributes atts )
  {
    final QName qNameFT = new QName( uri, localName );
    final IFeatureType featureType = m_provider.getFeatureType( qNameFT );
    final String fid;
    // GMLContentHandler.print( atts );
    // TODO check for alternatives xml:id gml:fid
    final int fIDIndex = atts.getIndex( "fid" );
    final int gmlIDindex = atts.getIndex(NS.GML2, "id");
//    final int gmlIDindex = atts.getIndex("id");
    if( fIDIndex >= 0 )
      fid = atts.getValue( fIDIndex );
    else if(gmlIDindex>=0)
      fid = atts.getValue( gmlIDindex );
    else
      fid = null; // TODO the ID must be generated AFTER the other elements have been generated, so that it does not
                  // conflict with other ids
    final Feature feature = new Feature_Impl( featureType, fid, false );
    // System.out.println( " | created Feature " + fid + " " + featureType.getQName() );
    m_stackFE.push( feature );
  }

  public Feature getCurrentFeature( )
  {
    if( m_stackFE.empty() )
      return null;
    return m_stackFE.peek();
  }

  public void popFeature( )
  {
    m_stackFE.pop();
  }
}
