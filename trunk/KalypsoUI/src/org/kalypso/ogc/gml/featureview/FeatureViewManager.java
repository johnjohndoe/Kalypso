/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.gml.featureview;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Helper class to manage generated feature-view-templates.
 * 
 * @author Belger
 */
public class FeatureViewManager
{
  private Map<QName, FeatureviewType> m_templates = new HashMap<QName, FeatureviewType>();

  private boolean m_showTables = false;

  /** Generate new templates with or without tables. Cache is cleared. */
  public void setShowTables( final boolean showTable )
  {
    m_showTables = showTable;
    reset();
  }
  
  public boolean isShowTables( )
  {
    return m_showTables;
  }

  /** Return a view tmeplate for the given featureType. If no cached templae is present, a new von is generated. */
  public FeatureviewType get( final IFeatureType featureType, final Feature feature )
  {
    final QName qname = featureType.getQName();
    if( m_templates.containsKey( qname ) )
      return m_templates.get( qname );

    final FeatureviewType newView = FeatureviewHelper.createFeatureviewFromFeatureType( featureType, feature, m_showTables );
    m_templates.put( qname, newView );

    return newView;
  }

  private void reset( )
  {
    m_templates.clear();
  }
}
