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
package org.kalypso.ogc.gml.serialize;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureProvider;

/**
 * @author Gernot Belger
 */
public abstract class AbstractXLinkFeatureProvider implements IFeatureProvider
{
  private final String m_role;

  private final String m_actuate;

  private final String m_show;

  private final String m_title;

  private final String m_arcrole;

  private final Feature m_context;

  private final String m_uri;

  /**
   * @param context
   *          The context is used to find the feature.
   */
  public AbstractXLinkFeatureProvider( final Feature context, final String uri, final String role, final String arcrole, final String title, final String show, final String actuate )
  {
    m_context = context;
    m_uri = uri;
    m_role = role;
    m_arcrole = arcrole;
    m_title = title;
    m_show = show;
    m_actuate = actuate;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeatureProvider#getFeature()
   */
  public Feature getFeature( final String featureId )
  {
    if( featureId == null )
      return null;

    final GMLWorkspace workspace = getWorkspace();
    return workspace == null ? null : workspace.getFeature( featureId );
  }
  
  public String getActuate( )
  {
    return m_actuate;
  }

  public String getArcrole( )
  {
    return m_arcrole;
  }

  public String getRole( )
  {
    return m_role;
  }

  public String getShow( )
  {
    return m_show;
  }

  public String getTitle( )
  {
    return m_title;
  }

  protected Feature getContext( )
  {
    return m_context;
  }

  protected String getUri( )
  {
    return m_uri;
  }
}
