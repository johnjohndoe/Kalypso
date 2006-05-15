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
package org.kalypsodeegree_impl.model.feature;

import java.net.MalformedURLException;
import java.net.URL;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureProvider;

/**
 * @author Belger
 */
public class XLinkFeatureProvider implements IFeatureProvider
{
  private final URL m_href;

  private final String m_role;

  private final String m_actuate;

  private final String m_show;

  private final String m_title;

  private final String m_arcrole;

  private Feature m_feature;

  private final Feature m_context;

  private final IFeatureType m_targetFeatureType;

  /**
   * @param context
   *          The context is used to find the feature.
   */
  public XLinkFeatureProvider( final Feature context, final IFeatureType targetFeatureType, final String href, final String role, final String arcrole, final String title, final String show, final String actuate )
  {
    m_context = context;
    m_targetFeatureType = targetFeatureType;
    m_role = role;
    m_arcrole = arcrole;
    m_title = title;
    m_show = show;
    m_actuate = actuate;

    m_href = parseHref( href );
  }

  private static final URL parseHref( final String href )
  {
    try
    {
      return new URL( href );
    }
    catch( final MalformedURLException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();

      return null;
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeatureProvider#getFeatureType()
   */
  public IFeatureType getFeatureType( )
  {
    return m_targetFeatureType;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeatureProvider#getFeature()
   */
  public Feature getFeature( )
  {
    if( m_feature == null )
    {
      final GMLWorkspace contextWorkspace = m_context.getWorkspace();
      final String ref = m_href.getRef();
      m_feature = contextWorkspace.getFeature( ref );

      // TODO: support externally linked features
      // maybe like this:
      // final String path = m_href.getPath();
      // final IWorkspaceLoader loader = contextWorkspace.getAdaptable( IWorkspaceLoader.class );
      // final GmlWorkspace targetWorkspace = loader.loadWorkspace( path );
      // return targetWorkspace.getFature( ref );
      // END TODO
    }

    return m_feature;
  }

  public String getActuate( )
  {
    return m_actuate;
  }

  public String getArcrole( )
  {
    return m_arcrole;
  }

  public URL getHref( )
  {
    return m_href;
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

}
