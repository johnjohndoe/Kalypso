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
package org.kalypso.ogc.gml.loader;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.serialize.AbstractXLinkFeatureProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyInfo;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureProvider;

/**
 * @author Gernot Belger
 */
public class PooledXLinkFeatureProvider extends AbstractXLinkFeatureProvider implements IFeatureProvider, IPoolListener
{
  private IPoolableObjectType m_key;

  private GMLWorkspace m_workspace;

  /**
   * @param context
   *          The context is used to find the feature.
   */
  public PooledXLinkFeatureProvider( final Feature context, final IFeatureType targetFeatureType, final String href, final String role, final String arcrole, final String title, final String show, final String actuate )
  {
    super( context, targetFeatureType, href, role, arcrole, title, show, actuate );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeatureProvider#dispose()
   */
  public void dispose( )
  {
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    pool.removePoolListener( this );
  }

  

  /**
   * @see org.kalypsodeegree.model.feature.IFeatureProvider#getFeature()
   */
  public Feature getFeature( )
  {
    final String featureId = getFeatureId();

    /* Quickly return if already initialized */
    if( m_workspace != null )
      return m_workspace.getFeature( featureId );

    if( m_key == null )
    {
      final GMLWorkspace contextWorkspace = getContext().getWorkspace();

      /* Immediately handle local features */
      final String uri = getUri();
      
      if( uri == null && featureId != null )
      {
        m_workspace = contextWorkspace;
        return m_workspace.getFeature( featureId );
      }
      else if( uri == null || featureId == null )
        return null;

      m_key = new PoolableObjectType( "gml", uri, contextWorkspace.getContext() );

      final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
      final KeyInfo info = pool.addPoolListener( this, m_key );
      try
      {
        info.join();
      }
      catch( final InterruptedException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoGisPlugin.getDefault().getLog().log( status );
      }
    }

    if( m_workspace != null )
      return m_workspace.getFeature( featureId );

    return null;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    if( key == m_key )
    {
      if( status.isOK() )
        m_workspace = (GMLWorkspace) newValue;
      else
        m_workspace = null;
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    if( key == m_key )
      m_workspace = null;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#dirtyChanged(org.kalypso.util.pool.IPoolableObjectType, boolean)
   */
  public void dirtyChanged( final IPoolableObjectType key, final boolean isDirty )
  {
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#isDisposed()
   */
  public boolean isDisposed( )
  {
    return false;
  }

}
