/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.loader.IPooledObject;
import org.kalypso.loader.LoaderException;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyComparator;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;

/**
 * Wrapped UserStyle to provide fireModellEvent Method
 * 
 * @author doemming
 */
public class GisTemplateUserStyle extends KalypsoUserStyle implements IPoolListener, IPooledObject, IWorkbenchAdapter
{
  private final PoolableObjectType m_styleKey;

  private boolean m_loaded = false;

  public GisTemplateUserStyle( final PoolableObjectType poolableStyleKey, final String styleName )
  {
    super( createDummyStyle( "loading..." ), styleName );
    m_styleKey = poolableStyleKey;
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    pool.addPoolListener( this, m_styleKey );
  }

  /**
   * @return a empty style
   */
  private static UserStyle createDummyStyle( final String name )
  {
    return new UserStyle_Impl( name, "title", "abstract", false, new FeatureTypeStyle[0] ); //$NON-NLS-1$ //$NON-NLS-2$ 
  }

  public GisTemplateUserStyle( final UserStyle style, final String name )
  {
    super( style, name );
    m_styleKey = null;
    m_loaded = true;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    if( KeyComparator.getInstance().compare( m_styleKey, key ) == 0 && newValue != null )
    {
      try
      {
        final StyledLayerDescriptor sld = (StyledLayerDescriptor) newValue;
        m_userStyle = sld.findUserStyle( m_styleName );
        if( m_userStyle == null )
        {
          final String msg = "No user style with name: " + m_styleName + ". Dummy style created instead.";
          System.out.println( msg );
          m_userStyle = createDummyStyle( msg );
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
      m_loaded = true;

      fireStyleChanged();
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    if( KeyComparator.getInstance().compare( m_styleKey, key ) == 0 )
    {
      m_loaded = false;
      m_userStyle = createDummyStyle( "Pool object was invalidated..." );

      fireStyleChanged();
    }
  }

  @Override
  public void dispose( )
  {
    super.dispose();
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    pool.removePoolListener( this );
    m_userStyle = createDummyStyle( "Disposed" );
  }

  /**
   * @see org.kalypso.loader.IPooledObject#isLoaded()
   */
  public boolean isLoaded( )
  {
    return m_loaded;
  }

  /**
   * @param styleType
   */
  public void fillStyleType( final List<Style> stylesList, final Style styleType )
  {
    if( m_styleKey == null )
      return;
    styleType.setActuate( "onRequest" ); //$NON-NLS-1$
    styleType.setHref( m_styleKey.getLocation() );
    styleType.setLinktype( m_styleKey.getType() );
    styleType.setStyle( m_styleName );
    styleType.setType( "simple" ); //$NON-NLS-1$
    stylesList.add( styleType );
  }

  public PoolableObjectType getPoolKey( )
  {
    return m_styleKey;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#dirtyChanged(org.kalypso.util.pool.IPoolableObjectType, boolean)
   */
  public void dirtyChanged( final IPoolableObjectType key, final boolean isDirty )
  {
    // TODO change label according to dirty
  }

  /**
   * @see org.kalypso.ogc.gml.KalypsoUserStyle#getLabel(java.lang.Object)
   */
  @Override
  public String getLabel( final Object o )
  {
    final String label = super.getLabel( o );

    if( isLoaded() )
      return label;

    return label + " (wird geladen...)";
  }

  public void save( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
      Object object = pool.getObject( m_styleKey );
      pool.saveObject( object, monitor );
    }
    catch( LoaderException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
  }
}
