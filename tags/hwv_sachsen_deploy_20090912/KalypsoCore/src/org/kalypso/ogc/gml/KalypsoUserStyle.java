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

import java.util.Collection;
import java.util.HashSet;

import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.kalypso.contribs.eclipse.core.runtime.SafeRunnable;
import org.kalypso.core.i18n.Messages;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.xml.Marshallable;

/**
 * Wrapped UserStyle to provide fireModellEvent Method
 * 
 * @author bce
 */
public class KalypsoUserStyle implements UserStyle, Marshallable, IWorkbenchAdapter
{
  private final Collection<IKalypsoUserStyleListener> m_listeners = new HashSet<IKalypsoUserStyleListener>();

  protected final String m_styleName;

  private boolean m_disposed = false;

  protected UserStyle m_userStyle;

  public KalypsoUserStyle( final UserStyle style, final String styleName )
  {
    m_userStyle = style;
    m_styleName = styleName;
  }

  /**
   * @see org.kalypsodeegree.xml.Marshallable#exportAsXML()
   */
  public String exportAsXML( )
  {
    return ((Marshallable) m_userStyle).exportAsXML();
  }

  public void addFeatureTypeStyle( final FeatureTypeStyle featureTypeStyle )
  {
    m_userStyle.addFeatureTypeStyle( featureTypeStyle );
  }

  public String getAbstract( )
  {
    return m_userStyle.getAbstract();
  }

  public FeatureTypeStyle[] getFeatureTypeStyles( )
  {
    return m_userStyle.getFeatureTypeStyles();
  }

  public String getName( )
  {
    return m_userStyle.getName();
  }

  public String getTitle( )
  {
    return m_userStyle.getTitle();
  }

  public boolean isDefault( )
  {
    return m_userStyle.isDefault();
  }

  public void removeFeatureTypeStyle( final FeatureTypeStyle featureTypeStyle )
  {
    m_userStyle.removeFeatureTypeStyle( featureTypeStyle );
  }

  public void setAbstract( final String abstract_ )
  {
    m_userStyle.setAbstract( abstract_ );
  }

  public void setDefault( final boolean default_ )
  {
    m_userStyle.setDefault( default_ );
  }

  public void setFeatureTypeStyles( final FeatureTypeStyle[] featureTypeStyles )
  {
    m_userStyle.setFeatureTypeStyles( featureTypeStyles );
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.UserStyle#getFeatureTypeStyle(java.lang.String)
   */
  public FeatureTypeStyle getFeatureTypeStyle( String featureTypeStyleName )
  {
    return m_userStyle.getFeatureTypeStyle( featureTypeStyleName );
  }

  public void setName( final String name )
  {
    m_userStyle.setName( name );
  }

  public void setTitle( final String title )
  {
    m_userStyle.setTitle( title );
  }

  public boolean isDisposed( )
  {
    return m_disposed;
  }

  public void dispose( )
  {
    m_disposed = true;
    m_listeners.clear();
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object o )
  {
    if( o != this )
      throw new IllegalStateException();

    // TODO: chech this..
    return m_userStyle.getFeatureTypeStyles()[0].getRules();
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
   */
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    if( object != this )
      throw new IllegalStateException();

    return null;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
   */
  public String getLabel( final Object o )
  {
    if( o != this )
      throw new IllegalStateException();

    /* If present, the title is the user-firendly label. */
    if( getTitle() != null )
      return getTitle();

    /* Fallback: if no titel is present, take the name (id like). */
    if( getName() != null )
      return getName();

    return Messages.getString("org.kalypso.ogc.gml.KalypsoUserStyle.0"); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getParent(java.lang.Object)
   */
  public Object getParent( final Object o )
  {
    return null;
  }

  /**
   * * Adds a listener to the list of listeners. Has no effect if the same listeners is already registered.
   */
  public void addStyleListener( final IKalypsoUserStyleListener l )
  {
    m_listeners.add( l );
  }

  /**
   * Removes a listener from the list of listeners. Has no effect if the listeners is not registered.
   */
  public void removeStyleListener( final IKalypsoUserStyleListener l )
  {
    m_listeners.remove( l );
  }

  /**
   * Runns the given runnable on every listener in a safe way.
   */
  public void fireStyleChanged( )
  {
    final IKalypsoUserStyleListener[] listeners = m_listeners.toArray( new IKalypsoUserStyleListener[m_listeners.size()] );
    for( final IKalypsoUserStyleListener l : listeners )
    {
      final ISafeRunnable code = new SafeRunnable()
      {
        public void run( ) throws Exception
        {
          l.styleChanged( KalypsoUserStyle.this );
        }
      };

      SafeRunner.run( code );
    }
  }

}
