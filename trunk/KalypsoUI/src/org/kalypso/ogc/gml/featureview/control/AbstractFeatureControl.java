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
package org.kalypso.ogc.gml.featureview.control;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.util.SafeRunnable;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 */
public abstract class AbstractFeatureControl implements IFeatureControl
{
  private Feature m_feature;

  private final IPropertyType m_ftp;

  private Collection<IFeatureChangeListener> m_changelisteners = new ArrayList<IFeatureChangeListener>();

  public AbstractFeatureControl( final IPropertyType ftp )
  {
    this( null, ftp );
  }

  public AbstractFeatureControl( final Feature feature, final IPropertyType ftp )
  {
    m_feature = feature;
    m_ftp = ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#dispose()
   */
  public void dispose()
  {
    m_changelisteners.clear();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#getFeature()
   */
  public final Feature getFeature()
  {
    return m_feature;
  }

  public void setFeature( final Feature feature )
  {
    m_feature = feature;
  }

  public IPropertyType getFeatureTypeProperty()
  {
    return m_ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addChangeListener(org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public final void addChangeListener( final IFeatureChangeListener l )
  {
    m_changelisteners.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeChangeListener(org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public final void removeChangeListener( final IFeatureChangeListener l )
  {
    m_changelisteners.remove( l );
  }

  protected final void fireFeatureChange( final FeatureChange change )
  {
    if( change == null )
      return;

    final IFeatureChangeListener[] listeners = m_changelisteners
        .toArray( new IFeatureChangeListener[m_changelisteners.size()] );
    for( int i = 0; i < listeners.length; i++ )
    {
      final IFeatureChangeListener listener = listeners[i];
      Platform.run( new SafeRunnable()
      {
        public void run() throws Exception
        {
          listener.featureChanged( change );
        }
      } );
    }
  }

  protected final void fireOpenFeatureRequested( final Feature feature, final IPropertyType ftp )
  {
    final IFeatureChangeListener[] listeners = m_changelisteners.toArray( new IFeatureChangeListener[m_changelisteners.size()] );
    for( int i = 0; i < listeners.length; i++ )
    {
      final IFeatureChangeListener listener = listeners[i];
      Platform.run( new SafeRunnable()
      {
        public void run() throws Exception
        {
          listener.openFeatureRequested( feature, ftp );
        }
      } );
    }
  }
}
