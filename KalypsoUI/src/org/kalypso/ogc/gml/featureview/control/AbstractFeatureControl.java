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
import java.util.Iterator;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureControl;

/**
 * @author belger
 */
public abstract class AbstractFeatureControl implements IFeatureControl
{
  private Feature m_feature;

  private final FeatureTypeProperty m_ftp;
  
  private Collection m_changelisteners = new ArrayList();
  
  public AbstractFeatureControl(  )
  {
    this( null, null );
  }

  public AbstractFeatureControl( final FeatureTypeProperty ftp )
  {
    this( null, ftp );
  }

  public AbstractFeatureControl( final Feature feature, final FeatureTypeProperty ftp )
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
  
  public final void setFeature( final Feature feature )
  {
    m_feature = feature;
  }
  
  public FeatureTypeProperty getFeatureTypeProperty()
  {
    return m_ftp;
  }
  
  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addChangeListener(org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public void addChangeListener( final IFeatureChangeListener l )
  {
    m_changelisteners.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeChangeListener(org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public void removeChangeListener( final IFeatureChangeListener l )
  {
    m_changelisteners.remove( l );
  }
  
  protected void fireChange( final FeatureChange change )
  {
    if( change == null )
      return;
    
    for( Iterator iter = m_changelisteners.iterator(); iter.hasNext(); )
      ((IFeatureChangeListener)iter.next()).featureChanged( change );
  }
}
