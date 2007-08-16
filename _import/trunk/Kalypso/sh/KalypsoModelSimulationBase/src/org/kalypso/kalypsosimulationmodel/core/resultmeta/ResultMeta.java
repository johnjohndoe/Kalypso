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
package org.kalypso.kalypsosimulationmodel.core.resultmeta;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Thomas Jung
 * 
 */
public abstract class ResultMeta extends AbstractFeatureBinder implements IResultMeta
{
  private final IFeatureWrapperCollection<IResultMeta> m_children = new FeatureWrapperCollection<IResultMeta>( getFeature(), IResultMeta.class, QNAME_PROP_CHILDREN );

  public ResultMeta( Feature featureToBind, QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#getChildren()
   */
  public IFeatureWrapperCollection<IResultMeta> getChildren( )
  {
    return m_children;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#getParent()
   */
  public IResultMeta getParent( )
  {
    final Feature parentFeature = (Feature) getFeature().getProperty( QNAME_PROP_PARENT );
    if( parentFeature == null )
      return null;

    return (IResultMeta) parentFeature.getAdapter( IResultMeta.class );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#getPath()
   */
  public String getPath( )
  {
    return (String) getWrappedFeature().getProperty( QNAME_PROP_PATH );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#getStatus()
   */
  public IStatus getStatus( )
  {
    final Feature statusFeature = (Feature) getFeature().getProperty( QNAME_PROP_STATUS );
    if( statusFeature == null )
      return null;

    return (IStatus) statusFeature.getAdapter( IStatus.class );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#setParent(org.kalypso.kalypsosimulationmodel.core.result.IResultMeta)
   */
  public void setParent( IResultMeta resultMeta )
  {
    getFeature().setProperty( QNAME_PROP_PARENT, resultMeta.getWrappedFeature() );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#setPath(java.lang.String)
   */
  public void setPath( String path )
  {
    getFeature().setProperty( QNAME_PROP_PATH, path );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#setStatus(org.eclipse.core.runtime.IStatus)
   */
  public void setStatus( IStatus status )
  {
    if( status instanceof org.kalypsodeegree_impl.gml.binding.commons.IStatus )
      getFeature().setProperty( QNAME_PROP_STATUS, ((org.kalypsodeegree_impl.gml.binding.commons.IStatus) status).getWrappedFeature() );
    else
    {
      // TODO: create new Status and copy values to it
    }
  }

}
