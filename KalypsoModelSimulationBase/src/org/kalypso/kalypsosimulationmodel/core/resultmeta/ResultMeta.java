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

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.kalypso.afgui.model.UnversionedModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Thomas Jung
 *
 */
public abstract class ResultMeta extends UnversionedModel implements IResultMeta
{
  private final IFeatureWrapperCollection<IResultMeta> m_children = new FeatureWrapperCollection<IResultMeta>( getFeature(), IResultMeta.class, QNAME_PROP_CHILDREN );

  public ResultMeta( final Feature featureToBind, final QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#getChildren()
   */
  @Override
  public IFeatureWrapperCollection<IResultMeta> getChildren( )
  {
    return m_children;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#getParent()
   */
  @Override
  public IResultMeta getParent( )
  {
    final Feature parentFeature = getFeature().getParent();
    if( parentFeature == null )
      return null;

    return (IResultMeta) parentFeature.getAdapter( IResultMeta.class );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#getPath()
   */
  @Override
  public IPath getPath( )
  {
    final String path = (String) getFeature().getProperty( QNAME_PROP_PATH );
    if( path == null )
      return null;

    return Path.fromPortableString( path );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#getStatus()
   */
  @Override
  public IStatus getStatus( )
  {
    final Feature statusFeature = (Feature) getFeature().getProperty( QNAME_PROP_STATUS );
    if( statusFeature == null )
      return null;

    return (IStatus) statusFeature.getAdapter( IStatus.class );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#setPath(java.lang.String)
   */
  @Override
  public void setPath( final IPath path )
  {
    getFeature().setProperty( QNAME_PROP_PATH, path.toPortableString() );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.result.IResultMeta#setStatus(org.eclipse.core.runtime.IStatus)
   */
  @Override
  public void setStatus( final IStatus status )
  {
    if( status instanceof org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus )
      getFeature().setProperty( QNAME_PROP_STATUS, ((org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus) status).getFeature() );
    else
    {
      // TODO: create new Status and copy values to it
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta#getFullPath()
   */
  @Override
  public IPath getFullPath( )
  {
    final IPath path = getPath();
    final IResultMeta parent = getParent();
    if( parent == null )
      return path;

    final IPath parentPath = parent.getFullPath();
    if( parentPath == null )
      return null;

    return parentPath.append( path );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta#deleteChild(org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta)
   */
  @Override
  public void removeChild( final IResultMeta result )
  {
    final IFeatureWrapperCollection<IResultMeta> children = getChildren();
    if( children != null )
      children.remove( result );
  }

}
