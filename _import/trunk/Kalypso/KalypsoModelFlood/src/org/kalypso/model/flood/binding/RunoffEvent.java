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
package org.kalypso.model.flood.binding;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * @author Thomas Jung
 */
public class RunoffEvent extends AbstractFeatureBinder implements IRunoffEvent
{
  private final FeatureWrapperCollection<ITinReference> m_tinReferences;

  public RunoffEvent( final Feature featureToBind )
  {
    super( featureToBind, IRunoffEvent.QNAME );

    m_tinReferences = new FeatureWrapperCollection<ITinReference>( featureToBind, ITinReference.class, QNAME_PROP_TIN_MEMBER );
  }

  /**
   * @see org.kalypso.model.flood.binding.IRunoffEvent#getResultCoverages()
   */
  public ICoverageCollection getResultCoverages( )
  {
    final Feature coveragesFeature = (Feature) getFeature().getProperty( QNAME_PROP_RESULT_COVERAGES );
    if( coveragesFeature == null )
      return null;

    return (ICoverageCollection) coveragesFeature.getAdapter( ICoverageCollection.class );
  }

  /**
   * @see org.kalypso.model.flood.binding.IRunoffEvent#getTins()
   */
  public IFeatureWrapperCollection<ITinReference> getTins( )
  {
    return m_tinReferences;
  }

  /**
   * @see org.kalypso.model.flood.binding.IRunoffEvent#getDataPath()
   */
  public IPath getDataPath( )
  {
    final String path = (String) getFeature().getProperty( QNAME_PROP_DATAPATH );
    if( path == null )
      return null;

    return Path.fromPortableString( path );
  }

  public void setDataPath( final IPath path )
  {
    getFeature().setProperty( QNAME_PROP_DATAPATH, path.toPortableString() );
  }
}
