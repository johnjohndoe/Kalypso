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
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResultCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.kalypsodeegree_impl.model.sort.SplitSort;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Dejan Antanaskovic
 */
public class RestartNodes
{
  private final double DEFAULT_SEARCH_DISTANCE = 0.5;

  private final FeatureList m_nodes = new SplitSort( null, null );

  @SuppressWarnings( "unchecked" )
  public void addResultUrl( final URL restartURL ) throws CoreException
  {
    try
    {
      final GMLWorkspace resultWorkspace = GmlSerializer.createGMLWorkspace( restartURL, null );
      final TransformVisitor visitor = new TransformVisitor( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      final Feature rootFeature = resultWorkspace.getRootFeature();
      resultWorkspace.accept( visitor, rootFeature, FeatureVisitor.DEPTH_INFINITE );
      final INodeResultCollection nodeResults = (INodeResultCollection)rootFeature.getAdapter( INodeResultCollection.class );
      for( final INodeResult node : nodeResults.getNodeResults() )
        m_nodes.add( node );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      throw new CoreException( status );
    }
  }

  /**
   * Returns parameters for node at certain position, or nearest (depends on search distance)
   */
  public INodeResult getNodeResultAtPosition( final GM_Point point )
  {
    return getNodeResult( point );
  }

  private INodeResult getNodeResult( final GM_Point point )
  {
    final Feature feature = GeometryUtilities.findNearestFeature( point, DEFAULT_SEARCH_DISTANCE, m_nodes, GMLNodeResult.QNAME_PROP_LOCATION );
    if( feature == null )
      return null;

    return (INodeResult)feature.getAdapter( INodeResult.class );
  }

  public int getSize( )
  {
    if( m_nodes == null )
      return 0;

    return m_nodes.size();
  }
}