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

import java.io.File;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResultCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.sort.SplitSort;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Dejan Antanaskovic
 */
public class RestartNodes
{
  private static final CS_CoordinateSystem COORDINATE_SYSTEM = KalypsoCorePlugin.getDefault().getCoordinatesSystem();

  public static RestartNodes createRestartNodes( final IFolder scenarioFolder, final IControlModel1D2D controlModel ) throws CoreException
  {
    final List<IRestartInfo> restartInfos = controlModel.getRestartInfos();

    final RestartNodes restartNodes = new RestartNodes();
    for( final IRestartInfo restartInfo : restartInfos )
    {
      final IPath restartFilePath = restartInfo.getRestartFilePath();
      if( restartFilePath == null )
        continue;

      final IFile ifile = scenarioFolder.getFile( restartFilePath );
      final File file = ifile.getLocation().toFile();
      restartNodes.addResultFile( file );
    }

    return restartNodes;
  }

  private final double DEFAULT_SEARCH_DISTANCE = 0.5;

  private final FeatureList m_nodes = new SplitSort( null, null );

  public void addResultFile( final File file ) throws CoreException
  {
    if( file == null )
      throw new CoreException( StatusUtilities.createErrorStatus( "Restart file(s) not defined: " + file.getAbsolutePath() ) );

    if( !file.exists() )
      throw new CoreException( StatusUtilities.createErrorStatus( "Restart file(s) does not exists: " + file.getAbsolutePath() ) );

    try
    {
      final GMLWorkspace resultWorkspace = GmlSerializer.createGMLWorkspace( file.toURL(), null );
      final Feature rootFeature = resultWorkspace.getRootFeature();
      final INodeResultCollection nodeResults = (INodeResultCollection) rootFeature.getAdapter( INodeResultCollection.class );
      for( final INodeResult node : nodeResults )
        m_nodes.add( node.getWrappedFeature() );
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
  public INodeResult getNodeResultAtPosition( final double x, final double y )
  {
    final GM_Point point = GeometryFactory.createGM_Point( x, y, COORDINATE_SYSTEM );
    return getNodeResult( point );
  }

  private INodeResult getNodeResult( final GM_Point point )
  {
    final Feature feature = GeometryUtilities.findNearestFeature( point, DEFAULT_SEARCH_DISTANCE, m_nodes, GMLNodeResult.QNAME_PROP_LOCATION );
    if( feature == null )
      return null;

    return (INodeResult) feature.getAdapter( INodeResult.class );
  }

}
