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
package org.kalypso.model.wspm.tuhh.core.results;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.CalculationReibConstWspmTuhhSteadyState;
import org.kalypso.model.wspm.tuhh.core.gml.CalculationWspmTuhhSteadyState;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public final class WspmResultFactory
{
  private WspmResultFactory( )
  {
    throw new UnsupportedOperationException( "Helper class, do not instantiate" ); //$NON-NLS-1$
  }

  public static final IWspmResultNode createResultNode( final IWspmResultNode parent, final Feature feature )
  {
    if( feature instanceof TuhhReach )
      return new WspmResultReachNode( parent, (TuhhReach) feature );

    if( feature instanceof WspmWaterBody )
      return new WspmResultWaterNode( parent, (WspmWaterBody) feature );

    if( feature instanceof TuhhCalculation )
      return createCalculationNode( parent, (TuhhCalculation) feature );

    if( feature instanceof TuhhWspmProject )
      return new WspmResultProjectNode( (TuhhWspmProject) feature );

    return null;
  }

  public static final IWspmResultNode createCalculationNode( final IWspmResultNode parent, final TuhhCalculation calculation )
  {
    if( calculation instanceof CalculationWspmTuhhSteadyState )
      return new WspmResultCalculationNode( parent, (CalculationWspmTuhhSteadyState) calculation );
    if( calculation instanceof CalculationReibConstWspmTuhhSteadyState )
      return new WspmResultPolynomeCalculationNode( parent, (CalculationReibConstWspmTuhhSteadyState) calculation );

    return null;
  }

  /**
   * This function creates wspm result nodes, containing WSPM projects in the workspace.
   * 
   * @return The root nodes.
   */
  public static final IWspmResultNode[] createRootNodes( ) throws Exception
  {
    List<IWspmResultNode> rootNodes = new ArrayList<IWspmResultNode>();

    IWorkspace workspace = ResourcesPlugin.getWorkspace();
    IWorkspaceRoot root = workspace.getRoot();
    IProject[] projects = root.getProjects();
    for( IProject project : projects )
    {
      if( !project.isOpen() )
        continue;

      IFile wspmFile = project.getFile( "WSPM.gmv" ); //$NON-NLS-1$
      if( !wspmFile.exists() )
        continue;

      IFile modelFile = project.getFile( "modell.gml" ); //$NON-NLS-1$
      if( !modelFile.exists() )
        continue;

      URL modelURL = ResourceUtilities.createURL( modelFile );
      GMLWorkspace modelWorkspace = GmlSerializer.createGMLWorkspace( modelURL, null );
      TuhhWspmProject tuhhWspmProject = (TuhhWspmProject) modelWorkspace.getRootFeature();

      IWspmResultNode rootNode = createResultNode( null, tuhhWspmProject );
      rootNodes.add( rootNode );
    }

    return rootNodes.toArray( new IWspmResultNode[] {} );
  }
}