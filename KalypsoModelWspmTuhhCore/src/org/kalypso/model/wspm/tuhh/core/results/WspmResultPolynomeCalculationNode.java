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

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.kalypso.contribs.eclipse.core.resources.CollectFolderVisitor;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.gml.CalculationReibConstWspmTuhhSteadyState;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class WspmResultPolynomeCalculationNode extends AbstractWspmResultNode implements ITuhhCalculationNode
{
  private final CalculationReibConstWspmTuhhSteadyState m_calculation;

  public WspmResultPolynomeCalculationNode( final IWspmResultNode parent, final CalculationReibConstWspmTuhhSteadyState calculation )
  {
    super( parent );

    m_calculation = calculation;
  }

  @Override
  protected IWspmResultNode[] createChildren( )
  {
    final Collection<IWspmResultNode> result = new ArrayList<>();

    final IFolder resultFolder = findResultFolder();
    if( resultFolder != null )
    {
      try
      {
        final CollectFolderVisitor dirVisitor = new CollectFolderVisitor( new IFolder[] { resultFolder.getFolder( ".svn" ) } ); //$NON-NLS-1$
        resultFolder.accept( dirVisitor, IResource.DEPTH_ONE, false );
        final IFolder[] folders = dirVisitor.getFolders();
        for( final IFolder folder : folders )
        {
          if( !resultFolder.equals( folder ) )
          {
            result.add( new WspmResultFolderNode( this, folder ) );
          }
        }
      }
      catch( final CoreException e )
      {
        KalypsoModelWspmTuhhCorePlugin.getDefault().getLog().log( e.getStatus() );
      }
    }

    return result.toArray( new IWspmResultNode[result.size()] );
  }

  private IFolder findResultFolder( )
  {
    final GMLWorkspace workspace = m_calculation.getWorkspace();
    if( workspace == null )
      return null;

    final IProject project = ResourceUtilities.findProjectFromURL( workspace.getContext() );
    if( project == null )
      return null;

    final IPath resultPath = m_calculation.getResultFolder();
    return project.getFolder( resultPath );
  }

  @Override
  public String getLabel( )
  {
    final IWspmResultNode[] childResults = getChildResults();
    if( ArrayUtils.isEmpty( childResults ) )
      return String.format( Messages.getString( "WspmResultCalculationNode_0" ), m_calculation.getName() ); //$NON-NLS-1$

    return m_calculation.getName();
  }

  @Override
  protected String getInternalName( )
  {
    return m_calculation.getId();
  }

  @Override
  public Object getObject( )
  {
    return m_calculation;
  }

  @Override
  public TuhhCalculation getCalculation( )
  {
    return m_calculation;
  }
}