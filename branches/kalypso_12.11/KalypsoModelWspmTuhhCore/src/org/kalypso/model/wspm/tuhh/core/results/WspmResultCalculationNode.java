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

import java.io.FileFilter;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.io.filefilter.NameFileFilter;
import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.resources.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.CollectFolderVisitor;
import org.kalypso.contribs.eclipse.core.resources.FileFilterVisitor;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.gml.CalculationWspmTuhhSteadyState;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class WspmResultCalculationNode extends AbstractWspmResultNode implements ITuhhCalculationNode
{
  private final CalculationWspmTuhhSteadyState m_calculation;

  public WspmResultCalculationNode( final IWspmResultNode parent, final CalculationWspmTuhhSteadyState calculation )
  {
    super( parent );

    m_calculation = calculation;
  }

  @Override
  protected IWspmResultNode[] createChildren( )
  {
    final IFolder resultFolder = findResultFolder( m_calculation );
    if( Objects.isNull( resultFolder ) )
      return new IWspmResultNode[] {};

    final Set<IWspmResultNode> result = new LinkedHashSet<>();

    /* Collect all results with length sections. */
    try
    {
      final FileFilter filter = new NameFileFilter( IWspmTuhhConstants.FILE_RESULT_LENGTH_SECTION_GML );
      final FileFilterVisitor visitor = new FileFilterVisitor( filter );
      resultFolder.accept( visitor );

      final IFile[] files = visitor.getFiles();
      for( final IFile file : files )
      {
        final String label = file.getParent().getParent().getName();
        result.add( new WspmResultContainer( this, file, label ) );
      }
    }
    catch( final Throwable t )
    {
      KalypsoModelWspmTuhhCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( t ) );
    }

    /* Collect all folders. */
    try
    {
      final CollectFolderVisitor dirVisitor = new CollectFolderVisitor( new IFolder[] { resultFolder.getFolder( ".svn" ) } ); //$NON-NLS-1$
      resultFolder.accept( dirVisitor, IResource.DEPTH_ONE, false );

      final IFolder[] folders = dirVisitor.getFolders();
      for( final IFolder folder : folders )
      {
        if( resultFolder.equals( folder ) )
          continue;
        else if( wasAdded( result, folder ) )
          continue;

        final WspmResultFolderNode folderNode = new WspmResultFolderNode( this, folder );
        result.add( folderNode );
      }
    }
    catch( final Throwable t )
    {
      KalypsoModelWspmTuhhCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( t ) );
    }

    return result.toArray( new IWspmResultNode[result.size()] );
  }

  private boolean wasAdded( final Set<IWspmResultNode> result, final IFolder current )
  {
    for( final IWspmResultNode node : result )
    {
      if( !(node instanceof WspmResultContainer) )
        continue;

      final WspmResultContainer container = (WspmResultContainer) node;
      final IFile lengthsection = container.getLengthSectionFile();
      if( current.equals( lengthsection ) )
        return true;

      return FileUtilities.isParent( current, lengthsection );
    }

    return false;
  }

  public static IFolder findResultFolder( final TuhhCalculation calculation )
  {
    final GMLWorkspace workspace = calculation.getWorkspace();
    if( workspace == null )
      return null;

    final IProject project = ResourceUtilities.findProjectFromURL( workspace.getContext() );
    if( project == null )
      return null;

    final IPath resultPath = calculation.getResultFolder();
    return project.getFolder( resultPath );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode#getLabel()
   */
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

  /**
   * @see org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode#getObject()
   */
  @Override
  public Object getObject( )
  {
    return m_calculation;
  }

  @Override
  public CalculationWspmTuhhSteadyState getCalculation( )
  {
    return m_calculation;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.results.AbstractWspmResultNode#toString()
   */
  @Override
  public String toString( )
  {
    return String.format( "%s\n%s", super.toString(), m_calculation.getName() ); //$NON-NLS-1$
  }

}
