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
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.io.filefilter.NameFileFilter;
import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.kalypso.contribs.eclipse.core.resources.FileFilterVisitor;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
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

  /**
   * @see org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode#getChildResults()
   */
  @Override
  public IWspmResultNode[] getChildResults( )
  {
    final Collection<IWspmResultNode> result = new ArrayList<IWspmResultNode>();

    final IFolder resultFolder = findResultFolder( m_calculation );
    if( resultFolder != null )
    {
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
      catch( final CoreException e )
      {
        KalypsoModelWspmTuhhCorePlugin.getDefault().getLog().log( e.getStatus() );
      }
    }

    return result.toArray( new IWspmResultNode[result.size()] );
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
      return String.format( Messages.getString("WspmResultCalculationNode_0"), m_calculation.getName() ); //$NON-NLS-1$

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

}
