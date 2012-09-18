/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.resources.FileFilterVisitor;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;

/**
 * Represents one result folder like '_aktuell'
 *
 * @author Gernot Belger
 */
public class WspmResultFolderNode extends AbstractWspmResultNode implements ITuhhCalculationNode
{
  private final IFolder m_folder;

  public WspmResultFolderNode( final ITuhhCalculationNode parentNode, final IFolder folder )
  {
    super( parentNode );

    m_folder = folder;
  }

  @Override
  public IWspmResultNode[] getChildResults( )
  {
    final Collection<IWspmResultNode> result = new ArrayList<>();

    try
    {
      /* Collect all results with length sections. */
      final Pattern lsPattern = Pattern.compile( IWspmTuhhConstants.FILE_PATTERN_POLYNOME_LENGTH_SECTIONS_GML );
      final WildcardFileFilter fileFilter = new WildcardFileFilter( IWspmTuhhConstants.FILE_RESULT_POLYNOME_LENGTH_SECTIONS_GML );
      final FileFilterVisitor visitor = new FileFilterVisitor( fileFilter );
      m_folder.accept( visitor );
      final IFile[] files = visitor.getFiles();
      for( final IFile lsFile : files )
      {
        final Matcher matcher = lsPattern.matcher( lsFile.getName() );
        if( matcher.matches() )
        {
          final String label = matcher.group( 1 );

          final IWspmResultNode node = new WspmResultContainer( this, lsFile, label );
          result.add( node );
        }
      }

      /* Collect all results with q interval results. */
      final FileFilterVisitor qIntervalVisitor = new FileFilterVisitor( new WildcardFileFilter( "qIntervallResults*.gml" ) ); //$NON-NLS-1$
      m_folder.accept( qIntervalVisitor );
      final IFile[] qIntervalVisitorFiles = qIntervalVisitor.getFiles();
      for( final IFile qIntervalFile : qIntervalVisitorFiles )
      {
        result.add( new WspmResultQIntervalNode( this, qIntervalFile.getFullPath(), qIntervalFile.getName() ) );
      }
    }
    catch( final CoreException e )
    {
      KalypsoModelWspmTuhhCorePlugin.getDefault().getLog().log( e.getStatus() );
    }

    return result.toArray( new IWspmResultNode[result.size()] );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return m_folder.getName();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.results.AbstractWspmResultNode#getInternalName()
   */
  @Override
  protected String getInternalName( )
  {
    return m_folder.getName();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode#getObject()
   */
  @Override
  public Object getObject( )
  {
    return m_folder;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.results.ITuhhCalculationNode#getCalculation()
   */
  @Override
  public TuhhCalculation getCalculation( )
  {
    return ((ITuhhCalculationNode) getParent()).getCalculation();
  }
}