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
package org.kalypso.model.wspm.tuhh.ui.export;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.contribs.eclipse.jface.wizard.IFileChooserDelegate;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultContentProvider;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLabelProvider;

/**
 * @author Gernot Belger
 */
public class ExportCsvPage extends ExportFileChooserPage
{
  private final IWspmResultNode m_rootNode;

  private final Set<IWspmResultNode> m_results = new HashSet<IWspmResultNode>();

  public ExportCsvPage( final IFileChooserDelegate fileChooser, final String extension, final IWspmResultNode results )
  {
    super( fileChooser, extension );
    m_rootNode = results;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.ExportFileChooserPage#createPageContent(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected void createPageContent( final Composite parent )
  {
    super.createPageContent( parent );

    final Composite waterlevelGroup = createWaterlevelGroup( parent );
    waterlevelGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  private Composite createWaterlevelGroup( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout() );
    group.setText( "Weitere Daten" );

    // add a tree-table that shows the fetcher data
    final CheckboxTreeViewer treeViewer = new CheckboxTreeViewer( group, SWT.SINGLE | SWT.FULL_SELECTION | SWT.CHECK | SWT.V_SCROLL | SWT.H_SCROLL );
    treeViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    WspmResultContentProvider.initTreeViewer( treeViewer );

    treeViewer.setContentProvider( new WspmResultContentProvider() );
    treeViewer.setLabelProvider( new WspmResultLabelProvider( treeViewer ) );
    treeViewer.setInput( m_rootNode );

    treeViewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final Object element = event.getElement();
        if( element instanceof IWspmResultNode )
        {
          if( event.getChecked() )
            addResultNode( (IWspmResultNode) element );
          else
            removeResultNode( (IWspmResultNode) element );
        }
      }
    } );

    return group;
  }

  protected void addResultNode( final IWspmResultNode node )
  {
    m_results.add( node );
  }

  protected void removeResultNode( final IWspmResultNode node )
  {
    m_results.remove( node );
  }

  public IWspmResultNode[] getResults( )
  {
    return m_results.toArray( new IWspmResultNode[m_results.size()] );
  }
}
