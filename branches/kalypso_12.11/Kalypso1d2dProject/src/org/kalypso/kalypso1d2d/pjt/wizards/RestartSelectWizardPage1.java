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
package org.kalypso.kalypso1d2d.pjt.wizards;

import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.contribs.eclipse.jface.viewers.tree.CollapseAllTreeItemsAction;
import org.kalypso.contribs.eclipse.jface.viewers.tree.ExpandAllTreeItemsAction;
import org.kalypso.contribs.eclipse.jface.viewers.tree.ITreeViewerProvider;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.ui.wizards.results.IResultControlFactory;
import org.kalypso.ui.wizards.results.ResultMetaInfoViewer;
import org.kalypso.ui.wizards.results.SelectResultData.ShowType;
import org.kalypso.ui.wizards.results.SelectResultTreeComposite;
import org.kalypso.ui.wizards.results.filters.DocumentResultViewerFilter;

/**
 * @author Dejan Antanaskovic
 */
public class RestartSelectWizardPage1 extends WizardPage implements ITreeViewerProvider
{
  private final RestartSelectData m_data;

  // TODO: most use cases of result viewer only need the information about a result node, not creation of a theme. We should separate these concerns.
  private IResultControlFactory m_factory;

  private DatabindingWizardPage m_binding;

  private final SelectResultTreeComposite m_treeComposite;

  public RestartSelectWizardPage1( final String pageName, final RestartSelectData data )
  {
    super( pageName );

    m_data = data;

    m_treeComposite = new SelectResultTreeComposite( m_data );
    m_treeComposite.setFilter( new DocumentResultViewerFilter() );

    // FIXME: actually, selecting results outside the current scenario already works (if not calculating via WPS service).
    // BUT: pre-checking the configured restarts does not work with those elements. This would need reworking the setCheckstate stuff of the page (maybe based on prefix of result url?)
    data.setShowOptions( true );

    setTitle( Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizard.3" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizard.4" ) ); //$NON-NLS-1$

    addAction( new CollapseAllTreeItemsAction( this ) );
    addAction( new ExpandAllTreeItemsAction( this ) );
    addAction( null );
  }

  @Override
  public void dispose( )
  {
    if( m_binding != null )
      m_binding.dispose();

    m_treeComposite.dispose();

    super.dispose();
  }

  /**
   * Adds an action to the toolbar. Must be called before {@link #createControl(Composite)} is called.
   */
  public final void addAction( final IAction action )
  {
    m_treeComposite.addAction( action );
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DatabindingWizardPage( this, null );

    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );

    GridLayoutFactory.swtDefaults().numColumns( 2 ).equalWidth( true ).applyTo( panel );

    /* tree panel */
    final Control treeControl = m_treeComposite.createControls( m_binding, panel, SWT.BORDER );
    treeControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    final TreeViewer treeViewer = m_treeComposite.getTreeViewer();
    treeViewer.setLabelProvider( new RestartSelectLabelProvider( m_data ) );
    treeViewer.getControl().setFocus();

    /* initialize restart elements */
    // REMARK: at this place, so we do not need to update labels of sorter composite
    final ITreeContentProvider contentProvider = (ITreeContentProvider)treeViewer.getContentProvider();
    m_data.initalizeRestartElements( contentProvider );

    /* right panel */
    final Composite rightPanel = new Composite( panel, SWT.NONE );
    rightPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    GridLayoutFactory.fillDefaults().applyTo( rightPanel );

    /* step result list */
    final Group restartResultsGroup = new Group( rightPanel, SWT.NONE );
    restartResultsGroup.setText( Messages.getString("RestartSelectWizardPage1.0") ); //$NON-NLS-1$
    restartResultsGroup.setLayout( new FillLayout() );
    restartResultsGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    final RestartSorterComposite sorterComposite = new RestartSorterComposite( m_binding, restartResultsGroup, m_data );
    sorterComposite.getClass();

    /* Info View for one result */
    final ResultMetaInfoViewer resultViewer = new ResultMetaInfoViewer( m_factory, m_data );
    final Control resultControl = resultViewer.createControl( m_binding, rightPanel );
    resultControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    // REMARK: necessary, else the minimum width of the tree control is not applied
    parent.layout();

    /* add element on double click */
    treeViewer.addDoubleClickListener( new IDoubleClickListener()
    {
      @Override
      public void doubleClick( final DoubleClickEvent event )
      {
        treeDoubleClicked( treeViewer );
      }
    } );

    /* autoexpand restart elements */
    autoexpandRestartElements( treeViewer );
  }

  private void autoexpandRestartElements( final TreeViewer treeViewer )
  {
    /* automatically show alien projects/scenarios if the restart set contanis those */
    final ShowType type = m_data.findShowType( m_data.getRestartResults() );
    m_data.setShowAllType( type );

    /* now content provider contains the right elements, expand them! */
    final ITreeContentProvider contentProvider = (ITreeContentProvider)treeViewer.getContentProvider();

    for( final Object object : m_data.getRestartResultSet() )
    {
      final RestartElement element = (RestartElement)object;

      final IStepResultMeta stepResult = element.getStepResult();

      final Object parentToExpand = contentProvider.getParent( stepResult );
      if( parentToExpand != null )
        treeViewer.expandToLevel( parentToExpand, 1 );
    }
  }

  protected void treeDoubleClicked( final TreeViewer treeViewer )
  {
    final Object treeSelection = m_data.getTreeSelection();
    if( treeSelection instanceof IStepResultMeta )
    {
      final IObservableList resultSet = m_data.getRestartResultSet();

      final RestartElement oldElement = m_data.findRestartElement( (IStepResultMeta)treeSelection );

      if( oldElement == null )
      {
        final RestartElement newElement = m_data.createRestartElement( (IStepResultMeta)treeSelection );
        if( !newElement.isValid() )
        {
          /* war user that result is invalid */
          final String message = String.format( RestartSorterLabelProvider.STR_INVALID_RESULT, newElement.getRestartInfoPath() );
          MessageDialog.openWarning( getShell(), getTitle(), message );
          return;
        }

        resultSet.add( newElement );
        m_data.setSelectedRestart( newElement );
      }
      else
      {
        resultSet.remove( oldElement );
        m_data.setSelectedRestart( null );
      }
    }
    else
    {
      /* toggle expansion */
      treeViewer.setExpandedState( treeSelection, !treeViewer.getExpandedState( treeSelection ) );
    }
  }

  @Override
  public TreeViewer getTreeViewer( )
  {
    return m_treeComposite.getTreeViewer();
  }
}