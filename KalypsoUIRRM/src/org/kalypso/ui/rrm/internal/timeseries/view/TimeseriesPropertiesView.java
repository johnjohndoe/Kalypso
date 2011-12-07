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
package org.kalypso.ui.rrm.internal.timeseries.view;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.commons.databinding.forms.DatabindingForm;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;

/**
 * @author Gernot Belger
 */
public class TimeseriesPropertiesView extends ViewPart
{
  final static String ID = "org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesPropertiesView"; //$NON-NLS-1$

  private final ISelectionChangedListener m_selectionListener = new ISelectionChangedListener()
  {
    @Override
    public void selectionChanged( final SelectionChangedEvent event )
    {
      handleSelectionChanged( (IStructuredSelection) event.getSelection() );
    }
  };

  private Form m_form;

  private FormToolkit m_toolkit;

  private DatabindingForm m_binding;

  private TimeseriesNode m_node;

  @Override
  public void createPartControl( final Composite parent )
  {
    m_toolkit = ToolkitUtils.createToolkit( parent );
    m_form = m_toolkit.createForm( parent );
    m_toolkit.decorateFormHeading( m_form );

    final Composite body = m_form.getBody();
    GridLayoutFactory.fillDefaults().applyTo( body );

    m_binding = new DatabindingForm( m_form, m_toolkit );

    hookSelection();
  }

  void hookSelection( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    if( workbench.isClosing() )
      return;

    final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
    final IWorkbenchPage page = window.getActivePage();
    if( page == null )
      return;

    final IViewPart managementView = page.findView( TimeseriesManagementView.ID );
    if( managementView == null )
      return;

    final ISelectionProvider selectionProvider = managementView.getViewSite().getSelectionProvider();
    if( selectionProvider == null )
      return;

    selectionProvider.addSelectionChangedListener( m_selectionListener );

    setNode( null );
  }

  private void setNode( final TimeseriesNode node )
  {
    m_node = node;

    updateControl();
  }

  @Override
  public void setFocus( )
  {
    m_form.setFocus();
  }

  private void updateControl( )
  {
    m_binding.deactivate();
    ControlUtils.disposeChildren( m_form.getBody() );
    m_binding.activate();

    if( m_node == null )
      updateNullNode();
    else
      updateNonNullNode( m_node );

    m_form.getBody().layout();
// m_form.reflow( true );
  }

  private void updateNullNode( )
  {
    m_form.setText( "- no element selected -" );
  }

  private void updateNonNullNode( final TimeseriesNode node )
  {
    final ITimeseriesNodeUiHandler uiHandler = node.getUiHandler();

    m_form.setText( uiHandler.getTypeLabel() );

    final Composite body = m_form.getBody();

    final Control control = uiHandler.createControl( m_toolkit, body, m_binding );
    if( control != null )
      control.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    final TimeseriesNode node = findNode( selection );
    setNode( node );
  }

  private TimeseriesNode findNode( final IStructuredSelection selection )
  {
    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof TimeseriesNode )
      return (TimeseriesNode) firstElement;

    return null;
  }

  protected void handlePartOpened( final IWorkbenchPartReference partRef )
  {
    if( TimeseriesManagementView.ID.equals( partRef.getId() ) )
      hookSelection();
  }
}