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
package org.kalypso.ui.rrm.internal.utils.featureTree;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.commons.databinding.forms.DatabindingForm;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class TreePropertiesView extends ViewPart
{
  public final static String ID = "org.kalypso.ui.rrm.internal.utils.featureTree.TreePropertiesView"; //$NON-NLS-1$

  private final ISelectionChangedListener m_selectionListener = new ISelectionChangedListener()
  {
    @Override
    public void selectionChanged( final SelectionChangedEvent event )
    {
      handleSelectionChanged( (IStructuredSelection) event.getSelection() );
    }
  };

  private ScrolledForm m_form;

  private FormToolkit m_toolkit;

  private DatabindingForm m_binding;

  private TreeNode m_node;

  @Override
  public void createPartControl( final Composite parent )
  {
    m_toolkit = ToolkitUtils.createToolkit( parent );
    m_form = m_toolkit.createScrolledForm( parent );
    m_toolkit.decorateFormHeading( m_form.getForm() );

    final Composite body = m_form.getBody();
    GridLayoutFactory.fillDefaults().applyTo( body );

    m_binding = new DatabindingForm( m_form, m_toolkit );
  }

  public void hookSelection( final ISelectionProvider provider )
  {
    provider.addSelectionChangedListener( m_selectionListener );

    setNode( null );
  }

  private void setNode( final TreeNode node )
  {
    if( m_node == node )
      return;

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

    m_form.reflow( true );
  }

  private void updateNullNode( )
  {
    m_form.setText( Messages.getString( "TreePropertiesView_0" ) ); //$NON-NLS-1$
  }

  private void updateNonNullNode( final TreeNode node )
  {
    final ITreeNodeUiHandler uiHandler = node.getUiHandler();

    m_form.setText( uiHandler.getTypeLabel() );

    final Composite body = m_form.getBody();
    body.setLayout( GridLayoutFactory.fillDefaults().create() );

    final Control control = uiHandler.createControl( body, m_binding );
    if( control != null )
      control.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    final TreeNode node = findNode( selection );
    setNode( node );
  }

  private TreeNode findNode( final IStructuredSelection selection )
  {
    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof TreeNode )
      return (TreeNode) firstElement;

    return null;
  }
}