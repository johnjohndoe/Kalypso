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
package org.kalypso.core.gml.provider;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ICheckable;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

/**
 * @author Gernot Belger
 */
public class GmlSourceChooserPage extends WizardPage implements IWizardPage
{
  private final IGmlSourceProvider[] m_provider;

  private final GmlSourceContentProvider m_contentProvider = new GmlSourceContentProvider();

  private Object[] m_checkedElements;

  public GmlSourceChooserPage( final String pageName, final String title, final ImageDescriptor titleImage, final IGmlSourceProvider[] provider )
  {
    super( pageName, title, titleImage );

    m_provider = provider;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 2, false ) );

    /* Tree View */
    final CheckboxTreeViewer treeViewer = new CheckboxTreeViewer( panel, SWT.BORDER );
    final GridData treeGridData = new GridData( SWT.FILL, SWT.FILL, false, true );
    treeGridData.widthHint = 250;
    treeViewer.getControl().setLayoutData( treeGridData );

    /* Info Panel */
    final Group infoGroup = new Group( panel, SWT.NONE );
    infoGroup.setText( "Information" );
    infoGroup.setLayout( new FillLayout() );
    final GridData infoGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    infoGroup.setLayoutData( infoGridData );

    treeViewer.setContentProvider( m_contentProvider );
    treeViewer.setLabelProvider( new GmlSourceLabelProvider( m_contentProvider ) );
    treeViewer.setInput( m_provider );

    /* Hook listeners */
    treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( event, infoGroup );
      }
    } );

    treeViewer.addCheckStateListener( new ICheckStateListener()
    {
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        handleCheckStateChanged( event, treeViewer );
      }
    } );

    setControl( panel );

    setPageComplete( false );
  }

  protected void handleCheckStateChanged( final CheckStateChangedEvent event, final CheckboxTreeViewer treeViewer )
  {
    final Object element = event.getElement();

    // Set checkstate of children as well
    // TODO: implementation does not work well, as Dirk for better implementation
    setChildrenCheckstate( event.getCheckable(), element, event.getChecked(), treeViewer );
    treeViewer.update( element, null );

    m_checkedElements = treeViewer.getCheckedElements();

    verifyPage();
  }

  private void setChildrenCheckstate( final ICheckable checkable, final Object element, final boolean checked, final CheckboxTreeViewer treeViewer )
  {
    final Object[] children = m_contentProvider.getChildren( element );
    for( final Object child : children )
    {
      checkable.setChecked( element, checked );
      treeViewer.update( element, null );
      setChildrenCheckstate( checkable, child, checked, treeViewer );
    }
  }

  protected void handleSelectionChanged( final SelectionChangedEvent event, final Group infoGroup )
  {
    final IStructuredSelection selection = (IStructuredSelection) event.getSelection();

    final Object firstElement = selection.getFirstElement();

    /* Update info box for firstElement */
    final Control[] children = infoGroup.getChildren();
    for( final Control control : children )
      control.dispose();

    if( firstElement == null )
    {
      final Label label = new Label( infoGroup, SWT.NONE );
      label.setAlignment( SWT.CENTER );
      label.setText( "Selektieren Sie Elemente aus dem Baum" );
    }
    else
    {
      final IGmlSourceProvider provider = m_contentProvider.getProvider( firstElement );
      provider.createInfoControl( infoGroup, firstElement );
    }

    infoGroup.layout( true, true );
  }

  public IGmlSource[] getChoosenSources( )
  {
    final List<IGmlSource> result = new ArrayList<IGmlSource>();

    for( final Object element : m_checkedElements )
    {
      final IGmlSource source = m_contentProvider.getSource( element );
      if( source != null )
        result.add( source );
    }

    return result.toArray( new IGmlSource[result.size()] );
  }

  private void verifyPage( )
  {
    // REMARK: it is light-weight enough just to create the sources....
    final IGmlSource[] choosenSources = getChoosenSources();
    setPageComplete( choosenSources.length > 0 );

    if( choosenSources.length == 0 )
      setMessage( "W‰hlen Sie bitte mindestens einen Wasserspiegel", WARNING );
    else
      setMessage( null );
  }

}
