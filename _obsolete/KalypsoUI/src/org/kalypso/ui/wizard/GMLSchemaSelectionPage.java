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
package org.kalypso.ui.wizard;

import java.util.Set;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.i18n.Messages;

/**
 * @author Gernot
 */
public class GMLSchemaSelectionPage extends WizardPage implements ISelectionChangedListener
{
  private String m_namespace;
  private ListViewer m_viewer;

  public GMLSchemaSelectionPage( )
  {
    super( "gmlschemaSelectionPage", Messages.getString("org.kalypso.ui.wizard.GMLSchemaSelectionPage.1"), null ); //$NON-NLS-1$ //$NON-NLS-2$
    
    setPageComplete( false );
    setMessage( Messages.getString("org.kalypso.ui.wizard.GMLSchemaSelectionPage.2") ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    m_viewer = new ListViewer( parent, SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL );
    final ArrayContentProvider provider = new ArrayContentProvider();
    m_viewer.setContentProvider( provider );
    m_viewer.setLabelProvider( new LabelProvider() );
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final Set<String> namespaces = schemaCatalog.getDefaultCatalog().getCatalog().keySet();
    m_viewer.setInput( namespaces );
    ViewerSorter sorter = new ViewerSorter();
    m_viewer.setSorter( sorter );
    final List list = m_viewer.getList();
    list.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    setControl( list );
    m_viewer.addSelectionChangedListener( this );
  }

  public ISelectionProvider getSelectionProvider( )
  {
    return m_viewer;
  }
  
  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
    m_namespace = (String) selection.getFirstElement();

    setPageComplete( m_namespace != null );
  }

  public String getNamespace( )
  {
    return m_namespace;
  }
}
