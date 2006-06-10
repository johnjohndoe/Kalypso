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

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeLabelProvider;

/**
 * @author Gernot
 */
public class FeatureTypeSelectionPage extends WizardPage implements ISelectionChangedListener
{

  private IFeatureType m_selectedfeatureType = null;

  private ListViewer m_viewer;

  private String m_namespace;

  private FeatureTypeLabelProvider m_featureTypeLabelProvider;

  private ArrayContentProvider m_contentProvider;

  public FeatureTypeSelectionPage( )
  {
    super( "FeatureTypeSelectionPage", "Auswahl des Basis-Elements", null );

    setPageComplete( false );
    setMessage( "W‰hlen Sie das Basis-Element der neuen GML Datei aus." );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    m_viewer = new ListViewer( parent, SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL );
    m_contentProvider = new ArrayContentProvider();
    m_viewer.setContentProvider( m_contentProvider );
    m_featureTypeLabelProvider = new FeatureTypeLabelProvider();
    m_viewer.setLabelProvider( m_featureTypeLabelProvider );
    final ViewerSorter sorter = new ViewerSorter();
    m_viewer.setSorter( sorter );
    final List list = m_viewer.getList();
    list.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    setControl( list );
    m_viewer.addSelectionChangedListener( this );

    m_viewer.addFilter( new ViewerFilter() {

      @Override
      public boolean select( final Viewer viewer, final Object parentElement, final Object element )
      {
        return element instanceof IFeatureType && !((IFeatureType)element).isAbstract();
      }} );
    
    update();
  }
  
  /**
   * @see org.eclipse.jface.dialogs.DialogPage#dispose()
   */
  @Override
  public void dispose( )
  {
    m_featureTypeLabelProvider.dispose();
    m_contentProvider.dispose();
    
    super.dispose();
  }

  public void setNamespace( final String namespace )
  {
    if( m_namespace == null || !m_namespace.equals( namespace ) )
      m_namespace = namespace;
  }

  private void update( )
  {
    final ListViewer viewer = m_viewer;
    final String namespace = m_namespace;

    if( namespace == null )
    {
      m_viewer.setInput( null );
      return;
    }

    final ICoreRunnableWithProgress runnable = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        final GMLSchema schema = GMLSchemaCatalog.getSchema( namespace, (String) null );
        viewer.setInput( schema.getAllFeatureTypes() );
        return Status.OK_STATUS;
      }
    };
    RunnableContextHelper.execute( getContainer(), false, false, runnable );
  }

  /**
   * @see org.eclipse.jface.dialogs.DialogPage#setVisible(boolean)
   */
  @Override
  public void setVisible( boolean visible )
  {
    super.setVisible( visible );

    if( visible == true )
      update();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
    m_selectedfeatureType = (IFeatureType) selection.getFirstElement();

    setPageComplete( m_selectedfeatureType != null );
  }

  public IFeatureType getSelectedFeatureType( )
  {
    return m_selectedfeatureType;
  }

}
