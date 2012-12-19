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
package org.kalypso.model.wspm.pdb.ui.internal.checkout;

import java.util.Arrays;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.content.ConnectionContentControl;
import org.kalypso.model.wspm.pdb.ui.internal.content.WaterBodyStructure;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.CheckoutDataMapping;
import org.kalypso.model.wspm.pdb.wspm.CheckoutPdbData;
import org.kalypso.model.wspm.pdb.wspm.CheckoutPdbData.RemoveStrategy;

/**
 * @author Gernot Belger
 */
public class CheckoutPdbPreviewPage extends WizardPage
{
  private final CheckoutPdbData m_data;

  private DatabindingWizardPage m_binding;

  private CheckoutPdbLabelDecorator m_nameDecorator;

  private TreeViewer m_viewer;

  protected CheckoutPdbPreviewPage( final String pageName, final CheckoutPdbData data )
  {
    super( pageName );

    m_data = data;

    setTitle( Messages.getString( "CheckoutPdbPreviewPage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "CheckoutPdbPreviewPage.1" ) ); //$NON-NLS-1$
  }

  @Override
  public IWizardPage getPreviousPage( )
  {
    // not allowed to go back
    return null;
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    m_binding = new DatabindingWizardPage( this, null, IStatus.ERROR | IStatus.WARNING | IStatus.CANCEL );

    createTreePreview( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createWarningElements( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    createOptionsGroup( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
  }

  private Control createTreePreview( final Composite parent )
  {
    m_nameDecorator = new CheckoutPdbLabelDecorator();
    m_viewer = ConnectionContentControl.createContentTree( null, parent, m_nameDecorator );

    updateTreePreview();

    return m_viewer.getControl();
  }

  public void updateTreePreview( )
  {
    final CheckoutDataMapping mapping = m_data.getMapping();
    if( mapping == null )
      return;

    final WaterBody[] rootItems = mapping.getWaterBodies();
    final Set<Object> allItems = mapping.getAllPdbElements();
    final Set<Object> allPdbElementsWithWspm = mapping.getAllPdbElementsWithWspm();
    m_nameDecorator.setElements( allPdbElementsWithWspm );

    m_viewer.setInput( new WaterBodyStructure( Arrays.asList( rootItems ) ) );
    m_viewer.setFilters( new ViewerFilter[] { new CheckoutPdbFilter( allItems ) } );
    m_viewer.expandAll();
  }

  private Control createWarningElements( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( panel );

    addWarning( panel, validateSelection(), null );
    addWarning( panel, validateExistingElements(), CheckoutPdbData.PROPERTY_CONFIRM_EXISTING );

    return panel;
  }

  private void addWarning( final Composite panel, final IStatus status, final String confirmProperty )
  {
    if( status.isOK() )
      return;

    if( confirmProperty != null )
    {
      final Button confirmCheck = new Button( panel, SWT.CHECK );
      confirmCheck.setToolTipText( Messages.getString( "CheckoutPdbPreviewPage.2" ) ); //$NON-NLS-1$
      confirmCheck.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
      final ISWTObservableValue target = SWTObservables.observeSelection( confirmCheck );
      final IObservableValue model = BeansObservables.observeValue( m_data, confirmProperty );
      final DataBinder binder = new DataBinder( target, model );
      binder.addTargetAfterGetValidator( new CheckoutPdbConfirmValidator() );
      m_binding.bindValue( binder );
    }

    final StatusComposite statusComposite = new StatusComposite( panel, StatusComposite.HIDE_TEXT );
    statusComposite.setStatus( status );
    statusComposite.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );

    final String message = status.getMessage();

    final Label text = new Label( panel, SWT.WRAP );
    text.setText( message );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
  }

  private IStatus validateSelection( )
  {
    final CheckoutDataMapping mapping = m_data.getMapping();

    final CrossSection[] crossSections = mapping.getCrossSections();
    final Event[] events = mapping.getEvents();
    if( ArrayUtils.isEmpty( crossSections ) && ArrayUtils.isEmpty( events ) )
    {
      final String msg = Messages.getString( "CheckoutPdbPreviewPage.3" ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, msg );
    }

    return Status.OK_STATUS;
  }

  private IStatus validateExistingElements( )
  {
    final CheckoutDataMapping mapping = m_data.getMapping();
    final Set<Object> existingElements = mapping.getAllPdbElementsWithWspm();
    if( existingElements.isEmpty() )
      return Status.OK_STATUS;

    final String msg = Messages.getString( "CheckoutPdbPreviewPage.4" ); //$NON-NLS-1$
    return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, msg );
  }

  private Control createOptionsGroup( final Composite panel )
  {
    final Group group = new Group( panel, SWT.NONE );
    group.setText( Messages.getString( "CheckoutPdbPreviewPage.5" ) ); //$NON-NLS-1$
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( group );

    new Label( group, SWT.NONE ).setText( Messages.getString( "CheckoutPdbPreviewPage.6" ) ); //$NON-NLS-1$

    final ComboViewer combo = new ComboViewer( group, SWT.READ_ONLY | SWT.DROP_DOWN );
    combo.setContentProvider( new ArrayContentProvider() );
    combo.setLabelProvider( new LabelProvider() );
    combo.setInput( RemoveStrategy.values() );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( combo );
    final IObservableValue model = BeansObservables.observeValue( m_data, CheckoutPdbData.PROPERTY_REMOVE_STRATEGY );
    m_binding.bindValue( target, model );

    return group;
  }
}