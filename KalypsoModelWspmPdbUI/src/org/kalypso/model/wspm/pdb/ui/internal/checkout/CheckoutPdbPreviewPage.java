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

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.internal.wspm.CheckoutDataMapping;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.content.ConnectionContentControl;
import org.kalypso.model.wspm.pdb.ui.internal.content.WaterBodyStructure;

/**
 * @author Gernot Belger
 */
public class CheckoutPdbPreviewPage extends WizardPage
{
  private final CheckoutPdbData m_data;

  private DatabindingWizardPage m_binding;

  protected CheckoutPdbPreviewPage( final String pageName, final CheckoutPdbData data )
  {
    super( pageName );

    m_data = data;

    setTitle( "Preview" );
    setDescription( "All shown elements will be downloaded into your local workspace." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    m_binding = new DatabindingWizardPage( this, null, IStatus.ERROR | IStatus.WARNING | IStatus.CANCEL );

    createTreePreview( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createWarningElements( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  private Control createTreePreview( final Composite parent )
  {
    final CheckoutDataMapping mapping = m_data.getMapping();
    final WaterBody[] rootItems = mapping.getWaterBodies();
    final Set<Object> allItems = mapping.getAllPdbElements();
    final Set<Object> allPdbElementsWithWspm = mapping.getAllPdbElementsWithWspm();

    final ILabelDecorator nameDecorator = new CheckoutPdbLabelDecorator( allPdbElementsWithWspm );
    final TreeViewer viewer = ConnectionContentControl.createContentTree( null, parent, nameDecorator );

    viewer.setInput( new WaterBodyStructure( Arrays.asList( rootItems ) ) );
    viewer.addFilter( new CheckoutPdbFilter( allItems ) );

    viewer.expandAll();

    return viewer.getControl();
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

    final StatusComposite statusComposite = new StatusComposite( panel, StatusComposite.HIDE_TEXT );
    statusComposite.setStatus( status );
    statusComposite.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );

    final String message = status.getMessage();

    final Label text = new Label( panel, SWT.WRAP );
    text.setText( message );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    if( confirmProperty == null )
      new Label( panel, SWT.NONE );
    else
    {
      final Button confirmCheck = new Button( panel, SWT.CHECK );
      final ISWTObservableValue target = SWTObservables.observeSelection( confirmCheck );
      final IObservableValue model = BeansObservables.observeValue( m_data, confirmProperty );
      final DataBinder binder = new DataBinder( target, model );
      binder.addTargetAfterGetValidator( new CheckoutPdbConfirmValidator() );
      m_binding.bindValue( binder );
    }
  }

  private IStatus validateSelection( )
  {
    final CheckoutDataMapping mapping = m_data.getMapping();
    final CrossSection[] crossSections = mapping.getCrossSections();
    final Event[] events = mapping.getEvents();
    if( ArrayUtils.isEmpty( crossSections ) && ArrayUtils.isEmpty( events ) )
    {
      final String msg = "Selection does not contain any cross sections or water waterlevels.\nDownload will create an empty water body resp. state.";
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

    final String msg = "The marked elements already exists in the local workspace.\nExisting elements will be overwritten and local changes are lost.";
    return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, msg );
  }
}
