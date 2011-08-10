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
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.ColumnLayout;
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
    setDescription( "These are the elements that will be downloaded into your local workspace." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    m_binding = new DatabindingWizardPage( this, null );

    createTreePreview( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createWarningElements( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  private Control createTreePreview( final Composite parent )
  {
    final TreeViewer viewer = ConnectionContentControl.createContentTree( null, parent );

    final CheckoutDataMapping mapping = m_data.getMapping();
    final WaterBody[] rootItems = mapping.getWaterBodies();
    final Set<Object> allItems = mapping.getAllPdbElements();

    viewer.setInput( new WaterBodyStructure( Arrays.asList( rootItems ) ) );
    viewer.addFilter( new CheckoutPdbFilter( allItems ) );

    viewer.expandAll();

    return viewer.getControl();
  }

  private Control createWarningElements( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    final ColumnLayout layout = new ColumnLayout();
    layout.maxNumColumns = 1;
    panel.setLayout( layout );

    addWarning( panel, validateSelection() );
    addWarning( panel, validateExistingElements() );

    return panel;
  }

  private void addWarning( final Composite panel, final IStatus status )
  {
    if( status.isOK() )
      return;

    new StatusComposite( panel, SWT.NONE ).setStatus( status );
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
