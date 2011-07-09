/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.pdb.ui.internal.admin.gaf;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.hibernate.Session;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.NotNullValidator;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.gaf.ImportGafData;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.AddWaterBodyAction;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.WaterBodyViewer;
import org.kalypso.model.wspm.pdb.ui.internal.content.filter.WaterBodyFilterControl;

/**
 * @author Gernot Belger
 */
public class ChooseWaterPage extends WizardPage
{
  private final ImportGafData m_data;

  private WaterBodyViewer m_waterBodyViewer;

  private Session m_session;

  private WaterBodyFilterControl m_waterBodyFilterControl;

  ChooseWaterPage( final String pageName, final ImportGafData data )
  {
    super( pageName );

    m_data = data;

    setTitle( "Choose Water Body" );
    setDescription( "Choose the water body into which the profiles will be imported" );
  }

  @Override
  public void dispose( )
  {
    PdbUtils.closeSessionQuietly( m_session );

    super.dispose();
  }

  @Override
  public void createControl( final Composite parent )
  {
    openSession();

    m_waterBodyViewer = new WaterBodyViewer( m_session );

    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    final DatabindingWizardPage binding = new DatabindingWizardPage( this, null );

    final Control tableControl = createWaterBodyTable( binding, panel );
    final GridData tableData = new GridData( SWT.FILL, SWT.FILL, true, true );
    tableData.heightHint = 250;
    tableControl.setLayoutData( tableData );

    createSearchFields( panel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    createActions( panel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Hook search fields with viewer */
    final TableViewer viewer = m_waterBodyViewer.getViewer();
    m_waterBodyFilterControl.setViewer( viewer );
  }

  protected void openSession( )
  {
    try
    {
      m_session = m_data.getConnection().openSession();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private Control createWaterBodyTable( final DatabindingWizardPage binding, final Composite parent )
  {
    final TableViewer waterBodiesViewer = m_waterBodyViewer.createTableViewer( parent );

    /* selection -> data */
    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( waterBodiesViewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, ImportGafData.PROPERTY_WATER_BODY );

    final DataBinder dataBinder = new DataBinder( target, model );

    dataBinder.addTargetAfterGetValidator( new NotNullValidator<WaterBody>( WaterBody.class, IStatus.ERROR, "no water body is selected" ) );
    binding.bindValue( dataBinder );

    return m_waterBodyViewer.getControl();
  }

  private Control createSearchFields( final Composite parent )
  {
    final Group panel = new Group( parent, SWT.NONE );
    panel.setLayout( new FillLayout() );
    panel.setText( "Search" );

    m_waterBodyFilterControl = new WaterBodyFilterControl( null, panel );

    return panel;
  }

  private Control createActions( final Composite panel )
  {
    final Action addAction = new AddWaterBodyAction( m_session, m_waterBodyViewer, "Create New Water Body..." );
    return ActionHyperlink.createHyperlink( null, panel, SWT.NONE, addAction );
  }
}