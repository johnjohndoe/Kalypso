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
package org.kalypso.model.wspm.pdb.ui.internal.admin.gaf;

import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.NotNullValidator;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.gaf.GafCode;
import org.kalypso.model.wspm.pdb.gaf.GafPointCheck;
import org.kalypso.model.wspm.pdb.gaf.ImportGafData;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class GafOptionsPage extends WizardPage
{
  private final ImportGafData m_data;

  private StatusComposite m_readStatusComposite;

  private ScrolledForm m_form;

  private DatabindingWizardPage m_binding;

  protected GafOptionsPage( final String pageName, final ImportGafData data )
  {
    super( pageName );

    m_data = data;

    setTitle( Messages.getString( "GafOptionsPage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "GafOptionsPage.1" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    m_binding = new DatabindingWizardPage( this, null );

    createReadStatusControl( panel );

    m_form = new ScrolledForm( panel );
    m_form.setExpandHorizontal( true );
    m_form.setExpandVertical( true );
    m_form.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    GridLayoutFactory.swtDefaults().applyTo( m_form.getBody() );
  }

  private void createReadStatusControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setText( Messages.getString( "GafOptionsPage.2" ) ); //$NON-NLS-1$
    group.setLayout( new FillLayout() );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    m_readStatusComposite = new StatusComposite( group, StatusComposite.DETAILS );
  }

  public void updateControl( )
  {
    final IStatus readGafStatus = m_data.getReadGafStatus();
    m_readStatusComposite.setStatus( readGafStatus );

    final Composite body = m_form.getBody();
    ControlUtils.disposeChildren( body );

    createCodeGroup( body );
    createHykGroup( body );
    createRoughnesGroup( body );
    createVegetationGroup( body );

    m_form.reflow( true );
  }

  private void createCodeGroup( final Composite parent )
  {
    final GafPointCheck checker = m_data.getPointChecker();
    final String[] unknownCodes = checker.getUnknownCodes();

    final Group group = createUnknownGroup( parent );
    group.setText( Messages.getString( "GafOptionsPage.3" ) ); //$NON-NLS-1$

    if( unknownCodes.length == 0 )
    {
      createOkCodeControl( group, Messages.getString( "GafOptionsPage.4" ) ); //$NON-NLS-1$
      return;
    }

    final GafCode[] availableCodes = checker.getAvailableCodes();

    for( final String code : unknownCodes )
    {
      new Label( group, SWT.NONE ).setText( String.format( "\"%s\" = ", code ) ); //$NON-NLS-1$

      final IObservableValue model = new CodeObservableValue( checker, code );
      createCodeMappingCombo( group, availableCodes, model );
    }
  }

  private void createHykGroup( final Composite parent )
  {
    final GafPointCheck checker = m_data.getPointChecker();
    final String[] unknownCodes = checker.getUnknownHyks();

    final Group group = createUnknownGroup( parent );
    group.setText( Messages.getString( "GafOptionsPage.6" ) ); //$NON-NLS-1$

    if( unknownCodes.length == 0 )
    {
      createOkCodeControl( group, Messages.getString( "GafOptionsPage.7" ) ); //$NON-NLS-1$
      return;
    }

    final GafCode[] availableCodes = checker.getAvailableHyks();

    for( final String code : unknownCodes )
    {
      new Label( group, SWT.NONE ).setText( String.format( "\"%s\" = ", code ) ); //$NON-NLS-1$

      final IObservableValue model = new HykObservableValue( checker, code );
      createCodeMappingCombo( group, availableCodes, model );
    }
  }

  private void createRoughnesGroup( final Composite parent )
  {
    final GafPointCheck checker = m_data.getPointChecker();
    final String[] unknownCodes = checker.getUnknownRoughnes();

    final Group group = createUnknownGroup( parent );
    group.setText( Messages.getString( "GafOptionsPage.9" ) ); //$NON-NLS-1$

    if( unknownCodes.length == 0 )
    {
      createOkCodeControl( group, Messages.getString( "GafOptionsPage.10" ) ); //$NON-NLS-1$
      return;
    }

    final Roughness[] availableCodes = checker.getAvailableRoughness();

    for( final String code : unknownCodes )
    {
      new Label( group, SWT.NONE ).setText( String.format( "\"%s\" = ", code ) ); //$NON-NLS-1$

      final IObservableValue model = new RoughnessObservableValue( checker, code );
      createCodeMappingCombo( group, availableCodes, model );
    }
  }

  private void createVegetationGroup( final Composite parent )
  {
    final GafPointCheck checker = m_data.getPointChecker();
    final String[] unknownCodes = checker.getUnknownVegetation();

    final Group group = createUnknownGroup( parent );
    group.setText( Messages.getString( "GafOptionsPage.12" ) ); //$NON-NLS-1$

    if( unknownCodes.length == 0 )
    {
      createOkCodeControl( group, Messages.getString( "GafOptionsPage.13" ) ); //$NON-NLS-1$
      return;
    }

    final Vegetation[] availableVegetation = checker.getAvailableVegetation();

    for( final String code : unknownCodes )
    {
      new Label( group, SWT.NONE ).setText( String.format( "\"%s\" = ", code ) ); //$NON-NLS-1$

      final IObservableValue model = new VegetationObservableValue( checker, code );
      createCodeMappingCombo( group, availableVegetation, model );
    }
  }

  private Group createUnknownGroup( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).equalWidth( false ).applyTo( group );
    return group;
  }

  private void createCodeMappingCombo( final Group group, final Object[] availableCodes, final IObservableValue model )
  {
    final ComboViewer viewer = new ComboViewer( group, SWT.DROP_DOWN | SWT.READ_ONLY );
    viewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    viewer.getControl().setFont( JFaceResources.getTextFont() );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider() );
    viewer.setInput( availableCodes );
    viewer.setComparator( new ViewerComparableComparator() );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );

    final DataBinder dataBinder = new DataBinder( target, model );
    dataBinder.addTargetAfterGetValidator( new NotNullValidator<>( Object.class, IStatus.ERROR, Messages.getString( "GafOptionsPage.15" ) ) ); //$NON-NLS-1$

    m_binding.bindValue( dataBinder );
  }

  private void createOkCodeControl( final Composite parent, final String message )
  {
    final StatusComposite okControl = new StatusComposite( parent, SWT.NONE );
    okControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    okControl.setStatus( new Status( IStatus.OK, WspmPdbUiPlugin.PLUGIN_ID, message ) );
  }
}