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
package org.kalypso.model.wspm.tuhh.ui.panel.vegetation.pages;

import java.util.Collections;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.SimpleDataBinding;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.ui.pager.AbstractElementPage;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.IVegetationClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.core.util.vegetation.UpdateVegetationProperties;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.panel.classifications.utils.ClassificationLabelProvider;
import org.kalypso.model.wspm.tuhh.ui.panel.classifications.utils.WspmClassificationClassesComparator;
import org.kalypso.model.wspm.tuhh.ui.panel.roughness.utils.RoughnessPanelHelper;
import org.kalypso.model.wspm.tuhh.ui.panel.vegetation.utils.VegetationsDataModel;
import org.kalypso.observation.result.IComponent;

/**
 * @author Dirk Kuch
 */
public class VegetationClassesPage extends AbstractElementPage
{
  protected final IProfile m_profile;

  private IDataBinding m_binding;

  private final VegetationsDataModel m_model;

  private IVegetationClass[] m_vegetations;

  public VegetationClassesPage( final IProfile profile, final IComponent component )
  {
    super( VegetationClassesPage.class.getName() );
    m_profile = profile;

    m_model = new VegetationsDataModel( profile, component );
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "VegetationClassesPage.0" ); //$NON-NLS-1$
  }

  @Override
  public void dispose( )
  {
  }

  @Override
  public void render( final Composite body, final FormToolkit toolkit )
  {
    final Group group = new Group( body, SWT.NULL );
    group.setLayout( new GridLayout( 2, false ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( Messages.getString( "VegetationClassesPage.1" ) ); //$NON-NLS-1$
    toolkit.adapt( group );

    // FIXME: probably should use DatabindingWizardPage
    m_binding = new SimpleDataBinding( toolkit );

    build( group, toolkit, Messages.getString( "VegetationClassesPage.2" ), VegetationsDataModel.PROPERTY_LEFT_FLOODPLAIN_CLASS ); //$NON-NLS-1$
    build( group, toolkit, Messages.getString( "VegetationClassesPage.4" ), VegetationsDataModel.PROPERTY_RIGHT_FLOODPLAIN_CLASS ); //$NON-NLS-1$

    final ImageHyperlink lnkRemove = toolkit.createImageHyperlink( group, SWT.NULL );
    lnkRemove.setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false, 2, 0 ) );
    lnkRemove.setText( String.format( Messages.getString( "VegetationClassesPage.5" ), getLabel() ) ); //$NON-NLS-1$

    lnkRemove.addHyperlinkListener( new HyperlinkAdapter()
    {
      @Override
      public void linkActivated( final org.eclipse.ui.forms.events.HyperlinkEvent e )
      {
        if( MessageDialog.openConfirm( lnkRemove.getShell(), Messages.getString("VegetationClassesPage.10"), Messages.getString("VegetationClassesPage.11") ) ) //$NON-NLS-1$ //$NON-NLS-2$
          RoughnessPanelHelper.removeRoughness( m_profile, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS );
      }
    } );

    if( hasActions() )
    {

      final Group grActions = new Group( body, SWT.NULL );
      grActions.setLayout( new GridLayout() );
      grActions.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
      grActions.setText( Messages.getString( "VegetationClassesPage.6" ) ); //$NON-NLS-1$

      addWriteValueLink( grActions, toolkit, Messages.getString( "VegetationClassesPage.7" ) ); //$NON-NLS-1$

      toolkit.adapt( grActions );
    }
  }

  private void addWriteValueLink( final Composite body, final FormToolkit toolkit, final String label )
  {
    final ImageHyperlink lnk = toolkit.createImageHyperlink( body, SWT.NULL );
    lnk.setText( label );

    lnk.addHyperlinkListener( new HyperlinkAdapter()
    {
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        final boolean overwriteValues = MessageDialog.openQuestion( lnk.getShell(), Messages.getString( "VegetationClassesPage.8" ), Messages.getString( "VegetationClassesPage.9" ) ); //$NON-NLS-1$ //$NON-NLS-2$

        final UpdateVegetationProperties worker = new UpdateVegetationProperties( m_profile, overwriteValues );
        ProgressUtilities.busyCursorWhile( worker );

        final ProfileOperation operation = new ProfileOperation( "updating vegatation  values", m_profile, true ); //$NON-NLS-1$
        operation.addChange( worker.getChanges() );

        new ProfileOperationJob( operation ).schedule();
      }
    } );

  }

  private boolean hasActions( )
  {
    return WspmClassifications.hasVegetationProperties( m_profile );
  }

  protected void build( final Composite body, final FormToolkit toolkit, final String label, final String property )
  {
    toolkit.createLabel( body, label );

    final ComboViewer viewer = new ComboViewer( body, SWT.READ_ONLY | SWT.SINGLE | SWT.BORDER );
    viewer.getCombo().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new ClassificationLabelProvider( m_profile, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS ) );

    viewer.setInput( getVegetationClasses() );

    final IObservableValue viewerSelection = ViewersObservables.observeSingleSelection( viewer );
    final IObservableValue modelValue = m_model.getObservableValue( property );

    m_binding.bindValue( new DataBinder( viewerSelection, modelValue ) );
  }

  private IVegetationClass[] getVegetationClasses( )
  {
    if( m_vegetations != null )
      return m_vegetations;

    final IWspmClassification classification = WspmClassifications.getClassification( m_profile );
    if( Objects.isNull( classification ) )
      return new IVegetationClass[] {};

    final Set<IVegetationClass> vegetations = new TreeSet<>( new WspmClassificationClassesComparator() );
    Collections.addAll( vegetations, classification.getVegetationClasses() );
    m_vegetations = vegetations.toArray( new IVegetationClass[] {} );

    return m_vegetations;
  }
}
