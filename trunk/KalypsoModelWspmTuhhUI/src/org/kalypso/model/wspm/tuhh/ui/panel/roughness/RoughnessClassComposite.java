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
package org.kalypso.model.wspm.tuhh.ui.panel.roughness;

import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.commons.databinding.AbstractDatabinding;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.WspmProject;
import org.kalypso.model.wspm.core.gml.classifications.IRoughnessClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.util.roughnesses.UpdateSimpleRoughnessProperty;
import org.kalypso.observation.result.IComponent;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dirk Kuch
 */
public class RoughnessClassComposite extends AbstractRoughnessComposite
{
  public static final String LABEL = "Roughness: Classes";

  private String[] m_roughnesses;

  public RoughnessClassComposite( final IProfil profile, final IComponent component )
  {
    super( profile, component );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.panel.roughness.AbstractRoughnessComposite#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return LABEL;
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.pager.IElementPage#render(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public void render( final Composite body, final FormToolkit toolkit )
  {
    final Group group = new Group( body, SWT.NULL );
    group.setLayout( new GridLayout( 2, false ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( "Flow Zone Roughness" );
    toolkit.adapt( group );

    setBinding( new AbstractDatabinding( toolkit )
    {
    } );

    // TODO validators
    build( group, toolkit, "Left Flood-Plain", ProfileRoguhnessesDataModel.PROPERTY_LEFT_FLOODPLAIN_CLASS, null );
    build( group, toolkit, "River Tube", ProfileRoguhnessesDataModel.PROPERTY_RIVER_TUBE_CLASS, null );
    build( group, toolkit, "Right Flood-Plain", ProfileRoguhnessesDataModel.PROPERTY_RIGHT_FLOODPLAIN_CLASS, null );

    final ImageHyperlink lnkRemove = toolkit.createImageHyperlink( group, SWT.NULL );
    lnkRemove.setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false, 2, 0 ) );
    lnkRemove.setText( String.format( "Remove: %s", getLabel() ) );

    lnkRemove.addHyperlinkListener( new HyperlinkAdapter()
    {
      @Override
      public void linkActivated( final org.eclipse.ui.forms.events.HyperlinkEvent e )
      {
        RoughnessPanelHelper.removeRoughness( getProfile(), getComponent().getId() );
      }
    } );

    /** additional actions */
    if( hasActions() )
    {

      final Group grActions = new Group( body, SWT.NULL );
      grActions.setLayout( new GridLayout() );
      grActions.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
      grActions.setText( "Additional Actions" );

      if( Objects.isNotNull( getProfile().hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS ) ) )
        addWriteValueLink( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS, grActions, toolkit, "Update ks values from mapped roughness class" );

      if( Objects.isNotNull( getProfile().hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST ) ) )
        addWriteValueLink( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST, grActions, toolkit, "Update kst values from mapped roughness class" );

      toolkit.adapt( grActions );
      grActions.layout();
    }

    body.layout();
  }

  private void addWriteValueLink( final String property, final Composite body, final FormToolkit toolkit, final String label )
  {
    final ImageHyperlink lnk = toolkit.createImageHyperlink( body, SWT.NULL );
    lnk.setText( label );

    lnk.addHyperlinkListener( new HyperlinkAdapter()
    {
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        final boolean overwriteValues = MessageDialog.openQuestion( lnk.getShell(), "Overwrite", "Overwrite existing roughness values?" );

        final UpdateSimpleRoughnessProperty worker = new UpdateSimpleRoughnessProperty( getProfile(), property, overwriteValues );
        ProgressUtilities.busyCursorWhile( worker );

      }
    } );

  }

  private boolean hasActions( )
  {
    final IProfil profile = getProfile();

    if( Objects.isNotNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS ) ) )
      return true;
    else if( Objects.isNotNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST ) ) )
      return true;

    return false;
  }

  @Override
  protected void build( final Composite body, final FormToolkit toolkit, final String label, final String property, final IValidator validator )
  {
    toolkit.createLabel( body, label );

    final ComboViewer viewer = new ComboViewer( body, SWT.READ_ONLY | SWT.SINGLE | SWT.BORDER );
    viewer.getCombo().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider() );

    viewer.setInput( getRoughnessClasses() );

    viewer.setSelection( new StructuredSelection( property ) );

    final ISWTObservableValue targetValue = SWTObservables.observeSelection( viewer.getCombo() );
    final IObservableValue modelValue = getModel().getObservableValue( property );

    getBinding().bindValue( new DataBinder( targetValue, modelValue ) );
  }

  private String[] getRoughnessClasses( )
  {
    if( m_roughnesses != null )
      return m_roughnesses;

    final IProfil profile = getProfile();
    final Object source = profile.getSource();
    if( !(source instanceof Feature) )
      return new String[] {};

    final Feature feature = (Feature) source;
    final GMLWorkspace workspace = feature.getWorkspace();
    final Feature root = workspace.getRootFeature();
    if( !(root instanceof WspmProject) )
      return new String[] {};

    final WspmProject project = (WspmProject) root;
    final IWspmClassification classifications = project.getClassificationMember();

    final Set<String> roughnesses = new TreeSet<String>();

    final IRoughnessClass[] classes = classifications.getRoughnessClasses();
    for( final IRoughnessClass clazz : classes )
    {
      roughnesses.add( clazz.getName() );
    }

    m_roughnesses = roughnesses.toArray( new String[] {} );

    return m_roughnesses;
  }
}
