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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.forms.DatabindingForm;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.map.widgets.SelectionWidget;
import org.kalypso.ogc.gml.widgets.IWidget;

/**
 * Control for selection the profile theme and the profile for channel generation.
 *
 * @author Gernot Belger
 */
class ProfileSelectionSection extends Composite
{
  public ProfileSelectionSection( final FormToolkit toolkit, final Composite parent, final ChannelEditData data, final DatabindingForm binding, final int spinnerWidth )
  {
    super( parent, SWT.NONE );

    toolkit.adapt( this );

    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( this );

    /* add combo-box for the wspm-profile theme selection */
    final Control themeControl = createThemeCombo( toolkit, this, data, binding );
    themeControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Button for the wspm-profile selection */
    final Control widgetToggle = createProfileSelectionWidgetButton( toolkit, this, data, binding );
    widgetToggle.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );

    /* spinner for specifying the global number of profile intersection points */
    createSegmentSpinner( toolkit, this, data, binding, spinnerWidth );
  }

  private Control createThemeCombo( final FormToolkit toolkit, final Composite parent, final ChannelEditData data, final DatabindingForm binding )
  {
    final ComboViewer themeChooser = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    toolkit.adapt( themeChooser.getControl(), true, false );

    themeChooser.setContentProvider( new ArrayContentProvider() );
    themeChooser.setLabelProvider( new LabelProvider() );

    /* Binding */
    final IObservableValue targetInput = ViewersObservables.observeInput( themeChooser );
    final IObservableValue modelInput = BeansObservables.observeValue( data, ChannelEditData.PROPERTY_PROFILE_THEME_INPUT );
    binding.bindValue( targetInput, modelInput );

    final IObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( themeChooser );
    final IObservableValue modelSelection = BeansObservables.observeValue( data, ChannelEditData.PROPERTY_PROFILE_THEME_SELECTED );
    binding.bindValue( targetSelection, modelSelection );

    final IObservableValue targetEnablement = SWTObservables.observeEnabled( themeChooser.getCombo() );
    final IObservableValue modelEnablement = BeansObservables.observeValue( data, ChannelEditData.PROPERTY_PROFILE_THEME_SELECTION_ENABLED );
    binding.bindValue( targetEnablement, modelEnablement );

    return themeChooser.getControl();
  }

  private Control createProfileSelectionWidgetButton( final FormToolkit toolkit, final Composite parent, final ChannelEditData data, final DatabindingForm binding )
  {
    final IWidget selectProfileWidget = new SelectionWidget( StringUtils.EMPTY, StringUtils.EMPTY, new ProfileSelectorFunction( data ) );

    final SetWidgetAction selectProfilesWidgetAction = new SetWidgetAction( data, selectProfileWidget );

    setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.33" ) ); //$NON-NLS-1$
    selectProfilesWidgetAction.setImageDescriptor( KalypsoModel1D2DPlugin.getImageProvider().getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.SELECT ) );

    return ChannelEditUtil.createWidgetSelectionButton( toolkit, parent, data, binding, selectProfilesWidgetAction, ChannelEditData.PROPERTY_SELECT_PROFILE_WIDGET_ENABLED );
  }

  private void createSegmentSpinner( final FormToolkit toolkit, final Composite parent, final ChannelEditData data, final DatabindingForm binding, final int spinnerWidth )
  {
    final Label spinnerLabel = toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.36" ) ); //$NON-NLS-1$
    spinnerLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Spinner spinNumProfIntersections = new Spinner( parent, SWT.BORDER );
    toolkit.adapt( spinNumProfIntersections );

    final GridData gridDataSpinner = new GridData( SWT.FILL, SWT.CENTER, false, false );
    gridDataSpinner.widthHint = spinnerWidth;
    spinNumProfIntersections.setLayoutData( gridDataSpinner );

    spinNumProfIntersections.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.37" ) ); //$NON-NLS-1$
    spinNumProfIntersections.setValues( 1, 4, 99, 0, 1, 10 );

    final ISWTObservableValue targetNumProfileSegments = SWTObservables.observeSelection( spinNumProfIntersections );
    final IObservableValue modelNumProfileSegments = BeansObservables.observeValue( data, ChannelEditData.PROPERTY_NUM_PROFILE_SEGMENTS );
    binding.bindValue( targetNumProfileSegments, modelNumProfileSegments );
  }
}