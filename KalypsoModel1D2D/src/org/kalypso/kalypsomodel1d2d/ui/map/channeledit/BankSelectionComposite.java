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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.forms.DatabindingForm;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditData.SIDE;
import org.kalypso.ogc.gml.map.widgets.SelectionWidget;
import org.kalypso.ogc.gml.widgets.IWidget;

/**
 * @author Gernot Belger
 */
class BankSelectionComposite extends Composite
{
  BankSelectionComposite( final FormToolkit toolkit, final Composite parent, final ChannelEditData data, final DatabindingForm binding, final int spinnerWidth )
  {
    super( parent, SWT.NONE );

    toolkit.adapt( this );

    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( this );

    createBankSelectionLine( toolkit, this, data, binding, SIDE.LEFT, ChannelEditData.PROPERTY_BANK_THEME_SELECTED_LEFT );
    createBankSelectionLine( toolkit, this, data, binding, SIDE.RIGHT, ChannelEditData.PROPERTY_BANK_THEME_SELECTED_RIGHT );

    /* spinner for number for bank parts */
    createNumberSegmentsControl( toolkit, this, data, binding, spinnerWidth );
  }

  private void createBankSelectionLine( final FormToolkit toolkit, final Composite parent, final ChannelEditData data, final DatabindingForm binding, final SIDE side, final String selectedBankThemeProperty )
  {
    final ComboViewer themeChooser = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    toolkit.adapt( themeChooser.getControl(), true, false );
    themeChooser.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    themeChooser.setContentProvider( new ArrayContentProvider() );
    themeChooser.setLabelProvider( new LabelProvider() );

    final IObservableValue targetInput = ViewersObservables.observeInput( themeChooser );
    final IObservableValue modelInput = BeansObservables.observeValue( data, ChannelEditData.PROPERTY_BANK_THEME_INPUT );
    binding.bindValue( targetInput, modelInput );

    final IObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( themeChooser );
    final IObservableValue modelSelection = BeansObservables.observeValue( data, selectedBankThemeProperty );
    binding.bindValue( targetSelection, modelSelection );

    final IObservableValue targetEnabled = SWTObservables.observeEnabled( themeChooser.getControl() );
    final IObservableValue modelEnabled = BeansObservables.observeValue( data, ChannelEditData.PROPERTY_BANK_THEME_SELECTION_ENABLED );
    binding.bindValue( targetEnabled, modelEnabled );

    /* Button for the bank selection */
    final IWidget selectFromThemeWidget = new SelectionWidget( StringUtils.EMPTY, StringUtils.EMPTY, new BankSelectorFunction( data, side, modelSelection ) );
    final SetWidgetAction selectFromThemeAction = new SetWidgetAction( data, selectFromThemeWidget );
    selectFromThemeAction.setImageDescriptor( KalypsoModel1D2DPlugin.getImageProvider().getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.SELECT ) );

    final Button selectFromThemeButton = ChannelEditUtil.createWidgetSelectionButton( toolkit, parent, data, binding, selectFromThemeAction, ChannelEditData.PROPERTY_BANK_THEME_SELECTION_ENABLED );
    selectFromThemeButton.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.19" ) ); //$NON-NLS-1$
    selectFromThemeButton.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );

    /* Button for the bank drawing */
    final IWidget drawBankWidget = new DrawBanklineWidget( data, side );
    final SetWidgetAction drawBankAction = new SetWidgetAction( data, drawBankWidget );
    drawBankAction.setImageDescriptor( KalypsoModel1D2DPlugin.getImageProvider().getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.EDIT ) );

    final Button drawBankButton = ChannelEditUtil.createWidgetSelectionButton( toolkit, parent, data, binding, drawBankAction, null );
    drawBankButton.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );
  }

  private void createNumberSegmentsControl( final FormToolkit toolkit, final Composite parent, final ChannelEditData data, final DatabindingForm binding, final int spinnerWidth )
  {
    final Label label = toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.28" ) ); //$NON-NLS-1$
    label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Spinner spinNumberSegments = new Spinner( parent, SWT.BORDER );
    toolkit.adapt( spinNumberSegments );

    final GridData gridDataSpin = new GridData( SWT.FILL, SWT.CENTER, false, false, 2, 1 );
    gridDataSpin.widthHint = spinnerWidth;
    spinNumberSegments.setLayoutData( gridDataSpin );

    spinNumberSegments.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.29" ) ); //$NON-NLS-1$
    spinNumberSegments.setValues( 1, 2, 99, 0, 1, 10 );

    final ISWTObservableValue targetNumBankSegments = SWTObservables.observeSelection( spinNumberSegments );
    final IObservableValue modelNumBankSegments = BeansObservables.observeValue( data, ChannelEditData.PROPERTY_NUM_BANK_SEGMENTS );
    binding.bindValue( targetNumBankSegments, modelNumBankSegments );
  }
}