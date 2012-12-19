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

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.forms.DatabindingForm;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.widgets.IWidget;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
class BankOptionsSection extends Composite
{
  public BankOptionsSection( final FormToolkit toolkit, final Composite parent, final ChannelEditData data, final DatabindingForm binding, final int spinnerWidth )
  {
    super( parent, SWT.NONE );

    toolkit.adapt( this );

    GridLayoutFactory.fillDefaults().numColumns( 2 ).applyTo( this );

    createControls( toolkit, this, data, binding, spinnerWidth );
  }

  private void createControls( final FormToolkit toolkit, final Composite parent, final ChannelEditData data, final DatabindingForm binding, final int spinnerWidth )
  {
    final String labelDown = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.4" ); //$NON-NLS-1$
    final String tooltipDown = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.5" ); //$NON-NLS-1$

    final String labelUp = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.6" ); //$NON-NLS-1$
    final String tooltipUp = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.7" ); //$NON-NLS-1$

    final Color colorCyan = new Color( parent.getDisplay(), 128, 255, 255 );
    final Color colorMagenta = new Color( parent.getDisplay(), 255, 128, 255 );

    createNumberOfSegmentsSpinner( toolkit, parent, data, binding, spinnerWidth, labelDown, tooltipDown, ChannelEditData.PROPERTY_NUM_BANK_SEGMENTS_ENABLED_DOWN, ChannelEditData.PROPERTY_NUM_BANK_SEGMENTS_DOWN, colorCyan );
    createNumberOfSegmentsSpinner( toolkit, parent, data, binding, spinnerWidth, labelUp, tooltipUp, ChannelEditData.PROPERTY_NUM_BANK_SEGMENTS_ENABLED_UP, ChannelEditData.PROPERTY_NUM_BANK_SEGMENTS_UP, colorMagenta );

    createBankEditToggle( toolkit, parent, data, binding );

    parent.addDisposeListener( new DisposeListener()
    {
      @Override
      public void widgetDisposed( final DisposeEvent e )
      {
        colorCyan.dispose();
        colorMagenta.dispose();
      }
    } );
  }

  private void createNumberOfSegmentsSpinner( final FormToolkit toolkit, final Composite parent, final ChannelEditData data, final DatabindingForm binding, final int spinnerWidth, final String label, final String tooltip, final String enabledProperty, final String numberProperty, final Color background )
  {
    final Label labelNumIntersSegment = toolkit.createLabel( parent, label );
    labelNumIntersSegment.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    labelNumIntersSegment.setToolTipText( tooltip );

    /* spinner for specifiying the number of intersection points for the current segment */
    final Spinner spinnerNumProfileSegments = new Spinner( parent, SWT.BORDER );
    toolkit.adapt( spinnerNumProfileSegments );

    spinnerNumProfileSegments.setBackground( background );

    final GridData gridDataNumProfileSegmentsSpinner = new GridData( SWT.RIGHT, SWT.CENTER, true, false );
    gridDataNumProfileSegmentsSpinner.widthHint = spinnerWidth;
    spinnerNumProfileSegments.setLayoutData( gridDataNumProfileSegmentsSpinner );

    spinnerNumProfileSegments.setToolTipText( tooltip );
    spinnerNumProfileSegments.setValues( 1, 2, 99, 0, 1, 10 );

    /* disable AND hide spinner if not enabled */
    final ISWTObservableValue targetNumSegmentsVisible = SWTObservables.observeVisible( spinnerNumProfileSegments );
    final IObservableValue modelProfileEditingVisible = BeansObservables.observeValue( data, enabledProperty );
    binding.bindValue( targetNumSegmentsVisible, modelProfileEditingVisible );

    final ISWTObservableValue targetNumSegmentsEnabled = SWTObservables.observeEnabled( spinnerNumProfileSegments );
    final IObservableValue modelProfileEditingEnabled = BeansObservables.observeValue( data, enabledProperty );
    binding.bindValue( targetNumSegmentsEnabled, modelProfileEditingEnabled );

    final ISWTObservableValue targetNumSegmentsValue = SWTObservables.observeSelection( spinnerNumProfileSegments );

    final IObservableValue modelNumProfileSegments = BeansObservables.observeValue( data, numberProperty );
    binding.bindValue( targetNumSegmentsValue, modelNumProfileSegments );
  }

  private void createBankEditToggle( final FormToolkit toolkit, final Composite parent, final ChannelEditData data, final DatabindingForm binding )
  {
    /* edit button for bankline 1 */
    final Label labelBankline = toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.8" ) ); //$NON-NLS-1$
    labelBankline.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final IWidget editBanksWidget = new DragBankLineWidget( data );
    final SetWidgetAction editBankWidgetAction = new SetWidgetAction( data, editBanksWidget );
    editBankWidgetAction.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.9" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    editBankWidgetAction.setImageDescriptor( imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.EDIT ) );

    final Button buttonEditBank = ChannelEditUtil.createWidgetSelectionButton( toolkit, parent, data, binding, editBankWidgetAction, ChannelEditData.PROPERTY_PROFILE_EDITING_ENABLED );
    buttonEditBank.setLayoutData( new GridData( SWT.END, SWT.CENTER, true, false ) );
  }
}