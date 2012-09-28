/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.ui.panel;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.SimpleDataBinding;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.eclipse.swt.widgets.ComboMessageFocusListener;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.changes.ProfileChangeHint;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.ENERGYLOSS_TYPE;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.Energyloss;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.EnergylossProfileObject;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.panel.energyloss.EnergylossDataModel;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.ui.editor.styleeditor.binding.SLDBinding;

/**
 * @author kimwerner
 */
public class EnergylossPanel extends AbstractProfilView
{
  private FormToolkit m_toolkit = null;

  protected Composite m_propPanel;

  private DoubleModifyListener m_doubleModifyListener = null;

  private ComboMessageFocusListener m_comboFocusListener = null;

  public EnergylossPanel( final IProfile profile )
  {
    super( profile );
  }

  @Override
  protected Control doCreateControl( final Composite parent, final FormToolkit toolkit )
  {
    m_toolkit = toolkit;
    m_propPanel = m_toolkit.createComposite( parent );

    if( m_propPanel == null )
    {
      m_toolkit.createText( m_propPanel, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.EnergylossPanel.0" ) ); //$NON-NLS-1$
    }
    else
    {
      m_propPanel.setLayout( new GridLayout( 2, false ) );
    }
    updateControls();
    return m_propPanel;
  }

  private final EnergylossProfileObject getEnergyloss( )
  {
    final EnergylossProfileObject[] enlosses = getProfile().getProfileObjects( EnergylossProfileObject.class );
    return enlosses.length == 0 ? null : enlosses[0];
  }

  // FIXME: makes no sense in mixing databinding and modify listeners
  private final DoubleModifyListener getModifyListener( )
  {
    if( m_doubleModifyListener == null && m_propPanel != null )
    {
      final Display display = m_propPanel.getDisplay();
      final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
      final Color badColor = display.getSystemColor( SWT.COLOR_RED );
      m_doubleModifyListener = new DoubleModifyListener( goodColor, badColor );
    }
    return m_doubleModifyListener;
  }

  private final ComboMessageFocusListener getComboMessageListener( )
  {
    if( m_comboFocusListener == null && m_propPanel != null )
    {
      final Display display = m_propPanel.getDisplay();
      final Color textColor = display.getSystemColor( SWT.COLOR_BLACK );
      final Color messageColor = display.getSystemColor( SWT.COLOR_DARK_GRAY );
      m_comboFocusListener = new ComboMessageFocusListener( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.EnergylossPanel.3" ), messageColor, textColor ); //$NON-NLS-1$
    }
    return m_comboFocusListener;
  }

  private void buildText( final IDataBinding binding, final EnergylossDataModel model )
  {
    final Text text = m_toolkit.createText( m_propPanel, StringUtils.EMPTY, SWT.BORDER | SWT.RIGHT );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setMessage( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.EnergylossPanel.1" ) ); //$NON-NLS-1$
    text.setText( model.getEnergylossValue() == null ? "" : model.getEnergylossValue().toString() ); //$NON-NLS-1$
    text.addModifyListener( getModifyListener() );
    final DataBinder binder = bind( text, model );
    binding.bindValue( binder );
  }

  private void buildCombo( final IDataBinding dataBinding, final EnergylossDataModel model )
  {
    final ComboViewer elType = new ComboViewer( m_propPanel, SWT.BORDER );
    elType.setContentProvider( new ArrayContentProvider() );
    elType.setInput( ArrayUtils.remove( ENERGYLOSS_TYPE.values(), 0 ) );
    final Combo combo = elType.getCombo();
    combo.setLayoutData( new GridData( GridData.FILL, GridData.CENTER, true, false ) );
    combo.setToolTipText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.EnergylossPanel.2" ) ); //$NON-NLS-1$

    // FIXME: makes no sense: data binding, but also a focus listener ?!?
    combo.addFocusListener( new FocusListener()
    {
      @Override
      public void focusGained( final FocusEvent e )
      {
        //
      }

      @Override
      public void focusLost( final FocusEvent e )
      {
        if( !(e.getSource() instanceof Combo) )
        {
          return;
        }

        if( ((Combo)e.getSource()).getSelectionIndex() > -1 || ((Combo)e.getSource()).getText() != "" ) //$NON-NLS-1$
        {
          model.fireObjectChange( ((Combo)e.getSource()).getText(), model.getEnergylossValue() );
        }
        else if( ((Combo)e.getSource()).getText() == "" ) //$NON-NLS-1$
        {
          model.removeEnergyloss();
        }
      }
    } );
    combo.addFocusListener( getComboMessageListener() );
    m_toolkit.adapt( elType.getCombo() );
    final DataBinder binder = bind( elType.getCombo(), model );

    dataBinding.bindValue( binder );
    m_comboFocusListener.init( combo );
  }

  private DataBinder bind( final Text textField, final EnergylossDataModel model )
  {
    final ISWTObservableValue targetValue = SWTObservables.observeText( textField, SLDBinding.TEXT_DEFAULT_EVENTS );
    final IObservableValue modelValue = BeansObservables.observeValue( model, EnergylossDataModel.PROPERTY_ENERGYLOSS_VALUE );
    return new DataBinder( targetValue, modelValue );
  }

  private DataBinder bind( final Combo combo, final EnergylossDataModel model )
  {
    final ISWTObservableValue targetValue = SWTObservables.observeSelection( combo );
    final IObservableValue modelValue = BeansObservables.observeValue( model, EnergylossDataModel.PROPERTY_ENERGYLOSS_TYPE );
    return new DataBinder( targetValue, modelValue );
  }

  private final void clearPanel( )
  {
    if( m_propPanel == null || m_propPanel.isDisposed() )
    {
      return;
    }
    for( final Control control : m_propPanel.getChildren() )
    {
      control.dispose();
    }
  }

  final private boolean isEinlauf( final Energyloss[] energylosses, final int index )
  {
    if( index < energylosses.length )
    {
      final Energyloss energyloss = energylosses[index];
      return ENERGYLOSS_TYPE.eEinlauf.getId().equals( energyloss.getType() );
    }

    return false;
  }

  protected void createPropertyPanel( )
  {
    final IDataBinding binding = new SimpleDataBinding( m_toolkit );

    final Energyloss[] energylosses = getEnergyloss().getEnergylosses();

    int maxParamIndex = 4;
    EnergylossDataModel dataModel = new EnergylossDataModel( getProfile(), maxParamIndex + 1 );
    for( int i = 0; i < maxParamIndex; i++ )
    {
      if( isEinlauf( energylosses, i ) )
      {
        dataModel = new EnergylossDataModel( getProfile(), i );
        maxParamIndex++;
      }
      else
      {
        final EnergylossDataModel model = new EnergylossDataModel( getProfile(), i );
        buildCombo( binding, model );
        buildText( binding, model );
      }
    }

    final Label label = m_toolkit.createLabel( m_propPanel, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.EnergylossPanel.4" ) + ": " + ENERGYLOSS_TYPE.eEinlauf.toString() );//$NON-NLS-1$ //$NON-NLS-2$
    label.setToolTipText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.EnergylossPanel.5" ) );//$NON-NLS-1$
    dataModel.setEnergylossType( ENERGYLOSS_TYPE.eEinlauf.getId() );
    buildText( binding, dataModel ); //$NON-NLS-1$
    m_propPanel.layout();
  }

  // FIXME: makes no sense: data binding and updating the control like this
  protected void updateControls( )
  {
    clearPanel();
    createPropertyPanel();
  }

  @Override
  public void onProfilChanged( final ProfileChangeHint hint )
  {
    if( hint.isObjectDataChanged() )
    {
      final Control control = getControl();
      if( control != null && !control.isDisposed() )
      {
        control.getDisplay().asyncExec( new Runnable()
        {
          @Override
          public void run( )
          {
            updateControls();
          }
        } );
      }
    }
  }
}